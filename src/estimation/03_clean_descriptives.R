# Paper 3 Stage 3 clean reproduction runner. Generated from the immutable Phase III script; only the Phase II audit path is parameterised.
# Reads immutable Phase II inputs and writes separately versioned outputs only.

required <- c("haven", "dplyr", "tidyr", "readr", "survey", "purrr", "stringr")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) stop("Missing packages: ", paste(missing, collapse = ", "))
invisible(lapply(required, library, character.only = TRUE))
options(survey.lonely.psu = "adjust")

project_dir <- Sys.getenv("POSTHARVEST_PROJECT_DIR")
raw_dir <- Sys.getenv("GHS_RAW_DIR")
if (!nzchar(project_dir) || !dir.exists(project_dir)) stop("Set POSTHARVEST_PROJECT_DIR to the Paper 3 directory.")
if (!nzchar(raw_dir) || !dir.exists(raw_dir)) stop("Set GHS_RAW_DIR to the private GHS raw-data directory.")
out_dir <- file.path(project_dir, "phase_iii_controlled_descriptive_interpretation", "outputs")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

num <- function(x) suppressWarnings(as.numeric(haven::zap_labels(x)))
read_source <- function(name) haven::read_dta(file.path(raw_dir, name))
input_path <- file.path(project_dir, "data", "private", "postharvest_labour_records_private.rds")
if (!file.exists(input_path)) stop("Immutable locked analytic RDS not found.")

# Household base is retained for valid survey-design variance estimation: households
# without a qualifying arrangement contribute zero to both numerator and denominator.
households_raw <- bind_rows(
  read_source("secta_harvestw4.dta") %>% transmute(wave=4L, hhid=as.character(hhid), cluster, strata, household_weight=num(wt_wave4), rural=case_when(num(sector)==2 ~ "rural", num(sector)==1 ~ "urban", TRUE ~ NA_character_)),
  read_source("secta_harvestw5.dta") %>% transmute(wave=5L, hhid=as.character(hhid), cluster, strata, household_weight=num(wt_cross_wave5), rural=case_when(num(sector)==2 ~ "rural", num(sector)==1 ~ "urban", TRUE ~ NA_character_))
) %>% distinct(wave, hhid, .keep_all=TRUE)
if (anyDuplicated(households_raw[c("wave","hhid")])) stop("Duplicate household keys in cover-file base.")
cover_weight_audit <- households_raw %>% group_by(wave) %>% summarise(
  cover_households=n(), invalid_or_missing_weight=sum(is.na(household_weight)|household_weight<=0), .groups="drop")
households <- households_raw %>% filter(!is.na(household_weight), household_weight > 0)

records <- readr::read_rds(input_path) %>%
  filter(analytic_record == 1) %>%
  mutate(hhid=as.character(hhid),
         arrangement_key=paste(wave, hhid, cropcode, ifelse(is.na(indiv), "external", as.character(indiv)),
                               worker_arrangement, coalesce(worker_group, "household"), sep="|"))

worker_crop <- records %>%
  distinct(wave, hhid, cropcode, indiv, worker_arrangement, worker_group, member_sex,
           member_age_group, state, lga, rural, payment_status, household_weight,
           days_per_worker, hours_per_day, worker_count, daily_payment, person_hours,
           arrangement_key) %>%
  mutate(rural=case_when(num(rural) == 1 ~ "rural", num(rural) == 0 ~ "urban", TRUE ~ NA_character_),
         worker_type=case_when(
    worker_arrangement == "household_member" & member_age_group == "child_under_15" ~ "household member: child age 5–14",
    worker_arrangement == "household_member" & member_sex == "man" ~ "household member: man age 15+",
    worker_arrangement == "household_member" & member_sex == "woman" ~ "household member: woman age 15+",
    worker_arrangement == "hired_paid" & worker_group == "man" ~ "hired external: man age 15+",
    worker_arrangement == "hired_paid" & worker_group == "woman" ~ "hired external: woman age 15+",
    worker_arrangement == "hired_paid" & worker_group == "child_under_15" ~ "hired external: child under 15",
    worker_arrangement == "outside_household_unpaid" & worker_group == "man" ~ "unpaid/exchange external: man age 15+",
    worker_arrangement == "outside_household_unpaid" & worker_group == "woman" ~ "unpaid/exchange external: woman age 15+",
    worker_arrangement == "outside_household_unpaid" & worker_group == "child_under_15" ~ "unpaid/exchange external: child under 15",
    TRUE ~ "unclassified"
  ))
if (any(worker_crop$person_hours <= 0 | is.na(worker_crop$person_hours))) stop("Invalid arrangement-level person-hours.")
if (any(worker_crop$worker_type == "unclassified")) stop("Unclassified worker type in eligible data.")
if (nrow(anti_join(worker_crop %>% distinct(wave,hhid), households %>% distinct(wave,hhid), by=c("wave","hhid"))) > 0) {
  stop("An eligible analytic arrangement lacks a valid-weight household-frame record.")
}

task_map <- records %>% distinct(arrangement_key, task)
task_combinations <- task_map %>% group_by(arrangement_key) %>%
  summarise(task_combination=paste(sort(task), collapse=" + "), .groups="drop")
worker_crop <- worker_crop %>% left_join(task_combinations, by="arrangement_key")
if (any(is.na(worker_crop$task_combination))) stop("Missing task combination in eligible arrangement.")

design_for <- function(frame) {
  survey::svydesign(ids=~cluster, strata=~strata, weights=~household_weight, nest=TRUE, data=frame)
}

# Estimating a category's contribution divided by the full relevant arrangement or
# hours total. Zero contributions are included for all surveyed households in domain.
estimate_shares <- function(contributions, table_id, metric, domain_label="all", denominator_contributions=NULL) {
  stopifnot(all(c("wave","hhid","category","value") %in% names(contributions)))
  if (is.null(denominator_contributions)) denominator_contributions <- contributions
  stopifnot(all(c("wave","hhid","value") %in% names(denominator_contributions)))
  purrr::map_dfr(sort(unique(contributions$wave)), function(w) {
    base <- households %>% filter(wave == w)
    if (domain_label != "all") base <- base %>% filter(rural == domain_label)
    source <- contributions %>% filter(wave == w)
    denominator_source <- denominator_contributions %>% filter(wave == w)
    if (domain_label != "all") source <- source %>% filter(rural == domain_label)
    if (domain_label != "all") denominator_source <- denominator_source %>% filter(rural == domain_label)
    denom <- denominator_source %>% group_by(hhid) %>% summarise(denominator=sum(value), .groups="drop")
    cats <- sort(unique(source$category))
    purrr::map_dfr(cats, function(cat) {
      numerator <- source %>% filter(category == cat) %>% group_by(hhid) %>% summarise(numerator=sum(value), .groups="drop")
      frame <- base %>% select(hhid,cluster,strata,household_weight) %>%
        left_join(denom, by="hhid") %>% left_join(numerator, by="hhid") %>%
        mutate(denominator=coalesce(denominator,0), numerator=coalesce(numerator,0))
      des <- design_for(frame)
      ratio <- survey::svyratio(~numerator, ~denominator, des)
      total <- survey::svytotal(~numerator, des)
      tibble(table_id=table_id, metric=metric, wave=w, domain=domain_label, category=cat,
             unweighted_contribution=sum(source$value[source$category == cat]),
             unweighted_denominator=sum(denominator_source$value),
             weighted_contribution=unname(coef(total)[1]),
             weighted_denominator=unname(coef(survey::svytotal(~denominator, des))[1]),
             estimate=unname(coef(ratio)[1]), se=unname(survey::SE(ratio)[1]),
             ci_low=max(0,unname(coef(ratio)[1])-1.96*unname(survey::SE(ratio)[1])),
             ci_high=min(1,unname(coef(ratio)[1])+1.96*unname(survey::SE(ratio)[1])),
             n_households=nrow(base), n_psu=dplyr::n_distinct(base$cluster), n_strata=dplyr::n_distinct(base$strata))
    })
  })
}

arr_count <- worker_crop %>% transmute(wave,hhid,rural,category=worker_arrangement,value=1)
type_count <- worker_crop %>% transmute(wave,hhid,rural,category=worker_type,value=1)
payment_count <- worker_crop %>% transmute(wave,hhid,rural,category=payment_status,value=1)
arr_hours <- worker_crop %>% transmute(wave,hhid,rural,category=worker_arrangement,value=person_hours)
type_hours <- worker_crop %>% transmute(wave,hhid,rural,category=worker_type,value=person_hours)
task_count <- task_map %>% left_join(worker_crop %>% select(arrangement_key,wave,hhid,rural),by="arrangement_key") %>%
  transmute(wave,hhid,rural,category=task,value=1)
combo_count <- worker_crop %>% transmute(wave,hhid,rural,category=task_combination,value=1)

descriptives <- bind_rows(
  estimate_shares(arr_count,"T1_arrangement_composition","arrangement_count"),
  estimate_shares(type_count,"T2_worker_type_composition","arrangement_count"),
  estimate_shares(payment_count,"T3_payment_status_composition","arrangement_count"),
  estimate_shares(arr_hours,"T4_arrangement_hours_share","arrangement_hours"),
  estimate_shares(type_hours,"T5_worker_type_hours_share","arrangement_hours"),
  estimate_shares(task_count,"T6_multiple_response_task_incidence","arrangement_count", denominator_contributions=arr_count),
  estimate_shares(combo_count,"T7_task_combination_composition","arrangement_count"),
  estimate_shares(arr_count,"T8_rural_arrangement_composition","arrangement_count","rural"),
  estimate_shares(arr_count,"T8_rural_arrangement_composition","arrangement_count","urban")
) %>% mutate(estimate_percent=100*estimate, se_percent=100*se, ci_low_percent=100*ci_low, ci_high_percent=100*ci_high)

# Validated unit-accounting and unique identifiable-person checks.
unit_audit <- bind_rows(
  worker_crop %>% count(wave, name="count") %>% mutate(measure="worker_crop_arrangements"),
  records %>% count(wave, name="count") %>% mutate(measure="task_expanded_records"),
  worker_crop %>% filter(worker_arrangement=="household_member") %>% distinct(wave,hhid,indiv) %>% count(wave,name="count") %>% mutate(measure="unique_identifiable_household_members"),
  worker_crop %>% filter(worker_arrangement!="household_member") %>% count(wave,name="count") %>% mutate(measure="external_person_type_crop_arrangements_not_unique_people"),
  worker_crop %>% group_by(wave) %>% summarise(count=sum(person_hours),.groups="drop") %>% mutate(measure="arrangement_level_unweighted_person_hours"),
  records %>% group_by(wave) %>% summarise(count=sum(person_hours),.groups="drop") %>% mutate(measure="task_expanded_repeated_person_hours_PROHIBITED")
)

# Count-only upper-bound sensitivity: no unobserved task, worker count, hours, or
# payment is imputed. It only shows the maximum influence on W4 arrangement counts
# if every unresolved external person-type/crop record represented an arrangement.
audit_project_dir <- Sys.getenv("PAPER3_FROZEN_PROJECT_DIR")
if (!nzchar(audit_project_dir)) stop("Set PAPER3_FROZEN_PROJECT_DIR to locate the immutable Phase II external-screener audit.")
external_audit <- readr::read_csv(file.path(audit_project_dir,"phase_ii_descriptive_measurement_audit","outputs","PAPER_3_EXTERNAL_SCREENER_AUDIT.csv"),show_col_types=FALSE)
unresolved <- external_audit %>% filter(wave==4, status %in% c("missing_screener_no_followup","screener_yes_missing_worker_count")) %>%
  group_by(worker_arrangement) %>% summarise(unresolved_slots=sum(source_group_crop_records),.groups="drop")
w4_arr <- worker_crop %>% filter(wave==4) %>% count(worker_arrangement,name="observed_arrangements")
w4_total <- sum(w4_arr$observed_arrangements); unresolved_total <- sum(unresolved$unresolved_slots)
external_sensitivity <- w4_arr %>% full_join(unresolved,by="worker_arrangement") %>% mutate(unresolved_slots=coalesce(unresolved_slots,0L),
  observed_share=observed_arrangements/w4_total,
  maximum_count_only_share=(observed_arrangements+unresolved_slots)/(w4_total+unresolved_total),
  sensitivity_type="logical count-only upper bound; no task, worker count, hours, payment, or composition imputed") %>%
  bind_rows(tibble(worker_arrangement="all_external",observed_arrangements=sum(w4_arr$observed_arrangements[w4_arr$worker_arrangement!="household_member"]),
                   unresolved_slots=unresolved_total,
                   observed_share=sum(w4_arr$observed_arrangements[w4_arr$worker_arrangement!="household_member"])/w4_total,
                   maximum_count_only_share=(sum(w4_arr$observed_arrangements[w4_arr$worker_arrangement!="household_member"])+unresolved_total)/(w4_total+unresolved_total),
                   sensitivity_type="logical count-only upper bound; no task, worker count, hours, payment, or composition imputed"))

hours_validity <- tibble(
  check=c("hours_unit","task_hours","external_hours","household_hours","payment_unit"),
  status=c("PASS_WITH_QUALIFICATION","PROHIBITED","QUALIFIED","QUALIFIED","QUALIFIED"),
  decision=c("Use de-duplicated worker-crop arrangements only.",
             "Do not sum task-expanded person-hours or claim time shares by task.",
             "Count × days × typical hours is a reported group estimate, not an individual roster.",
             "Days × usual hours is associated with a member-crop arrangement, not individual task time.",
             "Daily hired payment is nominal conditional compensation, not a wage or total earnings.")
)

write_csv(descriptives, file.path(out_dir,"PAPER_3_PHASE_III_VALIDATED_DESCRIPTIVE_TABLES.csv"))
write_csv(unit_audit, file.path(out_dir,"PAPER_3_PHASE_III_UNIT_OF_ANALYSIS_AUDIT.csv"))
write_csv(external_sensitivity, file.path(out_dir,"PAPER_3_PHASE_III_EXTERNAL_WORKER_SENSITIVITY.csv"))
write_csv(hours_validity, file.path(out_dir,"PAPER_3_PHASE_III_HOURS_VALIDITY_REVIEW.csv"))
write_csv(cover_weight_audit, file.path(out_dir,"PAPER_3_PHASE_III_COVER_WEIGHT_AUDIT.csv"))
message("Phase III validated descriptive analysis complete: ", nrow(descriptives), " table rows; ", nrow(worker_crop), " worker-crop arrangements; ", nrow(records), " task-expanded records.")
