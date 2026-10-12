# Paper 3 Phase II: independent raw-source descriptive and measurement audit.
# This script does not overwrite locked construction or descriptive artifacts.

required <- c("haven", "dplyr", "tidyr", "readr", "survey")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) stop("Missing packages: ", paste(missing, collapse = ", "))

library(dplyr)
library(tidyr)
library(readr)
library(survey)

project_dir <- Sys.getenv("POSTHARVEST_PROJECT_DIR")
raw_dir <- Sys.getenv("GHS_RAW_DIR")
if (!nzchar(project_dir) || !dir.exists(project_dir)) stop("Set POSTHARVEST_PROJECT_DIR to the Paper 3 directory.")
if (!nzchar(raw_dir) || !dir.exists(raw_dir)) stop("Set GHS_RAW_DIR to the private GHS raw-data directory.")

out_dir <- file.path(project_dir, "phase_ii_descriptive_measurement_audit", "outputs")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

num <- function(x) suppressWarnings(as.numeric(haven::zap_labels(x)))
yes <- function(x) as.integer(num(x) == 1)
read_source <- function(name) haven::read_dta(file.path(raw_dir, name))
task_long <- function(data, cols, labels) {
  names(labels) <- cols
  data %>%
    pivot_longer(all_of(cols), names_to = "task_source", values_to = "task_flag") %>%
    mutate(task = unname(labels[task_source]), performed_task = yes(task_flag)) %>%
    select(-task_source, -task_flag)
}

labels4 <- c(phl_q01d__1 = "shelling_or_threshing", phl_q01d__2 = "drying",
             phl_q01d__3 = "cleaning", phl_q01d__4 = "processing")
labels5 <- c(phl_q4__1 = "shelling_or_threshing", phl_q4__2 = "sorting",
             phl_q4__3 = "drying", phl_q4__4 = "cleaning",
             phl_q4__5 = "processing", phl_q4__6 = "supervision")

hh <- bind_rows(
  read_source("secta_harvestw4.dta") %>% transmute(wave = 4L, hhid, cluster, strata, sector, household_weight = num(wt_wave4)),
  read_source("secta_harvestw5.dta") %>% transmute(wave = 5L, hhid, cluster, strata, sector, household_weight = num(wt_cross_wave5))
) %>% mutate(rural = case_when(num(sector) == 2 ~ 1L, num(sector) == 1 ~ 0L, TRUE ~ NA_integer_))

roster <- bind_rows(
  read_source("sect1_harvestw4.dta") %>% transmute(wave = 4L, hhid, indiv, member_sex = case_when(num(s1q2) == 1 ~ "man", num(s1q2) == 2 ~ "woman", TRUE ~ NA_character_), member_age = num(s1q4)),
  read_source("sect1_harvestw5.dta") %>% transmute(wave = 5L, hhid, indiv, member_sex = case_when(num(s1q2) == 1 ~ "man", num(s1q2) == 2 ~ "woman", TRUE ~ NA_character_), member_age = num(s1q6))
) %>% mutate(member_age_group = case_when(member_age < 15 ~ "child_under_15", member_age >= 15 ~ "adult", TRUE ~ NA_character_))

# Source uniqueness is verified separately before any record expansion.
key_audit <- bind_rows(
  tibble(wave=4L, source="PHL1 household", rows=nrow(read_source("sectaphl1_harvestw4.dta")), duplicate_keys=sum(duplicated(read_source("sectaphl1_harvestw4.dta")[c("hhid","cropcode","indiv")]))),
  tibble(wave=4L, source="PHL2 external", rows=nrow(read_source("sectaphl2_harvestw4.dta")), duplicate_keys=sum(duplicated(read_source("sectaphl2_harvestw4.dta")[c("hhid","cropcode")]))),
  tibble(wave=4L, source="Roster", rows=nrow(read_source("sect1_harvestw4.dta")), duplicate_keys=sum(duplicated(read_source("sect1_harvestw4.dta")[c("hhid","indiv")]))),
  tibble(wave=4L, source="Household cover", rows=nrow(read_source("secta_harvestw4.dta")), duplicate_keys=sum(duplicated(read_source("secta_harvestw4.dta")["hhid"]))),
  tibble(wave=5L, source="PHL1 household", rows=nrow(read_source("sectaphl1_harvestw5.dta")), duplicate_keys=sum(duplicated(read_source("sectaphl1_harvestw5.dta")[c("hhid","cropcode","indiv")]))),
  tibble(wave=5L, source="PHL2 external", rows=nrow(read_source("sectaphl2_harvestw5.dta")), duplicate_keys=sum(duplicated(read_source("sectaphl2_harvestw5.dta")[c("hhid","cropcode")]))),
  tibble(wave=5L, source="Roster", rows=nrow(read_source("sect1_harvestw5.dta")), duplicate_keys=sum(duplicated(read_source("sect1_harvestw5.dta")[c("hhid","indiv")]))),
  tibble(wave=5L, source="Household cover", rows=nrow(read_source("secta_harvestw5.dta")), duplicate_keys=sum(duplicated(read_source("secta_harvestw5.dta")["hhid"])))
)

h4_raw <- read_source("sectaphl1_harvestw4.dta")
h4 <- h4_raw %>%
  transmute(wave=4L, hhid, cropcode, indiv, worker_arrangement="household_member", worker_group=NA_character_,
            days_per_worker=num(phl_q01b), hours_per_day=num(phl_q01c), worker_count=1,
            daily_payment=NA_real_, across(all_of(names(labels4)), yes)) %>%
  mutate(participated=1L, person_hours=days_per_worker*hours_per_day) %>%
  task_long(names(labels4), labels4) %>%
  left_join(roster, by=c("wave","hhid","indiv"))

h5_raw <- read_source("sectaphl1_harvestw5.dta")
h5 <- h5_raw %>%
  transmute(wave=5L, hhid, cropcode, indiv, worker_arrangement="household_member", worker_group=NA_character_,
            days_per_worker=num(phl_q2), hours_per_day=num(phl_q3), worker_count=1,
            daily_payment=NA_real_, screener=num(phl_q1), across(all_of(names(labels5)), yes)) %>%
  mutate(participated=as.integer(screener==1 & days_per_worker>0 & hours_per_day>0),
         person_hours=days_per_worker*hours_per_day) %>%
  select(-screener) %>% task_long(names(labels5), labels5) %>%
  left_join(roster, by=c("wave","hhid","indiv"))

household_eligibility <- bind_rows(
  h4_raw %>%
    transmute(days=num(phl_q01b), hours=num(phl_q01c),
              task_any=rowSums(across(all_of(names(labels4)), ~num(.x)==1), na.rm=TRUE)>0) %>%
    mutate(status=case_when(days>0 & hours>0 & task_any ~ "eligible",
                            is.na(days)|is.na(hours) ~ "missing_duration_field",
                            days<=0|hours<=0 ~ "nonpositive_duration_field",
                            !task_any ~ "no_task_flag",
                            TRUE ~ "other_unresolved")) %>%
    count(status, name="count") %>% mutate(wave=4L, arrangement="household_member") %>%
    select(wave, arrangement, status, count),
  h5_raw %>%
    transmute(screener=num(phl_q1), days=num(phl_q2), hours=num(phl_q3),
              task_any=rowSums(across(starts_with("phl_q4__"), ~num(.x)==1), na.rm=TRUE)>0) %>%
    mutate(status=case_when(screener==2 ~ "screener_no",
                            screener==1 & days>0 & hours>0 & task_any ~ "eligible",
                            screener==1 ~ "screener_yes_incomplete_or_invalid",
                            TRUE ~ "missing_screener")) %>%
    count(status, name="count") %>% mutate(wave=5L, arrangement="household_member") %>%
    select(wave, arrangement, status, count)
)

external_audit <- list()
external_records <- list()
for (w in c(4L,5L)) {
  ext <- read_source(sprintf("sectaphl2_harvestw%d.dta",w))
  n_tasks <- if(w==4L) 4L else 6L
  task_names <- if(w==4L) unname(labels4) else unname(labels5)
  for (mode in c("hired_paid","outside_household_unpaid")) for (j in 1:3) {
    group <- c("man","woman","child_under_15")[j]
    screen_col <- if(mode=="hired_paid") paste0("phl_q02_",j) else paste0("phl_q08__",j)
    count_col <- if(mode=="hired_paid") paste0("phl_q03_",j) else paste0("phl_q09_",j)
    days_col <- if(mode=="hired_paid") paste0("phl_q04_",j) else paste0("phl_q10_",j)
    hours_col <- if(mode=="hired_paid") paste0("phl_q05_",j) else paste0("phl_q11_",j)
    payment_col <- if(mode=="hired_paid") paste0("phl_q06_",j) else NA_character_
    task_cols <- paste0(if(mode=="hired_paid") "phl_q07__" else "phl_q12__",seq_len(n_tasks),"_",j)
    screen <- num(ext[[screen_col]]); count <- num(ext[[count_col]])
    days <- num(ext[[days_col]]); hours <- num(ext[[hours_col]])
    payment <- if(is.na(payment_col)) rep(NA_real_,nrow(ext)) else num(ext[[payment_col]])
    task_matrix <- sapply(ext[task_cols], function(z) num(z)==1)
    any_task <- rowSums(task_matrix,na.rm=TRUE)>0
    followup_all_missing <- is.na(count) & is.na(days) & is.na(hours) & apply(is.na(task_matrix),1,all)
    status <- case_when(
      screen==2 ~ "screener_no",
      screen==1 & is.na(count) ~ "screener_yes_missing_worker_count",
      screen==1 & count<=0 ~ "screener_yes_nonpositive_worker_count",
      screen==1 & (is.na(days)|is.na(hours)|!any_task) ~ "screener_yes_incomplete_followup",
      screen==1 ~ "eligible",
      is.na(screen) & followup_all_missing ~ "missing_screener_no_followup",
      is.na(screen) ~ "missing_screener_other",
      TRUE ~ "other_unresolved"
    )
    external_audit[[length(external_audit)+1L]] <- tibble(wave=w, worker_arrangement=mode, worker_group=group, status=status) %>%
      count(wave,worker_arrangement,worker_group,status,name="source_group_crop_records")
    d <- tibble(wave=w, hhid=ext$hhid, cropcode=ext$cropcode, worker_arrangement=mode, worker_group=group,
                member_sex=ifelse(group=="child_under_15",NA_character_,group), member_age=NA_real_,
                member_age_group=ifelse(group=="child_under_15","child_under_15","adult"),
                worker_count=count,days_per_worker=days,hours_per_day=hours,daily_payment=payment,
                person_hours=count*days*hours,status=status)
    for(k in seq_along(task_cols)) d[[task_cols[k]]] <- as.integer(task_matrix[,k])
    external_records[[length(external_records)+1L]] <- d %>% filter(status=="eligible") %>%
      task_long(task_cols,stats::setNames(task_names,task_cols)) %>% mutate(participated=1L)
  }
}
external_audit <- bind_rows(external_audit)
external <- bind_rows(external_records)

independent <- bind_rows(h4,h5,external) %>%
  left_join(hh %>% select(wave,hhid,cluster,strata,rural,household_weight),by=c("wave","hhid")) %>%
  mutate(analytic_record=as.integer(participated==1 & performed_task==1 & !is.na(household_weight) & household_weight>0),
         payment_status=case_when(worker_arrangement=="hired_paid" ~ "paid",
                                  worker_arrangement=="outside_household_unpaid" ~ "unpaid",
                                  TRUE ~ "household_member")) %>%
  filter(analytic_record==1)

eligibility <- bind_rows(
  household_eligibility %>% mutate(stage="household_member_source",worker_group=NA_character_) %>%
    select(wave,stage,arrangement,worker_group,status,count),
  external_audit %>% transmute(wave,stage="external_screener",arrangement=worker_arrangement,worker_group,status,count=source_group_crop_records),
  independent %>% count(wave,worker_arrangement,name="count") %>%
    transmute(wave,stage="analytic_task_worker_records",arrangement=worker_arrangement,worker_group=NA_character_,status="eligible_task_records",count)
)

summary_task <- function(x) x %>% group_by(wave,worker_arrangement,worker_group,member_sex,member_age_group,payment_status,rural,task) %>%
  summarise(unweighted_task_records=n(),weighted_task_records=sum(household_weight),
            weighted_repeated_person_hours=sum(household_weight*person_hours),.groups="drop")

locked_path <- file.path(project_dir,"data","private","postharvest_labour_records_private.rds")
if(!file.exists(locked_path)) stop("Locked construction artifact unavailable.")
locked <- readr::read_rds(locked_path) %>% filter(analytic_record==1)
locked_summary <- summary_task(locked) %>% rename_with(~paste0("locked_",.x),starts_with("unweighted_")|starts_with("weighted_"))
independent_summary <- summary_task(independent) %>% rename_with(~paste0("independent_",.x),starts_with("unweighted_")|starts_with("weighted_"))
reproduction <- full_join(locked_summary,independent_summary,by=c("wave","worker_arrangement","worker_group","member_sex","member_age_group","payment_status","rural","task")) %>%
  mutate(across(starts_with("locked_"),~coalesce(.x,0)),across(starts_with("independent_"),~coalesce(.x,0)),
         record_difference=independent_unweighted_task_records-locked_unweighted_task_records,
         weighted_record_difference=independent_weighted_task_records-locked_weighted_task_records,
         weighted_hours_difference=independent_weighted_repeated_person_hours-locked_weighted_repeated_person_hours,
         within_tolerance=abs(record_difference)<1e-10 & abs(weighted_record_difference)<1e-8 & abs(weighted_hours_difference)<1e-6)

worker_crop <- independent %>% distinct(wave,hhid,cropcode,indiv,worker_arrangement,worker_group,member_sex,member_age_group,payment_status,rural,cluster,strata,household_weight,worker_count,days_per_worker,hours_per_day,daily_payment,person_hours)
design_audit <- worker_crop %>% group_by(wave) %>% summarise(worker_crop_records=n(),households=n_distinct(hhid),psus=n_distinct(cluster),strata=n_distinct(strata),missing_weight=sum(is.na(household_weight)|household_weight<=0),.groups="drop")
for(w in c(4L,5L)) {
  d <- worker_crop %>% filter(wave==w) %>% group_by(hhid,cluster,strata,household_weight) %>% summarise(record_contribution=n(),.groups="drop")
  invisible(svydesign(ids=~cluster,strata=~strata,weights=~household_weight,nest=TRUE,data=d))
}

core <- independent %>% filter(task %in% c("shelling_or_threshing","drying","cleaning","processing"))
sensitivity <- bind_rows(
  independent %>% count(wave,name="value") %>% mutate(test="all_questionnaire_tasks",metric="analytic_task_worker_records"),
  core %>% count(wave,name="value") %>% mutate(test="common_four_tasks_only",metric="analytic_task_worker_records"),
  worker_crop %>% count(wave,name="value") %>% mutate(test="collapse_task_expansion",metric="worker_crop_arrangements"),
  tibble(wave=c(4L,5L)) %>%
    left_join(independent %>% filter(cropcode==9999) %>% count(wave,name="value"),by="wave") %>%
    mutate(value=coalesce(value,0L),test="quarantined_other_crop_code",metric="analytic_task_worker_records"),
  tibble(wave=c(4L,5L)) %>%
    left_join(external_audit %>% filter(status=="screener_yes_missing_worker_count") %>% group_by(wave) %>% summarise(value=sum(source_group_crop_records),.groups="drop"),by="wave") %>%
    mutate(value=coalesce(value,0L),test="affirmative_external_screener_missing_followup",metric="source_group_crop_records_excluded")
)

crop_labels <- function(file) { x <- read_source(file); z <- attr(x$cropcode,"labels"); tibble(cropcode=unname(z),label=names(z)) }
crosswalk <- full_join(crop_labels("sectaphl1_harvestw4.dta") %>% rename(w4_phl1_label=label),crop_labels("sectaphl2_harvestw4.dta") %>% rename(w4_phl2_label=label),by="cropcode") %>%
  full_join(crop_labels("sectaphl1_harvestw5.dta") %>% rename(w5_phl1_label=label),by="cropcode") %>%
  full_join(crop_labels("sectaphl2_harvestw5.dta") %>% rename(w5_phl2_label=label),by="cropcode") %>% arrange(cropcode) %>%
  mutate(official_instrument_anchor="Wave 4 questionnaire p.30; Wave 5 questionnaire p.41",
         cross_wave_status=case_when(cropcode==9999 ~ "QUARANTINE: heterogeneous other category",
                                     cropcode==2194 ~ "QUALIFIED: broad green-vegetable category; source labels differ in detail",
                                     cropcode==3180 ~ "QUALIFIED: household label says fruit; instrument/PHL2 label says oil palm tree",
                                     TRUE ~ "PASS: stable numeric code and cross-wave source labels"))

write_csv(key_audit,file.path(out_dir,"PAPER_3_SOURCE_KEY_AUDIT.csv"))
write_csv(eligibility,file.path(out_dir,"PAPER_3_ELIGIBILITY_RECONCILIATION_TABLES.csv"))
write_csv(external_audit,file.path(out_dir,"PAPER_3_EXTERNAL_SCREENER_AUDIT.csv"))
write_csv(reproduction,file.path(out_dir,"PAPER_3_DESCRIPTIVE_REPRODUCTION_COMPARISON.csv"))
write_csv(design_audit,file.path(out_dir,"PAPER_3_SURVEY_DESIGN_AUDIT.csv"))
write_csv(sensitivity,file.path(out_dir,"PAPER_3_SENSITIVITY_AND_EXCEPTION_REGISTER.csv"))
write_csv(crosswalk,file.path(out_dir,"PAPER_3_CROP_CODE_CROSSWALK.csv"))
write_csv(summary_task(independent),file.path(out_dir,"PAPER_3_INDEPENDENT_TASK_DESCRIPTIVES.csv"))
write_csv(worker_crop %>% group_by(wave,worker_arrangement,worker_group,member_sex,member_age_group,payment_status,rural) %>%
  summarise(unweighted_worker_crop_records=n(),weighted_worker_crop_records=sum(household_weight),
            weighted_repeated_person_hours=sum(household_weight*person_hours),
            median_days_per_worker=median(days_per_worker),median_hours_per_day=median(hours_per_day),
            median_daily_payment=median(daily_payment,na.rm=TRUE),.groups="drop"),
  file.path(out_dir,"PAPER_3_INDEPENDENT_WORKER_CROP_DESCRIPTIVES.csv"))

if(any(!reproduction$within_tolerance)) stop("Independent reproduction differs from locked descriptives.")
message("Phase II audit complete: ",nrow(reproduction)," grouped comparisons match locked outputs.")
