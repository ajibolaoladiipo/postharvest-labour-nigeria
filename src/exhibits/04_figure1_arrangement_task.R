# Paper 3 Phase IV-B: arrangement-by-task exhibit audit.
# This script derives a new exhibit only from the immutable analytic record and
# survey cover files. It treats task flags as overlapping indicators and never
# allocates arrangement-level hours to tasks.

required <- c("haven", "dplyr", "tidyr", "readr", "survey", "purrr", "ggplot2")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) stop("Missing packages: ", paste(missing, collapse = ", "))
invisible(lapply(required, library, character.only = TRUE))
options(survey.lonely.psu = "adjust")

project_dir <- Sys.getenv("POSTHARVEST_PROJECT_DIR")
raw_dir <- Sys.getenv("GHS_RAW_DIR")
if (!nzchar(project_dir) || !dir.exists(project_dir)) stop("Set POSTHARVEST_PROJECT_DIR.")
if (!nzchar(raw_dir) || !dir.exists(raw_dir)) stop("Set GHS_RAW_DIR.")
out_dir <- file.path(project_dir, "phase_ivb_release_clearance", "outputs")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
num <- function(x) suppressWarnings(as.numeric(haven::zap_labels(x)))

# All valid-weight sample households remain in the design frame. Households
# without qualifying arrangements contribute zero to numerator and denominator.
cover <- bind_rows(
  haven::read_dta(file.path(raw_dir, "secta_harvestw4.dta")) %>%
    transmute(wave = 4L, hhid = as.character(hhid), cluster, strata,
              household_weight = num(wt_wave4)),
  haven::read_dta(file.path(raw_dir, "secta_harvestw5.dta")) %>%
    transmute(wave = 5L, hhid = as.character(hhid), cluster, strata,
              household_weight = num(wt_cross_wave5))
) %>% distinct(wave, hhid, .keep_all = TRUE) %>%
  filter(is.finite(household_weight), household_weight > 0)

records <- readr::read_rds(file.path(project_dir, "data", "private",
                                      "postharvest_labour_records_private.rds")) %>%
  filter(analytic_record == 1) %>%
  mutate(hhid = as.character(hhid),
         arrangement_key = paste(wave, hhid, cropcode,
           ifelse(is.na(indiv), "external", as.character(indiv)),
           worker_arrangement, dplyr::coalesce(worker_group, "household"), sep = "|"))

arrangements <- records %>% distinct(wave, hhid, arrangement_key, worker_arrangement)
if (anyDuplicated(arrangements[c("wave", "arrangement_key")])) stop("Duplicate arrangement keys.")
if (nrow(anti_join(arrangements, cover, by = c("wave", "hhid"))) > 0) stop("Eligible arrangement outside valid cover frame.")

core_tasks <- c("cleaning", "drying", "processing", "shelling_or_threshing")
labels <- c(cleaning = "Cleaning", drying = "Drying", processing = "Processing",
            shelling_or_threshing = "Shelling/threshing")
arr_labels <- c(household_member = "Household member", hired_paid = "Hired paid",
                outside_household_unpaid = "Unpaid/exchange external")
task_records <- records %>% filter(task %in% core_tasks) %>%
  distinct(wave, hhid, arrangement_key, worker_arrangement, task)

estimate_cell <- function(w, arrangement, task) {
  base <- cover %>% filter(wave == w)
  denom <- arrangements %>% filter(wave == w) %>% count(hhid, name = "denominator")
  numerator <- task_records %>% filter(wave == w, worker_arrangement == arrangement, task == !!task) %>%
    count(hhid, name = "numerator")
  frame <- base %>% select(hhid, cluster, strata, household_weight) %>%
    left_join(denom, by = "hhid") %>% left_join(numerator, by = "hhid") %>%
    mutate(denominator = coalesce(denominator, 0L), numerator = coalesce(numerator, 0L))
  des <- survey::svydesign(ids = ~cluster, strata = ~strata, weights = ~household_weight,
                           nest = TRUE, data = frame)
  ratio <- survey::svyratio(~numerator, ~denominator, des)
  total_num <- survey::svytotal(~numerator, des)
  tibble(wave = w, worker_arrangement = arrangement, task = task,
         unweighted_numerator = sum(frame$numerator), unweighted_denominator = sum(frame$denominator),
         weighted_numerator = unname(coef(total_num)[1]),
         weighted_denominator = unname(coef(survey::svytotal(~denominator, des))[1]),
         estimate = unname(coef(ratio)[1]), se = unname(survey::SE(ratio)[1]),
         ci_low = max(0, unname(coef(ratio)[1]) - 1.96 * unname(survey::SE(ratio)[1])),
         ci_high = min(1, unname(coef(ratio)[1]) + 1.96 * unname(survey::SE(ratio)[1])),
         n_households = nrow(base), n_psu = n_distinct(base$cluster), n_strata = n_distinct(base$strata))
}

cells <- tidyr::crossing(wave = c(4L, 5L), worker_arrangement = names(arr_labels), task = core_tasks) %>%
  purrr::pmap_dfr(~estimate_cell(..1, ..2, ..3)) %>%
  mutate(task_label = unname(labels[task]), arrangement_label = unname(arr_labels[worker_arrangement]),
         estimate_percent = 100 * estimate, ci_low_percent = 100 * ci_low, ci_high_percent = 100 * ci_high,
         measure = "Weighted incidence of an overlapping task-and-arrangement category among all eligible worker-crop arrangements")

# Independent source-to-extract check against Phase II task expansion summary.
locked <- readr::read_csv(file.path(project_dir, "phase_ii_descriptive_measurement_audit", "outputs",
                                    "PAPER_3_INDEPENDENT_TASK_DESCRIPTIVES.csv"), show_col_types = FALSE) %>%
  filter(task %in% core_tasks) %>% group_by(wave, worker_arrangement, task) %>%
  summarise(locked_unweighted_numerator = sum(unweighted_task_records),
            locked_weighted_numerator = sum(weighted_task_records), .groups = "drop")
comparison <- cells %>% left_join(locked, by = c("wave", "worker_arrangement", "task")) %>%
  mutate(unweighted_difference = unweighted_numerator - locked_unweighted_numerator,
         weighted_difference = weighted_numerator - locked_weighted_numerator,
         verification_status = if_else(unweighted_difference == 0 & abs(weighted_difference) < 1e-6,
                                       "PASS", "FAIL"))
if (any(comparison$verification_status != "PASS")) stop("Phase II task-summary concordance failed.")

readr::write_csv(cells, file.path(out_dir, "PAPER_3_PHASE_IVB_ARRANGEMENT_TASK_FIGURE_DATA.csv"))
readr::write_csv(comparison, file.path(out_dir, "PAPER_3_PHASE_IVB_ARRANGEMENT_TASK_SOURCE_CONCORDANCE.csv"))

p <- ggplot(cells, aes(x = task_label, y = estimate_percent, fill = arrangement_label)) +
  geom_col(position = position_dodge(width = 0.82), width = 0.72) +
  geom_errorbar(aes(ymin = ci_low_percent, ymax = ci_high_percent),
                position = position_dodge(width = 0.82), width = 0.22) +
  facet_wrap(~paste0("Wave ", wave), ncol = 1) +
  scale_y_continuous("Percent of all eligible worker-crop arrangements", limits = c(0, NA),
                     expand = expansion(mult = c(0, .08))) +
  scale_x_discrete("Overlapping post-harvest task") +
  scale_fill_manual(values = c("Household member" = "#2C7FB8", "Hired paid" = "#F03B20",
                               "Unpaid/exchange external" = "#7A0177"), name = "Labour arrangement") +
  labs(title = "Recorded post-harvest task incidence by labour arrangement",
       subtitle = "Each bar is a task-and-arrangement incidence among all eligible worker-crop arrangements; tasks overlap") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom", plot.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 20, hjust = 1), panel.grid.major.x = element_blank())
ggsave(file.path(out_dir, "PAPER_3_PHASE_IVB_FIGURE_1_ARRANGEMENT_TASK_INCIDENCE.png"), p,
       width = 8.0, height = 7.4, dpi = 300)
if (requireNamespace("svglite", quietly = TRUE)) {
  ggsave(file.path(out_dir, "PAPER_3_PHASE_IVB_FIGURE_1_ARRANGEMENT_TASK_INCIDENCE.svg"), p,
         width = 8.0, height = 7.4)
} else {
  message("SVG not written: optional package svglite is unavailable; PNG output is complete.")
}
message("PASS: 24 arrangement-task cells; Phase II source-summary concordance passed.")
