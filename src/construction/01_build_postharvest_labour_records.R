# Builds a private record of post-harvest labour from GHS Panel Waves 4 and 5.
# No raw input, household identifier, or row-level output may be committed.

required_packages <- c("haven", "dplyr", "tidyr", "readr", "stringr")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) stop("Install required packages: ", paste(missing_packages, collapse = ", "))

library(dplyr)
library(tidyr)
library(readr)
library(stringr)

project_dir <- Sys.getenv("POSTHARVEST_PROJECT_DIR", unset = getwd())
raw_dir <- Sys.getenv("GHS_RAW_DIR")
if (!nzchar(raw_dir)) stop("Set GHS_RAW_DIR to the private raw GHS folder before running this script.")

private_data_dir <- file.path(project_dir, "data", "private")
private_output_dir <- file.path(project_dir, "outputs", "private")
dir.create(private_data_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(private_output_dir, recursive = TRUE, showWarnings = FALSE)

source_file <- function(name) {
  path <- file.path(raw_dir, name)
  if (!file.exists(path)) stop("Missing required source file: ", path)
  path
}

read_source <- function(name) haven::read_dta(source_file(name))
numeric_value <- function(x) suppressWarnings(as.numeric(haven::zap_labels(x)))
binary_yes <- function(x) as.integer(numeric_value(x) == 1)

task_long <- function(data, task_columns, task_labels) {
  stopifnot(length(task_columns) == length(task_labels))
  names(task_labels) <- task_columns
  data %>%
    pivot_longer(cols = all_of(task_columns), names_to = "task_source", values_to = "task_flag") %>%
    mutate(
      task = unname(task_labels[task_source]),
      performed_task = binary_yes(task_flag)
    ) %>%
    select(-task_source, -task_flag)
}

roster_w4 <- read_source("sect1_harvestw4.dta") %>%
  transmute(
    wave = 4L, hhid, indiv,
    member_sex = case_when(numeric_value(s1q2) == 1 ~ "man", numeric_value(s1q2) == 2 ~ "woman", TRUE ~ NA_character_),
    member_age = numeric_value(s1q4),
    member_age_group = case_when(member_age < 15 ~ "child_under_15", member_age >= 15 ~ "adult", TRUE ~ NA_character_)
  )

roster_w5 <- read_source("sect1_harvestw5.dta") %>%
  transmute(
    wave = 5L, hhid, indiv,
    member_sex = case_when(numeric_value(s1q2) == 1 ~ "man", numeric_value(s1q2) == 2 ~ "woman", TRUE ~ NA_character_),
    member_age = numeric_value(s1q6),
    member_age_group = case_when(member_age < 15 ~ "child_under_15", member_age >= 15 ~ "adult", TRUE ~ NA_character_)
  )

household_w4 <- read_source("secta_harvestw4.dta") %>%
  transmute(wave = 4L, hhid, state, lga, sector, household_weight = numeric_value(wt_wave4))

household_w5 <- read_source("secta_harvestw5.dta") %>%
  transmute(wave = 5L, hhid, state, lga, sector, household_weight = numeric_value(wt_cross_wave5))

common_tasks_w4 <- c(
  phl_q01d__1 = "shelling_or_threshing",
  phl_q01d__2 = "drying",
  phl_q01d__3 = "cleaning",
  phl_q01d__4 = "processing"
)

common_tasks_w5 <- c(
  phl_q4__1 = "shelling_or_threshing",
  phl_q4__3 = "drying",
  phl_q4__4 = "cleaning",
  phl_q4__5 = "processing"
)

wave5_extra_tasks <- c(phl_q4__2 = "sorting", phl_q4__6 = "supervision")

self_w4_source <- read_source("sectaphl1_harvestw4.dta")
self_w4 <- self_w4_source %>%
  transmute(
    wave = 4L, hhid, indiv, cropcode,
    worker_arrangement = "household_member",
    worker_group = NA_character_,
    days_per_worker = numeric_value(phl_q01b),
    hours_per_day = numeric_value(phl_q01c),
    worker_count = 1,
    daily_payment = NA_real_,
    phl_q01d__1 = binary_yes(phl_q01d__1),
    phl_q01d__2 = binary_yes(phl_q01d__2),
    phl_q01d__3 = binary_yes(phl_q01d__3),
    phl_q01d__4 = binary_yes(phl_q01d__4)
  ) %>%
  mutate(
    participated = as.integer(!is.na(days_per_worker) & days_per_worker > 0 & !is.na(hours_per_day) & hours_per_day > 0),
    person_hours = worker_count * days_per_worker * hours_per_day
  ) %>%
  task_long(names(common_tasks_w4), common_tasks_w4) %>%
  left_join(roster_w4, by = c("wave", "hhid", "indiv"))

self_w5_source <- read_source("sectaphl1_harvestw5.dta")
self_w5 <- self_w5_source %>%
  transmute(
    wave = 5L, hhid, indiv, cropcode,
    worker_arrangement = "household_member",
    worker_group = NA_character_,
    days_per_worker = numeric_value(phl_q2),
    hours_per_day = numeric_value(phl_q3),
    worker_count = 1,
    daily_payment = NA_real_,
    reported_work = binary_yes(phl_q1),
    phl_q4__1 = binary_yes(phl_q4__1),
    phl_q4__2 = binary_yes(phl_q4__2),
    phl_q4__3 = binary_yes(phl_q4__3),
    phl_q4__4 = binary_yes(phl_q4__4),
    phl_q4__5 = binary_yes(phl_q4__5),
    phl_q4__6 = binary_yes(phl_q4__6)
  ) %>%
  mutate(
    participated = as.integer(reported_work == 1 & !is.na(days_per_worker) & days_per_worker > 0 & !is.na(hours_per_day) & hours_per_day > 0),
    person_hours = worker_count * days_per_worker * hours_per_day
  ) %>%
  task_long(c(names(common_tasks_w5), names(wave5_extra_tasks)), c(common_tasks_w5, wave5_extra_tasks)) %>%
  left_join(roster_w5, by = c("wave", "hhid", "indiv"))

build_external_labour <- function(file, wave, n_tasks) {
  source <- read_source(file)
  groups <- c("man", "woman", "child_under_15")
  bind_rows(lapply(seq_along(groups), function(j) {
    paid_task_cols <- paste0("phl_q07__", seq_len(n_tasks), "_", j)
    unpaid_task_cols <- paste0("phl_q12__", seq_len(n_tasks), "_", j)
    task_names <- if (n_tasks == 4) c("shelling_or_threshing", "drying", "cleaning", "processing") else c("shelling_or_threshing", "sorting", "drying", "cleaning", "processing", "supervision")
    paid <- bind_cols(
      tibble(
        wave = as.integer(wave), hhid = source$hhid, cropcode = source$cropcode, indiv = NA_real_, worker_arrangement = "hired_paid", worker_group = groups[j],
        member_sex = if (groups[j] == "woman") "woman" else if (groups[j] == "man") "man" else NA_character_,
        member_age = NA_real_, member_age_group = if (groups[j] == "child_under_15") "child_under_15" else "adult",
        worker_count = numeric_value(source[[paste0("phl_q03_", j)]]), days_per_worker = numeric_value(source[[paste0("phl_q04_", j)]]),
        hours_per_day = numeric_value(source[[paste0("phl_q05_", j)]]), daily_payment = numeric_value(source[[paste0("phl_q06_", j)]])
      ),
      as_tibble(lapply(source[paid_task_cols], binary_yes)) %>% setNames(paid_task_cols)
    ) %>%
      # The module skips all paid-worker follow-up fields when no worker is
      # reported. Treat that documented skip pattern as nonparticipation rather
      # than leaving the analytic eligibility flag indeterminate.
      mutate(participated = as.integer(dplyr::coalesce(worker_count > 0 & days_per_worker > 0 & hours_per_day > 0, FALSE)), person_hours = worker_count * days_per_worker * hours_per_day) %>%
      task_long(paid_task_cols, stats::setNames(task_names, paid_task_cols))
    unpaid <- bind_cols(
      tibble(
        wave = as.integer(wave), hhid = source$hhid, cropcode = source$cropcode, indiv = NA_real_, worker_arrangement = "outside_household_unpaid", worker_group = groups[j],
        member_sex = if (groups[j] == "woman") "woman" else if (groups[j] == "man") "man" else NA_character_,
        member_age = NA_real_, member_age_group = if (groups[j] == "child_under_15") "child_under_15" else "adult",
        worker_count = numeric_value(source[[paste0("phl_q09_", j)]]), days_per_worker = numeric_value(source[[paste0("phl_q10_", j)]]),
        hours_per_day = numeric_value(source[[paste0("phl_q11_", j)]]), daily_payment = NA_real_
      ),
      as_tibble(lapply(source[unpaid_task_cols], binary_yes)) %>% setNames(unpaid_task_cols)
    ) %>%
      # The same skip rule applies to the unpaid outside-household module.
      mutate(participated = as.integer(dplyr::coalesce(worker_count > 0 & days_per_worker > 0 & hours_per_day > 0, FALSE)), person_hours = worker_count * days_per_worker * hours_per_day) %>%
      task_long(unpaid_task_cols, stats::setNames(task_names, unpaid_task_cols))
    bind_rows(paid, unpaid)
  }))
}

external_w4 <- build_external_labour("sectaphl2_harvestw4.dta", 4, 4)
external_w5 <- build_external_labour("sectaphl2_harvestw5.dta", 5, 6)

labour_records <- bind_rows(self_w4, self_w5, external_w4, external_w5) %>%
  left_join(bind_rows(household_w4, household_w5), by = c("wave", "hhid")) %>%
  mutate(
    rural = case_when(numeric_value(sector) == 2 ~ 1L, numeric_value(sector) == 1 ~ 0L, TRUE ~ NA_integer_),
    payment_status = case_when(worker_arrangement == "hired_paid" ~ "paid", worker_arrangement == "outside_household_unpaid" ~ "unpaid", worker_arrangement == "household_member" ~ "household_member", TRUE ~ NA_character_),
    analytic_record = as.integer(participated == 1 & performed_task == 1 & !is.na(household_weight) & household_weight > 0)
  )

diagnostics <- labour_records %>%
  group_by(wave, worker_arrangement, worker_group, member_sex, member_age_group, task, payment_status) %>%
  summarise(
    records = n(), analytic_records = sum(analytic_record, na.rm = TRUE), missing_weight = sum(is.na(household_weight) | household_weight <= 0),
    missing_person_hours = sum(analytic_record == 1 & (is.na(person_hours) | person_hours <= 0)), .groups = "drop"
  )

readr::write_rds(labour_records, file.path(private_data_dir, "postharvest_labour_records_private.rds"), compress = "xz")
readr::write_csv(diagnostics, file.path(private_output_dir, "postharvest_labour_build_diagnostics_private.csv"))
message("Private post-harvest labour records built: ", nrow(labour_records), " task-worker records.")
