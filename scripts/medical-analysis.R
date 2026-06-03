# Medical analysis example for exported rsynthea data.
#
# Run from the repository root:
#   Rscript --vanilla scripts/medical-analysis.R
#
# By default this script generates a small synthetic population, exports it to a
# temporary directory, and then performs a simple clinical analysis on the CSVs.
# To analyze an existing export instead, set:
#   RSYNTHEA_ANALYSIS_INPUT=/path/to/exported/csvs Rscript --vanilla scripts/medical-analysis.R
#
# Optional generation controls when no input directory is provided:
#   RSYNTHEA_N=200 RSYNTHEA_CORES=6 Rscript --vanilla scripts/medical-analysis.R

if (file.exists("DESCRIPTION") && dir.exists("R") && requireNamespace("devtools", quietly = TRUE)) {
  devtools::load_all(".", quiet = TRUE)
} else {
  library(rsynthea)
}

read_table <- function(input_dir, name) {
  path <- file.path(input_dir, paste0(name, ".csv"))
  if (!file.exists(path)) return(data.frame())
  utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
}

top_values <- function(x, n = 10L) {
  x <- x[!is.na(x) & nzchar(as.character(x))]
  if (length(x) == 0L) return(data.frame())
  tab <- sort(table(x), decreasing = TRUE)
  head(data.frame(value = names(tab), count = as.integer(tab), row.names = NULL), n)
}

parse_time_col <- function(tbl, col) {
  if (nrow(tbl) == 0L || !col %in% names(tbl)) return(tbl)
  tbl[[col]] <- as.POSIXct(tbl[[col]], tz = "UTC")
  tbl
}

latest_time <- function(...) {
  vals <- c(...)
  vals <- vals[!is.na(vals)]
  if (length(vals) == 0L) return(as.POSIXct(NA, tz = "UTC"))
  max(vals)
}

count_by_patient <- function(tbl, patient_ids) {
  if (nrow(tbl) == 0L) {
    return(data.frame(patient_id = patient_ids, n_events = 0L, stringsAsFactors = FALSE))
  }
  counts <- as.data.frame(table(tbl$patient_id), stringsAsFactors = FALSE)
  names(counts) <- c("patient_id", "n_events")
  counts$n_events <- as.integer(counts$n_events)
  merge(
    data.frame(patient_id = patient_ids, stringsAsFactors = FALSE),
    counts,
    by = "patient_id",
    all.x = TRUE,
    sort = FALSE
  )
}

normalize_text <- function(x) {
  tolower(ifelse(is.na(x), "", x))
}

patients_with_pattern <- function(pattern, conditions) {
  if (nrow(conditions) == 0L) return(character())
  hits <- grepl(pattern, normalize_text(conditions$description), perl = TRUE)
  unique(conditions$patient_id[hits])
}

summarize_pattern <- function(label, pattern, patients_tbl, conditions_tbl, encounters_tbl, medications_tbl) {
  patient_ids <- patients_with_pattern(pattern, conditions_tbl)
  if (length(patient_ids) == 0L) {
    return(data.frame(
      condition = label,
      patients = 0L,
      prevalence_pct = 0,
      avg_encounters = 0,
      avg_conditions = 0,
      common_condition = NA_character_,
      stringsAsFactors = FALSE
    ))
  }

  encounter_counts <- count_by_patient(encounters_tbl, patient_ids)
  condition_counts <- count_by_patient(conditions_tbl, patient_ids)

  conds_for_patients <- conditions_tbl[conditions_tbl$patient_id %in% patient_ids, , drop = FALSE]
  common_cond <- if (nrow(conds_for_patients) > 0L) {
    top_values(conds_for_patients$description, 1L)$value[[1L]]
  } else {
    NA_character_
  }

  data.frame(
    condition = label,
    patients = length(patient_ids),
    prevalence_pct = round(100 * length(patient_ids) / nrow(patients_tbl), 1),
    avg_encounters = round(mean(encounter_counts$n_events, na.rm = TRUE), 1),
    avg_conditions = round(mean(condition_counts$n_events, na.rm = TRUE), 1),
    common_condition = common_cond,
    stringsAsFactors = FALSE
  )
}

input_dir <- Sys.getenv("RSYNTHEA_ANALYSIS_INPUT", unset = "")
generated_dir <- FALSE

if (!nzchar(input_dir)) {
  end_date <- as.POSIXct(Sys.getenv("RSYNTHEA_ANALYSIS_END_DATE", unset = "2020-01-01"), tz = "UTC")
  n_patients <- as.integer(Sys.getenv("RSYNTHEA_N", unset = "200"))
  requested_cores <- as.integer(Sys.getenv("RSYNTHEA_CORES", unset = "0"))

  default_mc_cores <- function() {
    available_cores <- parallel::detectCores(logical = FALSE)
    if (is.na(available_cores) || available_cores < 1L) available_cores <- 1L
    max(1L, available_cores - 1L)
  }
  mc_cores <- if (requested_cores > 0L) requested_cores else default_mc_cores()

  cat("No input directory provided; generating", n_patients, "patients...\n")
  modules <- load_all_modules()
  patients <- generate_population(
    n = n_patients,
    seed = 42L,
    modules = modules,
    end_date = end_date,
    mc.cores = mc_cores
  )
  input_dir <- file.path(tempdir(), "rsynthea_medical_analysis_input")
  export_population(patients, output_dir = input_dir)
  generated_dir <- TRUE
}

patients <- read_table(input_dir, "patients")
encounters <- read_table(input_dir, "encounters")
conditions <- read_table(input_dir, "conditions")
medications <- read_table(input_dir, "medications")
observations <- read_table(input_dir, "observations")
reports <- read_table(input_dir, "reports")
report_observations <- read_table(input_dir, "report_observations")

patients <- parse_time_col(patients, "birth_date")
patients <- parse_time_col(patients, "death_date")
encounters <- parse_time_col(encounters, "time")
encounters <- parse_time_col(encounters, "end_time")
conditions <- parse_time_col(conditions, "onset_time")
conditions <- parse_time_col(conditions, "end_time")
medications <- parse_time_col(medications, "start_time")
medications <- parse_time_col(medications, "end_time")
observations <- parse_time_col(observations, "time")
reports <- parse_time_col(reports, "time")
report_observations <- parse_time_col(report_observations, "time")

patients$birth_date <- as.POSIXct(patients$birth_date, tz = "UTC")
patients$death_date <- as.POSIXct(patients$death_date, tz = "UTC")

analysis_date <- if (generated_dir) {
  as.POSIXct(Sys.getenv("RSYNTHEA_ANALYSIS_END_DATE", unset = "2020-01-01"), tz = "UTC")
} else {
  latest_time(
    encounters$time, conditions$onset_time, medications$start_time,
    observations$time, reports$time, report_observations$time
  )
}

patient_summary <- data.frame(
  id = patients$id,
  age_years = round(as.numeric(difftime(analysis_date, patients$birth_date, units = "days")) / 365.25, 1),
  is_alive = patients$is_alive,
  gender = patients$gender,
  race = patients$race,
  n_encounters = count_by_patient(encounters, patients$id)$n_events,
  n_conditions = count_by_patient(conditions, patients$id)$n_events,
  n_medications = count_by_patient(medications, patients$id)$n_events,
  n_observations = count_by_patient(observations, patients$id)$n_events,
  stringsAsFactors = FALSE
)

condition_panels <- rbind(
  summarize_pattern("Diabetes", "diabetes", patients, conditions, encounters, medications),
  summarize_pattern("Hypertension", "hypertension|high blood pressure", patients, conditions, encounters, medications),
  summarize_pattern("Asthma", "asthma", patients, conditions, encounters, medications),
  summarize_pattern("COPD", "chronic obstructive pulmonary disease|copd", patients, conditions, encounters, medications),
  summarize_pattern("Depression", "depression", patients, conditions, encounters, medications),
  summarize_pattern("Obesity", "obesity|overweight", patients, conditions, encounters, medications),
  summarize_pattern("CKD", "chronic kidney disease|renal failure", patients, conditions, encounters, medications),
  summarize_pattern("Heart disease", "coronary artery disease|myocardial infarction|heart failure", patients, conditions, encounters, medications)
)

multimorbidity_bands <- cut(
  patient_summary$n_conditions,
  breaks = c(-Inf, 0, 1, 3, 5, Inf),
  labels = c("0", "1", "2-3", "4-5", "6+"),
  right = TRUE
)

utilization_by_band <- aggregate(
  patient_summary$n_encounters,
  by = list(multimorbidity = multimorbidity_bands),
  FUN = function(x) round(mean(x, na.rm = TRUE), 1)
)
names(utilization_by_band)[2L] <- "avg_encounters"

cat("\nMedical cohort summary\n")
cat("----------------------\n")
cat("Patients:", nrow(patients), "\n")
cat("Alive:", sum(patients$is_alive %in% TRUE), "\n")
cat("Dead:", sum(patients$is_alive %in% FALSE), "\n")
cat("Median age:", round(stats::median(patient_summary$age_years, na.rm = TRUE), 1), "\n")
cat("Female:", sum(tolower(patients$gender) == "f", na.rm = TRUE), "\n")
cat("Male:", sum(tolower(patients$gender) == "m", na.rm = TRUE), "\n")

cat("\nMost common diagnoses\n")
diagnostic_descriptions <- conditions$description[
  !grepl("\\(situation\\)|\\(finding\\)|\\(context-dependent\\)", normalize_text(conditions$description))
]
print(top_values(diagnostic_descriptions, 10L))

cat("\nChronic disease panels\n")
print(condition_panels)

cat("\nMultimorbidity distribution\n")
print(as.data.frame(table(multimorbidity_bands), stringsAsFactors = FALSE))

cat("\nAverage encounters by multimorbidity band\n")
print(utilization_by_band)

cat("\nTop medication descriptions\n")
print(top_values(medications$description, 10L))

cat("\nTop lab and clinical observation descriptions\n")
print(top_values(observations$description, 10L))

cat("\nHighest-utilization patients\n")
print(utils::head(patient_summary[order(-patient_summary$n_encounters), ], 10L))

cat("\nReport counts\n")
cat("Reports:", nrow(reports), "\n")
cat("Report observations:", nrow(report_observations), "\n")

cat("\nAnalysis complete.\n")
