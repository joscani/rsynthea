# Example analysis for exported rsynthea data.
#
# Run from the repository root:
#   Rscript --vanilla scripts/analyze-population.R
#
# By default this script generates a small synthetic population, exports it to a
# temporary directory, and then analyzes the exported CSVs. To analyze an
# existing export instead, set:
#   RSYNTHEA_ANALYSIS_INPUT=/path/to/exported/csvs Rscript --vanilla scripts/analyze-population.R
#
# Optional generation controls when no input directory is provided:
#   RSYNTHEA_N=200 RSYNTHEA_CORES=6 Rscript --vanilla scripts/analyze-population.R

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

safe_count <- function(x) {
  if (is.null(x) || length(x) == 0L) return(0L)
  sum(!is.na(x) & nzchar(as.character(x)))
}

top_values <- function(x, n = 10L) {
  x <- x[!is.na(x) & nzchar(as.character(x))]
  if (length(x) == 0L) return(data.frame())
  tab <- sort(table(x), decreasing = TRUE)
  head(data.frame(value = names(tab), count = as.integer(tab), row.names = NULL), n)
}

count_by_patient <- function(tbl, patients) {
  if (nrow(tbl) == 0L) {
    return(data.frame(patient_id = patients$id, n_events = 0L))
  }
  counts <- as.data.frame(table(tbl$patient_id), stringsAsFactors = FALSE)
  names(counts) <- c("patient_id", "n_events")
  counts$n_events <- as.integer(counts$n_events)
  merge(data.frame(patient_id = patients$id, stringsAsFactors = FALSE), counts,
    by = "patient_id", all.x = TRUE, sort = FALSE
  )
}

input_dir <- Sys.getenv("RSYNTHEA_ANALYSIS_INPUT", unset = "")
generated_dir <- FALSE

if (!nzchar(input_dir)) {
  end_date <- as.POSIXct(Sys.getenv("RSYNTHEA_ANALYSIS_END_DATE", unset = "2020-01-01"), tz = "UTC")
  n_patients <- as.integer(Sys.getenv("RSYNTHEA_N", unset = "200"))
  requested_cores <- as.integer(Sys.getenv("RSYNTHEA_CORES", unset = "0"))

  available_cores <- parallel::detectCores(logical = FALSE)
  if (is.na(available_cores) || available_cores < 1L) available_cores <- 1L
  mc_cores <- if (requested_cores > 0L) requested_cores else max(1L, available_cores - 1L)

  cat("No input directory provided; generating", n_patients, "patients...\n")
  modules <- load_all_modules()
  patients <- generate_population(
    n = n_patients,
    seed = 42L,
    modules = modules,
    end_date = end_date,
    mc.cores = mc_cores
  )
  input_dir <- file.path(tempdir(), "rsynthea_analysis_input")
  export_population(patients, output_dir = input_dir)
  generated_dir <- TRUE
}

patients <- read_table(input_dir, "patients")
encounters <- read_table(input_dir, "encounters")
conditions <- read_table(input_dir, "conditions")
medications <- read_table(input_dir, "medications")
procedures <- read_table(input_dir, "procedures")
observations <- read_table(input_dir, "observations")
immunizations <- read_table(input_dir, "immunizations")
allergies <- read_table(input_dir, "allergies")
careplans <- read_table(input_dir, "careplans")
imaging <- read_table(input_dir, "imaging")
devices <- read_table(input_dir, "devices")
reports <- read_table(input_dir, "reports")
report_observations <- read_table(input_dir, "report_observations")
supplies <- read_table(input_dir, "supplies")

patients$birth_date <- as.POSIXct(patients$birth_date, tz = "UTC")
patients$death_date <- as.POSIXct(patients$death_date, tz = "UTC")
analysis_date <- if (generated_dir) {
  as.POSIXct(Sys.getenv("RSYNTHEA_ANALYSIS_END_DATE", unset = "2020-01-01"), tz = "UTC")
} else {
  max(c(
    encounters$time, conditions$onset_time, medications$start_time,
    procedures$time, observations$time, immunizations$time, allergies$onset_time,
    careplans$start_time, imaging$time, devices$start_time, reports$time,
    report_observations$time, supplies$time
  ), na.rm = TRUE)
}

patient_summary <- data.frame(
  id = patients$id,
  age_years = round(as.numeric(difftime(analysis_date, patients$birth_date, units = "days")) / 365.25, 1),
  is_alive = patients$is_alive,
  gender = patients$gender,
  race = patients$race,
  n_encounters = count_by_patient(encounters, patients)$n_events,
  n_conditions = count_by_patient(conditions, patients)$n_events,
  n_medications = count_by_patient(medications, patients)$n_events,
  n_observations = count_by_patient(observations, patients)$n_events,
  n_procedures = count_by_patient(procedures, patients)$n_events,
  n_reports = count_by_patient(reports, patients)$n_events,
  stringsAsFactors = FALSE
)

cat("\nInput directory:\n")
cat(input_dir, "\n")

cat("\nPopulation summary:\n")
cat("Patients:", nrow(patients), "\n")
cat("Alive:", sum(patients$is_alive %in% TRUE), "\n")
cat("Dead:", sum(patients$is_alive %in% FALSE), "\n")
cat("Average age:", round(mean(patient_summary$age_years, na.rm = TRUE), 1), "\n")
cat("Median age:", round(stats::median(patient_summary$age_years, na.rm = TRUE), 1), "\n")

cat("\nEvent totals:\n")
event_totals <- data.frame(
  table = c("encounters", "conditions", "medications", "procedures", "observations",
            "immunizations", "allergies", "careplans", "imaging", "devices",
            "reports", "report_observations", "supplies"),
  rows = c(
    nrow(encounters), nrow(conditions), nrow(medications), nrow(procedures),
    nrow(observations), nrow(immunizations), nrow(allergies), nrow(careplans),
    nrow(imaging), nrow(devices), nrow(reports), nrow(report_observations),
    nrow(supplies)
  ),
  stringsAsFactors = FALSE
)
print(event_totals)

cat("\nEncounters by class:\n")
if (nrow(encounters) > 0L) {
  print(as.data.frame(sort(table(encounters$encounter_class), decreasing = TRUE)))
}

cat("\nTop condition codes:\n")
print(top_values(conditions$code, n = 10L))

cat("\nTop observation codes:\n")
print(top_values(observations$code, n = 10L))

cat("\nTop medication codes:\n")
print(top_values(medications$code, n = 10L))

cat("\nPatient event load:\n")
print(utils::head(patient_summary[order(-patient_summary$n_encounters), ], 10L))

cat("\nPatients by age band:\n")
age_band <- cut(patient_summary$age_years,
  breaks = c(-Inf, 17, 34, 49, 64, Inf),
  labels = c("0-17", "18-34", "35-49", "50-64", "65+"),
  right = TRUE
)
print(as.data.frame(sort(table(age_band), decreasing = TRUE)))

cat("\nAnalysis complete.\n")
