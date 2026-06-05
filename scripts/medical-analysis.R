# Clinical analysis of a synthetic population.
#
#   Rscript --vanilla scripts/medical-analysis.R
#   RSYNTHEA_N=50 RSYNTHEA_CORES=4 Rscript --vanilla scripts/medical-analysis.R
#
# To analyse an existing export instead of generating:
#   RSYNTHEA_INPUT=/path/to/csvs Rscript --vanilla scripts/medical-analysis.R

if (file.exists("DESCRIPTION") && requireNamespace("devtools", quietly = TRUE)) {
  devtools::load_all(".", quiet = TRUE)
} else {
  library(rsynthea)
}

input_dir <- Sys.getenv("RSYNTHEA_INPUT", unset = "")
end_date  <- as.POSIXct("2020-01-01")

if (!nzchar(input_dir)) {
  n     <- as.integer(Sys.getenv("RSYNTHEA_N",     unset = "20"))
  cores <- as.integer(Sys.getenv("RSYNTHEA_CORES", unset = "0"))
  if (cores == 0L) cores <- max(1L, parallel::detectCores(logical = FALSE) - 1L)

  modules  <- load_all_modules()
  patients <- generate_population(n, seed = 42L, modules = modules,
                                  end_date = end_date, mc.cores = cores)
  tbls     <- export_population(patients)
} else {
  read_csv <- function(name) {
    p <- file.path(input_dir, paste0(name, ".csv"))
    if (file.exists(p)) utils::read.csv(p, stringsAsFactors = FALSE) else data.frame()
  }
  tbls <- list(
    patients   = read_csv("patients"),
    encounters = read_csv("encounters"),
    conditions = read_csv("conditions"),
    medications = read_csv("medications"),
    observations = read_csv("observations")
  )
  tbls$patients$birth_date <- as.POSIXct(tbls$patients$birth_date, tz = "UTC")
}

p   <- tbls$patients
enc <- tbls$encounters
cnd <- tbls$conditions
med <- tbls$medications
obs <- tbls$observations

age        <- as.numeric(difftime(end_date, p$birth_date, units = "days")) / 365.25
enc_per_pt <- tabulate(match(enc$patient_id, p$id))
cnd_per_pt <- tabulate(match(cnd$patient_id, p$id))

cat("=== Population ===\n")
cat("Patients:", nrow(p), "| Alive:", sum(p$is_alive, na.rm = TRUE),
    "| Dead:", sum(!p$is_alive, na.rm = TRUE), "\n")
cat("Age  median:", round(median(age, na.rm = TRUE), 1),
    " mean:", round(mean(age, na.rm = TRUE), 1), "\n")
cat("Gender F:", sum(p$gender == "F", na.rm = TRUE),
    " M:", sum(p$gender == "M", na.rm = TRUE), "\n")

cat("\n=== Utilisation ===\n")
cat("Total encounters:", nrow(enc),
    "| per patient (mean):", round(mean(enc_per_pt), 1), "\n")
cat("Total conditions:", nrow(cnd),
    "| per patient (mean):", round(mean(cnd_per_pt), 1), "\n")
cat("Total medications:", nrow(med), "\n")
cat("Total observations:", nrow(obs), "\n")

cat("\n=== Top 10 conditions ===\n")
cond_tab <- sort(table(cnd$description), decreasing = TRUE)
print(head(as.data.frame(cond_tab, stringsAsFactors = FALSE), 10L))

cat("\n=== Encounters by class ===\n")
print(sort(table(enc$encounter_class), decreasing = TRUE))

cat("\n=== Chronic disease prevalence ===\n")
snomed_prev <- function(label, codes) {
  n_match <- length(unique(cnd$patient_id[cnd$code %in% codes & cnd$is_active]))
  cat(sprintf("  %-20s %3d / %d  (%.1f%%)\n", label, n_match, nrow(p), 100 * n_match / nrow(p)))
}
snomed_prev("T2DM",          "44054006")
snomed_prev("Prediabetes",   "714628002")
snomed_prev("Hypertension",  c("59621000", "38341003"))
snomed_prev("Asthma",        c("195967001", "233678006"))
snomed_prev("Depression",    c("35489007", "370143000", "36923009"))
snomed_prev("Obesity",       c("414916001", "162864005"))
snomed_prev("Heart failure", c("84114007", "85232009"))
snomed_prev("MI",            c("22298006", "401303003", "401314000"))
