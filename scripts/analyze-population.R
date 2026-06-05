# Quick analysis of a generated population.
#
#   Rscript --vanilla scripts/analyze-population.R
#   RSYNTHEA_N=50 RSYNTHEA_CORES=4 Rscript --vanilla scripts/analyze-population.R

if (file.exists("DESCRIPTION") && requireNamespace("devtools", quietly = TRUE)) {
  devtools::load_all(".", quiet = TRUE)
} else {
  library(rsynthea)
}

n        <- as.integer(Sys.getenv("RSYNTHEA_N",     unset = "20"))
cores    <- as.integer(Sys.getenv("RSYNTHEA_CORES", unset = "0"))
end_date <- as.POSIXct("2020-01-01")

if (cores == 0L) {
  cores <- max(1L, parallel::detectCores(logical = FALSE) - 1L)
}

modules  <- load_all_modules()
patients <- generate_population(n, seed = 42L, modules = modules,
                                end_date = end_date, mc.cores = cores)
tbls     <- export_population(patients)

# --- summary ---

age_years <- as.numeric(difftime(end_date, tbls$patients$birth_date, units = "days")) / 365.25

cat("Population:", nrow(tbls$patients), "patients\n")
cat("Alive:", sum(tbls$patients$is_alive, na.rm = TRUE), "\n")
cat("Age (median / mean):",
    round(median(age_years, na.rm = TRUE), 1), "/",
    round(mean(age_years, na.rm = TRUE), 1), "years\n")
cat("\nRecord counts:\n")
print(vapply(tbls, nrow, integer(1L)))

cat("\nTop conditions:\n")
cond_tab <- sort(table(tbls$conditions$description), decreasing = TRUE)
print(head(as.data.frame(cond_tab), 10L))

cat("\nEncounters by class:\n")
print(sort(table(tbls$encounters$encounter_class), decreasing = TRUE))
