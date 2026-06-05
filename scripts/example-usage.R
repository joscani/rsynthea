# Basic usage: generate a small synthetic population and export it.
#
#   Rscript --vanilla scripts/example-usage.R

if (file.exists("DESCRIPTION") && requireNamespace("devtools", quietly = TRUE)) {
  devtools::load_all(".", quiet = TRUE)
} else {
  library(rsynthea)
}

modules  <- load_all_modules()
patients <- generate_population(n = 10L, seed = 42L, modules = modules,
                                end_date = as.POSIXct("2020-01-01"))

tbls <- export_population(patients)

cat("Patients:", nrow(tbls$patients), "\n")
cat("Encounters:", nrow(tbls$encounters), "\n")
cat("Conditions:", nrow(tbls$conditions), "\n")
cat("Observations:", nrow(tbls$observations), "\n")
print(tbls$patients[, c("id", "gender", "race", "is_alive")])
