# Example usage for the rsynthea package.
#
# Run from the repository root while developing:
#   Rscript --vanilla scripts/example-usage.R
#
# To keep generated CSVs in a specific directory:
#   RSYNTHEA_EXAMPLE_OUTPUT=outputs/example_population Rscript --vanilla scripts/example-usage.R

if (file.exists("DESCRIPTION") && dir.exists("R") && requireNamespace("devtools", quietly = TRUE)) {
  devtools::load_all(".", quiet = TRUE)
} else {
  library(rsynthea)
}

end_date <- as.POSIXct("2020-01-01", tz = "UTC")
n_patients <- 200L
seed <- 42L

cat("Loading GMF modules...\n")
modules <- load_all_modules()
cat("Loaded", length(modules), "modules\n")

cat("Generating", n_patients, "patients...\n")
patients <- generate_population(
  n = n_patients,
  seed = seed,
  modules = modules,
  end_date = end_date
)

output_dir <- Sys.getenv(
  "RSYNTHEA_EXAMPLE_OUTPUT",
  unset = file.path(tempdir(), "rsynthea_example_population")
)

cat("Exporting tidy tables to:", output_dir, "\n")
tables <- export_population(patients, output_dir = output_dir)

cat("\nTable sizes:\n")
print(vapply(tables, nrow, integer(1)))

cat("\nPatients:\n")
print(tables$patients)

cat("\nFirst encounters:\n")
print(utils::head(tables$encounters, 10L))

cat("\nFirst observations:\n")
print(utils::head(tables$observations, 10L))

cat("\nCSV files written:\n")
print(list.files(output_dir, pattern = "\\.csv$", full.names = TRUE))
