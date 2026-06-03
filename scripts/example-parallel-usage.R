# Parallel example usage for the rsynthea package.
#
# Run from the repository root while developing:
#   Rscript --vanilla scripts/example-parallel-usage.R
#
# Optional configuration:
#   RSYNTHEA_N=50 RSYNTHEA_CORES=4 RSYNTHEA_EXAMPLE_OUTPUT=outputs/parallel_population \
#     Rscript --vanilla scripts/example-parallel-usage.R

if (file.exists("DESCRIPTION") && dir.exists("R") && requireNamespace("devtools", quietly = TRUE)) {
  devtools::load_all(".", quiet = TRUE)
} else {
  library(rsynthea)
}

end_date <- as.POSIXct("2020-01-01", tz = "UTC")
n_patients <- as.integer(Sys.getenv("RSYNTHEA_N", unset = "200"))
requested_cores <- as.integer(Sys.getenv("RSYNTHEA_CORES", unset = "0"))

default_mc_cores <- function() {
  available_cores <- parallel::detectCores(logical = FALSE)
  if (is.na(available_cores) || available_cores < 1L) available_cores <- 1L
  max(1L, available_cores - 1L)
}

mc_cores <- if (requested_cores > 0L) requested_cores else default_mc_cores()

cat("Loading GMF modules...\n")
modules <- load_all_modules()
cat("Loaded", length(modules), "modules\n")

cat(
  "Generating", n_patients, "patients with", mc_cores,
  "parallel worker(s)...\n"
)

elapsed <- system.time({
  patients <- generate_population(
    n = n_patients,
    seed = 42L,
    modules = modules,
    end_date = end_date,
    mc.cores = mc_cores
  )
})

output_dir <- Sys.getenv(
  "RSYNTHEA_EXAMPLE_OUTPUT",
  unset = file.path(tempdir(), "rsynthea_parallel_population")
)

cat("Exporting tidy tables to:", output_dir, "\n")
tables <- export_population(patients, output_dir = output_dir)

cat("\nElapsed time:\n")
print(elapsed)

cat("\nTable sizes:\n")
print(vapply(tables, nrow, integer(1)))

cat("\nPatients:\n")
print(tables$patients)

cat("\nCSV files written:\n")
print(list.files(output_dir, pattern = "\\.csv$", full.names = TRUE))
