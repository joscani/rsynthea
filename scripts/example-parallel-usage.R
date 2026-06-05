# Parallel usage: compare serial vs parallel generation time.
#
#   Rscript --vanilla scripts/example-parallel-usage.R
#   RSYNTHEA_N=50 RSYNTHEA_CORES=4 Rscript --vanilla scripts/example-parallel-usage.R

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

modules <- load_all_modules()

t_serial   <- system.time(generate_population(n, seed = 1L, modules = modules,
                                              end_date = end_date))
t_parallel <- system.time(generate_population(n, seed = 1L, modules = modules,
                                              end_date = end_date, mc.cores = cores))

cat(sprintf("n=%d  cores=%d\n", n, cores))
cat(sprintf("serial:   %.1fs\n", t_serial[["elapsed"]]))
cat(sprintf("parallel: %.1fs  (%.1fx speedup)\n",
            t_parallel[["elapsed"]],
            t_serial[["elapsed"]] / t_parallel[["elapsed"]]))
