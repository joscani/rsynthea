# R/generator.R

#' Simulate a synthetic patient population
#'
#' Creates `n` synthetic patients, samples their demographics, and runs the
#' full GMF module simulation for each one. The main entry point of the package.
#'
#' @param n Integer. Number of patients to generate. Default `1L`.
#' @param seed Integer or `NULL`. Base random seed. Patient `i` gets seed
#'   `seed + i - 1`. If `NULL`, seeds are drawn randomly.
#' @param state Character or `NULL`. US state to sample demographics from.
#' @param city Character or `NULL`. City to sample demographics from.
#' @param gender Character or `NULL`. Force gender (`"M"` or `"F"`).
#' @param min_age Integer. Minimum patient age at `end_date`. Default `0L`.
#' @param max_age Integer. Maximum patient age at `end_date`. Default `140L`.
#' @param modules List of Module objects or `NULL`. If `NULL`, modules are
#'   loaded automatically and cached for the rest of the session via an internal
#'   cache. Pass an explicit list to override (e.g. custom modules).
#' @param end_date POSIXct. Simulation end date. Default `Sys.time()`.
#' @param mc.cores Integer. Number of parallel workers. Values > 1 use
#'   `parallel::mclapply` (fork-based; Unix/macOS only). Default `1L`.
#' @param use_cpp Logical. Use the compiled C++ simulation engine (default
#'   `TRUE`). The C++ engine is ~2.7× faster than Java Synthea and ~11× faster
#'   than the pure R fallback. Set to `FALSE` only for debugging.
#' @param cpp_modules External pointer returned by [compile_all_modules()], or
#'   `NULL`. When `NULL` and `use_cpp = TRUE`, compiled modules are cached
#'   automatically for the session. Supply an explicit pointer only when using
#'   custom modules outside the session cache.
#'
#' @return A named list of tibbles (one per clinical domain):
#' \describe{
#'   \item{`patients`}{Demographics: one row per patient.}
#'   \item{`encounters`}{Clinical encounters with start/end times and SNOMED codes.}
#'   \item{`conditions`}{Diagnoses with onset/end times and `is_active` flag.}
#'   \item{`medications`}{Medication orders with start/end times and RxNorm codes.}
#'   \item{`procedures`}{Procedures performed.}
#'   \item{`observations`}{Lab and clinical observations with LOINC codes and values.}
#'   \item{`immunizations`}{Vaccines administered.}
#'   \item{`allergies`}{Allergy records.}
#' }
#'
#' @details
#' **Engine selection**: by default, the C++ engine (`use_cpp = TRUE`) is used.
#' It compiles all GMF modules once per session and simulates in C++17, which
#' is ~2.7× faster than the Java reference implementation. The pure R engine
#' (`use_cpp = FALSE`) is slower but easier to instrument for debugging.
#'
#' **Caching**: on the first call, `load_all_modules()` and
#' `compile_all_modules()` run automatically and their results are stored in an
#' internal session cache. Subsequent calls reuse the cache at no cost. Call
#' [rsynthea_clear_cache()] to force a reload (e.g. after modifying custom
#' modules).
#'
#' **Parallelism**: with `mc.cores > 1`, patients are distributed across forked
#' workers via `parallel::mclapply` (Unix/macOS only). Each worker gets its own
#' seed derived from the base `seed`, so results are reproducible.
#'
#' @examples
#' \dontrun{
#' # Modules are loaded and compiled automatically on first call
#' # and cached for the rest of the session.
#' tbls <- generate_population(10, seed = 42L,
#'                             end_date = as.POSIXct("2020-01-01"))
#'
#' # Parallel (Unix/macOS)
#' tbls <- generate_population(100, seed = 1L,
#'                             end_date = as.POSIXct("2020-01-01"),
#'                             mc.cores = parallel::detectCores(logical = FALSE))
#'
#' # Pass custom modules explicitly (bypasses cache)
#' my_modules <- load_all_modules()
#' tbls <- generate_population(10, seed = 1L, modules = my_modules,
#'                             end_date = as.POSIXct("2020-01-01"))
#' }
#'
#' @seealso [load_all_modules()], [compile_all_modules()], [rsynthea_clear_cache()]
#' @export
generate_population <- function(
  n           = 1L,
  seed        = NULL,
  state       = NULL,
  city        = NULL,
  gender      = NULL,
  min_age     = 0L,
  max_age     = 140L,
  modules     = NULL,
  end_date    = Sys.time(),
  mc.cores    = 1L,
  use_cpp     = TRUE,
  cpp_modules = NULL
) {
  .validate_generate_population_args(n, seed, gender, min_age, max_age,
                                     modules, end_date, mc.cores)

  # Use cached modules unless the caller supplied their own
  user_supplied_modules <- !is.null(modules)
  if (!user_supplied_modules) {
    modules <- .get_modules()
  }

  # ── C++ fast path ──────────────────────────────────────────────────────────
  if (use_cpp) {
    if (is.null(cpp_modules)) {
      # Use cached compiled modules; recompile only if user supplied custom modules
      cpp_modules <- if (user_supplied_modules) {
        compile_all_modules(modules)
      } else {
        .get_cpp_modules(modules)
      }
    }
    return(.generate_population_cpp(
      n           = n,
      seed        = seed,
      state       = state,
      city        = city,
      gender      = gender,
      min_age     = min_age,
      max_age     = max_age,
      modules     = modules,
      end_date    = end_date,
      mc.cores    = mc.cores,
      cpp_modules = cpp_modules
    ))
  }

  # ── R engine (original) ───────────────────────────────────────────────────
  person_seeds <- if (!is.null(seed)) seed + seq_len(n) - 1L
                  else sample.int(.Machine$integer.max, n)

  simulate_one <- function(person_seed) {
    set.seed(person_seed)
    p <- Person(seed = as.integer(person_seed))
    p <- sample_demographics(p,
      state    = state,
      city     = city,
      gender   = gender,
      min_age  = min_age,
      max_age  = max_age,
      end_date = end_date
    )
    simulate_life(p, modules, end_date)
  }

  if (mc.cores > 1L && .Platform$OS.type == "unix") {
    parallel::mclapply(person_seeds, simulate_one, mc.cores = mc.cores)
  } else {
    lapply(person_seeds, simulate_one)
  }
}

.validate_generate_population_args <- function(n, seed, gender, min_age, max_age,
                                               modules, end_date, mc.cores) {
  n <- .validate_count(n, "n", min = 1L)
  min_age <- .validate_count(min_age, "min_age", min = 0L)
  max_age <- .validate_count(max_age, "max_age", min = 0L)
  .validate_count(mc.cores, "mc.cores", min = 1L)

  if (min_age > max_age) {
    stop("`min_age` must be less than or equal to `max_age`.", call. = FALSE)
  }

  if (!is.null(seed)) {
    seed <- .validate_count(seed, "seed", min = 0L)
    if (as.numeric(seed) + as.numeric(n) - 1 > .Machine$integer.max) {
      stop("`seed + n - 1` must not exceed `.Machine$integer.max`.", call. = FALSE)
    }
  }

  if (!is.null(gender) &&
      !(is.character(gender) && length(gender) == 1L && gender %in% c("M", "F"))) {
    stop("`gender` must be NULL, \"M\", or \"F\".", call. = FALSE)
  }

  if (!inherits(end_date, "POSIXct") || length(end_date) != 1L || is.na(end_date)) {
    stop("`end_date` must be a single non-missing POSIXct value.", call. = FALSE)
  }

  if (!is.null(modules)) {
    if (!is.list(modules)) {
      stop("`modules` must be NULL or a list of Module objects.", call. = FALSE)
    }
    invalid <- vapply(modules, function(module) !inherits(module, "Module"), logical(1))
    if (any(invalid)) {
      stop("`modules` must contain only Module objects.", call. = FALSE)
    }
  }

  invisible(TRUE)
}

.validate_count <- function(value, name, min) {
  if (!is.numeric(value) || length(value) != 1L || is.na(value) ||
      !is.finite(value) || value != floor(value) || value < min ||
      value > .Machine$integer.max) {
    stop("`", name, "` must be a single integer >= ", min, ".", call. = FALSE)
  }
  as.integer(value)
}
