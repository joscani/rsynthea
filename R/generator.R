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
#' @param modules List of Module objects or `NULL`. If `NULL`, all modules are
#'   loaded from `inst/extdata/modules/` via [load_all_modules()].
#' @param end_date POSIXct. Simulation end date. Default `Sys.time()`.
#' @param mc.cores Integer. Number of parallel workers. Values > 1 use
#'   `parallel::mclapply` (fork-based; Unix/macOS only). Default `1L`.
#'
#' @return A list of `n` `Person` objects with populated `.record` environments
#'   containing encounters, conditions, medications, etc.
#'
#' @details
#' With `mc.cores > 1`, each patient is simulated in a forked child process.
#' The global `.REC$e` state is isolated per fork, so parallelism is safe.
#' Note that `.new_id()` counters reset per child: IDs are unique within a
#' patient but may collide across patients in the same run. Use
#' [export_population()] to assign globally-unique identifiers.
#'
#' Speedup with 12 physical cores (macOS M-series, 2026):
#' \itemize{
#'   \item 10 patients: ~3.6×
#'   \item 50 patients: ~6.5×
#'   \item 100 patients: ~7×
#' }
#'
#' @examples
#' \dontrun{
#' modules <- load_all_modules()
#'
#' # Serial
#' patients <- generate_population(10, seed = 42L, modules = modules,
#'                                 end_date = as.POSIXct("2020-01-01"))
#'
#' # Parallel (Unix/macOS)
#' patients <- generate_population(100, seed = 1L, modules = modules,
#'                                 end_date = as.POSIXct("2020-01-01"),
#'                                 mc.cores = parallel::detectCores(logical = FALSE))
#' }
#'
#' @seealso [export_population()], [load_all_modules()], [simulate_life()]
#' @export
generate_population <- function(
  n        = 1L,
  seed     = NULL,
  state    = NULL,
  city     = NULL,
  gender   = NULL,
  min_age  = 0L,
  max_age  = 140L,
  modules  = NULL,
  end_date = Sys.time(),
  mc.cores = 1L
) {
  .validate_generate_population_args(n, seed, gender, min_age, max_age,
                                     modules, end_date, mc.cores)

  if (is.null(modules)) {
    modules <- load_all_modules()
  }

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
