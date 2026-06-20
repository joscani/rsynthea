# R/module.R
# GMFState is a hashed environment — O(1) field access regardless of order.
# (Previously a named list with O(N) linear scan; ordering no longer matters.)

GMFState <- function(name, type, definition, transition, module_name = "") {
  wk_prefix <- if (nzchar(module_name)) paste0(module_name, "/") else ""
  e <- new.env(parent = emptyenv(), hash = TRUE)
  e$type           <- type
  e$transition     <- transition
  e$visited_key    <- paste0("__visited__", name)
  e$call_key       <- paste0("__call_submodule__", name)
  e$delay_key      <- paste0("__delay_until__", name)
  e$guard_allow    <- definition[["allow"]]
  e$is_wellness    <- isTRUE(definition[["wellness"]])
  e$wellness_key   <- paste0("__wellness_time__", wk_prefix, name)
  e$name           <- name
  e$codes          <- .parse_codes(.state_codes(definition))
  e$definition     <- definition
  e$encounter_class <- definition[["encounter_class"]] %||% "ambulatory"
  e$activities     <- .parse_codes(definition[["activities"]])
  e$sub_codes      <- lapply(definition[["observations"]] %||% list(),
                             function(o) .parse_codes(o[["codes"]]))
  e$cond_key       <- paste0("__condition_env__", name)
  e$med_key        <- paste0("__medication_env__", name)
  e$cp_key         <- paste0("__careplan_env__", name)
  e$allergy_key    <- paste0("__allergy_env__", name)
  e$device_key     <- paste0("__device_ref__", name)
  e$allergy_type   <- definition[["allergy_type"]] %||% NULL
  e$category       <- definition[["category"]] %||% NULL
  e$unit           <- definition[["unit"]] %||% NULL
  e$series         <- definition[["series"]] %||% list()
  e$vs_name        <- definition[["vital_sign"]] %||% ""
  e$sym_name       <- definition[["symptom"]] %||% ""
  e$sym_cause      <- definition[["cause"]] %||% NULL
  e$submodule_name <- definition[["submodule"]] %||% ""
  e$cond_end_key   <- paste0("__condition_env__",
                             definition[["condition_onset"]] %||% "")
  e$med_end_key    <- paste0("__medication_env__",
                             definition[["medication_order"]] %||% "")
  e$cp_end_key     <- paste0("__careplan_env__",
                             definition[["careplan"]] %||% "")
  e$alg_end_key    <- paste0("__allergy_env__",
                             definition[["allergy_onset"]] %||% "")
  e$device_end_key <- paste0("__device_ref__",
                             definition[["device"]] %||% "")
  e$attr_name      <- definition[["attribute"]] %||% NULL
  e$attr_value     <- definition[["value"]]
  e$counter_action <- definition[["action"]] %||% "increment"
  e$counter_amount <- as.numeric(definition[["amount"]] %||% 1)
  e
}

.state_codes <- function(definition) {
  codes <- definition[["codes"]]
  if (!is.null(codes)) return(codes)
  code <- definition[["code"]]
  if (!is.null(code)) list(code) else list()
}

Module <- function(name, states, submodules = list(), is_submodule = FALSE) {
  structure(
    list(
      name         = name,
      states       = states,
      submodules   = submodules,
      is_submodule = is_submodule,
      state_key    = paste0("__module_state__", name)
    ),
    class = "Module"
  )
}

load_module <- function(path) {
  raw        <- jsonlite::read_json(path, simplifyVector = FALSE)
  name       <- raw[["name"]] %||% tools::file_path_sans_ext(basename(path))
  states_raw <- raw[["states"]] %||% list()

  states_list <- Map(function(s, nm) {
    GMFState(
      name        = nm,
      type        = s[["type"]] %||% "Simple",
      definition  = s,
      transition  = parse_transition(s),
      module_name = name
    )
  }, states_raw, names(states_raw))

  states_env <- list2env(states_list, parent = emptyenv(), hash = TRUE)

  Module(name = name, states = states_env)
}

#' Load all GMF modules from a directory
#'
#' Reads every `.json` file in `modules_dir` (recursively) and parses each one
#' into a `Module` object. Modules that fail to parse emit a warning and are
#' silently dropped.
#'
#' @param modules_dir Character or `NULL`. Path to the directory containing
#'   module JSON files. Defaults to `inst/extdata/modules/` inside the installed
#'   package.
#'
#' @return A named list of `Module` objects, keyed by module path relative to
#'   `modules_dir` (e.g. `"diabetes"`, `"medications/metformin"`). Can be passed
#'   to `generate_population(modules = ...)` to override the session cache, or to
#'   [compile_all_modules()] to produce C++-ready structs.
#'
#' @details
#' You rarely need to call this directly. [generate_population()] loads and
#' caches modules automatically on the first call. Use `load_all_modules()`
#' explicitly only when working with **custom modules** in a non-default
#' directory, or when you want to inspect or modify the module list before
#' passing it to [compile_all_modules()].
#'
#' The bundled module set has 243 modules. Each `Module` stores its states as a
#' hashed environment so that state lookup is O(1).
#'
#' @examples
#' \dontrun{
#' # Inspect the bundled modules
#' m <- load_all_modules()
#' length(m)       # 243
#' names(m)[1:5]
#'
#' # Load custom modules from a local directory
#' m_custom <- load_all_modules("path/to/my/modules")
#' tbls <- generate_population(10, seed = 1L, modules = m_custom,
#'                             end_date = as.POSIXct("2020-01-01"))
#' }
#'
#' @seealso [generate_population()], [compile_all_modules()]
#' @export
load_all_modules <- function(modules_dir = NULL) {
  if (is.null(modules_dir)) {
    modules_dir <- system.file("extdata/modules", package = "rsynthea")
  }
  modules_dir <- normalizePath(modules_dir, mustWork = FALSE)
  json_files  <- list.files(
    modules_dir, pattern = "\\.json$", full.names = TRUE, recursive = TRUE
  )
  modules <- lapply(json_files, function(f) {
    rel    <- substring(normalizePath(f, mustWork = FALSE), nchar(modules_dir) + 2L)
    key    <- tools::file_path_sans_ext(rel)
    is_sub <- grepl("/", key, fixed = TRUE)
    tryCatch({
      m              <- load_module(f)
      m$is_submodule <- is_sub
      m$.key         <- key
      m
    }, error = function(e) {
      warning("Failed to load module: ", basename(f), " -- ", conditionMessage(e))
      NULL
    })
  })
  modules <- Filter(Negate(is.null), modules)
  stats::setNames(modules, vapply(modules, function(m) m$.key, character(1)))
}
