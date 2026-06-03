# R/module.R
# GMFState and Module are plain named lists. Fields ordered by descending access
# frequency so R's linear-search [[]] scans fewer names in the hot path:
#   type, transition, visited_key, call_key  (~1M accesses each per patient)
#   name, codes, definition                  (~100-200K accesses each)
#   ...rest

GMFState <- function(name, type, definition, transition) {
  list(
    type             = type,
    transition       = transition,
    visited_key      = paste0("__visited__", name),
    call_key         = paste0("__call_submodule__", name),
    is_wellness      = isTRUE(definition[["wellness"]]),
    wellness_key     = paste0("__wellness_time__", name),
    name             = name,
    codes            = .parse_codes(.state_codes(definition)),
    definition       = definition,
    encounter_class  = definition[["encounter_class"]] %||% "ambulatory",
    activities       = .parse_codes(definition[["activities"]]),
    sub_codes        = lapply(definition[["observations"]] %||% list(),
                              function(o) .parse_codes(o[["codes"]])),
    delay_key        = paste0("__delay_until__", name),
    cond_key         = paste0("__condition_env__", name),
    med_key          = paste0("__medication_env__", name),
    cp_key           = paste0("__careplan_env__", name),
    allergy_key      = paste0("__allergy_env__", name),
    device_key       = paste0("__device_ref__", name),
    allergy_type     = definition[["allergy_type"]] %||% NULL,
    category         = definition[["category"]] %||% NULL,
    unit             = definition[["unit"]] %||% NULL,
    series           = definition[["series"]] %||% list(),
    vs_name          = definition[["vital_sign"]] %||% "",
    sym_name         = definition[["symptom"]] %||% "",
    sym_cause        = definition[["cause"]] %||% NULL,
    submodule_name   = definition[["submodule"]] %||% "",
    cond_end_key     = paste0("__condition_env__",
                               definition[["condition_onset"]] %||% ""),
    med_end_key      = paste0("__medication_env__",
                               definition[["medication_order"]] %||% ""),
    cp_end_key       = paste0("__careplan_env__",
                               definition[["careplan"]] %||% ""),
    alg_end_key      = paste0("__allergy_env__",
                               definition[["allergy_onset"]] %||% ""),
    device_end_key   = paste0("__device_ref__",
                               definition[["device"]] %||% ""),
    attr_name        = definition[["attribute"]] %||% NULL,
    attr_value       = definition[["value"]],
    counter_action   = definition[["action"]] %||% "increment",
    counter_amount   = as.numeric(definition[["amount"]] %||% 1),
    guard_allow      = definition[["allow"]]
  )
}

.state_codes <- function(definition) {
  codes <- definition[["codes"]]
  if (!is.null(codes)) return(codes)
  code <- definition[["code"]]
  if (!is.null(code)) list(code) else list()
}

Module <- function(name, states, submodules = list()) {
  structure(
    list(
      name       = name,
      states     = states,
      submodules = submodules,
      state_key  = paste0("__module_state__", name)
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
      name       = nm,
      type       = s[["type"]] %||% "Simple",
      definition = s,
      transition = parse_transition(s)
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
#' @return A named list of `Module` objects, keyed by module name. The list is
#'   passed directly to [generate_population()] or [simulate_life()].
#'
#' @details
#' Each `Module` stores its states as a **hashed environment** (`list2env(...,
#' hash = TRUE)`) so that `mod_states[[state_name]]` is O(1). The bundled
#' module set has 242 modules with a median of 19 states each.
#'
#' Loading takes ~1–2 s. Cache the result and reuse it across calls to
#' [generate_population()] to avoid repeated I/O.
#'
#' @examples
#' \dontrun{
#' modules <- load_all_modules()
#' length(modules)  # 242
#'
#' patients <- generate_population(10, seed = 1L, modules = modules,
#'                                 end_date = as.POSIXct("2020-01-01"))
#' }
#'
#' @seealso [generate_population()], [simulate_life()]
#' @export
load_all_modules <- function(modules_dir = NULL) {
  if (is.null(modules_dir)) {
    modules_dir <- system.file("extdata/modules", package = "rsynthea")
  }
  json_files <- list.files(
    modules_dir, pattern = "\\.json$", full.names = TRUE, recursive = TRUE
  )
  modules <- lapply(json_files, function(f) {
    tryCatch(
      load_module(f),
      error = function(e) {
        warning("Failed to load module: ", basename(f), " -- ", conditionMessage(e))
        NULL
      }
    )
  })
  modules <- Filter(Negate(is.null), modules)
  stats::setNames(modules, vapply(modules, function(m) m$name, character(1)))
}
