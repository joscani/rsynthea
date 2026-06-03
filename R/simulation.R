# R/simulation.R

TIMESTEP_SECS <- 7L * 86400L  # 1 week in seconds

#' Simulate one patient's life through all GMF modules
#'
#' Advances `person` through `modules` in weekly timesteps from their birth
#' date to `end_date`, mutating the patient's `.record` environment in-place.
#'
#' @param person A `Person` object with `birth_date` in `@@attributes`.
#' @param modules Named list of `Module` objects, as returned by
#'   [load_all_modules()].
#' @param end_date POSIXct. Simulation end date. Default `Sys.time()`.
#'
#' @return The updated `person` object. Clinical events (encounters,
#'   conditions, medications, etc.) are stored in `person@@.record`.
#'
#' @details
#' **Hot-path design**: `.REC$e` is set to `person@@.record` once per call so
#' that state handlers access clinical data without S7 dispatch. `rec$.t_num`
#' caches `as.numeric(current_time)` per timestep. Terminal modules are
#' skipped via a pre-check before calling `advance_module()`.
#'
#' The weekly timestep (`TIMESTEP_SECS = 7 * 86400`) is the same as
#' py-synthea's default.
#'
#' @seealso [generate_population()], `advance_module()`
#' @export
simulate_life <- function(person, modules, end_date = Sys.time()) {
  birth <- person@attributes[["birth_date"]] %||% end_date
  t_cur <- as.numeric(birth)
  t_end <- as.numeric(end_date)

  # Cache .record and is_alive flag in .REC once per patient to avoid S7 dispatch
  # on every state handler and every loop iteration.
  rec <- person@.record
  .REC$e <- rec
  rec$.is_alive <- person@is_alive

  while (t_cur <= t_end && rec$.is_alive) {
    current_time <- .POSIXct(t_cur)
    rec$.t_num   <- t_cur
    for (module in modules) {
      if (identical(rec[[module$state_key]], "__terminal__")) next
      person <- advance_module(person, module, current_time, modules)
      if (!rec$.is_alive) break
    }
    t_cur <- t_cur + TIMESTEP_SECS
  }
  if (!rec$.is_alive) person@is_alive <- FALSE
  person
}

advance_module <- function(person, module, time, all_modules = list()) {
  mod_states   <- module$states
  state_key    <- module$state_key
  rec          <- .REC$e  # constant for this patient's lifetime — never re-read
  current_name <- rec[[state_key]]
  if (is.null(current_name)) current_name <- "Initial"

  if (identical(current_name, "__terminal__")) return(person)

  max_iter <- 500L
  iter     <- 0L

  while (iter < max_iter) {
    iter  <- iter + 1L
    state <- mod_states[[current_name]]
    if (is.null(state)) break

    # Wellness bypass: same-timestep duplicate guard without full call stack
    if (state[["type"]] == "Encounter" && state[["is_wellness"]]) {
      wt <- rec[[state[["wellness_key"]]]]
      if (!is.null(wt) && wt >= rec$.t_num) break
    }

    result    <- process_state(state, person, time)
    person    <- result[[1L]]
    next_name <- result[[2L]]

    # Terminal
    if (is.null(next_name)) {
      rec[[state_key]] <- "__terminal__"
      break
    }

    # CallSubmodule: run inline before advancing
    sub_name <- rec[[state[["call_key"]]]]
    if (!is.null(sub_name) && !is.null(all_modules[[sub_name]])) {
      rec[[state[["call_key"]]]] <- NULL
      person <- advance_module(person, all_modules[[sub_name]], time, all_modules)
    }

    # Stay (Delay / Guard)
    if (next_name == current_name) break

    current_name <- next_name
    rec[[state_key]] <- current_name

    # Stop if person died mid-module
    if (!rec$.is_alive) break
  }

  person
}
