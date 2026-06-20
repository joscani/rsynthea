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
#' Most users should call [generate_population()] instead, which handles module
#' loading, caching, and export automatically. `simulate_life()` is the R
#' engine's inner loop and is exported for advanced use cases such as custom
#' simulation harnesses, debugging, or profiling individual patients.
#'
#' The weekly timestep (`TIMESTEP_SECS = 7 * 86400`) matches py-synthea's
#' default. Clinical events are written to `person@@.record` in-place via the
#' `.REC` thread-local cache to avoid repeated S7 dispatch on the hot path.
#'
#' @seealso [generate_population()], [load_all_modules()]
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
  # ponytail: attrs cache avoids S7 @-dispatch in .cond_attribute (4.77% self); writers keep in sync
  rec$.attrs <- person@attributes

  # Pre-extract per-module hot fields so the inner loop uses [[i]] on vectors
  # instead of $-search on each Module list (saves ~8.8M $ calls + isTRUE per patient).
  mod_list <- unname(modules)
  n_mods   <- length(mod_list)
  is_sub   <- vapply(mod_list, `[[`, logical(1L),   "is_submodule")
  st_keys  <- vapply(mod_list, `[[`, character(1L), "state_key")

  while (t_cur <= t_end && rec$.is_alive) {
    current_time <- .POSIXct(t_cur)
    rec$.t_num   <- t_cur
    for (i in seq_len(n_mods)) {
      if (is_sub[[i]]) next
      if (identical(rec[[st_keys[[i]]]], "__terminal__")) next
      person <- advance_module(person, mod_list[[i]], current_time, modules)
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
