# R/simulation.R

TIMESTEP_SECS <- 7L * 86400L  # 1 week in seconds

simulate_life <- function(person, modules, end_date = Sys.time()) {
  current_time <- person@attributes[["birth_date"]] %||% end_date

  while (current_time <= end_date && person@is_alive) {
    for (module in modules) {
      person <- advance_module(person, module, current_time, modules)
      if (!person@is_alive) break
    }
    current_time <- current_time + TIMESTEP_SECS
  }
  person
}

advance_module <- function(person, module, time, all_modules = list()) {
  state_key    <- paste0("__module_state__", module@name)
  current_name <- person@attributes[[state_key]] %||% "Initial"

  if (identical(current_name, "__terminal__")) return(person)

  max_iter <- 500L
  iter     <- 0L

  while (iter < max_iter) {
    iter  <- iter + 1L
    state <- module@states[[current_name]]
    if (is.null(state)) break

    result      <- process_state(state, person, time)
    person      <- result$person
    next_name   <- result$next_state

    # Terminal
    if (is.null(next_name)) {
      person@attributes[[state_key]] <- "__terminal__"
      break
    }

    # CallSubmodule: run inline before advancing
    call_key <- paste0("__call_submodule__", state@name)
    sub_name <- person@attributes[[call_key]]
    if (!is.null(sub_name) && !is.null(all_modules[[sub_name]])) {
      person@attributes[[call_key]] <- NULL
      person <- advance_module(person, all_modules[[sub_name]], time, all_modules)
    }

    # Stay (Delay / Guard)
    if (next_name == current_name) break

    current_name <- next_name
    person@attributes[[state_key]] <- current_name

    # If newly entered state is Terminal, process and stop
    next_state_obj <- module@states[[current_name]]
    if (!is.null(next_state_obj) && next_state_obj@type == "Terminal") {
      person@attributes[[state_key]] <- "__terminal__"
      break
    }

    # Stop if person died mid-module
    if (!person@is_alive) break
  }

  person
}
