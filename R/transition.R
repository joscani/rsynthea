# R/transition.R

parse_transition <- function(state_def) {
  if ("direct_transition" %in% names(state_def)) {
    list(type = "direct", target = state_def[["direct_transition"]])
  } else if ("transition" %in% names(state_def)) {
    list(type = "direct", target = state_def[["transition"]])
  } else if ("distributed_transition" %in% names(state_def)) {
    list(type = "distributed", entries = state_def[["distributed_transition"]])
  } else if ("conditional_transition" %in% names(state_def)) {
    list(type = "conditional", entries = state_def[["conditional_transition"]])
  } else if ("complex_transition" %in% names(state_def)) {
    list(type = "complex", entries = state_def[["complex_transition"]])
  } else if ("lookup_table_transition" %in% names(state_def)) {
    list(type = "lookup_table", entries = state_def[["lookup_table_transition"]])
  } else {
    NULL
  }
}

resolve_transition <- function(transition, person, time) {
  if (is.null(transition)) return(NULL)
  switch(transition$type,
    "direct"       = transition$target,
    "distributed"  = .resolve_distributed(transition$entries, person),
    "conditional"  = .resolve_conditional(transition$entries, person, time),
    "complex"      = .resolve_complex(transition$entries, person, time),
    "lookup_table" = .resolve_lookup(transition$entries, person),
    NULL
  )
}

.resolve_weight <- function(dist_val, person) {
  if (is.list(dist_val)) {
    attr_name <- dist_val[["attribute"]]
    default   <- as.numeric(dist_val[["default"]] %||% 0)
    if (!is.null(attr_name)) as.numeric(person@attributes[[attr_name]] %||% default)
    else default
  } else {
    as.numeric(dist_val %||% 0)
  }
}

.resolve_distributed <- function(entries, person) {
  weights <- vapply(entries, function(e) .resolve_weight(e[["distribution"]], person), numeric(1))
  total <- sum(weights)
  if (total <= 0) return(entries[[length(entries)]][["transition"]])
  r <- runif(1) * total
  cumw <- 0
  for (i in seq_along(entries)) {
    cumw <- cumw + weights[[i]]
    if (r < cumw) return(entries[[i]][["transition"]])
  }
  entries[[length(entries)]][["transition"]]
}

.resolve_conditional <- function(entries, person, time) {
  for (e in entries) {
    if (is.null(e[["condition"]]) || evaluate_condition(e[["condition"]], person, time)) {
      return(e[["transition"]])
    }
  }
  NULL
}

.resolve_complex <- function(entries, person, time) {
  matching <- Filter(function(e) {
    is.null(e[["condition"]]) || evaluate_condition(e[["condition"]], person, time)
  }, entries)
  if (length(matching) == 0) return(NULL)
  first <- matching[[1]]
  if (!is.null(first[["distributions"]])) {
    .resolve_distributed(first[["distributions"]], person)
  } else {
    first[["transition"]]
  }
}

.resolve_lookup <- function(entries, person) {
  weights <- vapply(entries,
    function(e) as.numeric(e[["default_probability"]] %||% 0), numeric(1))
  total <- sum(weights)
  if (total <= 0) return(entries[[length(entries)]][["transition"]])
  r <- runif(1) * total
  cumw <- 0
  for (i in seq_along(entries)) {
    cumw <- cumw + weights[[i]]
    if (r < cumw) return(entries[[i]][["transition"]])
  }
  entries[[length(entries)]][["transition"]]
}
