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
    "lookup_table" = .resolve_lookup(transition$entries, person, time),
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
  if (length(entries) == 0) return(NULL)
  weights <- vapply(entries, function(e) .resolve_weight(e[["distribution"]], person), numeric(1))
  total <- sum(weights)
  if (total <= 0) return(entries[[length(entries)]][["transition"]])
  r <- stats::runif(1) * total
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

.LOOKUP_CACHE <- new.env(parent = emptyenv(), hash = TRUE)

.resolve_lookup <- function(entries, person, time = NULL) {
  if (length(entries) == 0) return(NULL)
  weights <- .lookup_weights(entries, person, time)
  total <- sum(weights)
  if (total <= 0) return(entries[[length(entries)]][["transition"]])
  r <- stats::runif(1) * total
  cumw <- 0
  for (i in seq_along(entries)) {
    cumw <- cumw + weights[[i]]
    if (r < cumw) return(entries[[i]][["transition"]])
  }
  entries[[length(entries)]][["transition"]]
}

.lookup_weights <- function(entries, person, time) {
  defaults <- vapply(entries,
    function(e) as.numeric(e[["default_probability"]] %||% 0), numeric(1))
  table_name <- entries[[1L]][["lookup_table_name"]]
  table <- .read_lookup_table(table_name)
  if (is.null(table) || nrow(table) == 0L) return(defaults)

  row <- .match_lookup_row(table, entries, person, time)
  if (is.null(row)) return(defaults)

  weights <- defaults
  for (i in seq_along(entries)) {
    col <- .lookup_probability_column(row, entries[[i]][["transition"]])
    if (!is.null(col)) {
      value <- suppressWarnings(as.numeric(row[[col]][[1L]]))
      if (!is.na(value)) weights[[i]] <- value
    }
  }
  weights
}

.read_lookup_table <- function(table_name) {
  if (is.null(table_name) || !nzchar(table_name)) return(NULL)
  cached <- .LOOKUP_CACHE[[table_name]]
  if (identical(cached, FALSE)) return(NULL)
  if (!is.null(cached)) return(cached)

  path <- .lookup_table_path(table_name)
  if (is.null(path)) {
    .LOOKUP_CACHE[[table_name]] <- FALSE
    return(NULL)
  }
  table <- tryCatch(
    suppressWarnings(utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)),
    error = function(e) NULL
  )
  if (!is.null(table)) table <- .prepare_lookup_table(table)
  .LOOKUP_CACHE[[table_name]] <- table %||% FALSE
  table
}

.lookup_table_path <- function(table_name) {
  candidates <- c(
    table_name,
    system.file("lt", table_name, package = "rsynthea"),
    system.file("extdata/lookup_tables", table_name, package = "rsynthea"),
    system.file("extdata/modules/lookup_tables", table_name, package = "rsynthea"),
    system.file("extdata/resources", table_name, package = "rsynthea"),
    system.file("extdata/modules", table_name, package = "rsynthea")
  )
  candidates <- candidates[nzchar(candidates)]
  matches <- candidates[file.exists(candidates)]
  if (length(matches) == 0L) NULL else matches[[1L]]
}

.match_lookup_row <- function(table, entries, person, time) {
  probability_cols <- .probability_columns(table, entries)
  criteria_cols <- setdiff(names(table), probability_cols)
  if (length(criteria_cols) == 0L) return(table[1L, , drop = FALSE])

  parsed <- attr(table, ".rsynthea_lookup")
  if (is.null(parsed)) {
    table <- .prepare_lookup_table(table)
    parsed <- attr(table, ".rsynthea_lookup")
  }

  matches <- rep(TRUE, nrow(table))
  for (col in criteria_cols) {
    criterion <- parsed[[col]]
    actual <- .lookup_actual_value(col, person, time)
    if (is.null(actual) || is.na(actual)) {
      matches <- matches & criterion$blank
    } else {
      actual_chr <- tolower(as.character(actual))
      actual_num <- suppressWarnings(as.numeric(actual))
      col_matches <- criterion$blank
      if (!is.na(actual_num)) {
        col_matches <- col_matches |
          (criterion$is_range & actual_num >= criterion$low & actual_num <= criterion$high)
      }
      col_matches <- col_matches | (!criterion$is_range & criterion$lower == actual_chr)
      matches <- matches & col_matches
    }
    if (!any(matches)) return(NULL)
  }

  table[which(matches)[[1L]], , drop = FALSE]
}

.probability_columns <- function(table, entries) {
  unique(unlist(Filter(Negate(is.null), lapply(entries, function(entry) {
    .lookup_probability_column(table, entry[["transition"]])
  })), use.names = FALSE))
}

.lookup_criterion_matches <- function(column, expected, person, time) {
  if (is.na(expected) || !nzchar(as.character(expected))) return(TRUE)
  actual <- .lookup_actual_value(column, person, time)
  if (is.null(actual) || is.na(actual)) return(FALSE)
  expected <- as.character(expected)

  actual_num <- suppressWarnings(as.numeric(actual))
  range <- strsplit(expected, "-", fixed = TRUE)[[1L]]
  if (length(range) == 2L && !is.na(actual_num)) {
    low <- suppressWarnings(as.numeric(trimws(range[[1L]])))
    high <- suppressWarnings(as.numeric(trimws(range[[2L]])))
    if (!is.na(low) && !is.na(high)) return(actual_num >= low && actual_num <= high)
  }

  tolower(as.character(actual)) == tolower(expected)
}

.lookup_actual_value <- function(column, person, time) {
  key <- tolower(column)
  if (key %in% c("age", "age_years")) {
    if (is.null(time)) return(NULL)
    return(age_at(person, time))
  }
  if (key %in% c("time", "date")) {
    t_num <- .REC$e$.t_num %||% if (!is.null(time)) as.numeric(time) else NULL
    if (is.null(t_num)) return(NULL)
    return(t_num * 1000)
  }
  person@attributes[[column]] %||% person@attributes[[key]]
}

.lookup_probability_column <- function(row, transition) {
  candidates <- c(transition, make.names(transition), gsub("[^A-Za-z0-9]+", "_", transition))
  match <- candidates[candidates %in% names(row)]
  if (length(match) == 0L) NULL else match[[1L]]
}

.prepare_lookup_table <- function(table) {
  parsed <- stats::setNames(vector("list", length(names(table))), names(table))
  for (col in names(table)) {
    values <- as.character(table[[col]])
    blank <- is.na(values) | !nzchar(values)
    parts <- strsplit(values, "-", fixed = TRUE)
    is_range <- lengths(parts) == 2L
    low <- rep(NA_real_, length(values))
    high <- rep(NA_real_, length(values))
    if (any(is_range)) {
      low[is_range] <- suppressWarnings(as.numeric(trimws(vapply(
        parts[is_range], `[[`, character(1), 1L
      ))))
      high[is_range] <- suppressWarnings(as.numeric(trimws(vapply(
        parts[is_range], `[[`, character(1), 2L
      ))))
    }
    is_range <- is_range & !is.na(low) & !is.na(high)
    parsed[[col]] <- list(
      blank = blank,
      lower = tolower(values),
      is_range = is_range,
      low = low,
      high = high
    )
  }
  attr(table, ".rsynthea_lookup") <- parsed
  table
}
