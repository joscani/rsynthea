# R/cpp_engine.R — thin R wrapper around the C++ simulation engine.
# Called by generate_population() when use_cpp = TRUE.

#' Generate a population using the C++ engine (internal)
#' @keywords internal
.generate_population_cpp <- function(
  n, seed, state, city, gender, min_age, max_age, modules, end_date,
  mc.cores, cpp_modules
) {
  # 1. Generate demographics for all N patients
  person_seeds <- if (!is.null(seed)) seed + seq_len(n) - 1L
                  else sample.int(.Machine$integer.max, n)

  t_end_num <- as.numeric(end_date)

  persons <- lapply(seq_len(n), function(i) {
    set.seed(person_seeds[i])
    p <- Person(seed = as.integer(person_seeds[i]))
    sample_demographics(p, state=state, city=city, gender=gender,
                        min_age=min_age, max_age=max_age, end_date=end_date)
  })

  pop_attrs <- lapply(persons, function(p) {
    # Merge vital_signs into attributes so C++ can initialise them
    attrs <- p@attributes
    vs <- p@vital_signs
    for (nm in names(vs)) {
      val <- vs[[nm]][["value"]]
      if (!is.null(val)) attrs[[paste0(".vs.", nm)]] <- as.numeric(val)
    }
    attrs
  })

  birth_nums <- vapply(pop_attrs, function(a) {
    b <- a[["birth_date"]]
    if (is.null(b)) NA_real_ else as.numeric(b)
  }, numeric(1L))

  pat_ids <- vapply(person_seeds, function(s) {
    substr(digest::digest(as.integer(s), algo = "md5"), 1L, 16L)
  }, character(1L))

  # 2. Run C++ simulation + export
  batch_size <- max(1L, min(10L, ceiling(n / max(mc.cores, 1L))))
  batches    <- .split_chunks(seq_len(n), ceiling(n / batch_size))

  pb <- txtProgressBar(min = 0, max = n, style = 3,
                       width = 50, char = "=")
  done <- 0L

  if (mc.cores > 1L && .Platform$OS.type == "unix") {
    # Assign batches to cores
    core_chunks <- .split_chunks(seq_along(batches), mc.cores)

    results_nested <- parallel::mclapply(core_chunks, function(bidxs) {
      lapply(bidxs, function(bi) {
        idx <- batches[[bi]]
        generate_and_export_cpp(
          population_attrs = pop_attrs[idx],
          birth_nums       = birth_nums[idx],
          patient_ids      = pat_ids[idx],
          t_end            = t_end_num,
          seeds            = as.integer(person_seeds[idx]),
          modules_xptr     = cpp_modules
        )
      })
    }, mc.cores = mc.cores)

    # Flatten nested list of batch results
    batch_results <- unlist(results_nested, recursive = FALSE)

  } else {
    batch_results <- lapply(seq_along(batches), function(bi) {
      idx <- batches[[bi]]
      res <- generate_and_export_cpp(
        population_attrs = pop_attrs[idx],
        birth_nums       = birth_nums[idx],
        patient_ids      = pat_ids[idx],
        t_end            = t_end_num,
        seeds            = as.integer(person_seeds[idx]),
        modules_xptr     = cpp_modules
      )
      done <<- done + length(idx)
      setTxtProgressBar(pb, done)
      res
    })
  }

  # For parallel: update bar to 100% after all workers finish
  if (mc.cores > 1L) setTxtProgressBar(pb, n)
  close(pb)

  # Combine batches
  table_names <- names(batch_results[[1]])
  result <- stats::setNames(
    lapply(table_names, function(nm) {
      do.call(rbind, lapply(batch_results, `[[`, nm))
    }),
    table_names
  )

  tbls <- lapply(result, tibble::as_tibble)

  # Summary similar to Java/Python output
  alive <- sum(tbls$patients$is_alive, na.rm = TRUE)
  total <- nrow(tbls$patients)
  message(sprintf("Records: total=%d, alive=%d, dead=%d", total, alive, total - alive))

  tbls
}

# Split 1:n into at most k roughly-equal integer chunks
.split_chunks <- function(idx, k) {
  n <- length(idx)
  k <- min(k, n)
  sizes <- rep(n %/% k, k)
  sizes[seq_len(n %% k)] <- sizes[seq_len(n %% k)] + 1L
  ends   <- cumsum(sizes)
  starts <- c(1L, ends[-k] + 1L)
  lapply(seq_len(k), function(i) idx[starts[i]:ends[i]])
}
