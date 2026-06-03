# R/generator.R

generate_population <- function(
  n        = 1L,
  seed     = NULL,
  state    = NULL,
  city     = NULL,
  gender   = NULL,
  min_age  = 0L,
  max_age  = 140L,
  modules  = NULL,
  end_date = Sys.time()
) {
  if (is.null(modules)) {
    modules <- load_all_modules()
  }

  patients <- vector("list", n)
  for (i in seq_len(n)) {
    person_seed <- if (!is.null(seed)) seed + i - 1L
                   else sample.int(.Machine$integer.max, 1L)
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
    p <- simulate_life(p, modules, end_date)
    patients[[i]] <- p
  }
  patients
}
