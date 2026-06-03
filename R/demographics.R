# R/demographics.R

sample_demographics <- function(person, state = NULL, city = NULL,
                                gender = NULL, min_age = 0L, max_age = 140L,
                                end_date = Sys.time()) {
  # Gender
  person@attributes[["gender"]] <- gender %||%
    sample(c("M", "F"), 1L, prob = c(0.49, 0.51))

  # Race (US census approximation)
  person@attributes[["race"]] <- sample(
    c("white", "black", "asian", "native", "other"),
    1L, prob = c(0.723, 0.127, 0.06, 0.02, 0.07)
  )

  # Ethnicity (slightly correlated with race)
  p_hisp <- if (person@attributes[["race"]] %in% c("other", "native")) 0.4 else 0.15
  person@attributes[["ethnicity"]] <- sample(
    c("hispanic", "non_hispanic"), 1L, prob = c(p_hisp, 1 - p_hisp)
  )

  # Socioeconomic status
  person@attributes[["socioeconomic_status"]] <- sample(
    c("low", "middle", "high"), 1L, prob = c(0.3, 0.5, 0.2)
  )

  # Age -> birth date
  min_age <- as.integer(min_age)
  max_age <- as.integer(max_age)
  age <- if (min_age == max_age) min_age
         else sample(seq(min_age, max_age), 1L, prob = .age_weights(min_age, max_age))
  person@attributes[["birth_date"]] <- end_date - age * 365.25 * 86400

  # Name
  person@attributes[["first_name"]] <- .sample_first_name(person@attributes[["gender"]])
  person@attributes[["last_name"]]  <- .sample_surname()

  # Location
  person@attributes[["state"]] <- state %||% "Massachusetts"
  person@attributes[["city"]]  <- city  %||% "Boston"

  person
}

.age_weights <- function(min_age, max_age) {
  ages <- seq(min_age, max_age)
  w <- ifelse(ages <= 40, 1.0, exp(-0.02 * (ages - 40)))
  w / sum(w)
}

.sample_first_name <- function(gender) {
  male   <- c("James", "John", "Robert", "Michael", "William",
               "David", "Joseph", "Charles", "Thomas", "Daniel")
  female <- c("Mary", "Patricia", "Jennifer", "Linda", "Barbara",
               "Elizabeth", "Susan", "Jessica", "Sarah", "Karen")
  sample(if (identical(gender, "M")) male else female, 1L)
}

.sample_surname <- function() {
  sample(c("Smith", "Johnson", "Williams", "Brown", "Jones",
           "Garcia", "Miller", "Davis", "Wilson", "Martinez"), 1L)
}
