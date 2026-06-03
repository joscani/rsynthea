# R/classes.R
library(S7)

Code <- new_class("rsynthea_Code",
  package    = NULL,
  properties = list(
    system  = class_character,
    code    = class_character,
    display = class_character
  )
)

Entry <- new_class("rsynthea_Entry",
  package    = NULL,
  properties = list(
    id    = class_character,
    time  = class_any,
    codes = class_list,
    name  = new_property(class = class_any, default = NULL)
  )
)

Encounter <- new_class("rsynthea_Encounter",
  package    = NULL,
  parent     = Entry,
  properties = list(
    encounter_class    = new_property(class = class_character, default = "ambulatory"),
    provider_id        = new_property(class = class_any, default = NULL),
    reason_code        = new_property(class = class_any, default = NULL),
    end_time           = new_property(class = class_any, default = NULL),
    conditions         = new_property(class = class_list, default = list()),
    procedures         = new_property(class = class_list, default = list()),
    medications        = new_property(class = class_list, default = list()),
    observations       = new_property(class = class_list, default = list()),
    careplans          = new_property(class = class_list, default = list()),
    immunizations      = new_property(class = class_list, default = list()),
    imaging_studies    = new_property(class = class_list, default = list()),
    devices            = new_property(class = class_list, default = list()),
    supplies           = new_property(class = class_list, default = list()),
    diagnostic_reports = new_property(class = class_list, default = list())
  )
)

Condition <- new_class("rsynthea_Condition",
  package    = NULL,
  parent     = Entry,
  properties = list(
    is_active = new_property(class = class_logical, default = TRUE),
    end_time  = new_property(class = class_any, default = NULL),
    cause     = new_property(class = class_any, default = NULL)
  )
)

Medication <- new_class("rsynthea_Medication",
  package    = NULL,
  parent     = Entry,
  properties = list(
    is_active    = new_property(class = class_logical, default = TRUE),
    end_time     = new_property(class = class_any, default = NULL),
    reasons      = new_property(class = class_list, default = list()),
    dosage       = new_property(class = class_any, default = NULL),
    duration     = new_property(class = class_any, default = NULL),
    prescription = new_property(class = class_any, default = NULL)
  )
)

Procedure <- new_class("rsynthea_Procedure",
  package    = NULL,
  parent     = Entry,
  properties = list(
    reasons  = new_property(class = class_list, default = list()),
    duration = new_property(class = class_any, default = NULL)
  )
)

Observation <- new_class("rsynthea_Observation",
  package    = NULL,
  parent     = Entry,
  properties = list(
    value    = new_property(class = class_any, default = NULL),
    unit     = new_property(class = class_any, default = NULL),
    category = new_property(class = class_any, default = NULL),
    obs_type = new_property(class = class_any, default = NULL)
  )
)

DiagnosticReport <- new_class("rsynthea_DiagnosticReport",
  package    = NULL,
  parent     = Entry,
  properties = list(
    observations = new_property(class = class_list, default = list())
  )
)

Immunization <- new_class("rsynthea_Immunization",
  package    = NULL,
  parent     = Entry,
  properties = list()
)

AllergyIntolerance <- new_class("rsynthea_AllergyIntolerance",
  package    = NULL,
  parent     = Entry,
  properties = list(
    is_active    = new_property(class = class_logical, default = TRUE),
    end_time     = new_property(class = class_any, default = NULL),
    allergy_type = new_property(class = class_any, default = NULL),
    category     = new_property(class = class_any, default = NULL)
  )
)

CarePlan <- new_class("rsynthea_CarePlan",
  package    = NULL,
  parent     = Entry,
  properties = list(
    is_active  = new_property(class = class_logical, default = TRUE),
    end_time   = new_property(class = class_any, default = NULL),
    reasons    = new_property(class = class_list, default = list()),
    activities = new_property(class = class_list, default = list())
  )
)

ImagingStudy <- new_class("rsynthea_ImagingStudy",
  package    = NULL,
  parent     = Entry,
  properties = list(
    series = new_property(class = class_list, default = list())
  )
)

Device <- new_class("rsynthea_Device",
  package    = NULL,
  parent     = Entry,
  properties = list(
    is_active = new_property(class = class_logical, default = TRUE),
    end_time  = new_property(class = class_any, default = NULL),
    udi       = new_property(class = class_any, default = NULL)
  )
)

HealthRecord <- new_class("HealthRecord",
  package    = NULL,
  properties = list(
    encounters    = new_property(class = class_list, default = list()),
    conditions    = new_property(class = class_list, default = list()),
    medications   = new_property(class = class_list, default = list()),
    procedures    = new_property(class = class_list, default = list()),
    observations  = new_property(class = class_list, default = list()),
    immunizations = new_property(class = class_list, default = list()),
    allergies     = new_property(class = class_list, default = list()),
    careplans     = new_property(class = class_list, default = list()),
    imaging       = new_property(class = class_list, default = list()),
    devices       = new_property(class = class_list, default = list()),
    reports       = new_property(class = class_list, default = list()),
    supplies      = new_property(class = class_list, default = list())
  )
)

# --- Person ---

Person <- new_class("Person",
  package    = NULL,
  properties = list(
    seed           = class_integer,
    id             = class_character,
    is_alive       = new_property(class = class_logical, default = TRUE),
    attributes     = new_property(class = class_list, default = list()),
    vital_signs    = new_property(class = class_list, default = list()),
    symptoms       = new_property(class = class_list, default = list()),
    module_history = new_property(class = class_list, default = list()),
    health_record  = new_property(class = class_any, default = NULL)
  ),
  constructor = function(seed = NULL) {
    seed <- if (is.null(seed)) sample.int(.Machine$integer.max, 1L) else as.integer(seed)
    id   <- substr(digest::digest(seed, algo = "md5"), 1L, 16L)
    new_object(S7_object(),
      seed           = seed,
      id             = id,
      is_alive       = TRUE,
      attributes     = list(),
      vital_signs    = list(),
      symptoms       = list(),
      module_history = list(),
      health_record  = HealthRecord()
    )
  }
)

# Generic: age_at(person, time) -> numeric years
age_at <- new_generic("age_at", "x")
method(age_at, Person) <- function(x, time) {
  birth <- x@attributes[["birth_date"]]
  if (is.null(birth)) return(0)
  birth_d <- as.Date(format(birth, "%Y-%m-%d"))
  time_d  <- as.Date(format(time,  "%Y-%m-%d"))
  years <- as.integer(format(time_d,  "%Y")) - as.integer(format(birth_d, "%Y"))
  # Subtract 1 if anniversary hasn't occurred yet this year
  had_birthday <- (as.integer(format(time_d, "%m")) > as.integer(format(birth_d, "%m"))) ||
    (as.integer(format(time_d, "%m")) == as.integer(format(birth_d, "%m")) &&
     as.integer(format(time_d, "%d")) >= as.integer(format(birth_d, "%d")))
  if (!had_birthday) years <- years - 1L
  as.numeric(years)
}
