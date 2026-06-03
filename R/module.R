# R/module.R

GMFState <- new_class("GMFState",
  package    = NULL,
  properties = list(
    name       = class_character,
    type       = class_character,
    definition = class_list,
    transition = new_property(class = class_any, default = NULL)
  )
)

Module <- new_class("Module",
  package    = NULL,
  properties = list(
    name       = class_character,
    states     = class_list,
    submodules = new_property(class = class_list, default = list())
  )
)

load_module <- function(path) {
  raw    <- jsonlite::read_json(path, simplifyVector = FALSE)
  name   <- raw[["name"]] %||% tools::file_path_sans_ext(basename(path))
  states_raw <- raw[["states"]] %||% list()

  states <- Map(function(s, nm) {
    GMFState(
      name       = nm,
      type       = s[["type"]] %||% "Simple",
      definition = s,
      transition = parse_transition(s)
    )
  }, states_raw, names(states_raw))

  Module(name = name, states = states)
}

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
  stats::setNames(modules, vapply(modules, function(m) m@name, character(1)))
}
