# R/cache.R — session-level cache for modules and compiled C++ modules.
# Avoids reloading/recompiling on every call to generate_population().

.rsynthea_cache <- new.env(parent = emptyenv())

#' Return cached modules, loading them on first call
#' @keywords internal
.get_modules <- function() {
  if (is.null(.rsynthea_cache$modules)) {
    message("rsynthea: loading GMF modules (once per session)...")
    .rsynthea_cache$modules <- load_all_modules()
  }
  .rsynthea_cache$modules
}

#' Return cached compiled C++ modules, compiling them on first call
#' @keywords internal
.get_cpp_modules <- function(modules) {
  if (is.null(.rsynthea_cache$cpp_modules)) {
    message("rsynthea: compiling modules for C++ engine (once per session)...")
    .rsynthea_cache$cpp_modules <- compile_all_modules(modules)
  }
  .rsynthea_cache$cpp_modules
}

#' Clear the module cache
#'
#' Forces [generate_population()] to reload and recompile modules on the next
#' call. Useful after reinstalling the package or modifying custom modules.
#'
#' @export
rsynthea_clear_cache <- function() {
  .rsynthea_cache$modules     <- NULL
  .rsynthea_cache$cpp_modules <- NULL
  invisible(NULL)
}
