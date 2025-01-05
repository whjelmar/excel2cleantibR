#' Sanitize Sheet Names
#'
#' Transforms sheet names to meet naming conventions.
#'
#' @param names A character vector of sheet names.
#' @param custom_function A user-defined function for sanitization (optional).
#' @return Sanitized sheet names.
sanitize_sheet_names <- function(names, custom_function = NULL) {
  if (is.null(custom_function)) {
    janitor::make_clean_names(names)
  } else if (is.function(custom_function)) {
    custom_function(names)
  } else {
    stop("Invalid custom_function. Provide a valid function or leave it NULL.")
  }
}