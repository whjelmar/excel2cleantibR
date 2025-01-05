#' Clean Data Based on Configuration
#'
#' Applies cleaning steps to a data frame based on the provided configuration.
#'
#' @param sheet_data A data frame to clean.
#' @param config A list of cleaning options.
#' @return A cleaned data frame.
clean_data <- function(sheet_data, config) {
  if (config$remove_empty_rows) {
    sheet_data <- janitor::remove_empty(sheet_data, which = "rows")
  }
  if (config$remove_empty_cols) {
    sheet_data <- janitor::remove_empty(sheet_data, which = "cols")
  }
  if (config$standardize_names) {
    sheet_data <- janitor::clean_names(sheet_data)
  }
  if (config$fill_missing_values) {
    sheet_data <- zoo::na.locf(sheet_data, na.rm = FALSE)
  }
  sheet_data
}