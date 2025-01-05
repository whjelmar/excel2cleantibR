#' Read Excel File and Import Sheets as Tibbles
#'
#' This function reads an Excel file and converts its sheets into tibbles, applying
#' configurable cleaning and naming conventions.
#'
#' @param file Path to the Excel file.
#' @param config Path to a configuration file (json or yaml). Optional.
#' @param output_format Output format: "list" (default) or "global_env".
#' @return A named list of tibbles (default) or tibbles in the global environment.
#' @examples
#' read_excel_tibbles("example.xlsx")
read_excel_tibbles <- function(file, config = NULL, output_format = "list") {
  sheets <- readxl::excel_sheets(file)
  sanitized_names <- sanitize_sheet_names(sheets)
  
  # Load configuration
  default_config <- list(
    remove_empty_rows = TRUE,
    remove_empty_cols = TRUE,
    standardize_names = TRUE,
    fill_missing_values = FALSE
  )
  config <- if (!is.null(config)) configr::read.config(config) else default_config
  
  # Process each sheet
  tibble_list <- lapply(sheets, function(sheet) {
    data <- readxl::read_excel(file, sheet = sheet)
    clean_data(data, config)
  })
  
  # Output results
  if (output_format == "list") {
    setNames(tibble_list, sanitized_names)
  } else if (output_format == "global_env") {
    purrr::walk2(tibble_list, sanitized_names, ~ assign(.y, .x, envir = .GlobalEnv))
    NULL
  } else {
    stop("Invalid output_format. Use 'list' or 'global_env'.")
  }
}