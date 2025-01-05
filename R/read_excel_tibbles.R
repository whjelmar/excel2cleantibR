#' Read Excel File and Import Sheets as Tibbles
#'
#' Reads an Excel file and converts its sheets into tibbles, applying configurable cleaning and naming conventions.
#'
#' @param file Path to the Excel file.
#' @param config Path to a configuration file (optional). If NULL, the function attempts to load
#'        a configuration file using `load_config()`.
#' @param output_format Output format: "list" (default) or "global_env".
#' @return A named list of tibbles or NULL (if output to the global environment).
#' @examples
#' read_excel_tibbles("example.xlsx")
read_excel_tibbles <- function(file, config = NULL, output_format = "list") {
  # Load configuration if not explicitly provided
  if (is.null(config)) {
    config <- load_config(file)
  } else {
    config <- jsonlite::read_json(config)
  }
  
  # Read sheet names from the Excel file
  sheets <- readxl::excel_sheets(file)
  
  # Sanitize sheet names
  sanitized_names <- sanitize_sheet_names(sheets)
  
  # Process each sheet into a tibble
  tibble_list <- lapply(sheets, function(sheet) {
    data <- readxl::read_excel(file, sheet = sheet)
    clean_data(data, config)  # Apply cleaning based on configuration
  })
  
  # Return tibbles as a list or assign to the global environment
  if (output_format == "list") {
    setNames(tibble_list, sanitized_names)
  } else if (output_format == "global_env") {
    purrr::walk2(tibble_list, sanitized_names, ~ assign(.y, .x, envir = .GlobalEnv))
    NULL
  } else {
    stop("Invalid output_format. Use 'list' or 'global_env'.")
  }
}
