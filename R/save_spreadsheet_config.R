#' Save Spreadsheet-Specific Configuration
#'
#' Saves a configuration file specific to a spreadsheet in the current working directory.
#'
#' @param file Path to the spreadsheet file.
#' @param config A list containing the configuration options.
#' @return A message indicating the file was saved.
#' @examples
#' save_spreadsheet_config("example.xlsx", list(remove_empty_rows = TRUE))
save_spreadsheet_config <- function(file, config) {
  if (!is.character(file) || !nzchar(file)) {
    stop("Invalid file path provided.")
  }
  if (!is.list(config)) {
    stop("The 'config' parameter must be a list.")
  }
  
  base_name <- tools::file_path_sans_ext(basename(file))
  config_path <- file.path(dirname(file), paste0(base_name, "_config.json"))
  
  # Write the configuration file
  jsonlite::write_json(config, config_path, pretty = TRUE, auto_unbox = TRUE)
  message("Spreadsheet-specific configuration saved to: ", config_path)
}
