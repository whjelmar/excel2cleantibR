#' Save Default Configuration
#'
#' Saves a default configuration file to the user's home directory for use by `read_excel_tibbles`.
#'
#' @return A message indicating the file was saved.
#' @examples
#' save_default_config()
save_default_config <- function() {
  home_dir <- Sys.getenv("HOME")
  config_path <- file.path(home_dir, ".excel2cleantibr_config.json")
  
  default_config <- list(
    remove_empty_rows = TRUE,
    remove_empty_cols = TRUE,
    standardize_names = TRUE,
    fill_missing_values = FALSE
  )
  
  # Write the configuration as JSON
  jsonlite::write_json(default_config, config_path, pretty = TRUE, auto_unbox = TRUE)
  message("Default configuration saved to ", config_path)
}
