#' Load Configuration
#'
#' Loads the appropriate configuration file for a given spreadsheet.
#' First checks for a spreadsheet-specific configuration file in the current working directory,
#' and falls back to the default configuration in the user's home directory.
#'
#' @param file Path to the spreadsheet file.
#' @return A list containing the configuration options.
#' @examples
#' config <- load_config(".excel2cleantibr_config.json")
load_config <- function(file) {
  home_dir <- Sys.getenv("HOME")
  home_config_path <- file.path(home_dir, ".excel2cleantibr_config.json")
  
  base_name <- tools::file_path_sans_ext(basename(file))
  local_config_path <- file.path(getwd(), paste0(base_name, "_config.json"))
  
  default_config <- list(
    remove_empty_rows = TRUE,
    remove_empty_cols = TRUE,
    standardize_names = TRUE,
    fill_missing_values = FALSE,
    verbose = FALSE
  )
  
  config <- default_config
  
  if (file.exists(local_config_path)) {
    local_config <- jsonlite::read_json(local_config_path, simplifyVector = TRUE)
    config <- modifyList(config, local_config)
    config$verbose <- isTRUE(config$verbose)
    if (config$verbose) message("Loaded spreadsheet-specific configuration: ", local_config_path)
  } else if (file.exists(home_config_path)) {
    home_config <- jsonlite::read_json(home_config_path, simplifyVector = TRUE)
    config <- modifyList(config, home_config)
    config$verbose <- isTRUE(config$verbose)
    if (config$verbose) message("Loaded default configuration from home directory: ", home_config_path)
  } else {
    if (config$verbose) message("No configuration file found. Using default configuration.")
  }
  
  # Validate and ensure all fields match default_config structure
  config <- modifyList(default_config, config)
  validated_config <- lapply(config, function(x) if (!is.logical(x)) as.logical(x) else x)
  
  if (validated_config$verbose) {
    message("Final Config:")
    print(validated_config)
  }
  
  validated_config
}
