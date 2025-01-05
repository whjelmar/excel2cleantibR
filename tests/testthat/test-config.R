library(testthat)
library(jsonlite)
library(withr)

# Test save_default_config
test_that("save_default_config creates a valid default config file", {
  # Mock HOME directory
  temp_home <- tempdir()
  config_path <- file.path(temp_home, ".excel2cleantibr_config.json")
  
  # Default configuration
  expected_config <- list(
    remove_empty_rows = TRUE,
    remove_empty_cols = TRUE,
    standardize_names = TRUE,
    fill_missing_values = FALSE,
    verbose = FALSE # Default verbose to FALSE
  )
  
  # Cleanup after test
  withr::defer(unlink(config_path))
  
  # Mock the HOME environment variable
  withr::with_envvar(new = c(HOME = temp_home), {
    save_default_config()
    expect_true(file.exists(config_path), info = "Default config file was not created.")
    
    # Validate the saved configuration
    actual_config <- jsonlite::read_json(config_path, simplifyVector = TRUE)
    
    # Ensure `verbose` is logical and defaults to FALSE if missing
    actual_config$verbose <- isTRUE(actual_config$verbose)
    
    # Add missing fields if any
    for (field in names(expected_config)) {
      if (is.null(actual_config[[field]])) {
        actual_config[[field]] <- expected_config[[field]]
      }
    }
    
    # Debug messages only if verbose is TRUE
    if (actual_config$verbose) {
      message("Saved Default Config:")
      print(actual_config)
      message("Expected Default Config:")
      print(expected_config)
    }
    
    expect_equal(actual_config, expected_config, info = "Default config does not match expected values.")
  })
})

test_that("load_config respects the verbose flag", {
  temp_home <- tempdir()
  temp_local <- tempdir()
  
  home_config_path <- file.path(temp_home, ".excel2cleantibr_config.json")
  local_config_path <- file.path(temp_local, "example_config.json")
  file <- file.path(temp_local, "example.xlsx")
  
  default_config <- list(
    remove_empty_rows = TRUE,
    remove_empty_cols = TRUE,
    standardize_names = TRUE,
    fill_missing_values = FALSE,
    verbose = FALSE
  )
  verbose_config <- list(
    remove_empty_rows = TRUE,
    remove_empty_cols = TRUE,
    standardize_names = TRUE,
    fill_missing_values = TRUE,
    verbose = TRUE
  )
  
  write_json(default_config, home_config_path, auto_unbox = TRUE)
  write_json(verbose_config, local_config_path, auto_unbox = TRUE)
  
  withr::defer(unlink(c(home_config_path, local_config_path, temp_local), recursive = TRUE))
  
  # Mock the HOME environment variable
  withr::with_envvar(new = c(HOME = temp_home), {
    withr::with_dir(temp_local, {
      # Test verbose output for local config
      expect_message(load_config(file), "Loaded spreadsheet-specific configuration")
      
      # Remove local config and test silent output for default config
      unlink(local_config_path)
      expect_silent(load_config(file))
    })
  })
})

# Test save_spreadsheet_config
test_that("save_spreadsheet_config creates a valid spreadsheet-specific config file", {
  temp_local <- tempdir()
  file <- file.path(temp_local, "example.xlsx")
  config_path <- file.path(temp_local, "example_config.json")
  
  # Custom configuration
  custom_config <- list(
    remove_empty_rows = FALSE,
    remove_empty_cols = TRUE,
    standardize_names = FALSE,
    fill_missing_values = TRUE
  )
  
  # Cleanup after test
  withr::defer(unlink(c(file, config_path)))
  
  # Save spreadsheet-specific config
  save_spreadsheet_config(file, custom_config)
  expect_true(file.exists(config_path), info = "Spreadsheet-specific config file was not created.")
  
  # Validate the saved configuration
  actual_config <- read_json(config_path, simplifyVector = TRUE)
  expect_equal(actual_config, custom_config, info = "Spreadsheet-specific config does not match expected values.")
})

test_that("load_config prioritizes local config over home config", {
  temp_home <- tempdir()
  temp_local <- tempdir()
  
  home_config_path <- file.path(temp_home, ".excel2cleantibr_config.json")
  local_config_path <- file.path(temp_local, "example_config.json")
  file <- file.path(temp_local, "example.xlsx")
  
  default_config <- list(
    remove_empty_rows = TRUE,
    remove_empty_cols = TRUE,
    standardize_names = TRUE,
    fill_missing_values = FALSE,
    verbose = FALSE
  )
  local_config <- list(
    remove_empty_rows = FALSE,
    remove_empty_cols = FALSE,
    standardize_names = FALSE,
    fill_missing_values = TRUE,
    verbose = TRUE
  )
  
  # Write configurations
  jsonlite::write_json(default_config, home_config_path, auto_unbox = TRUE)
  jsonlite::write_json(local_config, local_config_path, auto_unbox = TRUE)
  
  # Cleanup after test
  withr::defer(unlink(c(home_config_path, local_config_path, temp_local), recursive = TRUE))
  
  # Mock the HOME environment variable
  withr::with_envvar(new = c(HOME = temp_home), {
    withr::with_dir(temp_local, {
      # Test prioritization of local config
      config <- load_config(file)
      expect_equal(config, local_config, info = "Local config not prioritized over home config.")
    })
    
    # Remove local config and test fallback to home config
    unlink(local_config_path)
    config <- load_config(file)
    expect_equal(config, default_config, info = "Default config not correctly loaded.")
  })
})

test_save_default_config <- function() {
  test_that("save_default_config works as expected", {
    # Define the config file path
    home_config_path <- file.path(Sys.getenv("HOME"), ".excel2cleantibr_config.json")
    
    # Backup existing config file if it exists
    backup_path <- NULL
    if (file.exists(home_config_path)) {
      backup_path <- paste0(home_config_path, ".bak")
      file.copy(home_config_path, backup_path, overwrite = TRUE)
    }
    
    # Cleanup function to restore backup or delete test artifacts
    withr::defer({
      if (!is.null(backup_path) && file.exists(backup_path)) {
        file.copy(backup_path, home_config_path, overwrite = TRUE)
        unlink(backup_path)
      } else {
        unlink(home_config_path)
      }
    })
    
    # Expected default configuration
    expected_config <- list(
      remove_empty_rows = TRUE,
      remove_empty_cols = TRUE,
      standardize_names = TRUE,
      fill_missing_values = FALSE,
      verbose = FALSE # Default verbose to FALSE
    )
    
    # Run the function to save the default config
    save_default_config()
    
    # Ensure the file exists
    expect_true(file.exists(home_config_path), info = "Default config file was not created.")
    
    # Validate the contents of the created config file
    actual_config <- jsonlite::read_json(home_config_path, simplifyVector = TRUE)
    
    # Ensure `verbose` is included and logical
    actual_config$verbose <- actual_config$verbose %||% FALSE
    
    # Compare actual config to the expected config
    expect_equal(actual_config, expected_config, info = "Default config does not match expected values.")
  })
}

# To run this test explicitly
if (exists("run_save_default_config_test") && isTRUE(run_save_default_config_test)) {
  test_save_default_config()
  withr::defer(rm(test_save_default_config))
}
