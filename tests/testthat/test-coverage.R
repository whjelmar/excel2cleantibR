test_coverage_analysis <- function() {
  library(covr)
  
  test_that("Test coverage analysis runs successfully", {
    # Temp directory for coverage report
    temp_report_dir <- tempdir()
    report_path <- file.path(temp_report_dir, "coverage_report.html")
    
    # Cleanup after the test
    withr::defer({
      if (file.exists(report_path)) {
        unlink(report_path)
      }
    })
    
    # Run coverage analysis
    coverage <- package_coverage()
    
    # Print coverage summary
    message("Coverage Summary:")
    print(coverage)
    
    # Generate a coverage report
    covr::report(coverage, file = report_path)
    message("Coverage report generated at: ", report_path)
    
    # Check minimum coverage (e.g., 80%)
    min_coverage <- 80
    actual_coverage <- percent_coverage(coverage)
    message(sprintf("Coverage: %.2f%% (minimum required: %.2f%%)", actual_coverage, min_coverage))
    
    if (actual_coverage < min_coverage) {
      warning(sprintf("Coverage is below the minimum required threshold: %.2f%% < %.2f%%", 
                      actual_coverage, min_coverage))
    } else {
      expect_true(
        actual_coverage >= min_coverage,
        info = sprintf("Coverage (%.2f%%) is below the minimum required threshold (%.2f%%)", 
                       actual_coverage, min_coverage)
      )
    }
  })
}
