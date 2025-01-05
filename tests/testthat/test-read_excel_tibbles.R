test_that("read_excel_tibbles handles basic files", {
  file <- system.file("testthat/testdata/basic.xlsx", package = "excel_to_tibbles")
  expect_true(file.exists(file)) # Ensure the file exists
  tibbles <- read_excel_tibbles(file)
  expect_type(tibbles, "list")
  expect_true(all(purrr::map_lgl(tibbles, is_tibble)))
})
