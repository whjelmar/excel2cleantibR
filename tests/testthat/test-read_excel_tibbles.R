test_that("read_excel_tibbles handles basic files", {
  file <- "tests/testthat/testdata/basic.xlsx"
  tibbles <- read_excel_tibbles(file)
  expect_type(tibbles, "list")
  expect_true(all(purrr::map_lgl(tibbles, is_tibble)))
})