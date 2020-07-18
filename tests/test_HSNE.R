source("../R/HSNE.R", chdir = TRUE)
library(testthat)

# testthat::test_dir(“../tests”)
# testthat::test_file("../tests/test_HSNE.R")

test_that("Test HSNE parser: number of scales", {
  hsne_parser <- HSNE_parser()
  hsne_parser$read_HSNE_binary("../R/r_parser_HSNE/mnis_aoi.hsne")
  e <- ha$num_scales

  expect_equal(e, 3)
})

test_that("Test HSNE parser: toString", {
  hsne_parser <- HSNE_parser()
  hsne_parser$read_HSNE_binary("../R/r_parser_HSNE/mnis_aoi.hsne")
  e <- ha$to_string()

  #expect_reference(ha$get_topscale(), DataScale)
  expect_equal(e, "HSNE hierarchy with 3 scales.")
})

test_that("Test HSNE parser: top scale is datascale/dataframe", {
  hsne_parser <- HSNE_parser()
  hsne_parser$read_HSNE_binary("../R/r_parser_HSNE/mnis_aoi.hsne")
  e <- ha$get_topscale()

  expect_equal(class(e), "data.frame")
})
