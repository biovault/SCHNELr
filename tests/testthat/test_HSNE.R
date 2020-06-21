library(testthat)
#source("../R/HSNE.R", chdir = TRUE)
#source("./data-raw/mnis_aoi.hsne", chdir = TRUE)
#source("./'R/HSNE.R")

# testthat::test_dir(“../tests”)
# testthat::test_file("../tests/test_HSNE.R")

# path to read file for mnis_aoi.hsne from this directory: "../../data-raw/mnis_aoi.hsne"

test_that("Test HSNE parser: number of scales", {
  hsne_parser <- HSNE_parser()
  hier <- hsne_parser$read_HSNE_binary("../../data-raw/mnis_aoi.hsne")
  e <- hier$num_scales

  expect_equal(e, 3)
})

test_that("Test HSNE parser: toString", {
  hsne_parser <- HSNE_parser()
  hier <- hsne_parser$read_HSNE_binary("../../data-raw/mnis_aoi.hsne")
  e <- hier$toString()

  #expect_reference(ha$get_topscale(), DataScale)
  expect_equal(e, "HSNE hierarchy with 3 scales.")
})

test_that("Test HSNE parser: top scale is datascale/dataframe", {
  hsne_parser <- HSNE_parser()
  hier <- hsne_parser$read_HSNE_binary("../../data-raw/mnis_aoi.hsne")
  e <- hier$get_topscale()

  expect_equal(class(e), "data.frame")
})
