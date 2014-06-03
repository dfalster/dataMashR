## Rebuild the example data set.
source("helper-baad.R")
rebuild()

ref <- readRDS("example/baad_ref.rds")
baad <- readRDS("example/output/baad.rds")

context("Regression tests")

test_that("Dimensions of data", {
  expect_that(dim(baad), equals(dim(ref)))
})

test_that("Everything equals", {
  expect_that(baad, equals(ref))
})
