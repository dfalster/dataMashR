## Rebuild the example data set.
source("helper-baad.R")

context("Regression tests")

test_that("rebuild", {
  expect_that(rebuild(), is_true())
})

ref <- readRDS("example/baad_ref.rds")
baad <- readRDS("example/output/baad.rds")

test_that("Dimensions of data", {
  expect_that(dim(baad), equals(dim(ref)))
})

# Check all output matches reference dataset
for(v in c("data","methods","contacts","references","bibtex" ,"dictionary"))
	test_that(paste(v, "equals"), {
  		expect_that(baad[[v]], equals(ref[[v]]))
	})
