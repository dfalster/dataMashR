## Rebuild the example data set.
source("helper-baad.R")

context("Check package setup")

setwd("example") # TODO: remove this, perhaps function to validate entire package which takes patha s argument?

dataMashR::validateConfig("config")

context("Check datasets")

for(d in dir("data"))
	dataMashR::validateStudy(d, "config")


context("Build")

setwd("..")

test_that("rebuild", {
  expect_that(rebuild(), is_true())
})

context("Regression tests")

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
