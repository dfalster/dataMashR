##' @import testthat

##' @export
validate <- function(studyName, silent=FALSE) {
  context(studyName)
  # test all files are present first
  #
  # test dataMatchColumns.csv before data.csv
  validate_data.csv(studyName)
}

## All the tests about data.csv alone
validate_data.csv <- function(studyName) {
  test_that("data.csv", {
    path <- "data" # for now.
    full_path <- file.path(path, studyName)
    expect_that(file.path(full_path, "data.csv"),
                is_present())

    dmc <- read.csv(file.path(full_path, "dataMatchColumns.csv"),
                    stringsAsFactors=FALSE,
                    na.strings=c("NA", ""), strip.white=TRUE)
    dat <- read.csv(file.path(path, studyName, "data.csv"),
                    stringsAsFactors=FALSE)

    ## Every name in data.csv must be in the dataMatchColumns.csv$var_in
    expect_that(dat, has_names(dmc$var_in, ignore.order=TRUE))
  })
}

## Extra testthat tests.
is_present <- function() {
  function(filename) {
    expectation(file.exists(filename),
                sprintf("File %s was not found", filename))
  }
}
