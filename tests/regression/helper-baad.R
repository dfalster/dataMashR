library(testthat)

rebuild <- function() {
  ok <- system("make -C example clean baad")
  if (ok != 0) {
    stop("Running dataMashR failed")
  }
  TRUE
}

