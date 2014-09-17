RUN_TESTS="library(methods); library(testthat); test_file('test-unit.R'); test_file('test-data.R'); test_file('test-regression.R');"
test:
	Rscript -e $(RUN_TESTS)
.PHONY: test
