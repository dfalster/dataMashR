all: baad

BAAD=output/baad.rds

baad: ${BAAD}

${BAAD}:
	Rscript build-baad.R

clean:
	rm -fr output/baad.rds output/cache output/baad

test: ${BAAD}
	make -C tests

check-eol:
	./scripts/check_line_endings.sh csv

test-dat:
	Rscript -e "library(methods); testthat::test_file('testing.R')"

.PHONY: baad clean test check-eol test-dat
