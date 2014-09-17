source("helper-baad.R")

setwd("example") # TODO: remove this, perhaps function to validate entire package which takes patha s argument?

context("Check config")

dataMashR::validateConfig("config")

context("Check datasets")

for(d in dir("data"))
	dataMashR::validateStudy(d, "config")
