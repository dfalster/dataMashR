rm(list=ls())
library("devtools")
install_github("dataMashR","dfalster")

#install_github("rDependencyGraph","dfalster")
#library(rDependencyGraph)

source("~/git/rDependencyGraph/R/functions.R")

plotDependencyForAPackage("dataMashR")
