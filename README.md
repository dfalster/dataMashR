dataMashR
================

[![Build Status](https://travis-ci.org/dfalster/dataMashR.svg?branch=master)](https://travis-ci.org/dfalster/dataMashR)

Daniel Falster, Remko Duursma, Rich FitzJohn, Diego Barneche

An R package for merging data from different studies in a transparent and reproducible fashion.

**WARNING:** Please note, this package is currently under active development. If you decide to use it, please be aware that the package structure is unstable, so future changes may break your scripts.

## Installation

Use the [devtools](http://cran.r-project.org/web/packages/devtools/index.html) package:

```
install.packages("devtools")
devtools::install_github("dataMashR", "dfalster")
library(dataMashR)
```

## Documentation

Reference documentation is available here [here](http://dfalster.github.io/dataMashR) (generated with [staticdocs](https://github.com/hadley/staticdocs)). 

Once you have specified your configuration files, you can build database using

```
mashData(verbose=TRUE)
```
