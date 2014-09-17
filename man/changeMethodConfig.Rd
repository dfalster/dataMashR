\name{changeMethodConfig}
\alias{changeMethodConfig}
\title{Replaces existing method by a new one in config/methodsDefinitions.csv}
\usage{
  changeMethodConfig(pattern, replacement)
}
\arguments{
  \item{pattern}{Pattern (i.e. method code) to be found}

  \item{replacement}{Replacement string (i.e. new method
  code)}
}
\description{
  \code{changeMethodConfig} replaces an existing method in
  the existing method reference database located at
  config/methodsDefinitions.csv.
}
\seealso{
  \code{\link{fullChangeOfMethods}} and
  \code{\link{changeMethodCode}}
}

