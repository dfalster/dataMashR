\name{changeMethodCode}
\alias{changeMethodCode}
\title{Replaces existing method by a new one in studyName/dataMatchColumns.csv}
\usage{
  changeMethodCode(studyName, pattern, replacement)
}
\arguments{
  \item{studyName}{Name of a study in \code{data} folder.}

  \item{pattern}{Pattern (i.e. method code) to be found}

  \item{replacement}{Replacement string (i.e. new method
  code)}
}
\description{
  \code{changeMethodCode} replaces an existing method in
  dataMatchColumns with a new value. The output will
  overwrite the existing dataMatchColumns.csv file.
}
\seealso{
  \code{\link{grepPosition}} and
  \code{\link{replaceMethod}}
}

