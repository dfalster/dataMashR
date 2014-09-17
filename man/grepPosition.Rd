\name{grepPosition}
\alias{grepPosition}
\title{Creates a list with positions where replacements will have to occur}
\usage{
  grepPosition(pattern, methodsList)
}
\arguments{
  \item{studyName}{Name of a study in \code{data} folder.}

  \item{pattern}{Pattern (i.e. method code) to be found}

  \item{methodsList}{List of methods found in
  studyName/dataMatchColumns.csv}
}
\value{
  List with pattern as names and respective positions where
  they are found in methodsList
}
\description{
  \code{grepPosition} will search for the positions of of
  fixed patterns given a list of methods from
  \code{\link{changeMethodCode}}.
}
\seealso{
  \code{\link{changeMethodCode}} and
  \code{\link{replaceMethod}}
}

