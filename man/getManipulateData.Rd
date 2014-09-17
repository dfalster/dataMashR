\name{getManipulateData}
\alias{getManipulateData}
\title{Load the data manipulation function, if present}
\usage{
  getManipulateData(studyName)
}
\arguments{
  \item{studyName}{folder where data is stored}
}
\value{
  function for manipulating a data.frame
}
\description{
  If the `dataManipulate.R` file is present within a
  study's data directory, it must contain the function
  `manipulate`.  Otherwise we return the identity function
  to indicate no manipulations will be done.  The function
  must take a data.frame as an argument and return one as
  the return value, but this is not checked at present.
}

