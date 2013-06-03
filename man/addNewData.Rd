\name{addNewData}
\alias{addNewData}
\title{Modifies data by adding new values from table studyName/dataNew.csv}
\usage{
  addNewData(studyName, data)
}
\arguments{
  \item{studyName}{Name of a study in \code{data} folder.}

  \item{data}{existing data.frame (from addAllColumns)}
}
\value{
  modified data.frame
}
\description{
  Within the column given by \code{newVariable}, replace
  values that match \code{lookupValue} within column
  \code{lookupVariable} with the value \code{newValue}.  If
  \code{lookupVariable} is \code{NA}, then replace all
  elements of \code{newVariable} with the value
  \code{newValue}. Note that lookupVariable can be the same
  as newVariable.
}

