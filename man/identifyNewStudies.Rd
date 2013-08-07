\name{identifyNewStudies}
\alias{identifyNewStudies}
\title{Identifies new studies within the project directory}
\usage{
  identifyNewStudies(studyNames = getStudyNames(),
    verbose = TRUE)
}
\arguments{
  \item{studyNames}{Character vector containing study names
  to be investigated.  The default adds names via
  \code{getStudyNames()}}

  \item{verbose}{If TRUE, print messages to screen, good
  for isolating problems}
}
\value{
  a vector of study names
}
\description{
  Returns all studies that still do not contain inital
  files set up done.
}
\details{
  This function searches for new studies within
  \code{mashrConfig("dir.raw")} and returns those that were
  identified as new to the project, i.e., not processed.
  This output shoulb be further used in
  \code{makeDataImport} and \code{setUpFiles} to implement
  initial files. See also \code{projectFiles}.
}

