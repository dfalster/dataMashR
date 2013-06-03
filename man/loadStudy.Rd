\name{loadStudy}
\alias{loadStudy}
\title{Load data from specified studyName}
\usage{
  loadStudy(studyName, reprocess = FALSE, verbose = FALSE)
}
\arguments{
  \item{studyName:}{name of folder where data stored}

  \item{reprocess:}{force data to be reprocessed}

  \item{verbose:}{print tsages to screen, good for
  isolating problems}
}
\value{
  list with three parts: data, reference, contact
}
\description{
  Load data from specified studyName
}

