\name{loadStudies}
\alias{loadStudies}
\title{Adds studies to the central dataset}
\usage{
  loadStudies(studyNames = getStudyNames(), data = NULL,
    reprocess = FALSE, verbose = FALSE)
}
\arguments{
  \item{studyNames}{Character vector containing study names
  to be added. The default adds names via getStudyNames()}

  \item{data}{If provided, will add studies to this
  dataframe}

  \item{reprocess}{If TRUE, will reprocess studies even if
  they already exist in the data directory}

  \item{verbose}{If TRUE, print messages to screen, good
  for isolating problems}
}
\value{
  merged list with three parts: data, reference, contact,
  each is a dataframe with all data combined.
}
\description{
  Standardises all folders within one main directory
  (default=data) and combines them into one single dataset
  via loadStudy()
}

