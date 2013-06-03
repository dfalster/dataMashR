\name{processStudy}
\alias{processStudy}
\title{Process a study}
\usage{
  processStudy(studyName, verbose = FALSE)
}
\arguments{
  \item{studyName}{}

  \item{verbose}{If TRUE, prints progress messages.}
}
\value{
  Transformed data saved in file output/data/studyName.csv
}
\description{
  Processes raw data from studyName, for incorporation into
  main dataset. Fixes column names, variable names, imports
  new data, makes standard dataframe.
}

