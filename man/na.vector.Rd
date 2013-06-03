\name{na.vector}
\alias{na.vector}
\title{Various utility functions}
\usage{
  na.vector(type, n)
}
\arguments{
  \item{type}{'character' or 'numeric'.}

  \item{n}{Length of vector.}

  \item{df}{A dataframe.}

  \item{x}{A vector}

  \item{obj}{An object with a 'names' attribute (e.g., a
  dataframe)}

  \item{names.from}{A vector of old names.}

  \item{names.to}{A vector of new names.}
}
\description{
  \code{na.vector} constructs a vector with NAs.
  \code{nonNA} subsets a dataframe, removing all columns
  that are NA only. \code{is.blank} (Logical) decides
  whether a value is NA or empty. \code{rename} replaces
  the names of an object (typically, a dataframe) with a
  new vector of names.
}

