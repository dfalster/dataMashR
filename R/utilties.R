#' Various utility functions
#' 
#' @description \code{na.vector} constructs a vector with NAs. \code{nonNA} subsets a dataframe, removing all columns that are NA only. \code{is.blank} (Logical) decides whether a value is NA or empty. \code{rename} replaces the names of an object (typically, a dataframe) with a new vector of names.
#' 
#' @param type 'character' or 'numeric'.
#' @param n Length of vector.
#' @param df A dataframe.
#' @param x A vector
#' @param obj An object with a 'names' attribute (e.g., a dataframe)
#' @param names.from A vector of old names.
#' @param names.to A vector of new names.
na.vector <- function(type, n) {
  x <- switch(type,
              character=NA_character_,
              numeric=NA_real_,
              stop("Unknown type", type))
  rep.int(x, n)
}


# Subset of dataframe, removing all columns that are NA only.
nonNA <- function(df)df[,sapply(df, function(x)!all(is.na(x)))]

is.blank <- function(x)
  is.na(x) | x == ""

rename <- function(obj, names.from, names.to) {
  if ( length(names.from) != length(names.to) )
    stop("names.from and names.to must be the same length")
  i <- match(names.from, names(obj))
  if ( any(is.na(i)) )
    stop(paste("Could not find names", paste(names.from[is.na(i)], collapse=", ")))
  names(obj)[i] <- names.to
  obj
}

