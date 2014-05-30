##' @import testthat

##' @export
validateConfig  <-  function(silent=FALSE) {
  # check if package has correct setup files
  validate_variableDefinitions.csv()
  validate_variableConversion.csv()

}

##' @export
validateSetUp  <-  function(studyName, silent=FALSE) {
  context(studyName)
  # test all files are present first
  #
  # test dataMatchColumns.csv before data.csv
  validate_data.csv(studyName)

}

## All the tests about config/variableDefinitions.csv alone
validate_variableDefinitions.csv <- function(conf_path="config", filePath="variableDefinitions.csv") {
  test_that("variableDefinitions.csv", {
    
    #does it exist?
    expect_that(file.path(conf_path, filePath),
                is_present())

    #so read it
    vdf <- read.csv(file.path(conf_path, filePath),
                    stringsAsFactors=FALSE,
                    na.strings=c("NA", ""), strip.white=TRUE)


    #is it utf text (contains specieal characters?)?

    #does it contain the right names?
    expect_that(vdf, has_names(c("Variable","Units","Group","Type","methodsVariable","essential","label","minValue","maxValue","allowableValues","Description"), ignore.order=TRUE))
 
    #every class in vdf$Type must be an allowed class
    expect_that(vdf$Type, has_allowed_classes())

    #methodsVariable and essential must be logical
    expect_that(is.logical(vdf$methodsVariable), is_true())
    expect_that(is.logical(vdf$essential),       is_true())

    #are Max Min values specified for numeric variables only?
    expect_identical(unique(vdf$Type[!is.na(vdf$minValue)]), "numeric")
    expect_identical(unique(vdf$Type[!is.na(vdf$maxValue)]), "numeric")

    #do all numeric variables have a range specified?
    checkForNAs(vdf$minValue[vdf$Type=='numeric'])
    checkForNAs(vdf$maxValue[vdf$Type=='numeric'])

    #Max Min columns must contain numbers only
    expect_that(is.numeric(vdf$minValue), is_true())
    expect_that(is.numeric(vdf$maxValue), is_true())

    #are allowableValues values specified for character variables only?
    expect_identical(unique(vdf$Type[!is.na(vdf$allowableValues)]), "character")

    #allowableValues columns must contain characters only
    expect_that(is.character(vdf$allowableValues), is_true())
  })
}

## All the tests about config/variableDefinitions.csv alone
validate_variableConversion.csv <- function(conf_path="config", filePath="variableConversion.csv") {
  test_that("variableConversion.csv", {
    
    #does it exist?
    expect_that(file.path(conf_path, filePath),
                is_present())

    #so read it
    vcv <- read.csv(file.path(conf_path, filePath),
                    stringsAsFactors=FALSE,
                    na.strings=c("NA", ""), strip.white=TRUE)

    #run tests if the dataset is not empty
    if(nrow(vcv) > 0) {
      #does it contain the right names?
      expect_that(vcv, has_names(c("unit_in","unit_out","conversion"), ignore.order=TRUE))
      
      #all columns must be characters  
      for(v in names(vcv))
          expect_that(is.character(vcv[[v]]), is_true())
      
      #are designated functions correct functions?
      for(i in 1:nrow(vcv))
        expect_that(is.function(eval(parse(text=paste0("as.function(alist(x=,", vcv$conversion[i],"))")))), is_true())    
    }

  })

}

## All the tests about data.csv alone
validate_data.csv <- function(studyName, conf_path="config", filePath="variableDefinitions.csv") {

  test_that("data.csv", {    
    #does it exist?
    expect_that(dataMashR:::data.path(studyName, "data.csv"),
                is_present())
    #so read it
    dat  <-  dataMashR:::readDataRaw(studyName)
    
    #does it contain duplicated colnames?
    expect_identical(length(unique(names(dat))),
                length(names(dat)))

    vdf <- read.csv(file.path(conf_path, filePath),
                    stringsAsFactors=FALSE,
                    na.strings=c("NA", ""), strip.white=TRUE)

    dmc <- read.csv(dataMashR:::data.path(studyName, "dataMatchColumns.csv"),
                    stringsAsFactors=FALSE,
                    na.strings=c("NA", ""), strip.white=TRUE)

    ## Every name in data.csv must be in the dataMatchColumns.csv$var_in
    expect_that(dat, has_names(dmc$var_in, ignore.order=TRUE))
  })
}

## Extra testthat tests.
is_present <- function() {
  function(filename) {
    expectation(file.exists(filename),
                sprintf("File %s was not found", filename))
  }
}

has_allowed_classes  <-  function() {
  function(vec) {
    expectation(allowed_classes(vec),
                sprintf("variableDefinitions.csv contains classes not allowed in this package"))
  }
}

## Auxiliary functions
allowed_classes  <-  function(vec) {
  all(vec %in% c("numeric", "character"))
}

checkForNAs  <-  function(vec) {
    NAtest <- length(vec) == length(vec[!is.na(vec)])
    if(!NAtest)
      warning("vector contains NA values")
}

## Test tests
#has_allowed_classes
test_that({
#throws error when it should
expect_that({ #one wrong entry in vector
  expect_that(c("abc", "numeric"), has_allowed_classes())
  }, throws_error())
expect_that({ #vector only contains wrong entries
  expect_that("abc", has_allowed_classes())
  }, throws_error())
})