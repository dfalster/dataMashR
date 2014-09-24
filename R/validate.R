##' @import testthat
NULL

#' @title Check data package
#' @param path folder where setup is kept
#' ... extra arguments to pass into \code{startDataMashR}, if configuration is different to default.
#' @export
checkPackage <-  function(path, ...) {

    on.exit(setwd(getwd()))
    setwd(path)

    startDataMashR(...)

    context("Check config")

    validateConfig("config")

    context("Check datasets")

    for(d in dir("data"))
        validateStudy(d, "config")

}


#' @title Check package configuration files
#' @param ... additonal arguments passed through to validate functions
#' @export
validateConfig <- function(...) {
    validate_variableDefinitions.csv(...)
    validate_variableConversion.csv(...)
}

#' Validates data files for particular study
#' @param studyName name of dircetory to check
#' @param ... additonal arguments passed through to validate functions
#' @export
validateStudy <- function(studyName, ...) {
    context(paste0("- ", studyName))
    # TODO: test all files are present first test dataMatchColumns.csv before data.csv
    validate_data.csv(studyName, ...)
    validate_dataMatchColumns.csv(studyName, ...)
    validate_dataNew.csv(studyName, ...)
    validate_studyContact.csv(studyName, ...)
    validate_studyRef.bib(studyName, ...)
}

## All the tests about config/variableDefinitions.csv alone
validate_variableDefinitions.csv <- function(conf_path = "config", filePath = "variableDefinitions.csv") {
    test_that(file.path(conf_path,"variableDefinitions.csv"), {
        # does it exist?
        expect_that(file.path(conf_path, filePath), is_present())

        # so read it
        vdf <- read.csv(file.path(conf_path, filePath), stringsAsFactors = FALSE,
            na.strings = c("NA", ""), strip.white = TRUE)


        # TODO: is it utf text (contains specieal characters?)?

        # does it contain the right names?
        essential <- c("variable", "units", "type", "methods","minValue","maxValue")
        expect_that(essential, is_in(names(vdf)))

        # every class in vdf$type must be an allowed class
        expect_that(vdf$type, has_allowed_classes())

        # methods must be logical
        expect_that(is.logical(vdf$methods), is_true())

        # are Max Min values specified for numeric variables only?
        for(v in c("minValue", "maxValue"))
            expect_identical(unique(vdf$type[!is.na(vdf[[v]])]), "numeric",
                info=sprintf("Incorrect %s value for %s", v,
                    paste(vdf$variable[!is.na(vdf[[v]]) & vdf$type!="numeric"], collapse= ", ")
                     ) )

        # do all numeric variables have a range specified?
        checkForNAs(vdf$minValue[vdf$type == "numeric"])  #TODO - error message form this test is completely unhelpful
        checkForNAs(vdf$maxValue[vdf$type == "numeric"])

        # Max Min columns must contain numbers only
        expect_that(is.numeric(vdf$minValue), is_true())
        expect_that(is.numeric(vdf$maxValue), is_true())

        # # are allowableValues values specified for character variables only?
        # expect_identical(unique(vdf$type[!is.na(vdf$allowableValues)]), "character")  # TODO This test not very helpful? better error message

    })
}

## All the tests about config/variableDefinitions.csv alone
validate_variableConversion.csv <- function(conf_path = "config", filePath = "variableConversion.csv") {
    test_that(file.path(conf_path,"variableConversion.csv"), {

        # does it exist?
        expect_that(file.path(conf_path, filePath), is_present())

        # so read it
        vcv <- read.csv(file.path(conf_path, filePath), stringsAsFactors = FALSE,
            na.strings = c("NA", ""), strip.white = TRUE)

        # run tests if the dataset is not empty
        if (nrow(vcv) > 0) {
            # does it contain the right names?
            expect_that(vcv, has_names(c("unit_in", "unit_out", "conversion"), ignore.order = TRUE))

            # all columns must be characters
            for (v in names(vcv)) expect_that(is.character(vcv[[v]]), is_true())

            # are designated functions correct functions?
            for (i in seq_len(nrow(vcv))) expect_that(is.function(eval(parse(text = paste0("as.function(alist(x=,",
                vcv$conversion[i], "))")))), is_true())

            # no duplicates
            expect_that(paste(vcv[["unit_in"]], "=>", vcv[["unit_out"]]), is_unique())
        }
    })

}

## All the tests about data.csv alone
validate_data.csv <- function(studyName, conf_path = "config", filePath = "variableDefinitions.csv") {

    test_that(file.path(studyName,"data.csv"), {
        # does it exist?
        filename <- data.path(studyName, "data.csv")
        expect_that(filename, is_present())
        # so read it
        dat <- dataMashR:::readDataRaw(studyName)

        # TODO: check numeric data has right type, by implication this checks whether na.strings defined correctly in dataImportOptions - when this it isn't get errors in fixType function

        expect_that(!any(names(dat)==""), is_true(), info=sprintf("%s contains empty columns", filename))


        expect_that(!any(duplicated(names(dat))), is_true(), info=sprintf("%s contains non-unique headers, %s", filename, paste(
            names(dat)[duplicated(names(dat))], collapse=", ")
            ))

        # does it contain duplicated colnames?
        expect_identical(length(unique(names(dat))), length(names(dat)))
        })
}

## All the tests about data.csv alone
validate_dataMatchColumns.csv <- function(studyName, conf_path = "config", filePath = "variableDefinitions.csv") {

    test_that(file.path(studyName,"dataMatchColumns.csv"), {

        # does it exist?
        expect_that(data.path(studyName, "dataMatchColumns.csv"), is_present())

        # so read it
        # TODO - add test here, for both matchColumns and manipulate
        data <- readDataRaw(studyName)
        data <- manipulateData(studyName, data)

        # read dataMatchColumns
        # TODO - add test here
        dataMatchColumns <- readMatchColumns(studyName)

        # Check has essential columns present
        expect_that(c("var_in","method","unit_in","var_out"), is_in(names(dataMatchColumns)))

        # Every name in data.csv must be in the dataMatchColumns.csv$var_in
        expect_that(names(data), is_in(dataMatchColumns$var_in))

        # read variable definitions
        # TODO - add function to read var.def
        vdf <- read.csv(file.path(conf_path, filePath), stringsAsFactors = FALSE,
            na.strings = c("NA", ""), strip.white = TRUE)

        # Every name in dataMatchColumns.csv$var_out is in variable definitions
        expect_that(dataMatchColumns$var_out[!is.na(dataMatchColumns$var_out)], is_in(vdf$variable))

        # Check all unit conversions present
        dataMatchColumns <- dataMatchColumns[!is.na(dataMatchColumns$var_out), ]

        # change variable name
        expect_that(dataMatchColumns$var_in, is_in(names(data)) )
        data <- renameColoumn(data, dataMatchColumns$var_in, dataMatchColumns$var_out)
        # Change units
        # TODO: reduce horrible duplication of code from convertData function
        info <- columnInfo()
        for (col in intersect(names(data), dataMatchColumns$var_out)) {
            idx <- match(col, dataMatchColumns$var_out)[[1]]
            if (info$type[[col]] == "numeric") {
                unit.from <- dataMatchColumns$unit_in[idx]
                unit.to <- info$units[[col]]
                expect_that(unit.from, is_not_na(), info=sprintf("NA in unit_from for %s", col))
                expect_that(unit.to, is_not_na(), info=sprintf("NA in unit_to for %s", col))
                if (!is.na(unit.from) & !is.na(unit.to) & unit.from != unit.to) {
                    expect_that(
                        length(getUnitConversion(unit.from, unit.to)),
                        equals(1),
                        info=sprintf("Incorrect conversion from %s to %s", unit.from, unit.to))
                }
            }
        }
    })
}


validate_dataNew.csv <- function(studyName, conf_path = "config", filePath = "variableDefinitions.csv") {

    test_that(file.path(studyName,"dataNew.csv"), {
         filename <- data.path(studyName, "dataNew.csv")

        # does it exist?
        expect_that(filename, is_present())

        # Load it
        # TODO - add test here
        data <- readNewData(studyName)

        # Check has essential columns present
        expect_that(c("lookupVariable","lookupValue","newVariable","newValue"), is_in(names(data)))
        })
}

validate_studyContact.csv <- function(studyName, conf_path = "config", filePath = "variableDefinitions.csv") {
    test_that(file.path(studyName,"studyContact.csv"), {
        filename <- data.path(studyName, "studyContact.csv")

        # does it exist?
        expect_that(filename, is_present())

        # Load it
        # TODO - add test here
        data <- readContact(studyName)

        # Check has essential columns present
        expect_that(c("name","email","address"), is_in(names(data)))

        })
}

validate_studyRef.bib <- function(studyName, conf_path = "config", filePath = "variableDefinitions.csv") {

    test_that(file.path(studyName,"studyRef.bib"), {
        filename <- data.path(studyName, "studyRef.bib")

        # does it exist?
        expect_that(filename, is_present())
        })
}

## Extra testthat tests.
is_present <- function() {
    function(filename) {
        expectation(file.exists(filename), sprintf("File %s was not found", filename))
    }
}


is_unique <- function() {
    function(x) {
        i <- duplicated(x)
        expectation(!any(i), paste0("duplicated variable conversions: ",
                paste0(x[i], collapse = ", ")))
        }
}

is_in <- function(expected) {
    function(x) {
         i <- x %in% expected
            expectation(all(i), paste0("following values not found in expected: ",
                paste0(x[!i], collapse = ", ")))
        }
}

has_allowed_classes <- function() {
    function(vec) {
        expectation(allowed_classes(vec), sprintf("variableDefinitions.csv contains classes not allowed in this package"))
    }
}

is_not_na <- function() {
    function(x) {
        expectation(!is.na(x), sprintf("NA present"))
    }
}

## Auxiliary functions
allowed_classes <- function(vec) {
    all(vec %in% c("numeric", "character"))
}

checkForNAs <- function(vec) {
    NAtest <- length(vec) == length(vec[!is.na(vec)])
    if (!NAtest)
        warning("vector contains NA values")
}
