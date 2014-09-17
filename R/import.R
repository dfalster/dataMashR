##' @import plyr
.mashrEnv <- new.env()

startDataMashR <- function(dir.raw = "data", dir.out = "output", dir.config = "config",
    config.files = list(conversions = "variableConversion.csv", variables = "variableDefinitions.csv",
        post = "postProcess.R")) {

    var.def <- read.table(file.path(dir.config, config.files$variables), header = TRUE,
        stringsAsFactors = FALSE, sep = ",")  #variable definitions
    conversions <- read.table(file.path(dir.config, config.files$conversions), header = TRUE,
        stringsAsFactors = FALSE, sep = ",", check.names = FALSE)

    postProcess <- getFunctionFromSource("postProcess", file.path(dir.config, config.files$post),
        identity)

    .mashrEnv$config <- list(dir.raw = dir.raw, dir.out = dir.out, dir.config = dir.config,
        conversions = conversions, var.def = var.def, postProcess = postProcess)
}

checkMashrIsSetup <- function() {
    if (!exists("config", .mashrEnv)) {
        startDataMashR()
    }
}

#' Return an item from dataMashR environment
#' @param detail to fetch
#' @return object requested
#' @export
mashrDetail <- function(detail) {
    checkMashrIsSetup()
    .mashrEnv$config[[detail]]
}

#' Path to a directory containing raw data
#' @param studyName name of dataset being requested
#' @param ... additonal arguments passed through to file.path
#' @return path to particular dataset
#' @export
data.path <- function(studyName, ...) {
    file.path(mashrDetail("dir.raw"), studyName, ...)
}

#' Returns all folders within dataMashR data directory
#' @return object requested
#' @export
getStudyNames <- function() {
    dir(mashrDetail("dir.raw"))
}

#' Adds studies to the central dataset
#' @description Standardises all folders within one main directory (default=data) and combines them into one single dataset via loadStudy()
#' @param studyNames Character vector containing study names to be added. The default adds names via getStudyNames()
#' @param reprocess If TRUE, will reprocess studies even if they already exist in the data directory
#' @param verbose If TRUE, print messages to screen, good for isolating problems
#' @param name of output
#' @return merged list with three parts: data, references, contacts,
#'    each is a dataframe with all data combined.
#' @export
#' @importFrom bibtex write.bib
mashData <- function(studyNames = getStudyNames(), reprocess = TRUE, verbose = FALSE,
    name = "mashup.rds") {

    if (verbose)
        message("Building ", name)

    # Path for saving output
    path <- file.path(mashrDetail("dir.out"), tools::file_path_sans_ext(name))
    dir.create(path, showWarnings = FALSE, recursive = TRUE)

    # run transformations for each data directory
    d <- llply(studyNames, loadStudy, reprocess = reprocess, verbose = verbose)
    names(d) <- studyNames

    # combine into single object
    x <- list(data = ldply(d, function(x) x[["data"]]), methods = ldply(d, function(x) x[["methods"]]),
        contacts = ldply(d, function(x) x[["contacts"]]), references = ldply(d, function(x) x[["references"]]))

    # ply adds column '.id -> change to studyName
    for (v in c("data", "methods", "contacts", "references")) x[[v]] <- renameColoumn(x[[v]],
        ".id", "studyName")

    # compile bib references into single file
    refs <- file.path(path, paste0("references", ".bib"))
    write.bib(d[[1]]$bibtex, file = refs, verbose = FALSE)
    l_ply(d[2:length(d)], function(x) write.bib(x$bibtex, refs, append = TRUE, verbose = FALSE))
    x[["bibtex"]] <- read.bib(refs)

    # ensure variable types are correct
    x$data <- fixType(x$data)
    for (v in names(x$methods)) x$methods[[v]] <- as.character(x$methods[[v]])

    # Data dictionary
    x[["dictionary"]] <- mashrDetail("var.def")

    # Save to file
    for (v in c("data", "methods", "contacts", "dictionary", "references")) write.csv(x[[v]],
        file.path(path, paste0(v, ".csv")), row.names = FALSE)

    saveRDS(x, file.path(mashrDetail("dir.out"), name))
}

#' Load data from specified studyName
#' @description Compiles a standardised list for a folder containing a dataset.
#' @param studyName name of folder where data is stored
#' @param reprocess force data to be reprocessed
#' @param verbose print stages to screen, good for isolating problems
#' @return list with three parts: data via readDataProcessed(), reference via readReference(), contact via readContact()
#' @export
loadStudy <- function(studyName, reprocess = FALSE, verbose = FALSE) {

    bibtex <- readReference(studyName)

    list(data = readDataProcessed(studyName, reprocess, verbose = verbose), methods = readMethods(studyName),
        bibtex = bibtex, contacts = readContact(studyName), references = getCitation(bibtex))
}

#' Reads processed and standardised dataset
#' @description Reads processed and standardised dataset.
#' @param studyName name of folder where data is stored
#' @param reprocess force data to be reprocessed
#' @return standardised data.frame
#' @export
readDataProcessed <- function(studyName, reprocess = TRUE, ...) {
    filename <- studyDataFile(studyName)
    if (reprocess || !file.exists(filename))
        processStudy(studyName, ...) else read.csv(filename, header = TRUE, stringsAsFactors = FALSE)
}

#' Process a study
#' @description Processes raw data from studyName, for incorporation into main dataset. Fixes column names, variable names, imports new data, makes standard dataframe.
#' @param studyName folder where data is stored
#' @param verbose If TRUE, prints progress messages.
#' @return Transformed data saved in file output/data/studyName.csv
processStudy <- function(studyName, verbose = FALSE) {

    if (verbose)
        message(studyName)

    if (verbose)
        message("   - load data")
    data <- readDataRaw(studyName)

    if (verbose)
        message("   - manipulate data")
    data <- manipulateData(studyName, data)

    # convert units and variable names
    if (verbose)
        message("   - convert units")
    data <- convertData(studyName, data)

    # Remove / add columns to mirror those in final database
    if (verbose)
        message("   - add/remove columns")
    data <- addAllColumns(data)

    if (verbose)
        message("   - import new data")
    data <- addNewData(studyName, data)

    if (verbose)
        message("   - fix type")
    data <- fixType(data)

    if (verbose)
        message("   - post process")
    data <- mashrDetail("postProcess")(data)

    if (verbose)
        message("   - write to file")
    outputName <- studyDataFile(studyName)
    dir.create(dirname(outputName), showWarnings = FALSE, recursive = TRUE)
    write.csv(data, outputName, row.names = FALSE)

    data
}

#' Load raw data from studyName
#'
#' @param studyName folder where data is stored
#' @return dataframe with raw data
readDataRaw <- function(studyName) {
    import <- readImport(studyName)

    read.csv(data.path(studyName, import$name), header = import$header, skip = import$skip,
        na.strings = import$na.strings, check.names = FALSE, stringsAsFactors = FALSE,
        strip.white = TRUE)
}


#' Perform arbitrary manipulations to the raw data
#'
#' @param studyName folder where data is stored
#' @param data raw data, as from readDataRaw
#' @return data.frame, with manipulations carried out
manipulateData <- function(studyName, data) {
    manipulate <- getManipulateData(studyName)
    manipulate(data)
}

#' Load the data manipulation function, if present
#'
#' If the `dataManipulate.R` file is present within a study's data
#' directory, it must contain the function `manipulate`.  Otherwise
#' we return the identity function to indicate no manipulations will
#' be done.  The function must take a data.frame as an argument and
#' return one as the return value, but this is not checked at
#' present.
#'
#' @param studyName folder where data is stored
#' @return function for manipulating a data.frame
getManipulateData <- function(studyName) {
    filename <- data.path(studyName, "dataManipulate.R")
    getFunctionFromSource("manipulate", filename, identity)
}

renameColoumn <- function(obj, names.from, names.to) {
    i <- match(names.from, names(obj))
    names(obj)[i] <- names.to
    obj
}

#' Convert data to desired format, changing units, variable names
#' @param studyName folder where data is stored
#' @param data existing data frame
#' @return modified data frame
convertData <- function(studyName, data) {

    var.match <- readMatchColumns(studyName)
    var.match <- var.match[!is.na(var.match$var_out), ]

    info <- columnInfo()

    # change varaible name
    data <- renameColoumn(data, var.match$var_in, var.match$var_out)

    ## Change units
    for (col in intersect(names(data), var.match$var_out)) {
        idx <- match(col, var.match$var_out)[[1]]
        if (info$type[[col]] == "numeric") {
            unit.from <- var.match$unit_in[idx]
            unit.to <- info$units[[col]]
            if (unit.from != unit.to) {
                x <- data[[col]]
                data[[col]] <- eval(parse(text = getUnitConversion(unit.from, unit.to)))
            }
        }
    }
    data
}

getUnitConversion <- function(from, to){
    i <- mashrDetail("conversions")$unit_in == from &
            mashrDetail("conversions")$unit_out == to
    mashrDetail("conversions")$conversion[i]
}

#' Standardise data columns to match standard template.
#'
#' May add or remove columns of data as needed so that all sets have
#' the same columns.
#'
#' @param data data.frame, after being run through convertData
#' @return data.frame
addAllColumns <- function(data) {

    #' @description \code{na.vector} constructs a vector with NAs.
    #' @param type 'character' or 'numeric'.
    #' @param n Length of vector.
    na.vector <- function(type, n) {
        x <- switch(type, character = NA_character_, numeric = NA_real_)
        rep.int(x, n)
    }

    info <- columnInfo()
    missing <- setdiff(info$allowedNames, names(data))
    if (length(missing) != 0) {
        missing.df <- as.data.frame(lapply(info$type[missing], na.vector, nrow(data)),
            stringsAsFactors = FALSE)
        data <- cbind(data[, names(data) %in% info$allowedNames], missing.df)
    } else {
        data <- data[, names(data) %in% info$allowedNames, drop=FALSE]
    }
    data[, info$allowedNames, drop=FALSE]
}

#' Modifies data by adding new values from table studyName/dataNew.csv
#'
#' @description Within the column given by \code{newVariable}, replace values that
#' match \code{lookupValue} within column \code{lookupVariable} with the value
#' \code{newValue}.  If \code{lookupVariable} is \code{NA}, then replace all elements
#' of \code{newVariable} with the value \code{newValue}. Note that lookupVariable can be the
#' same as newVariable.
#' @param studyName Name of a study in \code{data} folder.
#' @param data existing data.frame (from addAllColumns)
#' @return modified data.frame
addNewData <- function(studyName, data) {
    import <- readNewData(studyName)

    if (!is.null(import)) {
        for (i in seq_len(nrow(import))) {
            col.to <- import$newVariable[i]
            col.from <- import$lookupVariable[i]
            if (is.na(col.from)) {
                # apply to whole column
                data[col.to] <- import$newValue[i]
            } else {
                ## apply to subset
                rows <- data[[col.from]] == import$lookupValue[i]
                data[rows, col.to] <- import$newValue[i]
            }
        }
    }
    data
}

#' Utility function to read/process dataNew.csv for addNewData
#' @param studyName folder where data is stored
readNewData <- function(studyName) {
    filename <- data.path(studyName, "dataNew.csv")

    import <- read.csv(filename, header = TRUE, stringsAsFactors = FALSE, strip.white = TRUE)
    if (nrow(import) > 0)
        import$lookupVariable[import$lookupVariable == ""] <- NA
    import
}

readMatchColumns <- function(studyName) {
    filename <- file.path(mashrDetail("dir.raw"), studyName, "dataMatchColumns.csv")
    var.match <- read.csv(filename, header = TRUE, stringsAsFactors = FALSE, na.strings = c("NA",
        ""), strip.white = TRUE)

    var.match
}

# Paste together list of varNames and their values, used for aggregating varnames
# into 'grouping' variable NOTE: Used in dataManipulate.R
makeGroups <- function(data, varNames) {

    apply(cbind(data[, varNames]), 1, function(x) paste(varNames, "=", x, collapse = "; "))
}

## Ensures variables have correct type
fixType <- function(data) {
    cfg <- mashrDetail("var.def")
    for (i in seq_along(cfg$variable)) {
        v <- cfg$variable[i]
        typeFun <- switch(cfg$type[i], numeric = as.numeric, character = as.character)
        data[, v] <- typeFun(data[, v])
    }
    data
}

# creates name of file to store processed data
studyDataFile <- function(studyName) {
    file.path(mashrDetail("dir.out"), "cache", paste0(studyName, ".csv"))
}

readImport <- function(studyName) {
    filename <- file.path(mashrDetail("dir.raw"), studyName, "dataImportOptions.csv")

    tmp <- read.csv(filename, header = FALSE, row.names = 1, stringsAsFactors = FALSE)

    defaults <- list(na.strings = "NA")
    import <- modifyList(defaults, structure(as.list(tmp[[1]]), names = rownames(tmp)))
    import$header <- as.logical(import$header)
    import$skip <- as.integer(import$skip)
    if (!("NA" %in% import$na.strings))
        import$na.strings <- c("NA", import$na.strings)
    import
}

getContributors <- function(data) {
    data$contact[!duplicated(data$contact$name), ]
}

##' @importFrom bibtex read.bib
readReference <- function(studyName) {
    filename <- data.path(studyName, "studyRef.bib")
    myBib <- read.bib(filename)

    # Hack work around to change key in bib entry (bibtex entry redefines '[' and/or
    # '[[' in ways that cause nothing but grief)
    tmp <- unclass(myBib)
    attr(tmp[[1]], "key") <- studyName
    class(tmp) <- "bibentry"

    tmp
}

getCitation <- function(myBib) {
    doi <- url <- ""
    if (!is.null(myBib$doi))
        doi <- myBib$doi[1]
    if (doi != "") {
        url <- paste0("http://doi.org/", doi)
    } else if (!is.null(myBib$url)) {
        url <- myBib$url[1]
    }
    citation <- format(myBib, "text")

    # clean up bib entry - establish arrays to pass into gsub
    find <- c("<URL.+>", "<.+?>", " , .", ", .", "\n", "*", "_", "“", "”", "..",
        ",.", ". .", "'''.'", "''.")
    replace <- c("", "", ".", ".", " ", "", "", "'", "'", ".", ".", ".", "", "")
    fixed <- rep(1, length(find))
    fixed[c(1, 2)] <- 0
    for (i in 1:length(find)) citation <- gsub(find[i], replace[i], citation, fixed = fixed[i])

    data.frame(doi = doi, url = url, citation = citation, stringsAsFactors = FALSE)
}

readMethods <- function(studyName) {

    vars <- mashrDetail("var.def")$variable[mashrDetail("var.def")$methods]

    # make data frame with all variables requiring methods
    methods <- data.frame(t(rep("", length(vars))), stringsAsFactors = FALSE)
    names(methods) <- vars

    # fill with data from study
    var.match <- readMatchColumns(studyName)
    var.match <- var.match[!is.na(var.match$var_out), ]

    for (v in var.match$var_out[var.match$var_out %in% vars])
        methods[1, v] <- var.match[v == var.match$var_out, "method"]

    methods
}

readContact <- function(studyName) {
    filename <- data.path(studyName, "studyContact.csv")

    contacts <- read.csv(filename, header = TRUE, stringsAsFactors = FALSE, strip.white = TRUE)
    data.frame(contacts, stringsAsFactors = FALSE)
}


columnInfo <- function() {
    allowedNames <- mashrDetail("var.def")$variable
    type <- mashrDetail("var.def")$type  # variable type: charcater / numeric
    names(type) <- allowedNames

    units <- structure(mashrDetail("var.def")$units, names = mashrDetail("var.def")$variable)
    list(allowedNames = allowedNames, type = type, units = units)
}

getFunctionFromSource <- function(functionName, filename, default = NULL) {
    e <- new.env()
    if (file.exists(filename)) {
        source(filename, local = e)
    }
    if (exists(functionName, envir = e)) {
        match.fun(e[[functionName]])
    } else if (is.function(default)) {
        default
    } else {
        stop(sprintf("Expected function '%s' within %s", functionName, filename))
    }
}


#' Replaces existing method by a new one in studyName/dataMatchColumns.csv
#'
#' @description \code{changeMethodCode} replaces an existing method in dataMatchColumns
#' with a new value. The output will overwrite the existing dataMatchColumns.csv file.
#' @param studyName Name of a study in \code{data} folder.
#' @param pattern Pattern (i.e. method code) to be found
#' @param replacement Replacement string (i.e. new method code)
#' @seealso \code{\link{grepPosition}} and \code{\link{replaceMethod}}
#' @export
changeMethodCode  <-  function(studyName, pattern, replacement) {
    data    <-  readMatchColumns(studyName)
    method  <-  data$method
    methodsList      <-  lapply(method, function(x)strsplit(x,',')[[1]])
    replacementList  <-  grepPosition(pattern, methodsList)
    data$method      <-  replaceMethod(replacementList, method, replacement)
    write.csv(data, data.path(studyName, 'dataMatchColumns.csv'), row.names=FALSE)
}

#' Creates a list with positions where replacements will have to occur 
#'
#' @description \code{grepPosition} will search for the positions of of fixed patterns
#' given a list of methods from \code{\link{changeMethodCode}}.
#' @param studyName Name of a study in \code{data} folder.
#' @param pattern Pattern (i.e. method code) to be found
#' @param methodsList List of methods found in studyName/dataMatchColumns.csv
#' @return List with pattern as names and respective positions where they are found in methodsList 
#' @seealso \code{\link{changeMethodCode}} and \code{\link{replaceMethod}}
#' @export
grepPosition  <-  function(pattern, methodsList) {
    if(class(pattern) != 'character')  
        stop('input pattern must be a character vector')

    replacementList         <-  vector(mode='list', length=length(pattern))
    names(replacementList)  <-  pattern

    for(s in seq_along(replacementList)) {
        for(k in seq_along(methodsList)) {
            test               <-  any(pattern[s] == methodsList[[k]])
            test[is.na(test)]  <-  FALSE
            if(test) {
                replacementList[[s]]  <-  c(replacementList[[s]], k)
            }
        }
    }
    
    replacementList
}

#' Replaces patterns on a given vector
#'
#' @description \code{replaceMethod} will replace patterns (names in 
#' \code{replacementList}) in a vector of methods.
#' @param replacementList List with pattern as names and respective positions 
#' generated by \code{\link{grepPosition}}
#' @param method character vector of methods from dataMatchColumns.csv
#' (i.e. dataMatchColumns.csv[["method"]])
#' @param replacement Replacement string (i.e. new method code)
#' @return modified \code{method} vector
#' @seealso \code{\link{changeMethodCode}} and \code{\link{grepPosition}}
#' @export
replaceMethod  <-  function(replacementList, method, replacement){
    if(length(replacementList) != length(replacement))
        stop('Replacement list and replacement string must have the same length')

    for(i in seq_along(replacementList)) {
        for(j in seq_along(replacementList[[i]])) {
            method[replacementList[[i]][j]]  <-  sub(names(replacementList)[i], replacement[i], method[replacementList[[i]][j]])
        }
    }
    method
}

#' Replaces existing method by a new one in studyName/dataMatchColumns.csv
#' and config/methodsDefinitions.csv
#'
#' @description \code{fullChangeOfMethods} replaces an existing method in the
#' entire database. The output will overwrite the existing dataMatchColumns.csv files
#' and the original config/methodsDefinitions.csv.
#' @param pattern Pattern (i.e. method code) to be found
#' @param replacement Replacement string (i.e. new method code)
#' @seealso \code{\link{changeMethodConfig}} and \code{\link{changeMethodCode}}
#' @export
fullChangeOfMethods  <-  function(pattern, replacement) {
    if(length(pattern) != length(replacement))
        stop('pattern and replacement strings must have the same length')

    changeMethodConfig(pattern, replacement)
    studies  <-  dir(mashrDetail('dir.raw'))
    sapply(studies, function(x, pattern, replacement)changeMethodCode(x, pattern, replacement), pattern, replacement)
}

#' Replaces existing method by a new one in config/methodsDefinitions.csv
#'
#' @description \code{changeMethodConfig} replaces an existing method in the
#' existing method reference database located at config/methodsDefinitions.csv.
#' @param pattern Pattern (i.e. method code) to be found
#' @param replacement Replacement string (i.e. new method code)
#' @seealso \code{\link{fullChangeOfMethods}} and \code{\link{changeMethodCode}}
#' @export
changeMethodConfig  <-  function(pattern, replacement) {
    if(length(pattern) != length(replacement))
        stop('pattern and replacement strings must have the same length')
 
    methods  <-  read.csv(file.path(mashrDetail('dir.config'), 'methodsDefinitions.csv'), header=TRUE, stringsAsFactors=FALSE) 
    methods$method[methods$method %in% pattern]  <-  replacement

    if(length(methods$method) != length(unique(methods$method))) {
        tab  <-  table(methods$method)
        stop(sprintf("New method '%s' already exists", names(tab)[tab>1]))
    } else {
        write.csv(methods, file.path(mashrDetail('dir.config'), 'methodsDefinitions.csv'), row.names=FALSE)
    }
}