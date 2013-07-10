dataMashR
================

Daniel Falster, Remko Duursma, Rich FitzJohn, Diego Barneche

An R package for merging data from different studies in a transparent and reproducible fashion

For `dataMashR` developers:
* Manual pages are made with roxygen2. In Rstudio, make sure to set up automated
  roxygen compilation under `Build\Configure build tools...\Configure`.
* Do NOT edit the Rd files in /man manually. Ever.

**WARNING:** Please note, this package is currently under active development. If you decide to use it, please be aware that the package structure is unstable, so future changes may break your scripts.

Dependency graph
----------------
![Dependency graph](dep.png)

Issues to discuss, following Diego's trial on a new project
----------------

**Documentation** - how does one set up a project using dataMashR? How does one organize its project before the initial files set up? How does one follow each step at a time (e.g. go to R, uses the function to setup files, go back to created files, fill them in accordingly, go back to R, runs function loadStudies) ?

**Interplay between dataManipulate, dataMatchColumns and dataNew.** This is crucial to the user's understanding in my opinion. How manipulating columns in different files affect the flow. For example, when one sets up files initially, the dataMatchColumns will contain all columns from the original .csv; however, if for some reason you create or delete a column within dataManipulate, the user will have to manually add/delete that column from dataMatchColumns. Also, if a column is called, say, Site, and it is very important indexing to create for example the column status in dataNew, one has to indicate Site as status in var.out in dataMatchColumns, otherwise the system won't recognise the entry Site for the lookUpVariable in dataNew. That may sound trivial to us, but will certainly cause confusion in the user that only wants to run things quickly. That brings the issue that we should make it very clear that this is not a one-line-script-that-will-give-back-a-ready-product package. At the same time, we must explicitly raise all the advantages of doing it 'our' way, and why we don't use, say, mySQL and relational databases.

**studyRef.bib** I made a change and removed studyRef.csv from new set ups. I substituted that for a standard unpublished-study bib format (check it out in the repo). However, for projects that have defined references (like baad) we may want to create a function that grabs a table predefined by the user with study names and DOIs? That way it would automatically try to download the correct studyRef.bib? Otherwise the entry could be unpublished and would return this unpublished format I created as a standard.

**paths** dirRawData, dirCleanData, how do those sit in the current version of the package? Are they going to be fixed? Or are we coding it in such a way that the user will have the freedom to name folders on its own desired way?

**variableDefinitions:** We need a full documentation of variableDefinitions as many functions need some fixed columns (e.g. methodsVariable) in order for this to work. I made a small change to that as well in import.R, however one still needs that column in their var.def file to work. As I mentioned to you already, in the initial set up, we could add the creation of variableDefinitions, methodsDefinitions, etc?


Setup
----------------
The function `startDataMashR` defines location of various folders needed to run
dataMashR. If it hasn't already been run, this function is called from
`mashrDetail`, which is used to access elements of the setup.

If you want to deviate from default setup, you need to re-define `startDataMashR`
or run it with custom names, before running anything else in the dataMashR package.

Data preparation
----------------

1. Each dataset is organised around a single publication and put into a folder with first-author and year, e.g. "Aiba2005". For unpublished data, data can be organised into a logical publishable unit and year set to 0000, e.g. "Aiba000".

2. The general principle when preparing data is to make as few (ideally none) changes as possible to the raw data file. Any transformations, unit changes, name changes etc can be done in the R import scripts. If you have an excel file, simply export it as a csv file.

3. Each folder requires the following files
	- `data.csv`: the raw data from the study
	- `dataImportOptions.csv`: specifies options for loading `data.csv` into R
	- `dataManipulate.r`: Can be left empty, this file contains any custom manipulations for the study. For example,
	- `dataMatchColumns.csv`: This table containing a list of columns in the original data file `data.csv`, plus any created in `dataManipulate.r`, in the column `var_in`, and the name of the variable in our database that it corresponds to in the `var_out` column. (See the `variableDefinitions.csv` file for a list of variables.) Variables which do not match anything in the database can be left blank. For any numeric variable (e.g. leaf mass), list the `units` of the raw data and the `method` used to measure it. Methods are recorded using a code. (See `methodsDefinitions.csv` for details.) Note also, all variable names are case sensitive. So if the original data has a column called `Species` you need to put `species` in the column `var_out`. Any variables created in `dataManipulate` file, must also be added into `dataMatchColumns`.
	- `dataNew.csv`: This table contains crucial information that doesn't exist in `data.csv`, e.g., latitude. If latitude is a non-existing columns in `data.csv`, than it can be entered in 'newVariable' with it's respective value in 'newValue'. Also note the source, i.e., where did the value come from and who entered it. If a value on applies to a subset of the data, this can be specified with `lookupVariable` and `lookupValue`. Eg. let's say that the published paper states that all individuals of "Genius speciei" were collected at a latitude of 10.5; this data could be entered using the following line
    species, exampleSpecies1, latitude, 10.5, from paper (D Falster).
	- `pdf-Aiba2005.pdf`:
	- `questions.txt`:
	- `studyContact.csv`: Who's responsible for the dataset? emailed, address?
	- `studyMetadata.txt`:
	- `studyRef.csv`: a single line with the full reference where the dataset comes from.

## Script to aid file preparation

Diego has written some functions to help create the above files.

1. First create a file `dataImportOptions.csv` for each new study using `makeDataImport(newStudyName)`

2. Then enter arguments in the `dataImportOptions.csv` files just created. Does the new data need to skip row, does it have a header?

3. Then make the remaining files: `setUpFiles(newStudyName)`

4. Fill in values for `dataMatchColumns.csv`, `dataManipulate.R`, `dataNew.csv`,  `studyContact.csv`, `questions.txt`, `StudyRef.csv`.

## Stages of import

0. File set-up incomplete (does not pass tests)
1. Data processed, but incomplete, or not verified by co-author
2. Report sent to co-author, issues outstanding
3. Finish data processing, verified by co-author, updated report sent.


