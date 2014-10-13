% A Guide to using dataMashR

Intro
==========================

This is a guide for entering in data to create a database using
dataMashR with minimal coding involved.

DataMashR is a program for R which is able to effectively compile
various data files into a database. It's designed so that all
information is transparent; it's easy to identify all variables, where
they came from, any methods used and if something was entered
incorrectly it's also easy to change it.

This program is picky, however. It takes a little longer to enter in
data here than it would if you were just creating a database in a single
excel spreadsheet and if you have any mistakes it won't compile properly
until they are fixed. So take care to be as accurate as possible.

If you are working with multiple people on this project, make sure to
pull the project to Github using RStudio when you will begin working on
it, and push it to Github when you are done making changes. This gives
everyone the most up-to-date version when working on it.

Starting Your Database
==========================

This contains information on all of the files and folders necessary to
create your database from scratch. If you are using the template, you
need only read the first subsection.

Creating a New Database
-----------------------

For starters, you will already need to have RStudio and the datamashR
package installed on your computer. See Daniel about this.

1.  You will need to start a new git repository in Github (the + symbol
    next to your username). Give it a name , whatever you want to call
    your database folder

2.  Copy and paste the https address it will give you in the next
    option.

3.  Open RStudio, go to file -> new project ->version control -> Git->
    then copy and paste the HTTPS address it gives you into the top
    line.

    a.  This will create a folder in your documents, and this is where
        your database is going to be.

If you want to use the template I have given to start off your
database, copy and paste all files from the template into your
newly-created database folder. You can skip to the next section
“Entering in Data”. Unless

If you are making a database from scratch or want more
information on the folders and files used, then keep reading
this section.

The Essentials
--------------

Once you have your database folder created, it only contains 3 files
(blank, .Rhistory and an R Project file with the same name as your
folder). Ignore these.

You are going to need to add all of your files and folders from scratch,
and keep in mind all excel spreadsheets must be in .csv format, NOT
.xml.

The most important folder is the config folder. This *must* include the
following files:

-   variableConversion.csv: an excel file for converting between units.
    Row headings are:
    -   unit\_in
    -   unit\_out
    -   conversion
-   variableDefinitions.csv: an excel file for organizing all of your
    data. Row headings are:
    -   variable
    -   units
    -   methods (OPTIONAL) -- if you wish to report methods for your
        variables
    -   type
    -   label
    -   minValue
    -   maxValue
    -   allowable
    -   description
-   postProcess: an R file. Inside you will need to input the following
    code:

```
postProcess <- function(data, lookupSpecies="none"){

   data
   }
```

-   methodsDefinitions.csv: a csv file created in excel. This is
    optional. Include this if you wish to report methods for your
    variables (you should have also added a 'methods' heading in your
    variableDefinitions). Headings include:
    -   method
    -   definition

Other Folders and Files- Template
---------------------------------

Though not necessary for the program to run, you will need to create a
few more files in order to successfully incorporate data into your
database.

The first folder is the Template folder. This will contain all of the
empty files you will need to fill in for each data source in order for
it to compile into the database. You need only to copy, paste and rename
it for each data source you wish to add. Keep in mind all excel files
must be in .csv format, NOT .xml. Here are the files you need to add
into it:

-   data.csv: this is the excel file where you will place the data
    directly from your source.
    -   There are no headings -- you will fill these in according to the
        headings needed for your data.
-   dataImportOptions.csv: this excel file tells the program how to deal
    with your data folder.
-   dataManipulate: this R file must contain the following code:

```
   manipulate \<- function(raw) {
   raw
   }
```

-   dataMatchColumns.csv: this excel file is the go-between that tells
    the config files how to deal with your data. You will need to add
    the following headings:
    -   var\_in
    -   method (optional) -- include this if you decided to include
        methods
    -   unit\_in
    -   var\_out
    -   notes
-   dataNew.csv: this facilitates things if you need to alter variables
    and values later on. Headings you will need:
    -   lookupVariable
    -   lookupValue
    -   newVariable
    -   newValue
    -   source

-   studyContact.csv: this provides a record of who filled out each data
    file, and how to contact them. Headings include:
    -   name
    -   email
    -   address
-   studyMetadata.csv: this provides the metadata for each heading in
    your data file. Headings include:
    -   Topic
    -   Description

Other folders and Files - Output
--------------------------------

This folder will be automatically created by dataMashR if not already
present. But if you want to know what to expect, here it is:

-   Cache: It will eventually gather and store all of your data files in
    .csv as used in the database in the same formatting as used in the
    database.
-   Mashup: It will eventually contain your database and all other
    components from all of your papers compiled together.
    -   This will be further explained under the 'building your
        database' heading.

Other Folders and Files - Extras
--------------------------------

You already have everything you need. But here are a few suggestions of
folders you may find useful:

-   Data -- a folder with which to store all of your completed data files
    (made from your template)
-   Papers -- a folder to keep and organize scientific papers or other
    data sources you have yet to or have include in your database.
    Useful especially if you need to go back and double-check your
    values.

Entering Data
===================

This is the how-to of entering data into a database. It's written from
the perspective of entering papers from a scientific paper, but it can
be applied to a variety of sources. When entering data, copy and paste
as much as possible to avoid unnecessary errors.

### First steps: studyContact, data and studyMetadata

1.  Copy and paste your Template folder into the data folder. Rename it
    according to the source you are getting your data from so it's easy
    to identify later.
2.  Open the renamed Template file. Go into studyContact and enter your
    name, email and institution you are working for.
    a.  If anyone in the future needs to know who entered this data,
        they know how to contact you.
3.  All data from your paper needs to be entered into a single excel
    spreadsheet called 'data' within the template file. Don't worry
    about altering your column titles too much; it will be done later.
    a.  Ensure all data you want from your source is here in a single
        table.
        i.  Combine all tables from your data here;
        ii. Create new columns for data extracted from text
    b.  If any cells are blank for any reason (which happens a lot when
        combining multiple tables), put 'NA' in them rather than leave
        them blank
        i.  It's easy to do a 'find and replace' function for this.
    c.  Highlight and copy all of your data before closing the .csv file
        for the first time. Re-open and check that your formatting
        hasn't changed. You can paste it back if it has.
        i.  .csv files can mess with your formatting which can be
            tedious in your data files. But once it survives being
            closed once, it will survive it again.
    d.  A few things to keep in mind:
        i.  *Copy and paste as much as possible.* Even for single words
            or numbers. This drastically reduces errors from mindless
            typos.
        ii. Make sure your titles don't have any symbols besides full
            stops (.) and underscores (\_). Anything else will throw R
            into a fit, so you will have to alter your titles to
            accommodate this.
        iii. Make sure your titles don't have units, as this usually
            means that there are symbols involved. You'll add those in
            later.
        iv. Don't mix numbers with characters (symbols/letters) in your
            columns if the column only requires a number. You need to
            tell R later whether columns are numeric or character. You
            can have numeric values in character columns, but not the
            other way around.
            1.  This includes all symbols, so standard errors (±) must
                be removed as well as values presented in a range (2-6).
        v.  The =TRIM function is useful for removing unwanted spaces
            that can occur when importing tables from pdf format using
            adobe acrobat. This may save you all kinds of trouble when
            those odd spaces prevent you from compiling the database.
        vi. Consider adding a notes column at the end of the table.
            Sometimes values can use an explanation to better understand
            them

4.  Fill out the studyMetadata.
    a.  Open this spreadsheet and copy and paste in your headings from
        the 'data' spreadsheet (using the transpose option in 'paste
        special' if you right click). They should be listed vertically
        under the 'Topic' heading.
    b.  Under the 'description' heading enter information about each
        topic. State what it is referring to, what the units are (if
        any), and where the information was extracted from.
        i.  This makes things easier if you have forgotten what a
            variable was referring to, or if someone else needs to know
            where you got a piece of information from.
    c.  A few things to keep in mind:
        i.  From this table, one should understand exactly what each
            heading on your table is referring to and where you got that
            information from.
        ii. Make sure you copy and paste your heading titles into the
            topic heading from your data, because they must both match
            up exactly.

### Next steps: dataMatchColumns and variableDefinitions.

Papers will report the same data in different ways, and this next
process is what will standardize your variable names and units so you
can directly compare data across multiple sources. You will be going
back and forth a lot between your dataMatchColumns file and the files in
your config folder.

There is no exact order the config files need to be done in. As long as
it's done and done accurately everything will work just fine.

1.  Open dataMatchColumns and copy and paste your data titles from
    studyMetadata into the heading var\_in.
    a.  They will be the same as your headings from 'data' but already
        transposed for you, so it's just easier.
2.  Under the heading unit\_in, enter which units your variables are
    listed in.
    a.  You should have this information already in your studyMetadata
3.  Under the heading 'notes' add anything you need if you end up making
    any changes that someone may need to know about.
    a.  If in doubt, include it.
4.  Open variableDefinitions. We'll start with this file.
    a.  Under the heading 'variable', decide in what you want your
        variable to be called in your database, and type it in. You must
        have an entry for every variable in your paper.
        i.  If this is not your first paper, then likely you will
            instead find an existing entry that fits your variable from
            your paper
        ii. Don't use symbols besides . and \_ for your variable names
    b.  Each variable name within a paper must be different, but you
        will use the same variable name across multiple papers if it is
        the same measured variable.
        i.  Especially if this is not your first paper -- you want to
            re-use variable names wherever applicable so that data will
            end up in the same column and in the same units in your
            database and be directly comparable.
    a.  For example, looking at nitrogen by leaf mass I've seen N,
        n.mass, N%, Nmass in different papers, some in different units.
        It's the same measured variable though, and I want to have all
        of them in 1 column to compare them. Therefore I called the
        variable N\_mass in my variableDefinitions, and match all of
        these values up with that single name in every paper.
5.  Under the 'units' heading, type in what unit you want that variable
    reported in in your database, even if it's different from your
    paper. You will make a conversion later.
    a.  For N\_mass, I chose mg g-1.
        i.  If a column is containing only numbers, you must give it a
            unit even if it doesn't have one. Call it dms (for
            dimensionless) if you have to.
6.  Under 'methods', decide whether you should report a method used to
    obtain these values or not.
    a.  'TRUE' means there is always a method, 'FALSE' means no method
        is ever used.
7.  Under the heading type, decide if the column should contain only
    numbers, or letters numbers and symbols. Type in 'numeric' for
    numbers and 'character' for everything else.
    a.  If you decided on numeric, you MUST have units reported in the
        'units' column.
8.  Ignore 'label'.
9.  'minValue' and 'maxValue' must be filled in if your type is numeric.
    This is to create a cut-off point where only values within your min
    and max values will be included in your database.
    a.  For now, you might want to enter in -999999 and 999999 just to
        include everything; you can always go back and change it later.
10. Go back to dataMatchColumns. In the 'var\_out' column, copy and
    paste whatever it is you want to call that variable in your dataset
    from your 'variable' column.
    a.  For example, I might have n.mass listed in the table from a
        paper, so it will be listed the same in my data file and
        therefore under the 'var\_in' heading in my dataMatchColumns.
        But I want to call all nitrogen by mass measurements N\_mass in
        my database because it looks clearer. So I copy and paste
        N\_mass from 'variable' in my variableDefinitions file and paste
        it under the 'var\_out' heading in my dataMatchColumns, in the
        same row I reported n.mass in.
11. Repeat steps 4-10 for each variable in your paper that doesn't
    already have an entry that would suit. You can always come back and
    change things later if you decide to change your variable name and
    units.
    a.  Keep in mind: If you change anything in this folder, it must
        match up with every dataMatchColumns folder that has matching
        information.
12. At this point, in your dataMatchColumns you should have your
    var\_in, unit\_in and var\_out columns completed.
13. Things you can and cannot have with variableDefinitions
    a.  You can have rows of variables in this table that are not being
        used in any of your data files. As long as they are filled in
        correctly, this is not a problem
        i.  It will just be a blank column in your database
    b.  You cannot have any variables in your data files that are
        missing their matching variable in the variableDefinitions.
        Everything must be there or your database will fail.

### Fix your units with variableConversion.

1.  In config, open your variableConversion folder. Under the heading
    unit\_in, **copy and paste** the units from your dataMatchColumns.
    These should be the same ones listed in your paper.
2.  Under the heading unit\_out, copy and paste the units from the
    variableDefinitions; that is, the units you want each variable to be
    reported in for your database. They don't have to be the same as the
    ones in your data sources.
    a.  For example, for N.mass I would copy and paste g g-1 in my
        unit\_in column. I decided to report all N\_mass values in mg
        g-1, so I could paste that in the unit\_out column.
3.  In the conversion column, you type in a conversion that will apply
    to all of the values in your 'unit\_in' column to get the value in
    your 'unit\_out' column.
    a.  using an 'x' to represent your unit\_in column, divide or
        multiply x appropriately to get the units in your unit\_out
        column.
    b.  For example, to get from g g-1 in my unit\_in column to mg g-1
        in my unit\_out column, my conversion column will have the
        conversion x\*1000.
4.  Things you can and cannot have with variableConversion:
    a.  Every conversion you need must be there, even if the values
        match and you are not actually converting anything. If your
        unit\_in is % and your unit out is %, it must still be there.
        Your conversion is simply x.
    b.  If your 'unit\_in' value is the same units as another value but
        typed in differently, (mg/g vs mg g-1 vs mg g\*-1), they must
        all be in there, even if the conversion and unit\_out is exactly
        the same. If anything is unique in any way, it must be in there.
    c.  No two completely identical rows are allowed, so each unique
        conversion can happen only once.
        i.  R will get confused and not know which one to choose.
    d.  You can have extra rows. If for whatever reason you have a unit
        conversion in the table you are no longer using, leave it. It
        won't affect anything. What will be a problem is if you delete
        it and realize later when your database is not working that you
        were actually using that conversion for another paper.
5.  A helpful tip:
    a.  Before you compile your database, sort your variableConversion
        file by unit\_in then by unit\_out. This is a quick way to scan
        if you have any exact duplicates that should be removed.

### The methodsDefinitions

1.  Open the methodsDefinitions file in the config folder.
    a.  Under the 'Topic' heading, enter in a shortcut. This can be
        whatever you want and will identify that method to match it to
        your variable. Some tips:
    b.  As an example, I prefer alphanumeric values, such as a1. Many
        similar variables are obtained using different methods, so all
        methods used to obtain nitrogen I call 'a'. The first nitrogen
        measurement method will be a1, and the next unique one I come
        across in another paper will be a2. For chlorophyll measurement,
        the first method will be 'b1' and so on. But do whatever works
        for you.
    c.  I also like to make a method called x. It's a 'method
        unspecified' method. Just in case.
2.  Under the 'definition' heading, describe your method and what it is
    measuring.
    a.  It's up to you how detailed it is, and this information is
        usually obtained directly from your paper.
3.  Go back to your dataMatchColumns again.
    a.  Under the 'method' heading, copy and paste whatever you put
        under the 'Topic' heading in your methodsDefinitions folder.
        i.  This will now match up your described method with that
            value, and can be seen in the database if queried.
    b.  If you typed TRUE in the 'methods' heading in your
        variableDefinitions file for a particular variable, then it MUST
        have a method.
        i.  This is where method x comes in, as sometimes a method isn't
            always reported when it should be.
    c.  If you typed FALSE, then you cannot have a method in the
        dataMatchColumns.
        i.  If there is one you need to add in, then go back and change
            your variableDefinitions file (and every other paper already
            added to your database which also contains that variable).

ANY empty cells in your dataMatchColumns (usually in method and
unit\_in) must be filled in with NA.

### Last step: References

You will need to also enter a reference file into each template folder.
1.  Go into Mendeley, enter your paper and let Mendeley extract the
    relevant data for you.
2.  Insert the reference into your template folder
    a.  In Mendeley to go file ->export.
    b.  Rename it 'studyRef' and save it as a .bib (should be the
        default option) into your template folder.
And you are done! It will get easier. Once you add in more papers, many
of the var\_out values will already be there ready for copying and
pasting, methods may be the same and some of the exact conversions you
need will already be there. Remember: everything must match *perfectly*
in order for it to mash seamlessly into a nice clean database.

### Entering Data from Databases

There are only a few main differences when entering in data from a
database instead of a scientific paper:

1.  Your data will likely already be in a single table for you
    a.  It may be a formatting nightmare, but that initial step is done.
2.  Watch out for data that has been calculated from other columns in
    case you change the order
    a.  It's best to copy from the database and 'paste special ->
        values' in your data file.
3.  You will not have much metadata
    a.  Still fill out the file, but be realistic; if you are not sure
        what something is, make note of your uncertainty
4.  You will probably not have methods
    a.  Just use 'method x' for all of those variables that require a
        method.
5.  Your references may be lacking
    a.  If the database was given to you as raw data or just simply is
        not a published works that Mendeley can easily process, I have
        not gotten the references to work.
        i.  I copy and pasted a reference file from one of my other
            papers. Not the most professional, but it's a temporary
            solution that hopefully Daniel can fix.

Building Your Database
======================

This section describes how to error-check and build your database, as
well as explain the output after the database is built.

Code
--------------------------------------

Test


```
library(testthat)
library(dataMashR)
checkPackage(".")
```


Build

```
library (dataMashR)
options(error=recover)
mashData(verbose=T)
```

Testing and Building
--------------------

-   Test -- this will go through all components of your database and tell
    you where any fatal errors are. This is much better than building
    your database, having it fail and giving you no indication
    whatsoever what went wrong.
    -   If you run the code in RStudio, it will stop you when it gets to
        a part in your database where something isn't correct or doesn't
        match up
    -   Look at the code closely -- it usually tells you what file the
        error is located in and roughly what kind of error it is
    -   Once you think the error is fixed, run it again to find the next
        one
        -   Repeat ad nauseum until it finds no more errors
-   Build -- this is what actually puts your database together.
    -   Run this in RStudio after Test errors have been fixed using Test
    -   If it fails, try running Test again just to be sure it can't
        find anything else
        -   If test still fails to find any errors, go find Daniel.
I recommend you run these after each new paper is added to keep errors
down and make troubleshooting easy.

The Output Folder
------------------

As mentioned previously, this is the folder where your database is kept
and will be created automatically when your database is built. Most
files added into it will be self-evident when you click on them. But
I'll break it down anyways so you know what to expect in each of the two
files:

-   Cache: It will have a .csv file for each data source
    -   Exactly as mentioned earlier, this contains a cache of all the
        data files used in your database broken down for each source.
        -   It's a miniature version of the database that contains only
            data from one particular source.
-   Mashup: this now has a few .csv files added into it.
    -   contacts: this is a compilation of all studyContact files
    -   data: **THIS IS YOUR DATABASE. **
        -   It is a compilation of all data files, complete with chosen
            variable names and units you wanted all values to be
            displayed in.
    -   dictionary: basically your methodsDefinitions file all over
        again
    -   methods: this displays all methods (using the short form names)
        for every paper and every variable
    -   references (.bib): contains short form references in code
        format. I'm not positive what this is for.
    -   references (.csv): a compilation of all doi;s, url;s and the
        full our reference for each paper as put together by Mendeley.

A few Last Notes
----------------

If there are variables that could be calculated using other given
variables, that's where postProcess comes in in the config folder. Talk
to Daniel and he can arrange for these things to be filled in for you.

**Quick Flow Chart**
====================

A quick guide to check while working to make sure you don't miss any
steps.

Template, studyContact and studyMetadata
----------------------------------------

1.  Copy & Paste the Template folder into Data folder; rename it
2.  Fill out studyContact
3.  Add all data from your source into the 'data' spreadsheet
4.  Paste all headings from 'data' into studyMetadata
5.  Fill out all studyMetadata information

dataMatchColumns and Config
---------------------------

1.  Open dataMatchColumns - paste in headings from studyMetadata
2.  Enter given units into unit\_in
3.  Open variableDefinitions
4.  Create a new entry for your variable if necessary
5.  Otherwise: paste the suitable variable name into var\_out in
    dataMatchColumns
6.  variableConversion: make sure all values in your dataMatchColumns
    have a conversion
7.  methodsDefinitions: describe any new methods for variables that
    require one
8.  once all conversions are added: sort and remove exact duplicates
9.  paste the method shortcut into dataMatchColumns

Last Steps
----------

1.  Create your reference with Mendeley, save in your Template folder
2.  Run 'Test' in RStudio; fix errors
3.  Run 'Build' in RStudio. If it works, your entry is complete

