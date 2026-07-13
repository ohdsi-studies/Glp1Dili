## How to run the study

The following instructions will guide you through the process of setting
up your system to run this network study (**Glp1Dili**).

**If you are a data-contributing site**, you only need to touch two files:
`CodeToRun.R` (to add your connection details) and `ShareResults.R` (to
upload your results once execution finishes). Everything else in this
repository (`CreateStrategusAnalysisSpecification.R`, `UploadResults.R`,
`CreateResultsDataModel.R`, `app.R`, `DownloadCohorts.R`,
`EvidenceSynthesis.R`) is used by the study coordinator only, and you can
ignore it.

## System setup and configuration

Start by making sure your R environment is set up by following the instructions
on the [OHDSI HADES R Setup site](https://ohdsi.github.io/Hades/rSetup.html).
Be sure to install R, RTools, RStudio and Java. Next, verify that you can
properly connect from R to your OMOP CDM via DatabaseConnector per the instructions
[here](https://ohdsi.github.io/Hades/connecting.html#Configuring_your_connection) and
[here](https://ohdsi.github.io/DatabaseConnector/articles/Connecting.html).

Once you have installed the tools mentioned above and confirmed database
connectivity via R, you are ready to take the next step.

> **Note:** This study does not use the PatientLevelPrediction module, so a
> Python/Reticulate setup is **not** required to run it.

## Download the study package

To get started, you will want to download the study code in this repository
to your local machine. Instructions for downloading are found [here](https://docs.github.com/en/repositories/working-with-files/using-files/downloading-source-code-archives#downloading-source-code-archives-from-the-repository-view). In this guide, we will assume that you have
downloaded the .ZIP archive to **`D:/git/ohdsi-studies/Glp1Dili`.**

## Restore the execution environment

[Use RStudio to open the project file](https://support.posit.co/hc/en-us/articles/200526207-Using-RStudio-Projects#:~:text=There%20are%20several%20ways%20to,Rproj) `Glp1Dili.Rproj`, found in
the root directory of `D:/git/ohdsi-studies/Glp1Dili` (or wherever
you opted to download the study package). When you open the project, you will
see the following:

```r
R version 4.4.1 (2024-06-14 ucrt) -- "Race for Your Life"
Copyright (C) 2024 The R Foundation for Statistical Computing
Platform: x86_64-w64-mingw32/x64

R is free software and comes with ABSOLUTELY NO WARRANTY.
You are welcome to redistribute it under certain conditions.
Type 'license()' or 'licence()' for distribution details.

  Natural language support but running in an English locale

R is a collaborative project with many contributors.
Type 'contributors()' for more information and
'citation()' on how to cite R or R packages in publications.

Type 'demo()' for some demos, 'help()' for on-line help, or
'help.start()' for an HTML browser interface to help.
Type 'q()' to quit R.

# Bootstrapping renv 1.0.11 ---------------------------------------------------
- Downloading renv ... OK
- Installing renv  ... OK

- Project 'D:/git/ohdsi-studies/Glp1Dili' loaded. [renv 1.0.11]
- One or more packages recorded in the lockfile are not installed.
- Use `renv::status()` for more details.
```

The first time you run this study, you will need to restore the execution
environment using [renv](https://rstudio.github.io/renv/). To do this,
in the console run:

`renv::restore()`

and follow the prompts to install all of the packages. This will take some time
to complete (approximately 30 minutes) and will install the exact package
versions this study was validated against, including `CohortMethod` and
`Strategus`. Once this operation is complete, please close RStudio and
re-open the project. You will again see the message above stating that
packages recorded in the lockfile are not installed. You can safely ignore
this message moving forward. At this point you are ready to run the study.

## Running the study

Open the file `CodeToRun.R` found in the root of your study package
directory (e.g. `D:/git/ohdsi-studies/Glp1Dili`). This script will
require some modification to work in your environment. At the top of the
script, you will see a commented-out note to tell you to run `renv::restore()`. If
you followed the earlier instructions, this is not necessary but is a reminder.

### Environment Settings

```r
# ENVIRONMENT SETTINGS NEEDED FOR RUNNING Glp1Dili ------------
Sys.setenv("_JAVA_OPTIONS"="-Xmx4g") # Sets the Java maximum heap space to 4GB
Sys.setenv("VROOM_THREADS"=1) # Sets the number of threads to 1 to avoid deadlocks on file system
```

Please make sure that you either set these options by running these commands
or by adding them to your .Renviron file. If you choose to add these options to
your .Renviron file, please restart your R session after making these changes
to ensure the environment variables are set.

### Study execution settings & CDM connection details

The next block of code will require edits for your environment:

```r
##=========== START OF INPUTS ==========
cdmDatabaseSchema <- "[your CDM database schema]"
workDatabaseSchema <- "[a database/schema you can write to]"
outputLocation <- "[local path to your project, e.g. D:/git/ohdsi-studies/Glp1Dili]"
databaseName <- "[a short name for your database, used as a folder name]"
minCellCount <- 5
cohortTableName <- "glp1dili_cohorts"

# Create the connection details for your CDM
# More details on how to do this are found here:
# https://ohdsi.github.io/DatabaseConnector/reference/createConnectionDetails.html
connectionDetails <- DatabaseConnector::createConnectionDetails(
  dbms = "[your dbms, e.g. postgresql, sql server, redshift, spark]",
  connectionString = "[your connection string]",
  user = "[your username]",
  password = "[your password - consider using keyring::key_get() instead of a literal value]"
)

##=========== END OF INPUTS ==========
```

To detail these settings:

- **cdmDatabaseSchema**: The name of the database schema holding your OMOP CDM patient level data.
- **workDatabaseSchema**: The name of the database schema that will hold your cohort tables.
You must have write access to this schema to create/drop tables and to insert/delete data.
- **outputLocation**: Set this to the path where you have your project, in our
example `D:/git/ohdsi-studies/Glp1Dili`.
- **databaseName**: Set this to the name of your site's OMOP CDM database. Please avoid special
characters as this value is used to create a folder with the `databaseName` to hold the results
of running the study.
- **minCellCount**: Set this value based on any site specific requirements you have for censoring
small cell counts.
- **cohortTableName**: This is a prefix that will be appended to the cohort tables created in your
`workDatabaseSchema`. There is no need to change this value unless you'd like to have a different
way to identify the tables from this study.
- **connectionDetails**: This holds the connection details for your CDM. Please follow the
instructions here: https://ohdsi.github.io/DatabaseConnector/reference/createConnectionDetails.html to
configure this value. **IMPORTANT:** Please be sure to follow the instructions from above in the
System setup and configuration section.

### Executing the study

Once you have set the values as described above, you can run the entire study by
running the full `CodeToRun.R` script. Here we'll describe what the
code is doing to run the study:

```r
analysisSpecifications <- ParallelLogger::loadSettingsFromJson(
  fileName = "inst/fullStudyAnalysisSpecification.json"
)

executionSettings <- Strategus::createCdmExecutionSettings(
  workDatabaseSchema = workDatabaseSchema,
  cdmDatabaseSchema = cdmDatabaseSchema,
  cohortTableNames = CohortGenerator::getCohortTableNames(cohortTable = cohortTableName),
  workFolder = file.path(outputLocation, "results", databaseName, "strategusWork"),
  resultsFolder = file.path(outputLocation, "results", databaseName, "strategusOutput"),
  minCellCount = minCellCount
)

if (!dir.exists(file.path(outputLocation, "results", databaseName))) {
  dir.create(file.path(outputLocation, "results", databaseName), recursive = T)
}
ParallelLogger::saveSettingsToJson(
  object = executionSettings,
  fileName = file.path(outputLocation, "results", databaseName, "executionSettings.json")
)

Strategus::execute(
  analysisSpecifications = analysisSpecifications,
  executionSettings = executionSettings,
  connectionDetails = connectionDetails
)
```

The first section loads the `inst/fullStudyAnalysisSpecification.json` which
contains the full study specification as described in the protocol. Next,
the execution settings are built (based on your inputs). These execution
settings are saved using `ParallelLogger::saveSettingsToJson` in case you
need to refer back to these settings. Finally, `Strategus::execute` is
called to execute the analysis against your OMOP CDM.

Execution runs four modules in sequence: `CohortGeneratorModule`,
`CharacterizationModule`, `CohortIncidenceModule`, and
`CohortMethodModule`. A summary of each module's success/failure and
execution time is printed to the console when the run finishes, and full
details are written to `strategus-log.txt` in your results folder.

## Sharing Results

Once you have successfully executed the study, your results will be located in
the folder: `[your outputLocation]/results/[your databaseName]/strategusOutput`.
Within this folder you will see subfolders for each of the analytical modules
that produced results. These results are stored as CSV files which you can
inspect before providing the results to the study coordinator.

Once you have reviewed your results and are ready to provide them to the study
coordinator, you can use the `ShareResults.R` script located in the root of the
project in `D:/git/ohdsi-studies/Glp1Dili`. This script will require some
modifications to reflect the choices you made when running the study:

```r
##=========== START OF INPUTS ==========
outputLocation <- "[local path to your project, e.g. D:/git/ohdsi-studies/Glp1Dili]"
databaseName <- "[your site name, used as a folder name - must match CodeToRun.R]"

# The signed upload URL provided by the study coordinator via email.
# This is a single-use-path URL scoped to your site only.
gcsUploadUrl <- "[signed URL provided by the study coordinator]"
##=========== END OF INPUTS ==========
```

To explain these settings:

- **outputLocation**: Set this to the path where you have your project, in our
example `D:/git/ohdsi-studies/Glp1Dili`. This must match what you
set in your `CodeToRun.R`.
- **databaseName**: Set this to the name of your site's OMOP CDM database to
match what you set in your `CodeToRun.R`.
- **gcsUploadUrl**: A time-limited, write-only URL to a Google Cloud Storage
bucket, scoped only to your site's upload path. You do **not** need a Google
account or any cloud credentials of your own to use this - the study
coordinator will email you this link directly. If you don't have one yet,
email the study coordinator to request it. If your upload fails and the
error mentions an expired or invalid signature, your link has likely expired
(they are time-limited) - email the study coordinator for a new one.

Running `ShareResults.R` will zip your results folder and upload it directly
to the study coordinator via the link above - no further action is needed on
your end once the upload confirms success.
