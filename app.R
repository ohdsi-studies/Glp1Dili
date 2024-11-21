################################################################################
# INSTRUCTIONS: The code below assumes you uploaded results to a PostgreSQL 
# or SQLite database per the UploadResults.R script.This script will launch a 
# Shiny results viewer to analyze results from the study.
#
# See the Working with results section
# of the UsingThisTemplate.md for more details.
# 
# More information about working with results produced by running Glp1Dili 
# is found at:
# https://ohdsi.github.io/Glp1Dili/articles/WorkingWithResults.html
# ##############################################################################

library(ShinyAppBuilder)
library(OhdsiShinyModules)

# Settings ---------------------------------------------------------------------
resultsConnectionDetails <- DatabaseConnector::createConnectionDetails(
  dbms = "sqlite",
  server = "E:/GLP1DiliResults/Results.sqlite"
)
resultsDatabaseSchema <- "main"


# Don't make changes below this line -------------------------------------------
shinyConfig <- initializeModuleConfig() |>
  addModuleConfig(
    createDefaultAboutConfig()
  )  |>
  addModuleConfig(
    createDefaultDatasourcesConfig()
  )  |>
  addModuleConfig(
    createDefaultCohortGeneratorConfig()
  ) |>
  # addModuleConfig(
  #   createDefaultCohortDiagnosticsConfig()
  # ) |>
  addModuleConfig(
    createDefaultCharacterizationConfig()
  ) |>
  # addModuleConfig(
  #   createDefaultPredictionConfig()
  # ) |>
  addModuleConfig(
    createDefaultEstimationConfig()
  ) 

# now create the shiny app based on the config file and view the results
# based on the connection 
ShinyAppBuilder::createShinyApp(
  config = shinyConfig, 
  connectionDetails = resultsConnectionDetails,
  resultDatabaseSettings = createDefaultResultDatabaseSettings(schema = resultsDatabaseSchema)
)
