library(tidyr)
library(dplyr) #redundant

library(ggplot2)
library(ggsci)
library(grid) #plotting diagnostics
library(forestplot) # plotting meta-analysis
# library(Andromeda) #not used
# library(forestploter) #plotting diagnostics
# library(gt) #diagnostics table
# library(scales) #diagnostics table

# install.packages("jsonlite")
# install.packages("ggplot2")
# install.packages("webshot2") #to save gt table as an image

# To install EvidenceSynthesis
# system("sudo apt install cmake")
# devtools::install_github("ohdsi/EvidenceSynthesis")

# usethis::edit_r_environ()
# Sys.setenv(DATABASECONNECTOR_JAR_FOLDER="/home/rstudio/jdbcDrivers") #https://ohdsi.github.io/DatabaseConnector/articles/Connecting.html
Sys.setenv(GLP_output_folder="./results/output")
Sys.setenv(GLP_temp_folder="./results/temp")
resultFolder <- Sys.getenv("GLP_output_folder")
tempFolder <- Sys.getenv("GLP_temp_folder")

journalTheme <- "jama"

source("./extras/HelpersForReporting.R")

# Study setting    #####check !
tarPrime <- 0 # The Time-at-risk of primary analysis is defined as 60.
equipoiseBounds <- c(0.3,0.7)


colorTarget <- ggsci::pal_jama("default")(7)[2] #2nd color among 7 JAMA colors
colorComparator <- ggsci::pal_jama("default")(7)[1] # 1st color among 7 JAMA colors

targetColorR <- col2rgb(colorTarget)[1]/255#255/255
targetColorG <- col2rgb(colorTarget)[2]/255#99/255
targetColorB <- col2rgb(colorTarget)[3]/255#71/255

comparatorColorR <- col2rgb(colorComparator)[1]/255#30/255
comparatorColorG <- col2rgb(colorComparator)[2]/255#144/255
comparatorColorB <- col2rgb(colorComparator)[3]/255#255/255

# OHDSI shinydb read-only credentials
# DatabaseConnector::downloadJdbcDrivers("postgresql")
connectionDetails <- DatabaseConnector::createConnectionDetails(
  dbms = "postgresql",
  server = Sys.getenv("glp1_dili_server"),
  port = Sys.getenv("glp1_dili_port"),
  user = Sys.getenv("glp1_dili_user"),
  password = Sys.getenv("glp1_dili_password"),
  pathToDriver = Sys.getenv("DATABASECONNECTOR_JAR_FOLDER")
)
connection <- DatabaseConnector::connect(connectionDetails)

targetDatabase <- "shinydb"
resultsSchema <- "apac_glp1ra"

sql <- "SELECT tablename AS table_name FROM pg_catalog.pg_tables WHERE schemaname = '@results_schema'"
sql <- SqlRender::render(sql,results_schema = resultsSchema)
sql <- SqlRender::translate(sql, targetDialect = connection@dbms)
resultTables<- DatabaseConnector::querySql(connection, sql)
colnames(resultTables) <- colnames(resultTables) %>%
  SqlRender::snakeCaseToCamelCase() #snake to camel
resultTableNames <- resultTables %>% pull(tableName)


#Filter only cm tables
cmTableNames <- resultTables %>%
  filter(grepl("^cm",tableName)) %>%
  pull(tableName)

#Filter only es tables
esTableNames <- resultTables %>%
  filter(grepl("^es",tableName)) %>%
  pull(tableName)

####Tidy TCO, Analysis, DB list####

# Cohort Definition
cohortDefinition <- ohdsiPullTable(connection = connection,
                                 resultsSchema = resultsSchema, ###
                                 targetTable = "cg_cohort_definition", #"cd_cohort"
                                 limit = 0)

db <- ohdsiPullTable(connection = connection,
                   resultsSchema = resultsSchema, ###
                   targetTable = "database_meta_data", #"cd_cohort"
                   limit = 0)

cohortTidy <- cohortDefinition %>%
  select(cohortDefinitionId, cohortName)

cohortTidy$cohortNameAbbreviation <- c("GLP1RA", "DPP4i", "ALI") ###

# TCO
# returnCamelDf (targetTable = "cm_target_comparator_outcome",
#                andromedaObject = cmResultSuite)
targetComparatorOutcome <- ohdsiPullTable(connection = connection,
                                        resultsSchema = resultsSchema, ###
                                        targetTable = "cm_target_comparator_outcome", #"cd_cohort"
                                        limit = 0)
tco <- targetComparatorOutcome %>%
  filter (outcomeOfInterest == 1) %>%
  select (targetId, comparatorId, outcomeId)

tco$isPrimaryTco <- ifelse(tco$outcomeId == 16968, 1, 0) ###

tcoTidy <- tco %>% inner_join(cohortTidy,
                              by = c("targetId" = "cohortDefinitionId")) %>%
  rename(targetName = cohortName, targetAbbreviation = cohortNameAbbreviation) %>%
  inner_join(cohortTidy,
             by = c("comparatorId" = "cohortDefinitionId")) %>%
  rename(comparatorName = cohortName, comparatorAbbreviation = cohortNameAbbreviation) %>%
  inner_join(cohortTidy,
             by = c("outcomeId" = "cohortDefinitionId")) %>%
  rename(outcomeName = cohortName, outcomeAbbreviation = cohortNameAbbreviation)

#Assign colors to target and comparator ###
tcoTidy$targetColorR <- targetColorR
tcoTidy$targetColorG <- targetColorG
tcoTidy$targetColorB <- targetColorB
tcoTidy$comparatorColorR <- comparatorColorR 
tcoTidy$comparatorColorG <- comparatorColorG 
tcoTidy$comparatorColorB <- comparatorColorB

# DB list
dbs <- ohdsiPullTable(connection = connection,
                    resultsSchema = resultsSchema, ###
                    targetTable = "database_meta_data",
                    limit = 0)
dbTidy <- dbs %>% select(cdmSourceName, cdmSourceAbbreviation, databaseId)

#check!!!
dbTidy$cdmSourceAbbreviation <- ""

##Add abbreviated name  #check!!
dbTidy$cdmSourceAbbreviation[grep("Japan Medical Data Center",dbTidy$cdmSourceName)] <- "JMDC(JP)"
dbTidy$cdmSourceAbbreviation[grep("Optum.*Clinformatics.*Extended Data Mart.*DOD",dbTidy$cdmSourceName)] <- "Clinformatics(US)"
dbTidy$cdmSourceAbbreviation[grep("Optum EHR",dbTidy$cdmSourceName)] <- "Optum EHR(US)"
dbTidy$cdmSourceAbbreviation[grep("PharMetrics",dbTidy$cdmSourceName)] <- "PharMetrics(US)"
dbTidy$cdmSourceAbbreviation[grep("Veterans Affairs",dbTidy$cdmSourceName)] <- "VA(US)"
dbTidy$cdmSourceAbbreviation[grep("CDM",dbTidy$cdmSourceName)] <- "YUHS(KR)"
dbTidy$cdmSourceAbbreviation[grep("LPD.*Australia",dbTidy$cdmSourceName)] <- "LPD Australia(AU)"
dbTidy$cdmSourceAbbreviation[grep("Merative.* Medicaid Database",dbTidy$cdmSourceName)] <- "MDCD(US)"
dbTidy$cdmSourceAbbreviation[grep("Merative.* Benefits Database",dbTidy$cdmSourceName)] <- "MDCR(US)"
dbTidy$cdmSourceAbbreviation[grep("Health Verity Comprehensive Claims - Closed Claims Enrollment",dbTidy$cdmSourceName)] <- "HVCC(US)"
dbTidy$cdmSourceAbbreviation[grep("Merative.* Encounters Database",dbTidy$cdmSourceName)] <- "CCAE(US)"
dbTidy$cdmSourceAbbreviation[grep("LRx/Dx, US9-LAAD",dbTidy$cdmSourceName)] <- "LAAD(US)"
dbTidy$cdmSourceAbbreviation[grep("France Disease Analyzer",dbTidy$cdmSourceName)] <- "FDA(FR)"
dbTidy$cdmSourceAbbreviation[grep("Taipei Medical University",dbTidy$cdmSourceName)] <- "TMUCRD(TW)"

country <- stringr::str_extract(dbTidy$cdmSourceAbbreviation, "\\([A-Z][A-Z]\\)") %>% stringr::str_remove_all("\\(|\\)")
country <- factor(country, c("US","TW", "KR","JP", "AU", "DE","BE","IT", "FR"))
dbTidy$country <- country
dbTidy<- dbTidy %>%
  group_by(country) %>%
  arrange(cdmSourceAbbreviation, .by_group = TRUE)
dbTidy$dbOrder <- 1:nrow(dbTidy)

# Find Primary analysis
analysis <- ohdsiPullTable(connection = connection,
                         resultsSchema = resultsSchema,###
                         targetTable = "cm_analysis",
                         limit = 0)

analysisTidy <- analysis
analysisTidy$isPrimaryAnalysis <- 0

for(i in seq(nrow(analysisTidy))){
  a <- jsonlite::fromJSON(analysisTidy$definition[i])
  if(a$createStudyPopArgs$riskWindowEnd==tarPrime) analysisTidy$isPrimaryAnalysis[i] <- 1
}
analysisTidy <- analysisTidy %>%
  select(analysisId, description, isPrimaryAnalysis)
analysisTidy <- analysisTidy %>% rename(descriptionAnalysis = description)

### check!!
analysisIdPrime = analysisTidy$analysisId[analysisTidy$isPrimaryAnalysis==1]#analysisIdPrime = c(2)

#List of result#
# Results of Diagnostics
diagnosticsSummary <- ohdsiPullTable(connection = connection,
                                   resultsSchema = resultsSchema,
                                   targetTable = "cm_diagnostics_summary",
                                   limit = 0)

resultList <- diagnosticsSummary %>%
  left_join(dbTidy,
            by = "databaseId") %>%
  left_join(analysisTidy,
            by = "analysisId") %>%
  left_join(tcoTidy,
            by = c("targetId", "comparatorId", "outcomeId")
  )


####Study Protocol####
####For cohort definitions (json/sql)####
# cohortDefinition <- ohdsiPullTable(connection = connection,
#                                  resultsSchema = resultsSchema,###
#                                  targetTable = "cg_cohort_definition", #"cd_cohort"
#                                  limit = 0)

####Databases#### 
# dbs <- ohdsiPullTable(connection = connection,
#                     resultsSchema = resultsSchema,###
#                     targetTable = "cd_database",
#                     limit = 0)
# 
# dbMeta <- ohdsiPullTable(connection = connection,
#                        resultsSchema = resultsSchema,###
#                        targetTable = "cd_metadata",
#                        limit = 0)
# 
# databaseMetaData <- ohdsiPullTable(connection = connection,
#                                  resultsSchema = resultsSchema,###
#                                  targetTable = "database_meta_data",
#                                  limit = 0)
# 
# # Transform the meta data (column to rows)
# dbMetaTr <- dbMeta %>%
#   select(-startTime) %>% # Remove the startTime column
#   pivot_wider(names_from = variableField, values_from = valueField)# Pivot the table
# #%>% group_by(databaseId) %>% summarise(across(everything(), ~first(na.omit(.))), .groups = 'drop') # Corrected summarise call
# write.csv(dbMetaTr, file.path(resultFolder, "db_metadata_tr.csv"))
# write.csv(databaseMetaData, file.path(resultFolder, "db_metadata.csv"))
# 
# ####Negative control outcomes####
# # List of negative control outcomes
# analysisSpec <- jsonlite::fromJSON("./inst/analysisSpecification.json")
# negativeConOutcomes <- analysisSpec$sharedResources$negativeControlOutcomes$negativeControlOutcomeCohortSet[[2]][,c("outcomeConceptId", "cohortName")]
# 
# if(!file.exists(file.path(resultFolder,"analysis"))) dir.create(file.path(resultFolder,"analysis"))
# write.csv(negativeConOutcomes, file.path(resultFolder, "analysis", "negative_control_list.csv"))
# 
# result <- ohdsiPullTable(connection = connection,
#                        resultsSchema = resultsSchema,
#                        targetTable = "cm_result",
#                        limit = 0)

# controlResults <- result %>%
#   filter(targetId %in% tcoTidy$targetId) |> 
#   filter(comparatorId %in% tcoTidy$comparatorId) |> 
#   filter(analysisId %in% analysisIdPrime) 
#   filter(!is.na(p)) %>% #remove NA values
#   filter(outcomeId %in% 1001:1050) #negative control outcome Ids starts from 1001 to 2050
# 
# compTemp <- tcoTidy %>% select(comparatorId, comparatorAbbreviation) %>% unique()
# 
# controlResultsPerDb <- controlResults %>%
#   group_by(targetId, comparatorId, databaseId) %>% summarise(n=n()) %>%
#   merge(dbTidy) %>%
#   merge(compTemp)
# write.csv(controlResultsPerDb, file.path(resultFolder, "systematic_error", "control_results_per_db.csv"))
# 
# ####PS Model####
# # psModel <- ohdsiPullTable(connection = connection,
# #                         resultsSchema = resultsSchema,###
# #                         targetTable = "cm_propensity_model",
# #                         limit = 0)
# 
# 
####Attrition Summary and Inicdence rate####
####Attrition Summary and Inicdence rate####
cmAttrition <- ohdsiPullTable(connection = connection,
                              resultsSchema = resultsSchema,###
                              targetTable = "cm_attrition",
                              limit = 0)
cmResult <- ohdsiPullTable(connection = connection,
                           resultsSchema = resultsSchema,###
                           targetTable = "cm_result", #"cd_cohort"
                           limit = 0)
# tcoTidy
# check (analysisPrime)
attritionPrime <- cmAttrition %>%
  left_join(dbTidy, by = "databaseId") %>%
  filter(analysisId == analysisIdPrime) %>%
  filter(outcomeId == 16968)


## GLP1RA vs DPP4i
# Original cohorts (GLP1RA)
glp_glpdpp_orgin <- attritionPrime %>%
  filter (comparatorId == 19019) %>%
  filter (exposureId == 19018) %>%
  filter (sequenceNumber == 1) %>%
  arrange(dbOrder) %>%
  select(subjects, dbOrder) %>%
  rename(glp_glpdpp_orgin = subjects)

# Original cohorts (DPP4i)
dpp_glpdpp_orgin <- attritionPrime %>%
  filter (comparatorId == 19019) %>%
  filter (exposureId == 19019) %>%
  filter (sequenceNumber == 1) %>%
  arrange(dbOrder) %>%
  select(subjects, dbOrder) %>%
  rename(dpp_glpdpp_orgin = subjects)

# Matched cohorts (GLP1RA)
glp_glpdpp_matched <- attritionPrime %>%
  filter (comparatorId == 19019) %>%
  filter (exposureId == 19018) %>%
  filter (sequenceNumber == 6) %>%
  left_join(cmResult, by = c("analysisId", "targetId", "comparatorId", "outcomeId", "databaseId")) %>%
  arrange(dbOrder) %>%
  select(subjects, dbOrder, targetSubjects, targetDays, targetOutcomes) %>%
  mutate(glp_glpdpp_matched_incidence = targetOutcomes/targetDays) %>%
  rename(glp_glpdpp_matched_subjects = subjects, glp_glpdpp_matched_subjects_cm = targetSubjects, glp_glpdpp_matched_days = targetDays, glp_glpdpp_matched_outcome = targetOutcomes)


# Matched cohorts (DPP4i)
dpp_glpdpp_matched <- attritionPrime %>%
  filter (comparatorId == 19019) %>%
  filter (exposureId == 19019) %>%
  filter (sequenceNumber == 6) %>%
  left_join(cmResult, by = c("analysisId", "targetId", "comparatorId", "outcomeId", "databaseId")) %>%
  arrange(dbOrder) %>%
  select(subjects, dbOrder, comparatorSubjects, comparatorDays, comparatorOutcomes) %>%
  mutate(dpp_glpdpp_matched_incidence = comparatorOutcomes/comparatorDays) %>%
  rename(dpp_glpdpp_matched = subjects, dpp_glpdpp_matched_subjects_cm = comparatorSubjects, dpp_glpdpp_matched_days = comparatorDays, dpp_glpdpp_matched_outcome = comparatorOutcomes)

# ## FQ vs CPH
# # Original cohorts (FQ)
# fq_fqcph_orgin <- attritionPrime %>%
#   filter (comparatorId == 1782487001) %>%
#   filter (exposureId == 1782488001) %>%
#   filter (sequenceNumber == 1) %>%
#   arrange(dbOrder) %>%
#   select(subjects, dbOrder) %>%
#   rename(fq_fqcph_orgin = subjects)
# 
# # Original cohorts (CPH)
# cph_fqcph_orgin <- attritionPrime %>%
#   filter (comparatorId == 1782487001) %>%
#   filter (exposureId == 1782487001) %>%
#   filter (sequenceNumber == 1) %>%
#   arrange(dbOrder) %>%
#   select(subjects, dbOrder) %>%
#   rename(cph_fqcph_orgin = subjects)
# 
# # Matched cohorts (FQ)
# fq_fqcph_matched <- attritionPrime %>%
#   filter (comparatorId == 1782487001) %>%
#   filter (exposureId == 1782488001) %>%
#   filter (sequenceNumber == 6) %>%
#   left_join(cmResult, by = c("analysisId", "targetId", "comparatorId", "outcomeId", "databaseId")) %>%
#   arrange(dbOrder) %>%
#   select(subjects, dbOrder, targetSubjects, targetDays, targetOutcomes) %>%
#   mutate(fq_fqcph_matched_incidence = targetOutcomes/targetDays) %>%
#   rename(fq_fqcph_matched_subjects = subjects, fq_fqcph_matched_subjects_cm = targetSubjects, fq_fqcph_matched_days = targetDays, fq_fqcph_matched_outcome = targetOutcomes)
# 
# # Matched cohorts (CPH)
# cph_fqcph_matched <- attritionPrime %>%
#   filter (comparatorId == 1782487001) %>%
#   filter (exposureId == 1782487001) %>%
#   filter (sequenceNumber == 6) %>%
#   left_join(cmResult, by = c("analysisId", "targetId", "comparatorId", "outcomeId", "databaseId")) %>%
#   arrange(dbOrder) %>%
#   select(subjects, dbOrder, comparatorSubjects, comparatorDays, comparatorOutcomes) %>%
#   mutate(cph_fqcph_matched_incidence = comparatorOutcomes/comparatorDays) %>%
#   rename(cph_fqcph_matched = subjects, cph_fqcph_matched_subjects_cm = comparatorSubjects, cph_fqcph_matched_days = comparatorDays, cph_fqcph_matched_outcome = comparatorOutcomes)

dbTemp <- dbTidy %>%
  select(cdmSourceAbbreviation, dbOrder)
attritionIncidenceSummary <- dbTemp %>%
  left_join(glp_glpdpp_orgin, by = "dbOrder")%>%
  left_join(glp_glpdpp_matched, by = "dbOrder")%>%
  left_join(dpp_glpdpp_orgin, by = "dbOrder")%>%
  left_join(dpp_glpdpp_matched, by = "dbOrder")

###negative values and incidence calculation are needed

write.csv(attritionIncidenceSummary, file.path(resultFolder, "attrition_incidence_summary.csv"))


####Balance table and plot####
# sharedCovariateBalance <- ohdsiPullTable(connection = connection,
#                                        resultsSchema = resultsSchema,###
#                                        targetTable = "cm_shared_covariate_balance",
#                                        limit = 0)
covariateBalance <- ohdsiPullTable(connection = connection,
                                 resultsSchema = resultsSchema,###
                                 targetTable = "cm_covariate_balance",
                                 limit = 0)
covariate <- ohdsiPullTable(connection = connection,
                          resultsSchema = resultsSchema,###
                          targetTable = "cm_covariate",
                          limit = 0)

# returnCamelDf (targetTable = "cm_shared_covariate_balance",
#                andromedaObject = cmResultSuite)
# returnCamelDf (targetTable = "cm_covariate_balance",
#                andromedaObject = cmResultSuite)

#Filter results for balance
resultListForBalance <- resultList %>%
  filter(isPrimaryAnalysis == 1) %>%
  filter(isPrimaryTco == 1)

#Balance table
for(i in seq(nrow(resultListForBalance))){
  if(!resultListForBalance$isPrimaryAnalysis[i]) next #Only plot PS distribution for primary analysis #redundant#
  
  databaseId <- resultListForBalance[i,]$databaseId
  targetId <- resultListForBalance[i,]$targetId
  comparatorId <- resultListForBalance[i,]$comparatorId
  analysisId <- resultListForBalance[i,]$analysisId
  outcomeId = resultListForBalance$outcomeId[i]
  comparatorColorR <- resultListForBalance[i,]$comparatorColorR
  comparatorColorG <- resultListForBalance[i,]$comparatorColorG
  comparatorColorB <- resultListForBalance[i,]$comparatorColorB
  targetName <- resultListForBalance[i,]$targetAbbreviation
  comparatorName <- resultListForBalance[i,]$comparatorAbbreviation
  cdmSourceAbbreviation <- resultListForBalance[i,]$cdmSourceAbbreviation
  
  balanceTemp <- covariateBalance %>% filter(databaseId == !!databaseId,
                                             targetId == !!targetId,
                                             comparatorId == !!comparatorId,
                                             outcomeId == !!outcomeId,
                                             analysisId == !!analysisId)
  covariateTemp <- covariate %>%
    filter(databaseId == !!databaseId,
           analysisId == !!analysisId) %>%
    select(covariateId, covariateAnalysisId, covariateName)
  balance <- merge(balanceTemp, covariateTemp)
  print(nrow(balance))
}


for(i in seq(nrow(resultListForBalance))){
  if(!resultListForBalance$isPrimaryAnalysis[i]) next #Only plot PS distribution for primary analysis #redundant#
  
  databaseId <- resultListForBalance[i,]$databaseId
  targetId <- resultListForBalance[i,]$targetId
  comparatorId <- resultListForBalance[i,]$comparatorId
  analysisId <- resultListForBalance[i,]$analysisId
  outcomeId = resultListForBalance$outcomeId[i]
  comparatorColorR <- resultListForBalance[i,]$comparatorColorR
  comparatorColorG <- resultListForBalance[i,]$comparatorColorG
  comparatorColorB <- resultListForBalance[i,]$comparatorColorB
  targetName <- resultListForBalance[i,]$targetAbbreviation
  comparatorName <- resultListForBalance[i,]$comparatorAbbreviation
  cdmSourceAbbreviation <- resultListForBalance[i,]$cdmSourceAbbreviation
  
  balanceTemp <- covariateBalance %>% filter(databaseId == !!databaseId,
                                             targetId == !!targetId,
                                             comparatorId == !!comparatorId,
                                             outcomeId == !!outcomeId,
                                             analysisId == !!analysisId)
  covariateTemp <- covariate %>%
    filter(databaseId == !!databaseId,
           analysisId == !!analysisId) %>%
    select(covariateId, covariateAnalysisId, covariateName)
  balance <- merge(balanceTemp, covariateTemp)
  rm(balanceTemp, covariateTemp)
  
  if(nrow(balance)==0) next
  #change column name
  balance$analysisId <- NULL
  balance <- rename(balance, analysisId = covariateAnalysisId)
  
  #Table 1
  Table1 <- prepareTable1(balance,
                          beforeLabel = "Before matching",
                          afterLabel = "After matching",
                          targetLabel = targetName,
                          comparatorLabel = comparatorName,
                          percentDigits = 1,
                          stdDiffDigits = 2,
                          output = "latex",
                          pathToCsv = file.path("inst/","Table1Specs.csv")
                          )
  Table1[,1]<-as.character(Table1[,1])
  Table1[,1]<-gsub(" //(Unknown unit//)","",Table1[,1])

  # Table1<-labFormmating(Table1,percentDigits=1)
  if(!file.exists(file.path(resultFolder,"Table1"))) dir.create(file.path(resultFolder,"Table1"))

  write.csv(Table1,file.path(resultFolder, "Table1",sprintf("Table1_%s_t%s_c%s_o%s_a%s.csv",
                                                            cdmSourceAbbreviation,
                                                            targetName,
                                                            comparatorName,
                                                            outcomeId,
                                                            analysisId
  )))
  
  
  ## Balance plot
  balance$absStdDiffBefore <- abs(balance$stdDiffBefore)
  balance$absStdDiffAfter <- abs(balance$stdDiffAfter)
  
  balancePlot <- plotCovariateBalanceScatterPlot(balance,
                                                 beforeLabel = "Before matching",
                                                 afterLabel = "After matching",
                                                 dotColor = rgb(comparatorColorR, comparatorColorG, comparatorColorB, alpha = 0.3),
                                                 limits = c(0,0.4))
  
  balancePlot<-balancePlot+ annotate("label", label = sprintf(" Number of covariates: %s\nAfter matching max(absolute):%s",
                                                              format(nrow(balance), big.mark=",", scientific=FALSE),
                                                              format(round(max(balance$absStdDiffAfter),2),nsmall=2)),
                                     x = -Inf, y = Inf, hjust=-0.1,vjust=1.2, color = "black");balancePlot
  balancePlot <- balancePlot + ggtitle(sprintf("%s vs %s in %s", targetName, comparatorName, cdmSourceAbbreviation))#sprintf("%s vs %s in %s", targetName, comparatorName, dbName)
  balancePlot <- balancePlot + geom_hline(yintercept=0.1, linetype="dashed", color = "red")
  
  # assign(paste0("balancePlot","_",gsub("-","_",gsub(" ","",databaseId))), balancePlot, envir = .GlobalEnv)
  if(!file.exists(file.path(resultFolder,"balance_scatter_plot"))) dir.create(file.path(resultFolder,"balance_scatter_plot"))
  ggplot2::ggsave(file.path(resultFolder,"balance_scatter_plot",sprintf("balance_scatter_plot_%s_t%s_c%s_o%d_a%d.png",
                                                                        cdmSourceAbbreviation,
                                                                        targetName,
                                                                        comparatorName,
                                                                        outcomeId,
                                                                        analysisId)),
                  balancePlot, device = "png", width = 10, height = 10, units = "cm", dpi = 200)
  # ggplot2::ggsave(file.path(resultFolder,"balance_scatter_plot",sprintf("balance_scatter_plot_%s_t%s_c%s_o%d_a%d.eps",
  #                                                                       dbName,
  #                                                                       targetName,
  #                                                                       comparatorName,
  #                                                                       outcomeId,
  #                                                                       analysisId)),
  #                 balancePlot, device = "eps", width = 10, height = 10, units = "cm", dpi = 320)
  # ggplot2::ggsave(file.path(resultFolder,"balance_scatter_plot",sprintf("balance_scatter_plot_%s_t%s_c%s_o%d_a%d.pdf",
  #                                                                       dbName,
  #                                                                       targetName,
  #                                                                       comparatorName,
  #                                                                       outcomeId,
  #                                                                       analysisId)),
  #                 balancePlot, device = "pdf", width = 10, height = 10, units = "cm", dpi = 320)
  # ggplot2::ggsave(file.path(resultFolder,"balance_scatter_plot",sprintf("balance_scatter_plot_%s_t%s_c%s_o%d_a%d.tiff",
  #                                                                       dbName,
  #                                                                       targetName,
  #                                                                       comparatorName,
  #                                                                       outcomeId,
  #                                                                       analysisId)),
  #                 balancePlot, device = "tiff", width = 10, height = 10, units = "cm", dpi = 320)
  
  
  rm(balance)
}

# #Identify number of covariates in each analysis
# sharedCovariateBal <- ohdsiPullTable(connection = connection,
#                                    resultsSchema = resultsSchema,###
#                                    targetTable = "cm_shared_covariate_balance",
#                                    limit = 0)
# 
# sharedBalanceTable <- data.frame()
# for(i in seq(nrow(resultListForBalance))){
#   databaseId <- resultListForBalance[i,]$databaseId
#   targetId <- resultListForBalance[i,]$targetId
#   comparatorId <- resultListForBalance[i,]$comparatorId
#   analysisId <- resultListForBalance[i,]$analysisId
#   targetName <- resultListForBalance[i,]$targetAbbreviation
#   comparatorName <- resultListForBalance[i,]$comparatorAbbreviation
#   cdmSourceAbbreviation <- resultListForBalance[i,]$cdmSourceAbbreviation
#   
#   sharedBalanceTemp <- sharedCovariateBal %>% filter(databaseId == !!databaseId,
#                                                      targetId == !!targetId,
#                                                      comparatorId == !!comparatorId,
#                                                      analysisId == !!analysisId)
#   
#   sharedBalanceTableTemp <- data.frame(cdmSourceAbbreviation = cdmSourceAbbreviation,
#                                        targetName = targetName,
#                                        comparatorName = comparatorName,
#                                        covariateNumber = nrow(sharedBalanceTemp),
#                                        maxAbsStdDiffBefore = max(abs(sharedBalanceTemp$stdDiffBefore)),
#                                        maxAbsStdDiffAfter = max(abs(sharedBalanceTemp$stdDiffAfter))
#   )
#   
#   sharedBalanceTable <- rbind(sharedBalanceTable, sharedBalanceTableTemp)
# }
# write.csv(sharedBalanceTable, file.path(resultFolder, "shared_balance_table.csv"))

#####

####Distribution of Preference score####
preferenceScoreDist <- ohdsiPullTable(connection = connection,
                                    resultsSchema = resultsSchema,###
                                    targetTable = "cm_preference_score_dist",
                                    limit = 0)

tcdList <- resultList %>% select(analysisId, targetId, comparatorId, databaseId,
                                 equipoise, equipoiseDiagnostic, # unblind,
                                 cdmSourceName, cdmSourceAbbreviation,
                                 descriptionAnalysis, isPrimaryAnalysis,
                                 targetName, targetAbbreviation,
                                 comparatorName, comparatorAbbreviation,
                                 targetColorR, targetColorG, targetColorB,
                                 comparatorColorR, comparatorColorG, comparatorColorB
) %>%
  unique()
tcdList <- tcdList[!is.na(tcdList$comparatorColorB),]
for(i in seq(nrow(tcdList))){
  if(!tcdList$isPrimaryAnalysis[i]) next #Only plot PS distribution for primary analysis
  
  databaseId <- tcdList[i,]$databaseId
  targetId <- tcdList[i,]$targetId
  comparatorId <- tcdList[i,]$comparatorId
  analysisId <- tcdList[i,]$analysisId
  targetColorR <- tcdList[i,]$targetColorR
  targetColorG <- tcdList[i,]$targetColorG
  targetColorB <- tcdList[i,]$targetColorB
  comparatorColorR <- tcdList[i,]$comparatorColorR
  comparatorColorG <- tcdList[i,]$comparatorColorG
  comparatorColorB <- tcdList[i,]$comparatorColorB
  
  ps = preferenceScoreDist %>% filter(databaseId == !!databaseId,
                                      targetId == !!targetId,
                                      comparatorId == !!comparatorId,
                                      analysisId == !!analysisId)
  targetName <- tcdList[i,]$targetAbbreviation
  comparatorName <- tcdList[i,]$comparatorAbbreviation
  cdmSourceAbbreviation <- tcdList[i,]$cdmSourceAbbreviation
  print(i)
  print(cdmSourceAbbreviation)
  print(tcdList[i,]$cdmSourceName)
  if(nrow(ps)==0) next
  
  if(!file.exists(file.path(resultFolder,"ps"))) dir.create(file.path(resultFolder,"ps"))
  psPlot <- plotPs(ps,
                   targetName= targetName,
                   comparatorName = comparatorName,
                   showEquiposeLabel = TRUE,
                   equipoiseBounds = equipoiseBounds,
                   targetColorR = targetColorR,
                   targetColorG = targetColorG,
                   targetColorB = targetColorB,
                   comparatorColorR = comparatorColorR,
                   comparatorColorG = comparatorColorG,
                   comparatorColorB = comparatorColorB
  )
  psPlot <- psPlot + ggtitle(sprintf("%s vs %s in %s", targetName, comparatorName, cdmSourceAbbreviation))#sprintf("%s vs %s in %s", targetName, comparatorName, dbName)
  ggplot2::ggsave(file.path(resultFolder,"ps",
                            sprintf("%s_ps_t_%s_c_%s_a_%s_%s.png",i, targetName, comparatorName, analysisId, cdmSourceAbbreviation)),
                  #sprintf("ps_t_%s_c_%s_a_%s_%s.tiff",targetName, comparatorName, analysisId, dbName)),
                  psPlot,
                  width = 5, height = 3.8, #height was increased due to title
                  dpi = 200 #400
  )
}
#####

####Comparative PS overlap plot grid####
# preferenceScoreDist <- ohdsiPullTable(connection = connection,
#                                     resultsSchema = resultsSchema,###
#                                     targetTable = "cm_preference_score_dist",
#                                     limit = 0)
# 
# tcdList <- resultList %>% select(analysisId, targetId, comparatorId, databaseId,
#                                  equipoise, equipoiseDiagnostic, # unblind,
#                                  cdmSourceName, cdmSourceAbbreviation,
#                                  descriptionAnalysis, isPrimaryAnalysis,
#                                  targetName, targetAbbreviation,
#                                  comparatorName, comparatorAbbreviation,
#                                  # targetColorR, targetColorG, targetColorB,
#                                  comparatorColorR, comparatorColorG, comparatorColorB) %>%
#   unique()
# 
# ####plot PS for Grid####
# plotPsForGrid <- function(ps,
#                           targetName,
#                           comparatorName,
#                           targetColor,
#                           comparatorColor,
#                           showEquiposeLabel = TRUE,
#                           equipoiseBounds = c(0.3,0.7),
#                           fileName = NULL,
#                           targetColorR = 0.8,
#                           targetColorG = 0,
#                           targetColorB = 0,
#                           comparatorColorR = 0,
#                           comparatorColorG = 0,
#                           comparatorColorB = 0.8) {
#   psOrigin <- ps
#   ps <- rbind(data.frame(x = ps$preferenceScore, y = ps$targetDensity, group = targetName),
#               data.frame(x = ps$preferenceScore, y = ps$comparatorDensity, group = comparatorName))
#   ps$group <- factor(ps$group, levels = c(as.character(targetName), as.character(comparatorName)))
#   theme <- ggplot2::element_text(colour = "#000000", size = 12, margin = ggplot2::margin(0, 0.5, 0, 0.1, "cm"))
#   plot <- ggplot2::ggplot(ps,
#                           ggplot2::aes(x = x, y = y, color = group, group = group, fill = group)) +
#     ggplot2::geom_density(stat = "identity") +
#     # ggplot2::scale_fill_manual(values = c(rgb(0.8, 0, 0, alpha = 0.5),
#     #                                       rgb(0, 0, 0.8, alpha = 0.5))) +
#     # ggplot2::scale_color_manual(values = c(rgb(0.8, 0, 0, alpha = 0.5),
#     #                                        rgb(0, 0, 0.8, alpha = 0.5))) +
#     ggplot2::scale_fill_manual(values = c(rgb(targetColorR, targetColorG, targetColorB, alpha = 0.5),
#                                           rgb(comparatorColorR, comparatorColorG, comparatorColorB, alpha = 0.5))) +
#     ggplot2::scale_color_manual(values = c(rgb(targetColorR, targetColorG, targetColorB, alpha = 0.5),
#                                            rgb(comparatorColorR, comparatorColorG, comparatorColorB, alpha = 0.5))) +
#     # ggplot2::scale_fill_manual(values = c(rgb(targetColorR, targetColorG, targetColorB, alpha = 0.5),
#     #                                       rgb(comparatorColorR,alpha = 0.5))) +
#     # ggplot2::scale_color_manual(values = c(rgb(targetColorR, alpha = 0.5),
#     #                                        rgb(comparatorColorR, alpha = 0.5))) +
#     ggplot2::scale_x_continuous("Preference score", limits = c(0, 1)) +
#     ggplot2::scale_y_continuous("Density", breaks = seq(0, ceiling(max(ps$y)), by = 1)) + #Set the y-axis tick marks to appropriate intervals of integers.
#     ggplot2::theme(legend.title = ggplot2::element_blank(),
#                    panel.grid.major = ggplot2::element_blank(),
#                    panel.grid.minor = ggplot2::element_blank(),
#                    legend.position = "top",
#                    legend.text = theme,
#                    axis.text = theme,
#                    axis.title = theme)
#   if (showEquiposeLabel) {
#     labelsLeft <- c()
#     labelsRight <- c()
#     if (showEquiposeLabel) {
#       equiIndex <- psOrigin$preferenceScore>=equipoiseBounds[1] & psOrigin$preferenceScore<=equipoiseBounds[2]
#       equipoise <- mean (sum(psOrigin$targetDensity[equiIndex]), sum(psOrigin$comparatorDensity[equiIndex]))/100
#       labelsRight <- c(labelsRight, sprintf("%2.1f%% is in equipoise",
#                                             equipoise * 100))
#     }
#     if (length(labelsLeft) > 0) {
#       dummy <- data.frame(text = paste(labelsLeft, collapse = "/n"))
#       plot <- plot + ggplot2::geom_label(x = 0, #y = max(d$y) * 1.24,
#                                          hjust = "left", vjust = "top", alpha = 0.8,
#                                          ggplot2::aes(label = text), data = dummy, size = 3.5)
#     }
#     if (length(labelsRight) > 0) {
#       dummy <- data.frame(text = paste(labelsRight, collapse = "/n"))
#       plot <- plot + ggplot2::annotate("label", x = 1, y = max(ps$y) * 1,
#                                        hjust = "right", vjust = "top",
#                                        alpha = 0.8,
#                                        label = labelsRight,
#                                        #ggplot2::aes(label = labelsRight),
#                                        #ggplot2::aes(label = text), data = dummy,
#                                        size = 3.5)
#       # plot <- plot + ggplot2::geom_label(x = 1, y = max(ps$y) * 1.24,
#       #                                    hjust = "right", vjust = "top",
#       #                                    alpha = 0.8,
#       #                                    ggplot2::aes(label = labelsRight),
#       #                                    ggplot2::aes(label = text), data = dummy,
#       #                                    size = 3.5)
#     }
#   }
#   if (!is.null(fileName))
#     ggplot2::ggsave(fileName, plot, width = 5, height = 3.5,
#                     dpi = 400)
#   return(plot)
# }
# ######
# excludingDbs <- c("German DA(DE)", "LPD Begium(BE)", "LPD Italy(IT)")
# tcdListPrimary <- tcdList %>%
#   filter(isPrimaryAnalysis==1) %>%
#   filter(!cdmSourceAbbreviation %in% excludingDbs)
# dbOrder <- dbTidy %>% select(country, cdmSourceAbbreviation, dbOrder)
# tcdListPrimaryOrder <- tcdListPrimary %>%
#   left_join(dbOrder, by = c("cdmSourceAbbreviation"))
# 
# tcdListPrimaryOrder$dbOrder <- tcdListPrimaryOrder$dbOrder*2 - (tcdListPrimaryOrder$comparatorAbbreviation=="TMP")
# length(unique(tcdListPrimaryOrder$dbOrder)) == length(tcdListPrimaryOrder$dbOrder)
# 
# # psYMax <- 6
# #For US
# startNum = 1
# endNum = 14
# for(i in startNum:endNum){
#   
#   if (i==startNum) psPlotList <- list()
#   
#   databaseId <- tcdListPrimaryOrder%>% filter(dbOrder==i) %>% select(databaseId) %>% as.character()
#   targetId <- tcdListPrimaryOrder%>% filter(dbOrder==i) %>% select(targetId) %>% as.numeric()
#   comparatorId <- tcdListPrimaryOrder%>% filter(dbOrder==i) %>% select(comparatorId) %>% as.numeric()
#   analysisId <- tcdListPrimaryOrder%>% filter(dbOrder==i) %>% select(analysisId) %>% as.numeric()
#   comparatorColorR <- tcdListPrimaryOrder%>% filter(dbOrder==i) %>% select(comparatorColorR) %>% as.numeric()
#   comparatorColorG <- tcdListPrimaryOrder%>% filter(dbOrder==i) %>% select(comparatorColorG) %>% as.numeric()
#   comparatorColorB <- tcdListPrimaryOrder%>% filter(dbOrder==i) %>% select(comparatorColorB) %>% as.numeric()
#   
#   targetName <- tcdListPrimaryOrder%>% filter(dbOrder==i) %>% select(targetAbbreviation)%>% as.character()
#   comparatorName <- tcdListPrimaryOrder%>% filter(dbOrder==i) %>% select(comparatorAbbreviation)%>% as.character()
#   cdmSourceAbbreviation <- tcdListPrimaryOrder%>% filter(dbOrder==i) %>% select(cdmSourceAbbreviation)%>% as.character()
#   
#   ps = preferenceScoreDist %>% filter(databaseId == !!databaseId,
#                                       targetId == !!targetId,
#                                       comparatorId == !!comparatorId,
#                                       analysisId == !!analysisId)
#   
#   if(nrow(ps)==0) next
#   
#   # tcdPsDf <- rbind(tcdPsDf,tcdList[i,])
#   
#   # if(!file.exists(file.path(resultFolder,"ps"))) dir.create(file.path(resultFolder,"ps"))
#   psPlot <- plotPsForGrid(ps,
#                           targetName= targetName,
#                           comparatorName = comparatorName,
#                           showEquiposeLabel = TRUE,
#                           equipoiseBounds = equipoiseBounds,
#                           targetColorR = targetColorR,
#                           targetColorG = targetColorG,
#                           targetColorB = targetColorB,
#                           comparatorColorR = comparatorColorR,
#                           comparatorColorG = comparatorColorG,
#                           comparatorColorB = comparatorColorB
#   )
#   
#   psPlot <- psPlot + ggtitle(sprintf("%s vs %s in %s", targetName, comparatorName, cdmSourceAbbreviation))#sprintf("%s vs %s in %s", targetName, comparatorName, dbName)
#   psPlot <- psPlot + theme(legend.position="none") #remove legend
#   
#   if (!(i %in% c(endNum-1,endNum))){
#     psPlot <- psPlot + theme(axis.title.x=element_blank()) #remove x-axis title
#   }
#   
#   #remove y axis
#   if( (i %% 2) ==0){
#     psPlot <- psPlot + theme(axis.title.y=element_blank(),
#                              # axis.ticks.y=element_blank(),
#                              # axis.text.y=element_blank()
#     )
#   }
#   # psPlot <- psPlot + ylim(0, psYMax) # Ensure uniform y scale
#   
#   psPlotList[[length(psPlotList) + 1]] <- psPlot
# }
# # Arrange plots into a grid
# psPlotsGrid <- do.call(grid.arrange, c(psPlotList, ncol = 2, nrow = 7))
# ggsave(file.path(resultFolder,"ps","combined_plot_US.pdf"), psPlotsGrid, width = 8, height = 14)
# 
# #For non-US
# startNum = 15
# endNum = 28
# for(i in startNum:endNum){
#   
#   if (i==startNum) psPlotList <- list()
#   
#   databaseId <- tcdListPrimaryOrder%>% filter(dbOrder==i) %>% select(databaseId) %>% as.character()
#   targetId <- tcdListPrimaryOrder%>% filter(dbOrder==i) %>% select(targetId) %>% as.numeric()
#   comparatorId <- tcdListPrimaryOrder%>% filter(dbOrder==i) %>% select(comparatorId) %>% as.numeric()
#   analysisId <- tcdListPrimaryOrder%>% filter(dbOrder==i) %>% select(analysisId) %>% as.numeric()
#   comparatorColorR <- tcdListPrimaryOrder%>% filter(dbOrder==i) %>% select(comparatorColorR) %>% as.numeric()
#   comparatorColorG <- tcdListPrimaryOrder%>% filter(dbOrder==i) %>% select(comparatorColorG) %>% as.numeric()
#   comparatorColorB <- tcdListPrimaryOrder%>% filter(dbOrder==i) %>% select(comparatorColorB) %>% as.numeric()
#   
#   targetName <- tcdListPrimaryOrder%>% filter(dbOrder==i) %>% select(targetAbbreviation)%>% as.character()
#   comparatorName <- tcdListPrimaryOrder%>% filter(dbOrder==i) %>% select(comparatorAbbreviation)%>% as.character()
#   cdmSourceAbbreviation <- tcdListPrimaryOrder%>% filter(dbOrder==i) %>% select(cdmSourceAbbreviation)%>% as.character()
#   
#   ps = preferenceScoreDist %>% filter(databaseId == !!databaseId,
#                                       targetId == !!targetId,
#                                       comparatorId == !!comparatorId,
#                                       analysisId == !!analysisId)
#   
#   if(nrow(ps)==0) next
#   
#   # tcdPsDf <- rbind(tcdPsDf,tcdList[i,])
#   
#   # if(!file.exists(file.path(resultFolder,"ps"))) dir.create(file.path(resultFolder,"ps"))
#   psPlot <- plotPsForGrid(ps,
#                           targetName= targetName,
#                           comparatorName = comparatorName,
#                           showEquiposeLabel = TRUE,
#                           equipoiseBounds = equipoiseBounds,
#                           targetColorR = targetColorR,
#                           targetColorG = targetColorG,
#                           targetColorB = targetColorB,
#                           comparatorColorR = comparatorColorR,
#                           comparatorColorG = comparatorColorG,
#                           comparatorColorB = comparatorColorB
#   )
#   
#   psPlot <- psPlot + ggtitle(sprintf("%s vs %s in %s", targetName, comparatorName, cdmSourceAbbreviation))#sprintf("%s vs %s in %s", targetName, comparatorName, dbName)
#   psPlot <- psPlot + theme(legend.position="none") #remove legend
#   
#   if (!(i %in% c(endNum-1,endNum))){
#     psPlot <- psPlot + theme(axis.title.x=element_blank()) #remove x-axis title
#   }
#   
#   #remove y axis
#   if( (i %% 2) ==0){
#     psPlot <- psPlot + theme(axis.title.y=element_blank(),
#                              # axis.ticks.y=element_blank(),
#                              # axis.text.y=element_blank()
#     )
#   }
#   # psPlot <- psPlot + ylim(0, psYMax) # Ensure uniform y scale
#   
#   psPlotList[[length(psPlotList) + 1]] <- psPlot
# }
# # Arrange plots into a grid
# psPlotsGrid <- do.call(grid.arrange, c(psPlotList, ncol = 2, nrow = 7))
# ggsave(file.path(resultFolder,"ps","combined_plot_nonUS.pdf"), psPlotsGrid, width = 8, height = 14)
# #####
# 
# 
# ####Survival analysis####
kaplanMeierDist <- ohdsiPullTable(connection = connection,
                                resultsSchema = resultsSchema,###
                                targetTable = "cm_kaplan_meier_dist",
                                limit = 0)
result <- ohdsiPullTable(connection = connection,
                       resultsSchema = resultsSchema,###
                       targetTable = "cm_result",
                       limit = 0)

#Filter resluts for survival plots
resultListForSurvival <- resultList %>%
  filter(isPrimaryAnalysis == 1) %>%
  filter(isPrimaryTco == 1) #%>% filter(unblind == 1) #only results passing the diagnostics

# yLimUpperBound =
#   1- kaplanMeierDist %>%
#   filter(analysisId %in% unique(resultListForSurvival$analysisId)) %>%
#   filter(outcomeId %in% unique(resultListForSurvival$outcomeId)) %>%
#   summarise(yLimUpperBound = min(targetSurvivalLb, comparatorSurvivalLb))
# #convert into percent (*100)
# yLimUpperBound = round(yLimUpperBound*100*1.3, digits = 2)

for (i in seq(nrow(resultListForSurvival))){
  
  analysisId = resultListForSurvival$analysisId[i]
  targetId = resultListForSurvival$targetId[i]
  comparatorId = resultListForSurvival$comparatorId[i]
  targetName = resultListForSurvival$targetAbbreviation[i]
  comparatorName = resultListForSurvival$comparatorAbbreviation[i]
  outcomeId = resultListForSurvival$outcomeId[i]
  outcomeName = resultListForSurvival$outcomeAbbreviation[i]
  databaseId <- resultListForSurvival$databaseId[i]
  databaseName <- resultListForSurvival$cdmSourceAbbreviation[i]
  
  # targetColorR <- resultListForSurvival$targetColorR[i]
  # targetColorG <- resultListForSurvival$targetColorG[i]
  # targetColorB <- resultListForSurvival$targetColorB[i]
  comparatorColorR <- resultListForSurvival$comparatorColorR[i]
  comparatorColorG <- resultListForSurvival$comparatorColorG[i]
  comparatorColorB <- resultListForSurvival$comparatorColorB[i]
  
  kaplanMeier <- kaplanMeierDist[kaplanMeierDist$databaseId==databaseId&
                                   kaplanMeierDist$analysisId==analysisId&
                                   kaplanMeierDist$outcomeId==outcomeId&
                                   kaplanMeierDist$targetId == targetId&
                                   kaplanMeierDist$comparatorId == comparatorId
                                 ,]
  if(nrow(kaplanMeier)==0) next
  kpResult <- result[result$databaseId==databaseId&
                       result$analysisId==analysisId&
                       result$outcomeId==outcomeId&
                       result$targetId == targetId&
                       result$comparatorId == comparatorId
                     ,]
  #convert to 'percent' by multiplying with 100
  kaplanMeier$targetSurvival <- (1-kaplanMeier$targetSurvival)*100
  kaplanMeier$targetSurvivalLb <-(1-kaplanMeier$targetSurvivalLb)*100
  kaplanMeier$targetSurvivalUb <-(1-kaplanMeier$targetSurvivalUb)*100
  kaplanMeier$comparatorSurvival <-(1-kaplanMeier$comparatorSurvival)*100
  kaplanMeier$comparatorSurvivalLb <-(1-kaplanMeier$comparatorSurvivalLb)*100
  kaplanMeier$comparatorSurvivalUb <-(1-kaplanMeier$comparatorSurvivalUb)*100
  
  pValue =  sprintf("italic(P)==%#.3f",
                    unique(kpResult$p))
  
  # if(is.na(unique(kpResult$p))) next
  # if(length(unique(kpResult$p))>1) next
  #
  # if(unique(kpResult$p)<0.001){
  #   pValue =  sprintf("italic(P) < 0.001")
  #   if (journalTheme=="jama") pValue = sprintf("italic(P)<.001")
  #   pValue = sprintf("italic(P)<%s",".001")
  # }else {
  #   #if (journalTheme=="jama") pNum <- sub("^(-?)0.", "//1.", sprintf("%.3f",unique(kpResult$p)))
  #   pValue =  sprintf("italic(P)==%#.3f",
  #                     unique(kpResult$p))
  
  
  p <-plotKaplanMeier(kaplanMeier,
                      targetName,
                      comparatorName,
                      ylims = c(0,0.60),#c(0,round(max(kaplanMeier$comparatorSurvivalUb,kaplanMeier$targetSurvivalUb),2)+0.02),
                      xBreaks = NULL,#seq(from = 0,to = 2000, by = 250),#c(0,100,200,300),
                      targetColorR = targetColorR,
                      targetColorG = targetColorG,
                      targetColorB = targetColorB,
                      comparatorColorR = comparatorColorR,
                      comparatorColorG = comparatorColorG,
                      comparatorColorB = comparatorColorB,
                      pValue = pValue,
                      pValueLocation = "left middle",
                      yLabel = "Cumulative Incidence (%)",
                      title = sprintf("%s vs %s in %s ", targetName, comparatorName, databaseName))
  
  if(!file.exists(file.path(resultFolder,"kmplot"))) dir.create(file.path(resultFolder,"kmplot"))
  ggplot2::ggsave(file.path(resultFolder,"kmplot",sprintf("km_plot_%s_t%s_c%s_o%d_a%d.png",
                                                          databaseName,
                                                          targetName,
                                                          comparatorName,
                                                          outcomeId,
                                                          analysisId)), p, device = "png", width = 19, height = 12, units = "cm", dpi = 200)
  # ggplot2::ggsave(file.path(resultFolder,"kmplot",sprintf("km_plot_%s_t%s_c%s_o%d_a%d.eps",
  #                                                         databaseName,
  #                                                         targetName,
  #                                                         comparatorName,
  #                                                         outcomeId,
  #                                                         analysisId)), p, device = "eps", width = 19, height = 12, units = "cm", dpi = 400)
  ggplot2::ggsave(file.path(resultFolder,"kmplot",sprintf("km_plot_%s_t%s_c%s_o%d_a%d.pdf",
                                                          databaseName,
                                                          targetName,
                                                          comparatorName,
                                                          outcomeId,
                                                          analysisId)), p, device = "pdf", width = 19, height = 12, units = "cm", dpi = 400)
  # ggplot2::ggsave(file.path(resultFolder,"kmplot",sprintf("km_plot_%s_t%s_c%s_o%d_a%d.tiff",
  #                                                         databaseId,
  #                                                         targetId,
  #                                                         comparatorId,
  #                                                         outcomeId,
  #                                                         analysisId)), p, device = "tiff", width = 16, height = 12, units = "cm", dpi = 400)
  
}
# #####
####Diagnostics table (CohortMethod)####
cmDiagnostics <- ohdsiPullTable(connection = connection,
                                resultsSchema = resultsSchema,###
                                targetTable = "cm_diagnostics_summary", #"cd_cohort"
                                limit = 0)
# excludingDbs <- c("German DA(DE)", "LPD Begium(BE)", "LPD Italy(IT)")

diag <- cmDiagnostics %>%
  mutate(diagnostics = ifelse(unblind, "PASS", "FAIL")) %>%
  left_join(dbTidy, by = "databaseId") %>%
  left_join(analysisTidy,
            by = "analysisId") %>%
  left_join(tcoTidy,
            by = c("targetId", "comparatorId", "outcomeId"))

# diag$comparatorAbbreviation <- factor(diag$comparatorAbbreviation, c("TMP", "CPH"))

diagPrime <- diag %>%
  filter(isPrimaryAnalysis == 1) %>%
  filter(isPrimaryTco == 1) %>%
  # filter(!(.data$cdmSourceAbbreviation %in% excludingDbs)) %>%
  arrange(dbOrder) #%>% arrange(comparatorId)

if(!file.exists(file.path(resultFolder,"diagnostics"))) dir.create(file.path(resultFolder,"diagnostics"))
write.csv(diagPrime, file.path(resultFolder, "diagnostics", "diagnostics_cm.csv"))

# #####
# 
# ####Diagnostics plot(CohortMethod)####
# cmDiagnostics <- ohdsiPullTable(connection = connection,
#                               resultsSchema = resultsSchema,###
#                               targetTable = "cm_diagnostics_summary", #"cd_cohort"
#                               limit = 0)
# excludingDbs <- c("German DA(DE)", "LPD Begium(BE)", "LPD Italy(IT)")
# 
# diag <- cmDiagnostics %>%
#   left_join(dbTidy, by = "databaseId") %>%
#   left_join(analysisTidy,
#             by = "analysisId") %>%
#   left_join(tcoTidy,
#             by = c("targetId", "comparatorId", "outcomeId")) %>%
#   mutate(diagnostics = ifelse(unblind, "PASS", "FAIL"))
# 
# diag$comparatorAbbreviation <- factor(diag$comparatorAbbreviation, c("TMP", "CPH"))
# 
# # Calculating ranks and normalizing them for the columns
# diagRanked <- diag %>%
#   # mutate(across(everything(), ~ rank(., na.last = FALSE, ties.method = "average"))) %>% #ranking (higher value is always lower [better] rank)
#   mutate(across(c(equipoise, attritionFraction), ~ rank(., na.last = "keep", ties.method = "average"))) %>% #ranking (higher value is always lower [better] rank)
#   mutate(across(c(maxSdm, mdrr, ease), ~ rank(., na.last = "keep", ties.method = "average"))) %>% #ranking (higher value is always lower [better] rank) NA value should have max rank
#   mutate(across(c(equipoise, attritionFraction), ~ (. - min(.,na.rm=T)) / (max(.,na.rm=T) - min(.,na.rm=T)))) %>% #higher values -> higher rank (PS overlap, attrition fraction)
#   mutate(across(c(maxSdm, mdrr, ease), ~ (max(.,na.rm=T) - . ) / (max(.,na.rm=T) - min(.,na.rm=T)))) %>% #lower values -> higher rank (Max SDM, MDRR, EASE)
#   # mutate(across(everything(), ~ replace_na(.,0))) #replace all NA values with 0
#   mutate(across(c(equipoise, attritionFraction,maxSdm, mdrr, ease), ~ replace_na(.,0))) #replace all NA values with 0
# 
# diagPass <- diag %>%
#   mutate(maxSdm = balanceDiagnostic) %>%
#   mutate(equipoise = equipoiseDiagnostic) %>%
#   mutate(mdrr = mdrrDiagnostic) %>%
#   mutate(attritionFraction = attritionDiagnostic) %>%
#   mutate(ease = easeDiagnostic)
# 
# get_pass <- function(x) {
#   if(is.na(x)){
#     textColor = 'red' #'gray'
#   } else{
#     if(x == "PASS") textColor = 'black'
#     if(x == "FAIL") textColor = 'red' #'gray'
#     if(x == "NOT EVALUATED") textColor = 'red' #'gray'
#   }
#   return(textColor)
# }
# 
# diagPrime <- diag %>%
#   filter(isPrimaryAnalysis == 1) %>%
#   filter(isPrimaryTco == 1) %>%
#   filter(!(.data$cdmSourceAbbreviation %in% excludingDbs)) %>%
#   arrange(dbOrder) #%>% arrange(comparatorId)
# 
# 
# 
# diagRankedPrime <- diagRanked %>%
#   filter(isPrimaryAnalysis == 1) %>%
#   filter(isPrimaryTco == 1) %>%
#   filter(!(.data$cdmSourceAbbreviation %in% excludingDbs)) %>%
#   arrange(dbOrder) #%>% arrange(comparatorId)
# 
# diagPassPrime <- diagPass %>%
#   filter(isPrimaryAnalysis == 1) %>%
#   filter(isPrimaryTco == 1) %>%
#   filter(!(.data$cdmSourceAbbreviation %in% excludingDbs)) %>%
#   arrange(dbOrder) #%>% arrange(comparatorId)
# 
# diagPrimeTidy <- diagPrime %>%
#   # filter(.data$comparatorId == !!comparatorId) %>%
#   filter(!(.data$cdmSourceAbbreviation %in% excludingDbs)) %>%
#   arrange(dbOrder)
# 
# diagRankedPrimeTidy <- diagRankedPrime %>%
#   # filter(.data$comparatorId == !!comparatorId) %>%
#   filter(!(.data$cdmSourceAbbreviation %in% excludingDbs)) %>%
#   arrange(dbOrder)
# 
# diagPassPrimeTidy <- diagPassPrime %>%
#   # filter(.data$comparatorId == !!comparatorId) %>%
#   filter(!(.data$cdmSourceAbbreviation %in% excludingDbs)) %>%
#   arrange(dbOrder)
# 
# 
# # Creating the gt table with the combined data
# gt_table <- diagPrimeTidy %>%
#   select(cdmSourceAbbreviation, maxSdm, equipoise, mdrr, attritionFraction, ease, diagnostics,comparatorAbbreviation) %>%
#   gt()
# 
# for (comparatorId in unique(diagPrimeTidy$comparatorId)){
#   comparatorColor <- diagPrimeTidy %>%
#     filter(.data$comparatorId == !! comparatorId) %>%
#     select(comparatorColorR, comparatorColorG, comparatorColorB) %>%
#     unique() %>% slice (1)
#   # comparatorColor <- comparatorColor * 0.7
#   
#   # Create a color palette function
#   get_color <- function(x) {
#     col_numeric(palette = c("white", rgb(comparatorColor)), domain = c(0, 1))(x)
#   }
#   # Apply the background colors to each cell of the colored columns
#   colored_column_names <- c("maxSdm", "equipoise", "mdrr", "attritionFraction", "ease")
#   for (col_name in colored_column_names) {
#     for (i in seq_along(diagRankedPrimeTidy[[col_name]])) {
#       if(diagRankedPrimeTidy$comparatorId[i] == comparatorId){
#         gt_table <- gt_table %>%
#           tab_style(
#             style = cell_fill(color = get_color(diagRankedPrimeTidy[[col_name]][i])),
#             locations = cells_body(
#               columns = vars(!!sym(col_name)),
#               rows = i
#             )
#           )
#       }
#     }
#   }
# }
# 
# pass_column_names <- c("maxSdm", "equipoise", "mdrr", "attritionFraction", "ease")
# for (col_name in pass_column_names) {
#   for (i in seq_along(diagPassPrimeTidy[[col_name]])) {
#     gt_table <- gt_table %>%
#       tab_style(
#         style = cell_text(color = get_pass(diagPassPrimeTidy[[col_name]][i])),
#         locations = cells_body(
#           columns = vars(!!sym(col_name)),
#           rows = i
#         )
#       )
#   }
# }
# 
# 
# gt_table <- gt_table %>%
#   ##Rename each column##
#   cols_label(
#     cdmSourceAbbreviation	 = "CDM Source",
#     maxSdm = "Balance",
#     equipoise	 = "Equipoise",
#     mdrr = "Power",
#     attritionFraction	 = "Generalizability",
#     ease = "Systematic Bias",
#     diagnostics = "All Diagnostics"
#   ) %>%
#   #Rename each column
#   tab_style(
#     style = cell_text(color = "red"),
#     locations = cells_body(
#       columns = c(diagnostics),
#       rows = diagnostics == "FAIL"
#     )
#   )
# 
# gt_table_modified <- gt_table %>%
#   fmt_number(columns = everything(),
#              decimals = 3) %>%
#   cols_align(align = "right",
#              columns = c(diagnostics)) %>%
#   cols_width(cdmSourceAbbreviation ~ px(180),
#              everything()~px(140)) %>%
#   tab_footnote(
#     footnote = "estimated by max standardized difference of mean",
#     locations = cells_column_labels(columns = maxSdm)
#   ) %>%
#   tab_footnote(
#     footnote = "estimated by overlap of preference score distribution",
#     locations = cells_column_labels(columns = equipoise)
#   ) %>%
#   tab_footnote(
#     footnote = "estimated by max detectable relative risk",
#     locations = cells_column_labels(columns = mdrr)
#   ) %>%
#   tab_footnote(
#     footnote = "estimated by attrition fraction",
#     locations = cells_column_labels(columns = attritionFraction)
#   ) %>%
#   tab_footnote(
#     footnote = "estimated by expected absolute systematic error (EASE)",
#     locations = cells_column_labels(columns = ease)
#   ) %>%
#   tab_source_note(source_note = md(
#     "Shadow indicates robustness of each diagnostics. Red color means the cell did not pass the diagnostics")) %>%
#   tab_source_note(source_note = md(
#     "Abbreviations")) %>%
#   opt_footnote_marks(marks = "letters") %>%
#   ## grouping ##
#   tab_row_group(
#     label = md("FQ vs CPH"),
#     rows = comparatorAbbreviation == "CPH"
#   ) %>%
#   tab_row_group(
#     label = md("FQ vs TMP"),
#     rows = comparatorAbbreviation == "TMP"
#   ) %>%
#   cols_hide (columns = comparatorAbbreviation) %>% #hide column indicating comparator
#   # tab_options(row_group.background.color = '#d3d3d3') %>%  #light gray for grouping rows
#   tab_style(
#     style = list(
#       cell_text(align = "center")
#     ),
#     locations = cells_row_groups(groups = everything())
#   )
# 
# if(!file.exists(file.path(resultFolder,"diagnostics"))) dir.create(file.path(resultFolder,"diagnostics"))
# saveRDS(gt_table_modified, file.path(resultFolder, "diagnostics", sprintf("diagnostics_table.RDS")))
# gt_table_modified %>% gtsave(file.path(resultFolder, "diagnostics", sprintf("diagnostics_table.png")), expand = 10)
# gt_table_modified %>% gtsave(file.path(resultFolder, "diagnostics", sprintf("diagnostics_table.html")))
# gt_table_modified %>% gtsave(file.path(resultFolder, "diagnostics", sprintf("diagnostics_table.tex")))
# gt_table_modified %>% gtsave(file.path(resultFolder, "diagnostics", sprintf("diagnostics_table.rtf")))
# gt_table_modified %>% gtsave(file.path(resultFolder, "diagnostics", sprintf("diagnostics_table.pdf")))
# gt_table_modified %>% gtsave(file.path(resultFolder, "diagnostics", sprintf("diagnostics_table.docx")))
# ####
# 
# 
# 
# # ####Diagnostics table (SCCS)####
# # analysisSccs <- ohdsiPullTable(connection = connection,
# #                              resultsSchema = resultsSchema,###
# #                              targetTable = "sccs_analysis",
# #                              limit = 0)
# # analysisTidySccs <- analysisSccs
# # analysisTidySccs$isPrimaryAnalysis <- 0
# # analysisTidySccs$tar <- NA
# # analysisTidySccs$nestingCohortId <- NA
# # 
# # for(i in seq(nrow(analysisTidySccs))){
# #   a <- jsonlite::fromJSON(analysisTidySccs$definition[i])
# #   analysisTidySccs$tar[i] <- a$createIntervalDataArgs$eraCovariateSettings$end[2]
# #   if(a$createIntervalDataArgs$eraCovariateSettings$end[2]==tarPrime) analysisTidySccs$isPrimaryAnalysis[i] <- 1
# #   analysisTidySccs$nestingCohortId[i] <- a$getDbSccsDataArgs$nestingCohortId[1]
# # }
# # analysisTidySccs <- analysisTidySccs %>%
# #   select(analysisId, description, tar, nestingCohortId, isPrimaryAnalysis)
# # 
# # exposureSccs <- ohdsiPullTable(connection = connection,
# #                              resultsSchema = resultsSchema,###
# #                              targetTable = "sccs_exposure",
# #                              limit = 0)
# # 
# # exposureOutcomeSccs <- ohdsiPullTable(connection = connection,
# #                                     resultsSchema = resultsSchema,###
# #                                     targetTable = "sccs_exposures_outcome_set",
# #                                     limit = 0)
# # 
# # eraSccs <- ohdsiPullTable(connection = connection,
# #                         resultsSchema = resultsSchema,###
# #                         targetTable = "sccs_era",
# #                         limit = 0)
# # 
# # eraTidy <- eraSccs %>% select(eraId, eraName, eraType) %>% unique ()
# # 
# # exposureOutcome <- exposureOutcomeSccs %>%
# #   left_join(exposureSccs, by = "exposuresOutcomeSetId") %>%
# #   left_join(eraTidy, by = c("eraId"))
# # exposureOutcome$exposureCohortId <- as.numeric(gsub("//D","",exposureOutcome$eraName)) #extract numbers from string
# # 
# # # add outcome names
# # exposureOutcome <- tcoTidy %>%
# #   select(outcomeId, isPrimaryTco, outcomeName) %>%
# #   unique() %>%
# #   right_join(exposureOutcome, by = "outcomeId")
# # 
# # # add exposure names
# # exposureOutcome <- cohortDefinition %>%
# #   select(cohortDefinitionId, cohortName) %>%
# #   right_join(exposureOutcome, by = c("cohortDefinitionId"="exposureCohortId")) %>%
# #   rename(exposureName = cohortName, exposureCohortId = cohortDefinitionId)
# # 
# # exposureOutcome <- exposureOutcome %>%
# #   mutate(exposureAbbreviation = case_when(exposureName == "Fluoroquinolone systemic exposures with UTI" ~ "FQ",
# #                                           exposureName == "Cephalosporin systemic exposures with UTI" ~ "CPH",
# #                                           exposureName == "Trimethoprim systemic exposures with UTI" ~ "TMP"))
# # 
# # exposureOutcomeTidy <- exposureOutcome
# # #Assign colors to exposure
# # # tcoTidy$targetColorR <- targetColorR
# # # tcoTidy$targetColorG <- targetColorG
# # # tcoTidy$targetColorB <- targetColorB
# # # tcoTidy$comparatorColorR <- ifelse(tcoTidy$comparatorAbbreviation=="TMP", comparatorColorR1st, tcoTidy$tcOrder <- ifelse(tcoTidy$comparatorAbbreviation=="CPH", comparatorColorR2nd, NA))
# # # tcoTidy$comparatorColorG <- ifelse(tcoTidy$comparatorAbbreviation=="TMP", comparatorColorG1st, tcoTidy$tcOrder <- ifelse(tcoTidy$comparatorAbbreviation=="CPH", comparatorColorG2nd, NA))
# # # tcoTidy$comparatorColorB <- ifelse(tcoTidy$comparatorAbbreviation=="TMP", comparatorColorB1st, tcoTidy$tcOrder <- ifelse(tcoTidy$comparatorAbbreviation=="CPH", comparatorColorB2nd, NA))
# # 
# # diagnosticsSccs <- ohdsiPullTable(connection = connection,
# #                                 resultsSchema = resultsSchema,###
# #                                 targetTable = "sccs_diagnostics_summary",
# #                                 limit = 0)
# # diagnosticsSccsTidy <- diagnosticsSccs %>%
# #   left_join(exposureOutcomeTidy, by = "exposuresOutcomeSetId")
# # 
# # excludingDbs <- c("German DA(DE)", "LPD Begium(BE)", "LPD Italy(IT)")
# # 
# # # sccsDiagnostics %>% str()
# # diagSccs <- diagnosticsSccs %>%
# #   left_join(dbTidy, by = "databaseId") %>%
# #   left_join(analysisTidySccs,
# #             by = "analysisId") %>%
# #   left_join(exposureOutcomeTidy,
# #             by = "exposuresOutcomeSetId") %>%
# #   mutate(diagnostics = ifelse(unblind, "PASS", "FAIL")) %>%
# #   filter(!(cdmSourceAbbreviation %in% excludingDbs))
# # 
# # diagSccsFq <- diagSccs %>% filter(isPrimaryTco == 1) %>% filter(tar==60) %>%
# #   filter(exposureAbbreviation == "FQ") %>%
# #   arrange(dbOrder) %>%
# #   select(exposureAbbreviation, cdmSourceAbbreviation, mdrr, timeTrendP, preExposureP, ease, mdrrDiagnostic, timeTrendDiagnostic,preExposureDiagnostic, easeDiagnostic, diagnostics)#13
# # diagSccsTmp <- diagSccs %>% filter(isPrimaryTco == 1) %>% filter(tar==60) %>%
# #   filter(exposureAbbreviation == "TMP") %>%
# #   arrange(dbOrder) %>%
# #   select(exposureAbbreviation, cdmSourceAbbreviation, mdrr, timeTrendP, preExposureP, ease, mdrrDiagnostic, timeTrendDiagnostic,preExposureDiagnostic, easeDiagnostic, diagnostics)#13
# # diagSccsCph <- diagSccs %>% filter(isPrimaryTco == 1) %>% filter(tar==60) %>%
# #   filter(exposureAbbreviation == "CPH") %>%
# #   arrange(dbOrder) %>%
# #   select(exposureAbbreviation, cdmSourceAbbreviation, mdrr, timeTrendP, preExposureP, ease, mdrrDiagnostic, timeTrendDiagnostic,preExposureDiagnostic, easeDiagnostic, diagnostics)#13
# # 
# # diagSccsSummary <- rbind(diagSccsFq, diagSccsTmp, diagSccsCph)
# # write.csv(diagSccsSummary, file.path(resultFolder, "diagnostics", "diagnostics_sccs.csv"))
# 
# ###############
# 
# #################
# 
# #### Diagnostic plot####
# # # https://cran.r-project.org/web/packages/forestploter/vignettes/forestploter-intro.html
# # # https://github.com/cran/forestploter
# # # https://youtu.be/reXmH_QyOio?si=ku9j_dmatkWBPP-F
# # excludingDbs <- c("German DA(DE)", "LPD Begium(BE)", "LPD Italy(IT)")
# #
# # #redundant
# # result <- ohdsiPullTable(connection = connection,
# #                        resultsSchema = resultsSchema,###
# #                        targetTable = "cm_result", #"cd_cohort"
# #                        limit = 0)
# # cmDiagnostics <- ohdsiPullTable(connection = connection,
# #                               resultsSchema = resultsSchema,###
# #                               targetTable = "cm_diagnostics_summary", #"cd_cohort"
# #                               limit = 0)
# #
# # cmResult <- result %>% left_join(cmDiagnostics, by = c("analysisId", "targetId", "comparatorId", "outcomeId", "databaseId"))
# # cmResult <- cmResult %>%
# #   left_join(dbTidy, by = "databaseId") %>%
# #   arrange(dbOrder)
# #
# # #exclude results from excluding dbs
# # cmResult <- cmResult %>% filter(!(cdmSourceAbbreviation %in% excludingDbs))
# #
# # cmResult <- cmResult %>%
# #   mutate(targetIncidence = gsub("-","<",format(round(targetOutcomes/(targetDays/365.25)*1000,2), nsmall=2))) %>%
# #   mutate(comparatorIncidence = gsub("-","<",format(round(comparatorOutcomes/(comparatorDays/365.25)*1000,2), nsmall=2)))
# #
# # cmResult <- cmResult %>%
# #   mutate(`Max SDM` = format(round(sharedMaxSdm,2), nsmall=2)) %>%
# #   mutate(`PS overlap` = format(round(equipoise,2), nsmall=2)) %>%
# #   mutate(`EASE` = format(round(ease,2), nsmall=2)) %>%
# #   mutate(`HR (95% CI)` = paste0(format(round(calibratedRr,2), nsmall=2),
# #                                 "(",
# #                                 format(round(calibratedCi95Lb,2),  nsmall=2),
# #                                 "-",
# #                                 format(round(calibratedCi95Ub,2), nsmall=2),
# #                                 ")"
# #   )
# #   )
# # cmResult$`Diagnostics` <- ifelse(cmResult$unblind, "PASS", "FAIL")
# # cmResult$`Matched(n)` <- format(cmResult$targetSubjects, trim = F, big.mark = " ")
# #
# #
# # # diagnosticsList <- cmDiagnostics %>%
# # #   left_join(dbTidy,
# # #             by = "databaseId") %>%
# # #   left_join(analysisTidy,
# # #             by = "analysisId") %>%
# # #   left_join(tcoTidy,
# # #             by = c("targetId", "comparatorId", "outcomeId")
# # #   ) %>%
# # #   filter(isPrimaryAnalysis == 1) %>%
# # #   filter(isPrimaryTco == 1)
# #
# # ####FQ vs TMP
# # comparatorAbbreviation <- "TMP"
# # cmResultOfInt <- cmResult %>% filter(analysisId ==2,
# #                                      outcomeId == 1782489,
# #                                      targetId == 1782488001,
# #                                      comparatorId == 1782670001) ##TMP
# # comparatorColor = colorTMP#"#a6bddb"
# # cmResultOfInt$`HR (95% CI)`<- gsub("-      ","-",cmResultOfInt$`HR (95% CI)`) #Remove space
# #
# # #Rename columns
# # cmResultOfInt <- cmResultOfInt %>%
# #   rename(Source = cdmSourceAbbreviation) %>%
# #   rename(FQ = targetIncidence)
# #
# # colnames(cmResultOfInt)[colnames(cmResultOfInt)=="comparatorIncidence"] <- comparatorAbbreviation # rlang::ensym(comparatorAbbreviation)
# #
# # equipoiseAlpha <- (cmResultOfInt$equipoise)/2 #dividing by 2 to make brighter
# # equipoiseAlpha[is.na(equipoiseAlpha)]<-0 #replace NA with 0
# #
# # tm <- forestploter::forest_theme(core=list(
# #   # fg_params=list(hjust=c(1, 0, 0, 0.5),
# #   #                                                         x=c(0.9, 0.1, 0, 0.5)),
# #   bg_params=list(fill = comparatorColor, #"#f6eff7", "#d0d1e6", "#a6bddb", "#67a9cf"
# #                  alpha = equipoiseAlpha
# #   ))
# #   ,
# #   colhead=list(fg_params=list(hjust=c(1, 0, 0, 0, 0.5),
# #                               x=c(0.9, 0.1, 0, 0, 0.5)))
# # )
# #
# # cmResultOfInt <- cmResultOfInt %>%
# #   mutate(targetIncidence = gsub("-","<",format(round(targetOutcomes/(targetDays/365.25)*1000,2), nsmall=2))) %>%
# #   mutate(comparatorIncidence = gsub("-","<",format(round(comparatorOutcomes/(comparatorDays/365.25)*1000,2), nsmall=2)))
# #
# # cmResultOfInt$` ` <- paste(rep(" ", 20), collapse = " ")
# #
# # p <- forestploter::forest (cmResultOfInt[,c("Source", "Matched(n)", "FQ", comparatorAbbreviation," ","HR (95% CI)","Max SDM","PS overlap","EASE", "Diagnostics")], #
# #                            est = cmResultOfInt$calibratedRr,
# #                            lower = cmResultOfInt$calibratedCi95Lb,
# #                            upper = cmResultOfInt$calibratedCi95Ub,
# #                            # sizes = (log(cmResultOfInt$calibratedCi95Ub) - log(cmResultOfInt$calibratedCi95Lb))/1.96,
# #                            ci_column = 5,
# #                            ref_line = 1,
# #                            arrow_lab = c("Favors FQ", sprintf("Favors %s",comparatorAbbreviation)),
# #                            xlim = c(0.5, 2),
# #                            ticks_at = c(0.5, 1, 2),
# #                            x_trans = "log",
# #                            # footnote = "This is the demo data. Please feel free to change/nanything you want.",
# #                            theme = tm)
# # plot(p)
# #
# # if(!file.exists(file.path(resultFolder,"diagnostics"))) dir.create(file.path(resultFolder,"diagnostics"))
# #
# # ggsave(file.path(resultFolder, "diagnostics", sprintf("diagnostics_%s.png", comparatorAbbreviation)),
# #        p,
# #        width = 32, height = 12, units = "cm", dpi = 200)
# # ggsave(file.path(resultFolder, "diagnostics", sprintf("diagnostics_%s.pdf", comparatorAbbreviation)),
# #        p,
# #        width = 32, height = 12, units = "cm")
# #
# #
# # ####FQ vs CPH
# # comparatorAbbreviation <- "CPH"
# # cmResultOfInt <- cmResult %>% filter(analysisId ==2,
# #                                      outcomeId == 1782489,
# #                                      targetId == 1782488001,
# #                                      comparatorId == 1782487001) ##CPH
# # comparatorColor = colorCPH #"#dba6bd" #"#f6eff7", "#d0d1e6", "#a6bddb", "#67a9cf"
# # cmResultOfInt$`HR (95% CI)`<- gsub("-     ","-",cmResultOfInt$`HR (95% CI)`) #Remove space
# #
# # #Rename columns
# # cmResultOfInt <- cmResultOfInt %>%
# #   rename(Source = cdmSourceAbbreviation) %>%
# #   rename(FQ = targetIncidence)
# #
# # colnames(cmResultOfInt)[colnames(cmResultOfInt)=="comparatorIncidence"] <- comparatorAbbreviation # rlang::ensym(comparatorAbbreviation)
# #
# # equipoiseAlpha <- (cmResultOfInt$equipoise)/2 #dividing by 2 to make brighter
# # equipoiseAlpha[is.na(equipoiseAlpha)]<-0 #replace NA with 0
# #
# # tm <- forestploter::forest_theme(core=list(
# #   # fg_params=list(hjust=c(1, 0, 0, 0.5),
# #   #                                                         x=c(0.9, 0.1, 0, 0.5)),
# #   bg_params=list(fill = comparatorColor, #"#f6eff7", "#d0d1e6", "#a6bddb", "#67a9cf"
# #                  alpha = equipoiseAlpha
# #   ))
# #   ,
# #   colhead=list(fg_params=list(hjust=c(1, 0, 0, 0, 0.5),
# #                               x=c(0.9, 0.1, 0, 0, 0.5)))
# # )
# #
# # cmResultOfInt <- cmResultOfInt %>%
# #   mutate(targetIncidence = gsub("-","<",format(round(targetOutcomes/(targetDays/365.25)*1000,2), nsmall=2))) %>%
# #   mutate(comparatorIncidence = gsub("-","<",format(round(comparatorOutcomes/(comparatorDays/365.25)*1000,2), nsmall=2)))
# #
# # cmResultOfInt$` ` <- paste(rep(" ", 20), collapse = " ")
# #
# # p <- forestploter::forest (cmResultOfInt[,c("Source", "Matched(n)", "FQ", comparatorAbbreviation," ","HR (95% CI)","Max SDM","PS overlap","EASE", "Diagnostics")], #
# #                            est = cmResultOfInt$calibratedRr,
# #                            lower = cmResultOfInt$calibratedCi95Lb,
# #                            upper = cmResultOfInt$calibratedCi95Ub,
# #                            # sizes = (log(cmResultOfInt$calibratedCi95Ub) - log(cmResultOfInt$calibratedCi95Lb))/1.96,
# #                            ci_column = 5,
# #                            ref_line = 1,
# #                            arrow_lab = c("Favors FQ", sprintf("Favors %s",comparatorAbbreviation)),
# #                            xlim = c(0.5, 2),
# #                            ticks_at = c(0.5, 1, 2),
# #                            x_trans = "log",
# #                            # footnote = "This is the demo data. Please feel free to change/nanything you want.",
# #                            theme = tm)
# # plot(p)
# #
# # ggsave(file.path(resultFolder, "diagnostics", sprintf("diagnostics_%s.png", comparatorAbbreviation)),
# #        p,
# #        width = 32, height = 12, units = "cm", dpi = 200)
# # ggsave(file.path(resultFolder, "diagnostics", sprintf("diagnostics_%s.pdf", comparatorAbbreviation)),
# #        p,
# #        width = 32, height = 12, units = "cm")
# #
# # # ggsave(file.path(resultFolder, "diagnostics", sprintf("diagnostics_%s.png", comparatorAbbreviation)),p)
# ####################
# 
####Meta-analysis from OHDSI server####
cmResult <- ohdsiPullTable(connection = connection,
                           resultsSchema = resultsSchema,###
                           targetTable = "cm_result", #"cd_cohort"
                           limit = 0)
esCmResult <- ohdsiPullTable(connection = connection,
                             resultsSchema = resultsSchema,###
                             targetTable = "es_cm_result", #"cd_cohort"
                             limit = 0)

cmDiagnostics <- ohdsiPullTable(connection = connection,
                                resultsSchema = resultsSchema,###
                                targetTable = "cm_diagnostics_summary", #"cd_cohort"
                                limit = 0)
esCmDiagnostics <- ohdsiPullTable(connection = connection,
                                  resultsSchema = resultsSchema,###
                                  targetTable = "es_cm_diagnostics_summary", #"cd_cohort"
                                  limit = 0)

cmResult <- cmResult %>% left_join(cmDiagnostics, by = c("analysisId", "targetId", "comparatorId", "outcomeId", "databaseId"))
esCmResult <- esCmResult %>% left_join(esCmDiagnostics, by = c("analysisId", "targetId", "comparatorId", "outcomeId", "evidenceSynthesisAnalysisId"))

combiResult <- bind_rows(cmResult, esCmResult)%>%
  mutate(evidenceSynthesisAnalysisId = ifelse(is.na(evidenceSynthesisAnalysisId), 0, evidenceSynthesisAnalysisId)) %>% #replace NA value in `evidenceSynthesisAnalysisId` to 0
  left_join(dbTidy,
            by = "databaseId") %>%
  left_join(analysisTidy,
            by = "analysisId") %>%
  left_join(tcoTidy,
            by = c("targetId", "comparatorId", "outcomeId")
  )
# write.csv(combiResult, file.path(resultFolder,"combined_result.csv"))

####Forest plot####
# https://cran.r-hub.io/web/packages/forestplot/vignettes/forestplot.html

metaList <- combiResult %>%
  filter(evidenceSynthesisAnalysisId  == 1) %>%
  filter (unblind == 1) %>%
  filter (outcomeId %in% tcoTidy$outcomeId)

for(i in seq(nrow(metaList))){
  targetId <- metaList[i,]$targetId
  comparatorId <- metaList[i,]$comparatorId
  analysisId <- metaList[i,]$analysisId
  outcomeId = metaList$outcomeId[i]
  comparatorColorR <- metaList[i,]$comparatorColorR
  comparatorColorG <- metaList[i,]$comparatorColorG
  comparatorColorB <- metaList[i,]$comparatorColorB
  targetName <- metaList[i,]$targetAbbreviation
  comparatorName <- metaList[i,]$comparatorAbbreviation
  outcomeName <- metaList[i,]$outcomeAbbreviation
  
  comparatorColorR <- metaList[i,]$comparatorColorR
  comparatorColorG <- metaList[i,]$comparatorColorG
  comparatorColorB <- metaList[i,]$comparatorColorB
  
  metaData <- combiResult %>%
    filter(analysisId == !!analysisId,
           outcomeId == !!outcomeId,
           targetId == !!targetId,
           comparatorId == !!comparatorId)
  
  # add db names
  aggSingleResult <- metaData %>%
    filter(evidenceSynthesisAnalysisId==0) %>%
    filter (unblind == 1) %>% #only those passing diagnostics
    arrange(dbOrder)
  
  summaryResult <- metaData %>%
    filter(evidenceSynthesisAnalysisId  == 1) %>%
    filter (unblind == 1)
  
  header <- tibble(study = c("", "Source"),
                   targetIncidence = c("Event Rate", targetName),
                   comparatorIncidence = c("", comparatorName),
                   HR = c("Hazard ratio", "(95% CI)"),
                   summary = TRUE)
  
  metaResult <- tibble::tibble(mean  = aggSingleResult$calibratedRr,
                               lower = aggSingleResult$calibratedCi95Lb,
                               upper = aggSingleResult$calibratedCi95Ub,2,
                               study = aggSingleResult$cdmSourceAbbreviation,
                               targetIncidence = gsub("-","<",format(round(aggSingleResult$targetOutcomes/(aggSingleResult$targetDays/365.25)*1000,2), nsmall=2)),
                               comparatorIncidence = gsub("-","<",format(round(aggSingleResult$comparatorOutcomes/(aggSingleResult$comparatorDays/365.25)*1000,2), nsmall=2)),
                               # HR = sprintf("%s(%s-%s)",
                               #                     format(round(aggSingleResult$calibratedRr,2), nsmall =2 ),
                               #                     format(round(aggSingleResult$calibratedCi95Lb,2), nsmall = 2),
                               #                     format(round(aggSingleResult$calibratedCi95Ub,2), nsmall = 2)
                               #             )
                               HR = ifelse(is.na(aggSingleResult$calibratedRr),
                                           sprintf("%s/t", format(round(aggSingleResult$calibratedRr,2), nsmall =2 )),
                                           sprintf("%s(%s-%s)",
                                                   format(round(aggSingleResult$calibratedRr,2), nsmall =2 ),
                                                   format(round(aggSingleResult$calibratedCi95Lb,2), nsmall = 2),
                                                   format(round(aggSingleResult$calibratedCi95Ub,2), nsmall = 2)
                                           )
                               )
  )
  
  metaSummary <- tibble (mean  = summaryResult$calibratedRr,
                         lower = summaryResult$calibratedCi95Lb,
                         upper = summaryResult$calibratedCi95Ub,
                         study = sprintf("Meta-analysis"), #if we need to add tau value: format(round(summaryResult$tau[1],2), nsmall =2 )
                         targetIncidence = gsub("-","<",format(round(summaryResult$targetOutcomes/(summaryResult$targetDays/365.25)*1000,2), nsmall=2)),
                         comparatorIncidence = gsub("-","<",format(round(summaryResult$comparatorOutcomes/(summaryResult$comparatorDays/365.25)*1000,2), nsmall=2)),
                         HR = sprintf("%s(%s-%s)",
                                      format(round(summaryResult$calibratedRr,2), nsmall =2 ),
                                      format(round(summaryResult$calibratedCi95Lb,2), nsmall = 2),
                                      format(round(summaryResult$calibratedCi95Ub,2), nsmall = 2)
                         ),
                         summary = TRUE)
  
  emptyRow <- tibble(mean = NA_real_)
  
  cochraneOutputDf <- bind_rows(header,
                                metaResult,
                                emptyRow,
                                metaSummary)
  
  # Creating a named list for horizontal lines.
  # you cannot directly use variables as names in a list using the = assignment. Instead, you should use the setNames() function or similar approaches
  hrzl_lines_list <- setNames(
    list(gpar(lty = 2), gpar(lwd = 1, columns = 1:4#, col = colorTarget
                             )),
    c(as.character(3),  #first line
      as.character(nrow(cochraneOutputDf) - 1) #second line
    )
  )
  
  metaPlot <- cochraneOutputDf %>%
    forestplot(labeltext = c(study, targetIncidence, comparatorIncidence, HR),
               is.summary = summary,
               # clip = c(0.25, 4.0),
               # xticks = c(0.25, 0.5, 1.0, 2.0, 4.0),
               # xticks.digits = 1,
               hrzl_lines = hrzl_lines_list,
               xlog = TRUE,
               col = fpColors(box = rgb(comparatorColorR, comparatorColorG, comparatorColorB, alpha = 0.7),#"royalblue",
                              line = rgb(comparatorColorR, comparatorColorG, comparatorColorB),#"darkblue",
                              summary = rgb(comparatorColorR, comparatorColorG, comparatorColorB, alpha = 0.7)
                              ),
               title = sprintf("%s vs %s", targetName, comparatorName)
    )
  # grid::grid.text(paste("Tau:", round(tau_val, 2)), x = unit(0.95, "npc"), y = unit(0.05, "npc"), just = c("left", "bottom")) #to add tau value
  
  if(!file.exists(file.path(resultFolder,"meta"))) dir.create(file.path(resultFolder,"meta"))
  # save plot in png
  png(file.path(resultFolder, "meta", sprintf("meta_%svs%s_o%s_a%s.png", targetName, comparatorName, outcomeName, analysisId)),
      width = 1000,height = 580#, res = 500
  )
  plot(metaPlot)
  dev.off()
  
  # save plot in pdf
  pdf(file.path(resultFolder, "meta", sprintf("meta_%svs%s_o%s_a%s.pdf", targetName, comparatorName, outcomeName, analysisId)),
      width = 10,height = 5.8#,
  )
  plot(metaPlot)
  dev.off()
  
  # save plot in eps
  setEPS()
  postscript(file.path(resultFolder,"meta",sprintf("meta_%svs%s_o%s_a%s.eps", targetName, comparatorName, outcomeName, analysisId)),
             width = 10,height = 5.8)
  plot(metaPlot)
  dev.off()
}

####Negative control outcomes####
esCmResult <- ohdsiPullTable(connection = connection,
                           resultsSchema = resultsSchema,###
                           targetTable = "es_cm_result", #"cd_cohort"
                           limit = 0)

controlResults <- esCmResult %>%
  filter(targetId %in% tcoTidy$targetId) %>%
  filter(comparatorId %in% tcoTidy$comparatorId) %>%
  filter(analysisId %in% analysisIdPrime) %>%
  filter(evidenceSynthesisAnalysisId == 1)

##define effect size
controlResults$effectSize <- NA
#identify negative control outcome ids
idx <- !controlResults$outcomeId %in% unique(tcoTidy$outcomeId)

controlResults$effectSize[idx] <- 1
positiveControlOutcome = NULL

if (!is.null(positiveControlOutcome)) {
  idx <- controlResults$outcomeId %in% positiveControlOutcome$outcomeId
  controlResults$effectSize[idx] <- positiveControlOutcome$effectSize[match(results$outcomeId[idx],
                                                                            positiveControlOutcome$outcomeId)]
}
controlResults <- controlResults[!is.na(controlResults$effectSize), ]

for (comparatorIdInd in unique(tcoTidy$comparatorId)){
  controlResultsInd <- controlResults %>% filter(comparatorId == comparatorIdInd)
  
  comparatorAbbreviation <- tcoTidy$comparatorAbbreviation[tcoTidy$comparatorId==comparatorIdInd][1]
  targetAbbreviation <- tcoTidy$targetAbbreviation[tcoTidy$comparatorId==comparatorIdInd][1]
  comparatorColorR <- tcoTidy$comparatorColorR[tcoTidy$comparatorId==comparatorIdInd][1]
  comparatorColorG <- tcoTidy$comparatorColorG[tcoTidy$comparatorId==comparatorIdInd][1]
  comparatorColorB <- tcoTidy$comparatorColorB[tcoTidy$comparatorId==comparatorIdInd][1]
  analysisIdInd <- analysisIdPrime #only primary analysis
  
  systematicErrorPlot <- plotScatter(controlResultsInd) +
    ggtitle(sprintf("Systematic error control in %s vs %s",targetAbbreviation,comparatorAbbreviation)) +
    theme(plot.title = element_text(hjust = 0.5)) +
    geom_point(color=rgb(comparatorColorR,comparatorColorG,comparatorColorB))
  
  if(!file.exists(file.path(resultFolder,"systematic_error"))) dir.create(file.path(resultFolder,"systematic_error"))
  ggplot2::ggsave(file.path(resultFolder,"systematic_error",sprintf("systematic_error_plot_t%s_c%s_a%s.png",
                                                                    targetAbbreviation,
                                                                    comparatorAbbreviation,
                                                                    analysisIdInd)),
                  systematicErrorPlot, device = "png", width = 30, height = 20, units = "cm", dpi = 200)
  
  
  controlResultsInd <- controlResultsInd %>%
    mutate(pSignificant = controlResultsInd$p<0.05) %>%
    mutate(calibratedPSignificant = controlResultsInd$calibratedP<0.05)
  
  write.csv(controlResultsInd, file.path(resultFolder,"systematic_error",sprintf("systematic_error_plot_t%s_c%s_a%s.csv",
                                                                                 targetAbbreviation,
                                                                                 comparatorAbbreviation,
                                                                                 analysisIdInd)))
}

# 
# ####Two-dimensional Plotting####
# esCmResult <- ohdsiPullTable(connection = connection,
#                            resultsSchema = resultsSchema,###
#                            targetTable = "es_cm_result", #"cd_cohort"
#                            limit = 0)
# 
# 
# # Extract only TAR from analysis description
# analysisTidy$TAR <- analysisTidy$descriptionAnalysis %>%
#   stringr::str_extract_all("[0-9]{2,}d")
# 
# esCmResultTemp <- esCmResult %>%
#   left_join(analysisTidy, by = "analysisId") %>% #adding description of analysis
#   left_join(tcoTidy, by = c("targetId", "comparatorId", "outcomeId")) %>% #adding description of TCO
#   filter(outcomeId %in% tcoTidy$outcomeId) %>% #only outcomes of interest
#   filter(evidenceSynthesisAnalysisId == 1)
# 
# unique(esCmResultInd$TAR)
# 
# esCmResultInd <- esCmResultTemp %>%
#   filter(comparatorId == comparatorIdInd)
# 
# esCmResultInd <- esCmResultTemp
# 
# # comparatorAbbreviation <- tcoTidy$comparatorAbbreviation[tcoTidy$comparatorId==comparatorIdInd][1]
# # targetAbbreviation <- tcoTidy$targetAbbreviation[tcoTidy$comparatorId==comparatorIdInd][1]
# # comparatorColorR <- tcoTidy$comparatorColorR[tcoTidy$comparatorId==comparatorIdInd][1]
# # comparatorColorG <- tcoTidy$comparatorColorG[tcoTidy$comparatorId==comparatorIdInd][1]
# # comparatorColorB <- tcoTidy$comparatorColorB[tcoTidy$comparatorId==comparatorIdInd][1]
# 
# ## Leveling of factors
# esCmResultInd$TAR<-factor(esCmResultInd$TAR,levels = c("30d","60d","90d","365d"))
# esCmResultInd$outcomeAbbreviation <- factor(esCmResultInd$outcomeAbbreviation, levels = c("AD", "AA", "AD or AA"))
# esCmResultInd$comparatorAbbreviation <- factor(esCmResultInd$comparatorAbbreviation, levels = c("TMP", "CPH"))
# 
# 
# xLim = max(ceiling(median(esCmResultInd$ci95Ub, na.rm=TRUE)),ceiling(1/median(esCmResultInd$ci95Lb, na.rm=TRUE)),2, na.rm = TRUE)
# xLim<-ifelse(xLim< (max( 1/esCmResultInd$rr,esCmResultInd$rr,na.rm=TRUE)),ceiling(max( 1/esCmResultInd$rr,esCmResultInd$rr,na.rm=TRUE)),xLim)
# limits = c(1/xLim,xLim)
# limits = c(0.5,2.0)
# xLimits = c(0.5,2.0)
# 
# esCmResultInd$rr <- esCmResultInd$calibratedRr
# esCmResultInd$ci95Lb <- esCmResultInd$calibratedCi95Lb
# esCmResultInd$ci95Ub <- esCmResultInd$calibratedCi95Ub
# esCmResultInd$p <- esCmResultInd$calibratedP
# 
# esCmResultInd <- esCmResultInd %>% mutate(#ci95Lb =ifelse(ci95Lb < limits[1], ci95Lb-1, ci95Lb),
#   #ci95Ub = ifelse(ci95Lb > limits[2], ci95Ub+1, ci95Ub),
#   ci95LbOut = ifelse(ci95Lb < limits[1], rr - limits[1], NA),
#   ci95UbOut = ifelse(ci95Ub > limits[2], rr - limits[2], NA))
# 
# esCmResultInd$Significance<-factor(ifelse(esCmResultInd$p<0.05,"P<.05","Not significant"),
#                                    levels = c("P<.05","Not significant")
# )
# esCmResultInd$axis0 <-esCmResultInd$comparatorAbbreviation
# esCmResultInd$axis1 <-esCmResultInd$TAR
# esCmResultInd$axis2 <-esCmResultInd$outcomeAbbreviation
# 
# esCmResultInd$axis0 <-esCmResultInd$TAR
# esCmResultInd$axis1 <-esCmResultInd$comparatorAbbreviation
# esCmResultInd$axis2 <-esCmResultInd$outcomeAbbreviation
# 
# 
# 
# gridForest(results= esCmResultInd,
#            breaks = c(0.3, 0.5, 0.75, 1, 1.5, 2.0),
#            outlierMoverLower= 0.03,
#            outlierMoverUpper= 0.03,
#            xLimits=xLimits,
#            varX = "outcomeAbbreviation",
#            varY = "comparatorAbbreviation",
#            # cols = NULL,
#            xLab = "Outcome",
#            axis0Title = "TAR")
# 
# summaryP <- gridForest(results= esCmResultInd,
#                        breaks = c(0.3, 0.5, 0.75, 1, 1.5, 2.0),
#                        outlierMoverLower= 0.03,
#                        outlierMoverUpper= 0.03,
#                        xLimits=xLimits,
#                        varX = "outcomeAbbreviation",
#                        varY = "comparatorAbbreviation",
#                        # cols = NULL,
#                        xLab = "Outcome")
# summaryP <- update_labels(summaryP, list(colour="TAR")) # ADD legend
# 
# if(!file.exists(file.path(resultFolder,"summary"))) dir.create(file.path(resultFolder,"summary"))
# ggplot2::ggsave(file.path(resultFolder,"summary",sprintf("summary.jpg"
# )), summaryP)
# 
# 
# ggplot2::ggsave(file.path(resultFolder,"summary",sprintf("summary_%s_t%d_c%d_o%d_.eps",
#                                                          outcomeName,
#                                                          targetId,
#                                                          comparatorId,
#                                                          outcomeId)), summaryP, device = "eps" ,
#                 width = 20, height = 22, units = "cm", dpi = 320)
# 
# ggplot2::ggsave(file.path(resultFolder,"summary",sprintf("summary_%s_t%d_c%d_o%d_.pdf",
#                                                          outcomeName,
#                                                          targetId,
#                                                          comparatorId,
#                                                          outcomeId)), summaryP, device = "pdf" ,
#                 width = 20, height = 22, units = "cm", dpi = 320)
# 
# ggplot2::ggsave(file.path(resultFolder,"summary",sprintf("summary_%s_t%d_c%d_o%d_.tiff",
#                                                          outcomeName,
#                                                          targetId,
#                                                          comparatorId,
#                                                          outcomeId)), summaryP, device = "tiff" ,
#                 width = 20, height = 22, units = "cm", dpi = 320)
