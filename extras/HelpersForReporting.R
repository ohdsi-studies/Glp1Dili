#ParamsCheck
checkIsClass<- function(parameter,classes) {
  name = deparse(substitute(parameter))
  if (!inherits(x = parameter, what = classes)) {
    ParallelLogger::logError(paste0(name, ' should be of class:', classes))
    stop(paste0(name, ' is wrong class'))
  }
  return(TRUE)
}

#Pull result from Postresql
ohdsiPullTable <- function(connection,
                         resultsSchema,
                         targetTable,
                         limit = 0 #0 means no limit;
){
  checkIsClass(limit,c('integer', 'numeric'))
  sql <- "SELECT {@limit_true} ? {TOP @limit} *
          FROM @results_schema.@target_table;"
  sql <- SqlRender::render(sql,
                           results_schema = resultsSchema,
                           target_table = targetTable,
                           limit_true = ifelse(limit,TRUE,FALSE),
                           limit = limit)
  sql <- SqlRender::translate(sql, targetDialect = connection@dbms)
  result <- DatabaseConnector::querySql(connection, sql)
  colnames(result) <- colnames(result) %>%
    SqlRender::snakeCaseToCamelCase() #snake to camel
  return(result)
}

#Get balance
getBalance <- function(connection,
                       databaseId,
                       # dataFolder,
                       targetId,
                       comparatorId,
                       analysisId,
                       outcomeId = NULL){
  
  sql <- "SELECT *
          FROM @results_schema.@target_table
          WHERE database_id = @database_id
          AND target_id = @target_id
          AND comparator_id = @comparator_id
          AND analysis_id = @analysis_id"
  sql <- SqlRender::render(sql,
                           results_schema = resultsSchema,
                           target_table = targetTable,
                           database_id = databaseId,
                           target_id = targetId,
                           comparator_id = comparatorId,
                           analysis_id = analysisId)
  sql <- SqlRender::translate(sql, targetDialect = connection@dbms)
  balance <- DatabaseConnector::querySql(connection, sql)
  
  
  sql <- "SELECT {@limit_true} ? {TOP @limit} *
          FROM @results_schema.@target_table;"
  sql <- SqlRender::render(sql,
                           results_schema = resultsSchema,
                           target_table = targetTable,
                           limit_true = ifelse(limit,TRUE,FALSE),
                           limit = limit)
  sql <- SqlRender::translate(sql, targetDialect = connection@dbms)
  balance <- DatabaseConnector::querySql(connection, sql)
  
  colnames(balance)<-SqlRender::snakeCaseToCamelCase(colnames(balance))
  colnames(covariate)<-SqlRender::snakeCaseToCamelCase(colnames(covariate))
  
  if(is.null(outcomeId)){
    balance <- balance[balance$analysisId == analysisId, ]
  }else{
    balance <- balance[balance$analysisId == analysisId & balance$outcomeId == outcomeId, ]
  }
  
  covariate <- covariate[covariate$analysisId == analysisId,]
  balance <- merge(balance, covariate[,c("covariateId", "covariateAnalysisId", "covariateName")])
  balance <- balance[ c("covariateId",
                        "covariateName",
                        "covariateAnalysisId",
                        "targetMeanBefore",
                        "comparatorMeanBefore",
                        "stdDiffBefore",
                        "targetMeanAfter",
                        "comparatorMeanAfter",
                        "stdDiffAfter")]
  colnames(balance) <- c("covariateId",
                         "covariateName",
                         "analysisId",
                         "targetMeanBefore",
                         "comparatorMeanBefore",
                         "stdDiffBefore",
                         "targetMeanAfter",
                         "comparatorMeanAfter",
                         "stdDiffAfter")
  balance$absstdDiffBefore <- abs(balance$stdDiffBefore)
  balance$absstdDiffAfter <- abs(balance$stdDiffAfter)
  return(balance)
}

plotCovariateBalanceScatterPlot <- function(balance,
                                            beforeLabel = "Before stratification",
                                            afterLabel = "After stratification",
                                            limits = NULL,
                                            dotColor = rgb(0, 0, 0.8, alpha = 0.3)) {
  if(is.null(limits)){limits <- c(min(c(balance$absStdDiffBefore, balance$absStdDiffAfter),
                                      na.rm = TRUE),
                                  max(c(balance$absStdDiffBefore, balance$absStdDiffAfter),
                                      na.rm = TRUE))}
  theme <- ggplot2::element_text(colour = "#000000", size = 12)
  plot <- ggplot2::ggplot(balance, ggplot2::aes(x = absStdDiffBefore, y = absStdDiffAfter)) +
    ggplot2::geom_point(color = dotColor, shape = 16, size = 2) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::geom_vline(xintercept = 0) +
    ggplot2::scale_x_continuous(beforeLabel, limits = limits) +
    ggplot2::scale_y_continuous(afterLabel, limits = limits) +
    ggplot2::theme(text = theme)
  
  return(plot)
}

plotPs <- function(ps,
                   targetName,
                   comparatorName,
                   targetColor,
                   comparatorColor,
                   showEquiposeLabel = TRUE,
                   equipoiseBounds = c(0.3,0.7),
                   fileName = NULL,
                   targetColorR = 0.8,
                   targetColorG = 0,
                   targetColorB = 0,
                   comparatorColorR = 0,
                   comparatorColorG = 0,
                   comparatorColorB = 0.8) {
  psOrigin <- ps
  ps <- rbind(data.frame(x = ps$preferenceScore, y = ps$targetDensity, group = targetName),
              data.frame(x = ps$preferenceScore, y = ps$comparatorDensity, group = comparatorName))
  ps$group <- factor(ps$group, levels = c(as.character(targetName), as.character(comparatorName)))
  theme <- ggplot2::element_text(colour = "#000000", size = 12, margin = ggplot2::margin(0, 0.5, 0, 0.1, "cm"))
  plot <- ggplot2::ggplot(ps,
                          ggplot2::aes(x = x, y = y, color = group, group = group, fill = group)) +
    ggplot2::geom_density(stat = "identity") +
    ggplot2::scale_fill_manual(values = c(rgb(0.8, 0, 0, alpha = 0.5),
                                          rgb(0, 0, 0.8, alpha = 0.5))) +
    ggplot2::scale_color_manual(values = c(rgb(0.8, 0, 0, alpha = 0.5),
                                           rgb(0, 0, 0.8, alpha = 0.5))) +
    # ggplot2::scale_fill_manual(values = c(rgb(targetColorR, targetColorG, targetColorB, alpha = 0.5),
    #                                       rgb(comparatorColorR, comparatorColorG, comparatorColorB, alpha = 0.5))) +
    # ggplot2::scale_color_manual(values = c(rgb(targetColorR, targetColorG, targetColorB, alpha = 0.5),
    # rgb(comparatorColorR, comparatorColorG, comparatorColorB, alpha = 0.5))) +
    ggplot2::scale_x_continuous("Preference score", limits = c(0, 1)) +
    ggplot2::scale_y_continuous("Density") +
    ggplot2::theme(legend.title = ggplot2::element_blank(),
                   panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   legend.position = "top",
                   legend.text = theme,
                   axis.text = theme,
                   axis.title = theme)
  if (showEquiposeLabel) {
    labelsLeft <- c()
    labelsRight <- c()
    if (showEquiposeLabel) {
      equiIndex <- psOrigin$preferenceScore>=equipoiseBounds[1] & psOrigin$preferenceScore<=equipoiseBounds[2]
      equipoise <- mean (sum(psOrigin$targetDensity[equiIndex]), sum(psOrigin$comparatorDensity[equiIndex]))/100
      labelsRight <- c(labelsRight, sprintf("%2.1f%% is in equipoise",
                                            equipoise * 100))
    }
    if (length(labelsLeft) > 0) {
      dummy <- data.frame(text = paste(labelsLeft, collapse = "\n"))
      plot <- plot + ggplot2::geom_label(x = 0, #y = max(d$y) * 1.24,
                                         hjust = "left", vjust = "top", alpha = 0.8,
                                         ggplot2::aes(label = text), data = dummy, size = 3.5)
    }
    if (length(labelsRight) > 0) {
      dummy <- data.frame(text = paste(labelsRight, collapse = "\n"))
      plot <- plot + ggplot2::annotate("label", x = 1, y = max(ps$y) * 1,
                                       hjust = "right", vjust = "top",
                                       alpha = 0.8,
                                       label = labelsRight,
                                       #ggplot2::aes(label = labelsRight),
                                       #ggplot2::aes(label = text), data = dummy,
                                       size = 3.5)
      # plot <- plot + ggplot2::geom_label(x = 1, y = max(ps$y) * 1.24,
      #                                    hjust = "right", vjust = "top",
      #                                    alpha = 0.8,
      #                                    ggplot2::aes(label = labelsRight),
      #                                    ggplot2::aes(label = text), data = dummy,
      #                                    size = 3.5)
    }
  }
  if (!is.null(fileName))
    ggplot2::ggsave(fileName, plot, width = 5, height = 3.5,
                    dpi = 400)
  return(plot)
}


returnCamelDf <- function(targetTable, andromedaObject){
  sql <- "SELECT * FROM @target_table"
  sql <- SqlRender::render(sql,
                           target_table = targetTable)
  newData <- RSQLite::dbGetQuery(andromedaObject, sql)
  
  colnames(newData) <- SqlRender::snakeCaseToCamelCase(colnames(newData))
  camelCaseName <- targetTable %>%
    stringr::str_remove("^cm_") %>%
    stringr::str_remove("^es_") %>%
    SqlRender::snakeCaseToCamelCase()
  
  assign(camelCaseName, newData, envir = .GlobalEnv)
  
  invisible(NULL)
}

plotKaplanMeier <- function(kaplanMeier,
                            targetName,
                            comparatorName,
                            ylims = NULL,
                            xBreaks = NULL,
                            targetColorR = 255/255,
                            targetColorG = 99/255,
                            targetColorB = 71/255,
                            comparatorColorR = 30/255,
                            comparatorColorG = 144/255,
                            comparatorColorB = 255/255,
                            pValue = NULL,
                            pValueLocation = NULL,
                            title = NULL,
                            yLabel = NULL,
                            medianIqr = NULL) {
  data <- rbind(data.frame(time = kaplanMeier$time,
                           s = kaplanMeier$targetSurvival,
                           lower = kaplanMeier$targetSurvivalLb,
                           upper = kaplanMeier$targetSurvivalUb,
                           strata = paste0(" ", targetName, "    ")),
                data.frame(time = kaplanMeier$time,
                           s = kaplanMeier$comparatorSurvival,
                           lower = kaplanMeier$comparatorSurvivalLb,
                           upper = kaplanMeier$comparatorSurvivalUb,
                           strata = paste0(" ", comparatorName)))
  
  if(is.null(xBreaks)){
    xBreaks <- kaplanMeier$time[!is.na(kaplanMeier$targetAtRisk)]
  }else{
    xBreaks <- xBreaks[xBreaks %in% kaplanMeier$time[!is.na(kaplanMeier$targetAtRisk)]]
  }
  #xlims <- c(-max(data$time)/40, max(data$time))
  xlims <- c(-max(xBreaks)/40, max(xBreaks))
  
  if(is.null(ylims)){
    ylims <- c(min(data$lower), max(data$upper))
  }
  xLabel <- "Follow-Up Duration (Days)"
  if(is.null(yLabel)) yLabel <- "Cumulative Incidence"
  
  plot <- ggplot2::ggplot(data, ggplot2::aes(x = time,
                                             y = s,
                                             color = strata,
                                             fill = strata,
                                             ymin = lower,
                                             ymax = upper)) +
    ggplot2::geom_ribbon(color = rgb(0, 0, 0, alpha = 0)) +
    ggplot2::geom_step(size = 1) +
    ggplot2::scale_color_manual(values = c(rgb(targetColorR,targetColorG,targetColorB, alpha = 0.8),
                                           rgb(comparatorColorR,comparatorColorG,comparatorColorB, alpha = 0.8))) +
    ggplot2::scale_fill_manual(values = c(rgb(targetColorR,targetColorG,targetColorB, alpha = 0.3),
                                          rgb(comparatorColorR,comparatorColorG,comparatorColorB, alpha = 0.3))) +
    ggplot2::scale_x_continuous(xLabel, limits = xlims, breaks = xBreaks) +
    ggplot2::scale_y_continuous(yLabel, limits = ylims) +
    theme_bw()+ #remove background
    theme (panel.border = element_blank(), axis.line = element_line())+#only x and y axis, not box
    theme (panel.grid.major.x = element_blank() , #remove vertical grid line
           panel.grid.minor.x = element_blank()  #remove vertical grid line
    )+
    ggplot2::theme(legend.title = ggplot2::element_blank(),
                   legend.position = "top",
                   legend.key.size = ggplot2::unit(1, "lines"),
                   plot.title = ggplot2::element_text(hjust = 0.5)) +
    ggplot2::theme(axis.title.y = ggplot2::element_text(vjust = -10))
  
  if(!is.null(pValue)){
    if(is.null(pValueLocation)|(pValueLocation=="right lower")){
      plot <- plot+ggplot2::annotate("text", label = pValue, parse =T,
                                     x=Inf,
                                     y=-Inf,
                                     hjust=2,
                                     vjust=-2,
                                     color = "black")
    }
    if(pValueLocation=="left middle"){
      plot <- plot+ggplot2::annotate("text", label = pValue, parse =T,
                                     x = -Inf,
                                     y = Inf,
                                     hjust = -0.5, #-1
                                     vjust = 10,
                                     color = "black")
    }
    if(pValueLocation=="left upper"){
      plot <- plot+ggplot2::annotate("text", label = pValue, parse =T,
                                     x = -Inf,
                                     y = Inf,
                                     hjust = -0.5, #-1
                                     vjust = 3,
                                     color = "black")
    }
  }
  if(!is.null(title)){
    plot <- plot+ggplot2::ggtitle(title)
  }
  
  if(is.null(xBreaks)){
    targetAtRisk <- kaplanMeier$targetAtRisk[!is.na(kaplanMeier$targetAtRisk)]
    comparatorAtRisk <- kaplanMeier$comparatorAtRisk[!is.na(kaplanMeier$comparatorAtRisk)]
  } else {
    targetAtRisk <- kaplanMeier$targetAtRisk[(!is.na(kaplanMeier$targetAtRisk))&(kaplanMeier$time%in%xBreaks)]
    comparatorAtRisk <- kaplanMeier$comparatorAtRisk[!is.na(kaplanMeier$comparatorAtRisk)&(kaplanMeier$time%in%xBreaks)]
  }
  
  labels <- data.frame(x = c(0, xBreaks, xBreaks),
                       y = as.factor(c("Number at risk",
                                       rep(targetName, length(xBreaks)),
                                       rep(comparatorName, length(xBreaks)))),
                       label = c("",
                                 formatC(targetAtRisk, big.mark = ",", mode = "integer"),
                                 formatC(comparatorAtRisk, big.mark = ",", mode = "integer")))
  labels$y <- factor(labels$y, levels = c(comparatorName, targetName, "Number at risk"))
  dataTable <- ggplot2::ggplot(labels,
                               ggplot2::aes(x = x, y = y, label = label)
  ) +
    ggplot2::geom_text(size = 3.5, vjust = 0.5) +
    ggplot2::scale_x_continuous(xLabel,
                                limits = xlims,
                                breaks = xBreaks) +
    ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   legend.position = "none",
                   panel.border = ggplot2::element_blank(),
                   panel.background = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_text(color = "white"),
                   axis.title.x = ggplot2::element_text(color = "white"),
                   axis.title.y = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_line(color = "white")
    )
  plots <- list(plot, dataTable)
  grobs <- widths <- list()
  for (i in 1:length(plots)) {
    grobs[[i]] <- ggplot2::ggplotGrob(plots[[i]])
    widths[[i]] <- grobs[[i]]$widths[2:5]
  }
  maxwidth <- do.call(grid::unit.pmax, widths)
  for (i in 1:length(grobs)) {
    grobs[[i]]$widths[2:5] <- as.list(maxwidth)
  }
  plot <- gridExtra::grid.arrange(grobs[[1]], grobs[[2]], heights = c(400, 100))
  
  
  return(plot)
}

#split table to list by column group
# split_tibble <- function(tibble, col = 'col') tibble %>% split(., .[, col])
splitTable <- function(table, column = 'col') {
  table %>% split(., .[,column]) %>% lapply(., function(x) x[,setdiff(names(x),column)])
}

# Load meta-analysis data from data folder:
loadMetaFile <- function(file) {
  tableName <- gsub(".*\\/es_", "", file)
  tableName <- gsub("\\.csv", "", tableName)
  camelCaseName <- SqlRender::snakeCaseToCamelCase(tableName)
  newData <- read.csv(file)
  colnames(newData) <- SqlRender::snakeCaseToCamelCase(colnames(newData))
  # if (exists(camelCaseName, envir = .GlobalEnv)) {
  #   existingData <- get(camelCaseName, envir = .GlobalEnv)
  #   newData <- rbind(existingData, newData)
  # }
  assign(camelCaseName, newData, envir = .GlobalEnv)
  invisible(NULL)
}

#Create/prepare table 1
prepareTable1 <- function(balance,
                          beforeLabel = "Before stratification",
                          afterLabel = "After stratification",
                          targetLabel = "Target",
                          comparatorLabel = "Comparator",
                          percentDigits = 1,
                          stdDiffDigits = 2,
                          output = "latex",
                          pathToCsv = "./inst/Table1Specs.csv") {
  if (output == "latex") {
    space <- " "
  } else {
    space <- "&nbsp;"
  }
  specifications <- read.csv(pathToCsv, stringsAsFactors = FALSE)
  
  fixCase <- function(label) {
    idx <- (toupper(label) == label)
    if (any(idx)) {
      label[idx] <- paste0(substr(label[idx], 1, 1),
                           tolower(substr(label[idx], 2, nchar(label[idx]))))
    }
    return(label)
  }
  
  formatPercent <- function(x) {
    result <- format(round(100 * x, percentDigits), digits = percentDigits + 1, justify = "right")
    result <- gsub("^-", "<", result)
    result <- gsub("NA", "", result)
    result <- gsub(" ", space, result)
    return(result)
  }
  
  formatStdDiff <- function(x) {
    result <- format(round(x, stdDiffDigits), digits = stdDiffDigits + 1, justify = "right")
    result <- gsub("NA", "", result)
    result <- gsub(" ", space, result)
    return(result)
  }
  
  resultsTable <- data.frame()
  for (i in 1:nrow(specifications)) {
    if (specifications$analysisId[i] == "") {
      resultsTable <- rbind(resultsTable,
                            data.frame(Characteristic = specifications$label[i], value = ""))
    } else {
      idx <- balance$analysisId == specifications$analysisId[i]
      if (any(idx)) {
        if (specifications$covariateIds[i] != "") {
          covariateIds <- as.numeric(strsplit(specifications$covariateIds[i], ";")[[1]])
          idx <- balance$covariateId %in% covariateIds
        } else {
          covariateIds <- NULL
        }
        if (any(idx)) {
          balanceSubset <- balance[idx, ]
          if (is.null(covariateIds)) {
            balanceSubset <- balanceSubset[order(balanceSubset$covariateId), ]
          } else {
            balanceSubset <- merge(balanceSubset, data.frame(covariateId = covariateIds,
                                                             rn = 1:length(covariateIds)))
            balanceSubset <- balanceSubset[order(balanceSubset$rn, balanceSubset$covariateId), ]
          }
          balanceSubset$covariateName <- fixCase(gsub("^.*: ", "", balanceSubset$covariateName))
          if (specifications$covariateIds[i] == "" || length(covariateIds) > 1) {
            resultsTable <- rbind(resultsTable, data.frame(Characteristic = specifications$label[i],
                                                           targetMeanBefore = NA,
                                                           comparatorMeanBefore = NA,
                                                           stdDiffBefore = NA,
                                                           targetMeanAfter = NA,
                                                           comparatorMeanAfter = NA,
                                                           stdDiffAfter = NA,
                                                           stringsAsFactors = FALSE))
            resultsTable <- rbind(resultsTable, data.frame(Characteristic = paste0(space,
                                                                                   space,
                                                                                   space,
                                                                                   space,
                                                                                   balanceSubset$covariateName),
                                                           targetMeanBefore = balanceSubset$targetMeanBefore,
                                                           comparatorMeanBefore = balanceSubset$comparatorMeanBefore,
                                                           stdDiffBefore = balanceSubset$stdDiffBefore,
                                                           targetMeanAfter = balanceSubset$targetMeanAfter,
                                                           comparatorMeanAfter = balanceSubset$comparatorMeanAfter,
                                                           stdDiffAfter = balanceSubset$stdDiffAfter,
                                                           stringsAsFactors = FALSE))
          } else {
            resultsTable <- rbind(resultsTable, data.frame(Characteristic = specifications$label[i],
                                                           targetMeanBefore = balanceSubset$targetMeanBefore,
                                                           comparatorMeanBefore = balanceSubset$comparatorMeanBefore,
                                                           stdDiffBefore = balanceSubset$stdDiffBefore,
                                                           targetMeanAfter = balanceSubset$targetMeanAfter,
                                                           comparatorMeanAfter = balanceSubset$comparatorMeanAfter,
                                                           stdDiffAfter = balanceSubset$stdDiffAfter,
                                                           stringsAsFactors = FALSE))
          }
        }
      }
    }
  }
  resultsTable$targetMeanBefore <- formatPercent(resultsTable$targetMeanBefore)
  resultsTable$comparatorMeanBefore <- formatPercent(resultsTable$comparatorMeanBefore)
  resultsTable$stdDiffBefore <- formatStdDiff(resultsTable$stdDiffBefore)
  resultsTable$targetMeanAfter <- formatPercent(resultsTable$targetMeanAfter)
  resultsTable$comparatorMeanAfter <- formatPercent(resultsTable$comparatorMeanAfter)
  resultsTable$stdDiffAfter <- formatStdDiff(resultsTable$stdDiffAfter)
  
  headerRow <- as.data.frame(t(rep("", ncol(resultsTable))))
  colnames(headerRow) <- colnames(resultsTable)
  headerRow$targetMeanBefore <- targetLabel
  headerRow$comparatorMeanBefore <- comparatorLabel
  headerRow$targetMeanAfter <- targetLabel
  headerRow$comparatorMeanAfter <- comparatorLabel
  
  subHeaderRow <- as.data.frame(t(rep("", ncol(resultsTable))))
  colnames(subHeaderRow) <- colnames(resultsTable)
  subHeaderRow$Characteristic <- "Characteristic"
  subHeaderRow$targetMeanBefore <- "%"
  subHeaderRow$comparatorMeanBefore <- "%"
  subHeaderRow$stdDiffBefore <- "Std. diff"
  subHeaderRow$targetMeanAfter <- "%"
  subHeaderRow$comparatorMeanAfter <- "%"
  subHeaderRow$stdDiffAfter <- "Std. diff"
  
  resultsTable <- rbind(headerRow, subHeaderRow, resultsTable)
  
  colnames(resultsTable) <- rep("", ncol(resultsTable))
  colnames(resultsTable)[2] <- beforeLabel
  colnames(resultsTable)[5] <- afterLabel
  return(resultsTable)
}

# plotting systematic bias assessed by using negative controls outcomes
plotScatter <- function(controlResults,
                        onlyUncalibrated = F) {
  size <- 2
  labelY <- 0.7
  d <- rbind(data.frame(yGroup = "Uncalibrated",
                        logRr = controlResults$logRr,
                        seLogRr = controlResults$seLogRr,
                        ci95Lb = controlResults$ci95Lb,
                        ci95Ub = controlResults$ci95Ub,
                        trueRr = controlResults$effectSize),
             data.frame(yGroup = "Calibrated",
                        logRr = controlResults$calibratedLogRr,
                        seLogRr = controlResults$calibratedSeLogRr,
                        ci95Lb = controlResults$calibratedCi95Lb,
                        ci95Ub = controlResults$calibratedCi95Ub,
                        trueRr = controlResults$effectSize))
  d <- d[!is.na(d$logRr), ]
  d <- d[!is.na(d$ci95Lb), ]
  d <- d[!is.na(d$ci95Ub), ]
  if (nrow(d) == 0) {
    return(NULL)
  }
  d$Group <- as.factor(d$trueRr)
  d$Significant <- d$ci95Lb > d$trueRr | d$ci95Ub < d$trueRr
  temp1 <- aggregate(Significant ~ Group + yGroup, data = d, length)
  temp2 <- aggregate(Significant ~ Group + yGroup, data = d, mean)
  temp1$nLabel <- paste0(formatC(temp1$Significant, big.mark = ","), " estimates")
  temp1$Significant <- NULL
  
  temp2$meanLabel <- paste0(formatC(100 * (1 - temp2$Significant), digits = 1, format = "f"),
                            "% of CIs include ",
                            temp2$Group)
  temp2$Significant <- NULL
  dd <- merge(temp1, temp2)
  dd$tes <- as.numeric(as.character(dd$Group))
  
  breaks <- c(0.1, 0.25, 0.5, 1, 2, 4, 6, 8, 10)
  theme <- ggplot2::element_text(colour = "#000000", size = 12)
  themeRA <- ggplot2::element_text(colour = "#000000", size = 12, hjust = 1)
  themeLA <- ggplot2::element_text(colour = "#000000", size = 12, hjust = 0)
  
  d$Group <- paste("True hazard ratio =", d$Group)
  dd$Group <- paste("True hazard ratio =", dd$Group)
  alpha <- 1 - min(0.95 * (nrow(d)/nrow(dd)/50000)^0.1, 0.95)
  
  if(onlyUncalibrated){
    d <- d[d$yGroup=="Uncalibrated",]
    dd <- dd[dd$yGroup=="Uncalibrated",]
  }
  #To reverse the order of graph
  d$yGroup <- factor(d$yGroup, levels = c("Uncalibrated", "Calibrated"))
  dd$yGroup <- factor(dd$yGroup, levels = c("Uncalibrated", "Calibrated"))
  
  plot <- ggplot2::ggplot(d, ggplot2::aes(x = logRr, y = seLogRr), environment = environment()) +
    ggplot2::geom_vline(xintercept = log(breaks), colour = "#AAAAAA", lty = 1, size = 0.5) +
    ggplot2::geom_abline(ggplot2::aes(intercept = (-log(tes))/qnorm(0.025), slope = 1/qnorm(0.025)),
                         colour = rgb(0.8, 0, 0),
                         linetype = "dashed",
                         size = 1,
                         alpha = 0.5,
                         data = dd) +
    ggplot2::geom_abline(ggplot2::aes(intercept = (-log(tes))/qnorm(0.975), slope = 1/qnorm(0.975)),
                         colour = rgb(0.8, 0, 0),
                         linetype = "dashed",
                         size = 1,
                         alpha = 0.5,
                         data = dd) +
    ggplot2::geom_point(size = size,
                        color = rgb(0, 0, 0, alpha = 0.05),
                        alpha = alpha,
                        shape = 16) +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::geom_label(x = log(0.15),
                        y = 0.9,
                        alpha = 1,
                        hjust = "left",
                        ggplot2::aes(label = nLabel),
                        size = 5,
                        data = dd) +
    ggplot2::geom_label(x = log(0.15),
                        y = labelY,
                        alpha = 1,
                        hjust = "left",
                        ggplot2::aes(label = meanLabel),
                        size = 5,
                        data = dd) +
    ggplot2::scale_x_continuous("Hazard ratio",
                                limits = log(c(0.1, 10)),
                                breaks = log(breaks),
                                labels = breaks) +
    ggplot2::scale_y_continuous("Standard Error", limits = c(0, 1)) +
    ggplot2::facet_grid(yGroup ~ Group) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                   panel.background = ggplot2::element_blank(),
                   panel.grid.major = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   axis.text.y = themeRA,
                   axis.text.x = theme,
                   axis.title = theme,
                   legend.key = ggplot2::element_blank(),
                   strip.text.x = theme,
                   strip.text.y = theme,
                   strip.background = ggplot2::element_blank(),
                   legend.position = "none")
  
  return(plot)
}

#Two dimensional plot
gridForest <- function(results, breaks = c(0.9,1,1.1,1.2),
                       outlierMoverLower= 0.03,outlierMoverUpper= 0.1,
                       outlierMoverUse = T,
                       xLimits=c(0.85,1.3),
                       varX = "outcomeName",
                       varY = "TAR",
                       # cols = NULL,
                       xLab = "Definition of the Outcomes",
                       yLab = "Hazard Ratio (95% Confidence Interval)"){
  #hrExpression<-expression("Hazard Ratio (95% Confidence Interval) \n Favor Clopidogrel     Favor Tiacgrelor")
  hrExpression <- yLab
  shapeValue = c(17,21)#shape for closed and open center
  # if(is.null(cols)) cols = as.numeric(results$axis0)
  if (min(as.numeric(results$Significance))==2) shapeValue = c(21,17)
  if(outlierMoverUse){
    resultPlot<-ggplot2::ggplot(data=results,
                                aes(x = axis0,y = rr, ymin =  ci95Lb, ymax = ci95Ub, shape =  Significance))+
      ggplot2::geom_pointrange(aes(col = axis0, shape=Significance), size = 0.6
      )+
      scale_shape_manual(values=shapeValue)+ #shape for closed and open center
      geom_hline(yintercept=1, linetype="dotted")+
      #ggplot2::geom_hline(aes(fill=axis0),yintercept =1, linetype=2)+
      xlab(xLab)+ ylab(hrExpression)+
      ggplot2::geom_errorbar(aes(ymin=ci95Lb, ymax=ci95Ub,col=axis0),width=0.2,cex=1) +
      geom_segment(aes(x = axis0, xend = axis0, y = rr, yend = rr - ci95LbOut-outlierMoverLower,col=axis0),
                   arrow = ggplot2::arrow(angle=45,
                                          unit (0.3,"cm")),
                   size=1, show.legend=FALSE, na.rm = T)+
      geom_segment(aes(x = axis0, xend = axis0, y = rr, yend = rr - ci95UbOut+outlierMoverUpper,col=axis0),
                   arrow = ggplot2::arrow(angle=45,
                                          unit (0.3,"cm")),
                   size=1, show.legend=FALSE, na.rm = T)+
      ggplot2::facet_grid(as.formula(paste0(varX,"~",varY)))+
      #facet_wrap(~matching,strip.position="left",nrow=9,scales = "free_y") +
      ggplot2::theme(plot.title=element_text(size=18,face="bold"),
                     axis.text.y=element_blank(),
                     axis.ticks.y=element_blank(),
                     axis.text.x=element_text(face="bold"),
                     axis.title=element_text(size=18,face="bold"),
                     strip.text.y = element_text(hjust=0,vjust = 1,angle=180,face="bold"),
                     strip.text.x = element_blank()
      )+
      ggplot2::coord_flip(ylim = xLimits)+scale_y_continuous(trans='log10', breaks = breaks
      )+
      ggplot2::theme_bw()+
      ggplot2::theme(axis.text.y=element_blank(),
                     panel.grid.major.y = element_blank(),
                     axis.ticks.y=element_blank())+
      scale_x_discrete(limits=rev(levels(results$axis0))) +
      scale_fill_brewer(palette = "Greens")
  }else{
    resultPlot<-ggplot2::ggplot(data=results,
                                aes(x = axis0,y = rr, ymin =  ci95Lb, ymax = ci95Ub, shape =  Significance))+
      ggplot2::geom_pointrange(aes(col=axis0, shape=Significance), size = 0.6
      )+
      scale_shape_manual(values=shapeValue)+ #shape for closed and open center
      geom_hline(yintercept=1, linetype="dotted")+
      #ggplot2::geom_hline(aes(fill=axis0),yintercept =1, linetype=2)+
      xlab(xLab)+ ylab(hrExpression)+
      ggplot2::geom_errorbar(aes(ymin=ci95Lb, ymax=ci95Ub,col=axis0),width=0.2,cex=1) +
      
      ggplot2::facet_grid(as.formula(paste0(varX,"~",varY)))+
      #facet_wrap(~matching,strip.position="left",nrow=9,scales = "free_y") +
      ggplot2::theme(plot.title=element_text(size=18,face="bold"),
                     axis.text.y=element_blank(),
                     axis.ticks.y=element_blank(),
                     axis.text.x=element_text(face="bold"),
                     axis.title=element_text(size=18,face="bold"),
                     strip.text.y = element_text(hjust=0,vjust = 1,angle=180,face="bold"),
                     strip.text.x = element_blank()
      )+
      ggplot2::coord_flip(ylim = xLimits)+scale_y_continuous(trans='log10', breaks = breaks
      )+
      ggplot2::theme_bw()+
      ggplot2::theme(axis.text.y=element_blank(),
                     panel.grid.major.y = element_blank(),
                     axis.ticks.y=element_blank())+
      scale_x_discrete(limits=rev(levels(results$axis0)))
  }
  return(resultPlot)
}
