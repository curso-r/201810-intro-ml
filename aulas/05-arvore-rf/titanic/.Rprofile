#devtools::install_github("Thie1e/cutpointr")
library(cutpointr)
library(stringdist)
library(proxy)
library(pROC)
library(MLmetrics)
library(caret)
library(tidyverse)
library(magrittr)
library(glmnet)

myTwoClassSummary <- function(data, lev = NULL, model = NULL) {
  lvls <- levels(data$obs)
  if (length(lvls) > 2) 
    stop(paste("Your outcome has", length(lvls), "levels. The twoClassSummary() function isn't appropriate."))
  if (!all(levels(data[, "pred"]) == lvls)) 
    stop("levels of observed and predicted data do not match")
  data$y = as.numeric(data$obs == lvls[2])
  
  rocAUC <- ModelMetrics::auc(ifelse(data$obs == lev[2], 0, 
                                     1), data[, lvls[1]])
  out <- c(rocAUC, 
           sensitivity(data[, "pred"], data[, "obs"], lev[1]), 
           specificity(data[, "pred"], data[, "obs"], lev[2]),
           F1_Score(y_pred = data$pred, y_true = data$obs, positive = lev[1]),
           KS_Stat(y_pred = data$yes, y_true = data$obs))
  names(out) <- c("ROC", "Sens", "Spec", "F1", "KS")
  out
}
