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
library(rpart)
library(AUC)

metricas <- function(data, lev = NULL, model = NULL) {
  c(
    defaultSummary(data, lev, model), 
    twoClassSummary(data, lev, model)
  )
}

