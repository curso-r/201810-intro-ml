load("titanic.RData")




train_control_model01 <- trainControl(
   method = "cv"
  ,number = 5
  ,verboseIter = TRUE
  ,classProbs = TRUE
  ,summaryFunction = myTwoClassSummary
  ,allowParallel = FALSE
)

tune_grid_model01 <- expand.grid(
  lambda = c(0, 2^runif(50, min = -10, 1)),
  alpha = c(0,1)
)

model01 <- train(
  Survived ~ 1
  
  + (flag_quotes
  + flag_mr
  + flag_miss
  + flag_master
  + flag_parenteses
  + Sex
  + age_status
  + Pclass
  + SibSp
  + parch_v2
  + Fare
  + flag_has_cabin
  + age_v2
  + I(age_v2^2))^2
  
  ,data = titanic %>% dplyr::filter(base %in% "train")
  
  , method = "glmnet"
  , preProcess = NULL
  # , metric = c("ROC", "F1")
  , trControl = train_control_model01
  , tuneGrid = tune_grid_model01
  # , preProc=c("center", "scale")
)

save(model01, file = "model01.RData")
