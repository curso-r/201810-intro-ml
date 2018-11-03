load("titanic.RData")


train_control_model02 <- trainControl(
  method = "cv"
  ,number = 5
  ,verboseIter = TRUE
  ,classProbs = TRUE
  ,summaryFunction = myTwoClassSummary
  ,allowParallel = FALSE
)

len = 400
tune_grid_model02 <- data.frame(
  nrounds = sample(300:1000, size = len, replace = TRUE),
  max_depth = sample(3:9, replace = TRUE, size = len),
  eta = runif(len, min = .1, max = 1),
  gamma = runif(len, min = 0.01, max = 1),
  colsample_bytree = runif(len, min = .6, max = .9),
  min_child_weight = sample(5:20, size = len, replace = TRUE),
  subsample = runif(len, min = .75, max = 0.95)
)
tune_grid_model02$nrounds <- floor(tune_grid_model02$nrounds)
tune_grid_model02 <- tune_grid_model02[!duplicated(tune_grid_model02),]


model02 <- train(
  Survived ~ 1
  
  + flag_quotes
  + flag_mr
  + flag_miss
  + flag_master
  + flag_0_sibsp
  + flag_0_parch
  + pronoun_v2
  + flag_parenteses
  + Sex
  + age_status
  + factor(Pclass)
  # + flag_1_sibsp
  + SibSp
  + parch_v2
  + family_size
  + Fare
  + flag_has_cabin
  + age_v2
  + Embarked
  
  ,data = titanic %>% dplyr::filter(base %in% "train")
  
  , method = "xgbTree"
  , preProcess = NULL
  # , metric = c("ROC", "F1")
  , trControl = train_control_model02
  , tuneGrid = tune_grid_model02
  # , preProc=c("center", "scale")
)

save(model02, file = "model02.RData")
