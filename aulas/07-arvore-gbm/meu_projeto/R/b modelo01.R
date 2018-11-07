load(file = "aulas/07-arvore-gbm/meu_projeto/RData/dados.RData")

train_control_modelo01 <- trainControl(
  method = "cv"
  ,number = 5
  ,verboseIter = TRUE
  ,classProbs = TRUE
  ,summaryFunction = metricas
  ,allowParallel = FALSE
)

tune_grid_modelo01 <- expand.grid(

)

modelo01 <- train(
  formula =
  ,data = 
  
  , method = ""
  , metric = ""
  , trControl = train_control_modelo01
  , tuneGrid = tune_grid_modelo01
)

save(modelo01, file = "aulas/07-arvore-gbm/meu_projeto/RData/modelo01.RData")
