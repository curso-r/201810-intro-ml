load("aulas/07-arvore-gbm/meu_projeto/RData/dados.RData")
load("aulas/07-arvore-gbm/meu_projeto/RData/modelo01.RData")

dados_predicted <- dados %>% 
  mutate(score_modelo01 = predict(model01, newdata = ., type = "prob")$sim)

save(dados_predicted, file = "aulas/06-arvore-rf/dados/RData/dados_predicted.RData")

