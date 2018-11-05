# dados e funcoes auxiliares --------------------------------------------------------#
library(dplyr)
library(recipes)
library(caret)
library(rsample)
library(skimr)
set.seed(123)


ks <- function(pred, obs) {
  obs_lbls <- unique(obs)
  ks.test(pred[obs == obs_lbls[1]], pred[obs == obs_lbls[2]])$statistic
}

metricas <- function(data, lev = NULL, model = NULL) {
  c(
    defaultSummary(data, lev, model), 
    twoClassSummary(data, lev, model)
  )
}

data("credit_data")
glimpse(credit_data)


# data prep e receita --------------------------------------------------------#
credit_data <- credit_data %>%
  mutate(
    Status = forcats::lvls_reorder(Status, c(2, 1)),
    base = if_else(runif(n()) > 0.9, "teste", "treino")
  )

credit_receita <- recipe(
  Status ~ ., 
  data = credit_data %>% filter(base %in% "treino") %>% select(-base) 
) %>%
  step_meanimpute(all_numeric(), -all_outcomes()) %>%
  step_modeimpute(all_nominal(), -all_outcomes()) %>%
  step_dummy(all_nominal(), -all_outcomes()) %>%
  step_center(all_predictors()) %>%
  step_scale(all_predictors())


# modelagem via CARET --------------------------------------------------------#
# EXERCÍCIO: Preencha as áreas indicadas.
credit_tune_grid <- expand.grid(
  # PREENCHA AQUI!
  cp =
)

credit_train_control <- trainControl(
  method = "cv", 
  number = 5, 
  classProbs = TRUE,
  summaryFunction = metricas, # Atenção aqui. Volte nesta função para consultar as métricas.
  savePredictions = TRUE,
  verboseIter = TRUE # novidade
)

credit_arvore <- train(
  credit_receita, 
  credit_data %>% filter(base %in% "treino") %>% select(-base),
  method = "rpart",# PREENCHA AQUI!
  trControl = credit_train_control, 
  metric = "ROC",# PREENCHA AQUI! 
  tuneGrid = credit_tune_grid
)

credit_arvore
credit_arvore$bestTune
rpart.plot(credit_arvore$finalModel)
varImp(credit_arvore)


# gráfico de CP vs ROC
credit_arvore$results %>%
  ggplot() +
  geom_ribbon(aes(x = cp, ymax = ROC + ROCSD, ymin = ROC - ROCSD), alpha = 0.3) +
  geom_linerange(aes(x = cp, ymax = ROC + ROCSD, ymin = ROC - ROCSD), alpha = 0.3) +
  geom_line(aes(x = cp, y = ROC), colour = "royalblue") +
  geom_vline(xintercept = credit_arvore$bestTune$cp, colour = "red") +
  scale_x_reverse()




# BONUS: gráfico de KS
credit_prep <- prep(credit_receita)

base_com_pred <- bake(credit_prep, credit_data) %>%
  mutate(
    base = credit_data$base,
    pred = predict(credit_arvore$finalModel, newdata = ., type = "prob")[,"bad"]
  )


base_com_pred %>%
  ggplot(aes(x = pred, colour = Status)) +
  stat_ecdf() +
  facet_wrap(~base)

base_com_pred%>%
  group_by(base) %>%
  summarise(ks = ks(pred, Status))





# BONUS: uma maneira de tomar decisão com as predições
base_com_pred %>%
  filter(base %in% "teste") %>%
  mutate(Status_num = if_else(Status == "bad", 1, 0)) %>%
  ggplot() +
  stat_summary_bin(aes(x = pred, y = Status_num), fun.y = "mean", geom = "line", bins = 5) +
  stat_summary_bin(aes(x = pred, y = Status_num), fun.y = "mean", geom = "point", bins = 5)
