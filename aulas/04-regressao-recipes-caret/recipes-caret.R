
# Pacotes -----------------------------------------------------------------

library(recipes)
library(caret)
library(dplyr)


# REGRESSÃO LINEAR --------------------------------------------------------

# Dados -------------------------------------------------------------------

data(diamonds, package = "ggplot2")
glimpse(diamonds)

diamonds <- diamonds %>%
  mutate(
    cut = as.character(cut),
    color = as.character(color),
    clarity = as.character(clarity)
  )

# Criando uma base de 'novos diamantes'

id_new <- sample(nrow(diamonds), size = 1000)
new_diamonds <- diamonds[id_new,]
diamonds <- diamonds[-id_new,]

ggplot(diamonds, aes(x = price)) + 
  geom_histogram(bins = 20)


# Criando a receita -------------------------------------------------------

receita <- recipe(price ~ . , data = diamonds) %>%
  step_dummy(all_nominal()) %>%
  step_nzv(all_predictors()) %>%
  step_corr(all_predictors()) %>% 
  step_log(all_outcomes())

prep <- prep(receita, diamonds)

# Treinando o modelo ------------------------------------------------------

modelo <- train(
  receita, 
  diamonds, 
  method = "lm", 
  trControl = trainControl(method = "cv", number = 5)
)

modelo
summary(modelo$finalModel)
varImp(modelo)


# Fazendo previsões -------------------------------------------------------

new_X <- bake(prep, new_diamonds)

new_X %>% 
  mutate(pred = predict(modelo, new_X)) %>% 
  ggplot(aes(y = pred, x = price)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "blue")



# ÁRVORE DE DECISÃO -------------------------------------------------------

# Criando a receita -------------------------------------------------------

receita <- recipe(price ~ . , data = diamonds) %>%
  step_dummy(
    all_nominal(), 
    one_hot = TRUE
  ) %>%
  step_nzv(all_predictors()) %>%
  step_corr(all_predictors())

prep <- prep(receita, diamonds)

# Treinando o modelo

tune_grid <- data.frame(cp = c(0.01, 0.02, 0.03, 0.04))

modelo <- train(
  receita,
  diamonds,
  method = "rpart",
  trControl = trainControl(method = "cv", number = 5),
  tuneGrid = tune_grid
)

modelo
rpart.plot::rpart.plot(modelo$finalModel)
varImp(modelo)

# Fazendo previsões -------------------------------------------------------

new_X <- bake(prep, new_diamonds)

new_X %>% 
  mutate(pred = predict(modelo, new_X)) %>% 
  ggplot(aes(y = pred, x = price)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "blue")

