# dados e funcoes auxiliares --------------------------------------------------------#
library(dplyr)
library(recipes)
library(caret)
library(rsample)
library(skimr)
set.seed(123)

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
    base = if_else(runif(n()) > 0.8, "teste", "treino")
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

# EXERCÍCIO 3: REPLIQUE O EXERCÍCIO 2, TROCANDO O NECESSÁRIO PARA AJUSTAR UM 
# RANDOM FOREST EM VEZ DE UMA ÁRVORE. VERIFIQUE QUAL DOS MODELOS TEVE MELHOR PERFORMACE.
# NÃO ESQUEÇA DE COMPARAR AS MEDIDAS DE KS.

# EXERCICIO ESTRELINHA DE OURO: FAÇA UM GRÁFICO QUE ENDOSSE SUA DECISÃO DE MELHOR MODELO.

