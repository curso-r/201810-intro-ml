# pacotes e funcoes auxiliares -------------------------------------#
library(tidyverse)
library(caret)
library(e1071)
library(rpart)
library(rpart.plot)
library(recipes)

gini <- function(.data, .resposta, .pergunta) {
  .resposta <- enquo(.resposta)
  .pergunta <- enquo(.pergunta)
  
  .data %>% 
    count(!!.resposta, !!.pergunta) %>% 
    group_by(!!.pergunta) %>%
    mutate(p = n/sum(n)) %>%
    summarise(
      n = sum(n),
      gini = 1 - sum(p^2)
    ) %$% 
    sum(n * gini)
}

grafico_de_cp <- function(.arvore) {
  .arvore$cptable %>%
    as.data.frame %>%
    ggplot() +
    geom_ribbon(aes(x = CP, ymin = xerror - xstd, ymax = xerror + xstd), alpha = 0.3) +
    geom_line(aes(x = CP, y = xerror),colour = "red") +
    scale_x_reverse()
}



#
iris %<>% mutate(base = if_else(runif(n()) > 0.9, "teste", "treino"))
arvore_rpart <- rpart(
  Species ~ ., 
  data = iris, 
  control = rpart.control(
    minsplit = 1, 
    minbucket = 1, 
    maxdepth = 10
  ),
  parms = list(split = "gini")
)

rpart.plot(arvore_rpart)
summary(arvore_rpart)
arvore_rpart$cptable
arvore_rpart$splits
rpart:::predict.rpart(arvore_rpart, type = "prob")

arvore_rpart2 <- prune(arvore_rpart, cp = 0.02)
rpart.plot(arvore_rpart2)
