# pacotes e funcoes auxiliares -------------------------------------#
library(tidyverse)
library(caret)
library(e1071)
library(rpart)
library(rpart.plot)
library(recipes)

# dados
iris2 <- iris %>% mutate(base = if_else(runif(n()) > 0.9, "teste", "treino"))

# modelo de arvore
arvore_iris <- rpart(
  Species ~ ., 
  data = iris, 
  control = rpart.control(
    minsplit = 1, 
    minbucket = 1, 
    maxdepth = 10,
    cp = -1
  ),
  parms = list(split = "gini")
)

# grafico da árvore
rpart.plot(arvore_iris)

# Saídas úteis
summary(arvore_iris)
arvore_iris$cptable
arvore_iris$splits
predict(arvore_iris, type = "prob") %>% head


########################################################################################
# Exercício1: descubra qual valor de cp necessário para fazer a árvore ter apenas UMA quebra.

# Exercício2: use o valor de cp descoberto e faça a "poda". Confirme se a árvore foi podada graficamente.
arvore_podada <- prune(arvore_iris, cp = 0)
rpart.plot(arvore_podada)
