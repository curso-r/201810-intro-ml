library(tidyverse)
library(caret)
library(e1071)
library(rpart)
library(rpart.plot)
library(recipes)

pacientes <- tribble(
  ~paciente, ~pressao, ~glicose, ~diabetes,
  "Alfredo", "hipertensao",  92, "nao",
  "Beatriz", "normal",      130, "sim",
  "Carla",   "normal",      130, "nao",
  "Daniela", "normal",       55, "nao",
  "Ernesto", "hipertensao", 220, "sim",
  "Flavia",  "normal",      195, "sim",
)

primeira_decisao <- pacientes %>%
  mutate(
    p1 = glicose < 73.5,
    p2 = glicose < 111,
    p3 = glicose < 162.5,
    p4 = glicose < 207.5,
    p5 = pressao == "normal"
  ) %>%
  gather(pergunta, flag_pergunta, matches("p[0-9]$")) %>%
  group_by(pergunta) %>%
  nest %>%
  mutate(
    contagem_de_sims = map(data, ~ {
      .x %>% 
        group_by(flag_pergunta) %>% 
        summarise(
          sim = sum(diabetes %in% "sim"),
          n = length(diabetes)
        ) %>% 
        gather(sumario, valor, sim, n) %>%
        mutate(flag_pergunta = if_else(flag_pergunta, "esquerda", "direita")) %>%
        unite(flag_pergunta, flag_pergunta, sumario) %>%
        spread(flag_pergunta, valor)
    })
  ) %>%
  unnest(contagem_de_sims)

primeira_decisao %>%
  mutate(
    esquerda_gini = ,
    direita_gini
  )


arvore <- rpart(
  diabetes ~ pressao + glicose, 
  data = pacientes, 
  control = rpart.control(
    minsplit = 1, 
    minbucket = 1, 
    cp = -1, 
    maxdepth = 10
  )
)
rpart.plot(arvore)
summary(arvore)
arvore$cptable
arvore$splits


# info_rpart <- caret::getModelInfo("rpart", FALSE)
tune_grid <- data.frame(cp = 0)#2^c(-10:10)[1])
train_control <- trainControl(
  method = "none",
  summaryFunction = twoClassSummary,
  classProbs = TRUE
)

modelo <- train(
    diabetes ~ sexo + glicose,
    data = pacientes,
    method = "rpart",
    control = rpart.control(minsplit = 2, minbucket = 1),
    trControl = train_control,
    tuneGrid = tune_grid
  )

modelo$finalModel %>% rpart.plot
