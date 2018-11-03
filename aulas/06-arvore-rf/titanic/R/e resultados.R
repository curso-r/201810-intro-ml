load("titanic_predicted.RData")
load("model01.RData")
load("model02.RData")

model01$results %>% as.tibble()
model01$metric

library(pROC)

titanic_predicted %>%
  dplyr::filter(base %in% "train") %>%
  roc(Survived ~ score_model02, data = .) %>%
  plot

# 
# titanic_predicted %>% 
#   dplyr::filter(base %in% "train") %>%
#   cutpointr(score_model01, Survived)

titanic_predicted %>% 
  dplyr::filter(base %in% "train") %>%
  cutpointr(score_model02, Survived)
# 
# titanic_predicted %>% 
#   dplyr::filter(base %in% "train") %>%
#   cutpointr(score_modelXX, Survived)
