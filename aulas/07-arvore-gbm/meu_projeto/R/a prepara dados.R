set.seed(1)
nvars = 10
nobs = 4000

dados <- as.data.frame(matrix(rnorm(nvars * nobs), ncol = nvars)) %>%
  mutate(y = if_else(apply(., 2, function(x) x^2) %>% rowSums > rchisq(n(), nvars), "sim", "nao"),
         base = if_else(runif(n()) > 0.5, "teste", "treino"))


save(dados, file = "aulas/07-arvore-gbm/meu_projeto/RData/dados.RData")
