nvars = 10
nobs = 4000

dados <- as.data.frame(matrix(rnorm(nvars * nobs), ncol = nvars)) %>%
  mutate(y = if_else(apply(., 2, function(x) x^2) %>% rowSums > rchisq(n(), nvars), 1, 0),
         base = if_else(runif(n()) > 0.5, "teste", "treino"))

