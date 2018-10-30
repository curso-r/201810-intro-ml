# DADOS
set.seed(123456)
dados <- tibble(x = runif(100, -2, 2)) %>%#runif(200) - 0.5) %>%
  mutate(y = x - sin(pi * x) + rnorm(n(), 0, 0.25)) %>%
  mutate(y = if_else(runif(n()) < 0.11 & x %>% between(-0.5, 0), rnorm(n(), 3, 0.01), y)) %>%
  mutate(y = if_else(runif(n()) < 0.11 & x %>% between(1, 1.5), rnorm(n(), -2, 0.01), y))

# relacao  y ~ x
dados %>% ggplot(aes(x, y)) + geom_point()

# amostras bootstrap
bootstraps <- tibble(id = 1:100) %>%
  mutate(
    amostra = replicate(n(), sample(nrow(dados), 50, replace = TRUE), simplify = FALSE),
    modelo_linear = map(amostra, ~ lm(y ~ x, data = dados[.x, ])),
    # modelo_I_x_20_poly_2 = map(amostra, ~ lm(y ~ I(x < 20 ) * poly(x, 2), data = dados[.x, ])),
    modelo_arvore_profundidade_1 = map(amostra, ~ rpart(y ~ x, data = dados[.x, ], control = rpart.control(2, 1, maxdepth = 1, cp = 0.05))),
    modelo_arvore_profundidade_3 = map(amostra, ~ rpart(y ~ x, data = dados[.x, ], control = rpart.control(2, 1, maxdepth = 3, cp = 0.05))),
    modelo_arvore_profundidade_9 = map(amostra, ~ rpart(y ~ x, data = dados[.x, ], control = rpart.control(2, 1, maxdepth = 9, cp = 0.05))),
  ) %>%
  gather(modelo, modelo_objeto, -id, -amostra)

# escoragem em dados novos
dados_novos <- tibble(x = seq(-2, 2, by = 0.05))

predicoes <- bootstraps %>% 
  mutate(
    pred = map(modelo_objeto, ~ dados_novos %>% mutate(pred = predict(.x, .))),
  ) 

# grafico
predicoes %>%
  select(id, modelo, pred) %>%
  unnest %>%
  ggplot() +
  geom_line(aes(x = x, y = pred, group = paste(id, modelo), colour = modelo)) +
  geom_point(data = dados, aes(x = x, y = y)) +
  facet_wrap(~modelo) +
  theme_grey(20)

