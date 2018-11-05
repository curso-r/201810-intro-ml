# DADOS
set.seed(123456)
dados <- tibble(x = runif(100, -2, 2)) %>%#runif(200) - 0.5) %>%
  mutate(y = x - sin(pi * x) + rnorm(n(), 0, 0.25)) %>%
  mutate(y = if_else(runif(n()) < 0.11 & x %>% between(-0.5, 0), rnorm(n(), 3, 0.01), y)) %>%
  mutate(y = if_else(runif(n()) < 0.11 & x %>% between(1, 1.5), rnorm(n(), -2, 0.01), y))

# relacao  y ~ x
dados %>% ggplot(aes(x, y)) + geom_point()

# amostras bootstrap
bootstraps <- tibble(id = 1:1) %>%
  mutate(
    amostra = replicate(n(), sample(nrow(dados), 5000, replace = TRUE), simplify = FALSE),
    modelo_linear = map(amostra, ~ lm(y ~ x, data = dados[.x, ])),
    modelo_correto = map(amostra, ~ lm(y ~ x + sin(pi * x), data = dados[.x, ])),
    modelo_arvore_profundidade_1 = map(amostra, ~ rpart(y ~ x, data = dados[.x, ], control = rpart.control(2, 1, maxdepth = 1, cp = 0.05))),
    modelo_arvore_profundidade_3 = map(amostra, ~ rpart(y ~ x, data = dados[.x, ], control = rpart.control(2, 1, maxdepth = 3, cp = 0.05))),
    modelo_arvore_profundidade_9 = map(amostra, ~ rpart(y ~ x, data = dados[.x, ], control = rpart.control(2, 1, maxdepth = 9, cp = 0.02))),
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
  filter(modelo %in% c("modelo_arvore_profundidade_9", "modelo_linear")) %>%
  ggplot() +
  geom_line(aes(x = x, y = pred, group = paste(id, modelo), colour = modelo), size = 2) +
  geom_point(data = dados, aes(x = x, y = y)) +
  theme_grey(20)

