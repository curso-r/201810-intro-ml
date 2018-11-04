dados <- data.frame(x = runif(1000)) %>%
  mutate(y = if_else(x < 0.5, -2, 2) + rnorm(n(), 0, 1))

dados %>%
  ggplot() +
  geom_point(aes(x = x, y = y), alpha = 0.1) +
  geom_step(data = data.frame(x = c(0, 0.3, 1), y = c(-2, 1, 1)), aes(x = x, y = y)) +
  geom_vline(xintercept = 0.3, colour = "red")


variancia <- function(s) {
  dados %>%
    mutate(split = x < s) %>%
    group_by(split) %>%
    summarise(n = length(y), 
              media = mean(y),
              variancia = var(y),
              n_vezes_variancia = n * variancia)
}

grafico_dispersao <- function(s, tab_variancia) {
  dados %>%
    ggplot() +
    geom_point(aes(x = x, y = y), alpha = 0.1) +
    geom_step(data = data.frame(x = c(0, s, 1), y = tab_variancia$media[c(2,1,1)]), aes(x = x, y = y)) +
    geom_vline(xintercept = s, colour = "red") +
    lims(y = c(-5.3, 5.3), x = c(0, 1)) 
}

grafico_variancia <- function(ss, variancias) {
  tibble(
    variancia = variancias,
    split = ss
  ) %>%
    ggplot(aes(x = split, y = variancia)) +
    geom_point() +
    geom_line() +
    geom_vline(xintercept = ss %>% last, colour = "red") +
    lims(y = c(1000, 4900), x = c(0, 1))
}
library(patchwork)

splits <- tibble(x = seq(0.05, 0.95, by = 0.05)) %>%
  mutate(variancia = map(x, variancia),
         grafico_dispersao = map2(x, variancia, grafico_dispersao),
         variancia_acum = variancia %>% map("n_vezes_variancia") %>% map_dbl(sum) %>% purrr::accumulate(~c(.x, .y)),
         x_acum = x %>% purrr::accumulate(~c(.x, .y)),
         grafico_variancia = map2(x_acum, variancia_acum, grafico_variancia),
         grafico_final = map2(grafico_dispersao, grafico_variancia, ~ .x + .y + plot_layout(ncol = 1)))

splits$grafico_final[[2]]

install.packages("animation")
library(animation)

saveGIF(
  walk(splits$grafico_final, print),
  movie.name = "variancia_de_um_split.gif"
)
