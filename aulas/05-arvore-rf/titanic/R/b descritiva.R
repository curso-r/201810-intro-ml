titanic %>% 
  dplyr::filter(base %in% "train") %>% 
  count(Parch, Survived) %>% 
  spread(Survived, n) %>%
  mutate(p1 = yes/(no + yes)) 

titanic %>% 
  dplyr::filter(base %in% "train") %>% 
  ggplot() +
  stat_summary(aes(x = Fare, y = as.numeric(as.factor(Survived)) - 1), method = "glm", method.args = list(family = "binomial"), fun.y = "mean", geom = "point")


titanic %>% summarise_all(~sum(is.na(.x))) %>% t

