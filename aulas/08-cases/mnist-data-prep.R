# Baixar imgs
library(magrittr)
mnist <- keras::dataset_mnist()

x_train <- mnist$train$x[mnist$train$y %in% c(1,7),,]
y_train <- mnist$train$y[mnist$train$y %in% c(1,7)] %>% as.factor()

x_test <- mnist$test$x[mnist$test$y %in% c(1,7),,]
y_test <- mnist$test$y[mnist$test$y %in% c(1,7)] %>% as.factor()

x <- t(apply(x_train, 1, c)) %>% tibble::as_tibble()
x$y <- y_train

x2 <- t(apply(x_test, 1, c)) %>% tibble::as_tibble()
x2$y <- y_test

plot(as.raster(matrix(x[100,], nrow = 28), max = 255))

saveRDS(x, "aulas/08-cases/data/mnist_train.rds")
saveRDS(x2, "aulas/08-cases/data/mnist_test.rds")