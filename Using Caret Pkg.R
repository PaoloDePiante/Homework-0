library(Rborist)
library(tidyverse)
library(randomForest)
library(caret)

n <- 1000
sigma <- 0.25
x <- rnorm(n, 0, 1)
y <- 0.75 * x + rnorm(n, 0, sigma)
dat <- data.frame(x = x, y = y)

set.seed(1)

fit <- train(y ~ ., 
             method="Rborist", 
             tuneGrid= data.frame(predFixed = 1, minNode= seq(25,100,25)), 
             data = dat)


dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col = 2) 


#altro esercizio

library(dslabs)
data("tissue_gene_expression")
x <- tissue_gene_expression$x
y <- tissue_gene_expression$y
set.seed(1991)

fit <- train(x,y, method="rpart", tuneGrid=data.frame(cp=seq(0, 0.1, 0.01)))

plot(fit)

fit$bestTune

#------------

confusionMatrix(fit)

