library(caret)
library(tidyverse)
library(dplyr)
library(skimr)

# create the data set
set.seed(1) 
n <- 100
Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2) 
dat <- MASS::mvrnorm(n = 500, c(69, 69), Sigma) %>% data.frame() %>% setNames(c("x", "y")) 
set.seed(1)
rmse <- replicate(n, { test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
train_set <- dat %>% slice(-test_index)
test_set <- dat %>% slice(test_index)
fit <- lm(y ~ x, data = train_set)
y_hat <- predict(fit, test_set)
sqrt(mean((y_hat - test_set$y)^2)) 
})

mean(rmse)
sd(rmse)

skim(dat)
#-------------------------------------


set.seed(1)

n <- c(100, 500, 1000, 5000, 10000)
res <- sapply(n, function(n){
  Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
  dat <- MASS::mvrnorm(n, c(69, 69), Sigma) %>%
    data.frame() %>% setNames(c("x", "y"))
  rmse <- replicate(100, {
    test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
    train_set <- dat %>% slice(-test_index)
    test_set <- dat %>% slice(test_index)
    fit <- lm(y ~ x, data = train_set)
    y_hat <- predict(fit, newdata = test_set)
    sqrt(mean((y_hat-test_set$y)^2))
  })
  c(avg = mean(rmse), sd = sd(rmse))
})

res

# secondo esercizio con una matrice di correlazione maggiore

set.seed(1)
n <- 100
Sigma <- 9*matrix(c(1.0, 0.95, 0.95, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))
set.seed(1)
rmse <- replicate(n, { test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
train_set <- dat %>% slice(-test_index)
test_set <- dat %>% slice(test_index)
fit <- lm(y ~ x, data = train_set)
y_hat <- predict(fit, test_set)
sqrt(mean((y_hat - test_set$y)^2)) 
})

mean(rmse)
sd(rmse)


# terzo esercizio matrice 3x3

set.seed(1)
n <- 1
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.25, 0.75, 0.25, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))

cor(dat)

set.seed(1)
rmse <- replicate(n, { 
    test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
train_set <- dat %>% slice(-test_index)
test_set <- dat %>% slice(test_index)
fit <- lm(formula = y ~ x_1 + x_2, data = train_set)
y_hat <- predict(fit, test_set)
sqrt(mean((y_hat - test_set$y)^2)) 
})

mean(rmse)
sd(rmse)

# esercizio sul minimo rmse

set.seed(1)
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.25, 0.75, 0.25, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))

set.seed(1)
test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
train_set <- dat %>% slice(-test_index)
test_set <- dat %>% slice(test_index)
fit <- lm(formula = y ~ x_1 + x_2, data = train_set)
y_hat <- predict(fit, test_set)
sqrt(mean((y_hat - test_set$y)^2)) 

# esercizio con alta correlazione

set.seed(1)
n <- 1000
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.95, 0.75, 0.95, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))

cor(dat)

set.seed(1)
rmse <- replicate(n, { 
  test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
  train_set <- dat %>% slice(-test_index)
  test_set <- dat %>% slice(test_index)
  fit <- lm(formula = y ~ x_2, data = train_set)
  y_hat <- predict(fit, test_set)
  sqrt(mean((y_hat - test_set$y)^2)) 
})

mean(rmse)
sd(rmse)