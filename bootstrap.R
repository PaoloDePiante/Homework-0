library(dslabs)
library(caret)
library(broom)
library(tidyverse)
library(matrixStats)

data(mnist_27)
set.seed(1995)
indexes <- createResample(mnist_27$train$y, 10)

# nota che sta guardando negli indici se appare un 3, un 4 o un 7. No nei valori della y associati agli indici!
# nota anche che Indexes[[1]] è uguale a scrivere indexes$Resample01
# questo esercizio vuole dimostrare che gli indici a campione formano un set di dati indipendenti 
# dove uno stesso indice può essere ripescato 0 o più volte

sum(indexes[[1]] == 3)
#sum(indexes[[1]] == 4)
#sum(indexes[[1]] == 7)

sum(indexes[[1]] == 3)
sum(indexes[[2]] == 3)
sum(indexes[[3]] == 3)
sum(indexes[[4]] == 3)
sum(indexes[[5]] == 3)
sum(indexes[[6]] == 3)
sum(indexes[[7]] == 3)
sum(indexes[[8]] == 3)
sum(indexes[[9]] == 3)
sum(indexes[[10]] == 3)
