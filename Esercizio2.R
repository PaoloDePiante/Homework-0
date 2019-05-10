library(caret)
library(dslabs)
library(tidyverse)

data(iris)
iris <- iris[-which(iris$Species=='setosa'),]
y <- iris$Species
x1 <- iris$Sepal.Length
x2 <- iris$Sepal.Width
x3 <- iris$Petal.Length
x4 <- iris$Petal.Width

set.seed(2)
test_index <- createDataPartition(y,times=1,p=0.5,list=FALSE)
test <- iris[test_index,]
train <- iris[-test_index,]



cutoff <- seq(min(train$Sepal.Length), max(train$Sepal.Length), 0.1)

accuracy <- map_dbl(cutoff, function(x1) {
  y_hat <- ifelse(train$Sepal.Length > x1, "virginica", "versicolor") %>%
    factor(levels = levels(test$Species)) 
  mean(y_hat == train$Species) 
})

plot(cutoff, accuracy)
lines(cutoff, accuracy)

max(accuracy) #x1
best_cutoff <- cutoff[which.max(accuracy)]
best_cutoff


cutoff <- seq(min(train$Sepal.Width), max(train$Sepal.Width), 0.1)
accuracy <- map_dbl(cutoff, function(x2) {
  y_hat <- ifelse(train$Sepal.Width > x2, "virginica", "versicolor") %>%
    factor(levels = levels(test$Species)) 
  mean(y_hat == train$Species) 
})

plot(cutoff, accuracy)
lines(cutoff, accuracy)

max(accuracy) #x2
best_cutoff <- cutoff[which.max(accuracy)]
best_cutoff


cutoff <- seq(min(train$Petal.Length), max(train$Petal.Length), 0.1)
accuracy <- map_dbl(cutoff, function(x3) {
  y_hat <- ifelse(train$Petal.Length > x3, "virginica", "versicolor") %>%
    factor(levels = levels(test$Species)) 
  mean(y_hat == train$Species) 
})

plot(cutoff, accuracy)
lines(cutoff, accuracy)

max(accuracy) #x3
best_cutoff <- cutoff[which.max(accuracy)]
best_cutoff
y_hat <- ifelse(test$Petal.Length > best_cutoff, "virginica", "versicolor") %>%
  factor(levels = levels(test$Species))

mean(y_hat == test$Species) #overall accuracy



cutoff <- seq(min(train$Petal.Width), max(train$Petal.Width), 0.1)
accuracy <- map_dbl(cutoff, function(x4) {
  y_hat <- ifelse(train$Petal.Width > x4, "virginica", "versicolor") %>%
    factor(levels = levels(test$Species)) 
  mean(y_hat == train$Species) 
})

plot(cutoff, accuracy)
lines(cutoff, accuracy)

max(accuracy) #x4
best_cutoff <- cutoff[which.max(accuracy)]
best_cutoff

# come da esercizio
foo <- function(x){
  rangedValues <- seq(range(x)[1],range(x)[2],by=0.1)
  sapply(rangedValues,function(i){
    y_hat <- ifelse(x>i,'virginica','versicolor')
    mean(y_hat==train$Species)
  })
}
predictions <- apply(test[,-5],2,foo)
sapply(predictions,max)	


predictions <- foo(test[,3])
rangedValues <- seq(range(test[,3])[1],range(test[,3])[2],by=0.1)
cutoffs <-rangedValues[which(predictions==max(predictions))]

y_hat <- ifelse(test[,3]>cutoffs[1],'virginica','versicolor')
mean(y_hat==test$Species)
# Q5

y_hat <-ifelse(test$Petal.Length > 4.7 & test$Petal.Width>1.5 ,"virginica","versicolor") %>% factor(levels = levels(test$Species))

mean(y_hat == test$Species)
