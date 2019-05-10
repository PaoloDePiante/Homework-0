# richiede almeno r ver 3.6
models <- c("glm", "lda",  "naive_bayes",  "svmLinear", 
            "gamboost",  "gamLoess", "qda", 
            "knn", "kknn", "loclda", "gam",
            "rf", "ranger",  "wsrf", "Rborist", 
            "avNNet", "mlp", "monmlp",
            "adaboost", "gbm",
            "svmRadial", "svmRadialCost", "svmRadialSigma")


library(caret)
library(dslabs)
library(kernlab)

set.seed(1)
data("mnist_27")

fits <- lapply(models, function(model){ 
  print(model)
  train(y ~ ., method = model, data = mnist_27$train)
}) 

names(fits) <- models

length(mnist_27$test$y)
length(models)



pred <- sapply(fits, function(object) 
  predict(object, newdata = mnist_27$test))
dim(pred)

v <- seq(1,16,1)
acc <- sapply(v, function(v) 
  confusionMatrix(reference = factor(pred[,v]), mnist_27$test$y)$overall["Accuracy"])
mean(acc)

acc <- colMeans(pred == mnist_27$test$y)
acc
mean(acc)


v <- seq(1,200,1)
en_acc <- sapply(v, function(v) 
    as.integer(names(which.max(table(factor(pred[v,])))))
    )
confusionMatrix(reference = factor(en_acc), mnist_27$test$y)

plot(acc)+
  + abline(h=0.845)

ind <- acc > mean(y_hat == mnist_27$test$y)
sum(ind)
models[ind]


votes <- rowMeans(pred == "7")
y_hat <- ifelse(votes > 0.5, "7", "2")
mean(y_hat == mnist_27$test$y)


#cross validation using training data

fits <- lapply(models, function(model){ 
  print(model)
  train(y ~ ., method = model, data = mnist_27$train)
}) 

v <- seq(1,23,1)
tr_acc <- sapply(v, function(v) 
  min(fits[[v]]$results["Accuracy"])
)
mean(tr_acc)

acc_hat <- sapply(fits, function(fit) min(fit$results$Accuracy))
mean(acc_hat)

# ensemble con Accuracy >= .8

plot(tr_acc)
abline(h=0.8)

ind <- tr_acc >= 0.8
sum(ind)
models[ind]
new_model <- c("naive_bayes",  "gamboost",  "gamLoess", "qda", 
            "knn", "loclda", "gam",
            "rf", "wsrf", "Rborist", 
            "gbm", "svmRadial", "svmRadialCost", "svmRadialSigma")


fits <- lapply(new_model, function(model){ 
  print(model)
  train(y ~ ., method = model, data = mnist_27$train)}) 

names(fits) <- new_model

pred <- sapply(fits, function(object) 
  predict(object, newdata = mnist_27$test))
dim(pred)

v <- seq(1,200,1)
en_acc <- sapply(v, function(v) 
  as.integer(names(which.max(table(factor(pred[v,]))))))
confusionMatrix(reference = factor(en_acc), mnist_27$test$y)



# ipak function: install and load multiple R packages.
# check to see if packages are installed. Install them if they are not, then load them into the R session.

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# usage
# packages <- c("caret", "dslabs", "ggrepel", "gridExtra", "car")
# I then called ipak with the following vector:
  
  model_packages <- c("class", "e1071", "MASS", "Rborist", "rpart", "randomForest", "Rborist", "gam", "caret", "naivebayes", "kknn", "klaR", "ranger", "wsrf", "RSNNS", "monmlp", "ada", "JOUSBoost", "gbm", "mboost", "import", "fastAdaboost")

ipak(model_packages)