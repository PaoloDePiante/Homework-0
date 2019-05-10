library(dslabs)
data("tissue_gene_expression")

dim(tissue_gene_expression$x)
str(tissue_gene_expression$x)

# Funziona! Crea un csv con i dati del dataset!
# write.table(tissue_gene_expression$x, file="tissues_gene_expression.csv", quote=T, sep=";", dec=",", na="NA", row.names=T, col.names=T)

table(tissue_gene_expression$y)


#distanza euclidea tra le righe ossia le osservazioni cioè i tessuti
d <- dist(tissue_gene_expression$x)

ind <- c(1, 2, 39, 40, 73, 74)
as.matrix(d)[ind,ind]

image(as.matrix(d))

# Altro esercizio: per questi valori di K trova l'accuracy. Attenzione abbiamo una matrice x e un vettore y!

library(dslabs)
library(caret)
library(tidyverse)
 
data("tissue_gene_expression")
set.seed(1)

ks <- seq(1, 11, 2)

x <- tissue_gene_expression$x
y <- tissue_gene_expression$y

train_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE) 
train_set = x[train_index,] 
test_set = x[-train_index,] 
train_set_y = y[train_index] 
test_set_y = y[-train_index] 
F_1 <- sapply(ks, function(k){ 
  fit <- knn3(train_set,train_set_y, k = k) 
  y_hat <- predict(fit, test_set, type="class") 
  confusionMatrix(data = y_hat, reference = test_set_y)$overall["Accuracy"] })

F_1