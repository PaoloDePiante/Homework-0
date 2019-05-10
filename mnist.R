#minist
library(broom)
library(dslabs)
library(tidyverse)
library(matrixStats)

mnist <- read_mnist()

x<-mnist$train$images[1:1000,]
x[1,]

y <- mnist$train$labels[1:1000]
head(y)

# 4
grid <- matrix(x[3,],28,28)
image(1:28, 1:28, grid[,28:1])
# 5
grid <- matrix(x[1,],28,28)
image(1:28, 1:28, grid[,28:1])

rows_avg <- rowMeans(x)

#p <- data.frame(rows_avg,y) %>% group_by(y) %>% summarize(avg = mean(rows_avg)) 

p1 <- data.frame(avg=rowMeans(x),labels=y) %>%
  ggplot(aes(labels, avg, group=y))
p1 + geom_boxplot()


sds <- colSds(x)
#immagine della variabilità nei bordi è 0

barplot(sds)

qplot(as.vector(x), bins=30, color=I("Black"))

grid2 <- matrix(sds,28,28)
image(1:28, 1:28, grid2[,28:1])

new_x <- x[,colSds(x)>60]


bin_x <- x
bin_x[bin_x > 50 & bin_x <205] = 999
d <- rowCounts(bin_x, value = 999)
d1 <- sum(d)
d3 <- d1/(1000*784)

y <- rowMeans(mnist$train$images>50 & mnist$train$images<205)
qplot(as.factor(mnist$train$labels), y, geom = "boxplot")
mean(y)







x1 <- 1:5
x2 <- 6:10
m <- cbind(x1,x2)
class(m)
tot <- rowSums(m)
#estrae dalla matrice solo le righe 2 e 4
m[c(2,4),]
# continua a essere una matrice e non un vettore!
class(m[,1, drop=FALSE])
# calcola la z standardizzata
xm <- sweep(m,2,colMeans(m))
xs <- sweep(xm, 2, colSds(m), FUN ="/")
xs
