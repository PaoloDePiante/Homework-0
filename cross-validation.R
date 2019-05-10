set.seed(1996)
n <- 1000
p <- 10000
x <- matrix(rnorm(n*p), n, p)
colnames(x) <- paste("x", 1:ncol(x), sep = "_")
y <- rbinom(n, 1, 0.5) %>% factor()

#x_subset <- x[ ,sample(p, 100)]

#fit <- train(x_subset, y, method = "glm")
#fit$results

ind <- which(pvals <= 0.01)
x_subset <- x[ ,ind]

fit <- train(x_subset, y, method = "knn", tuneGrid = data.frame(k = seq(101, 301, 25)))
ggplot(fit)


#vede quali variabili vale la pena tenere per le classi di y 

library(devtools)
library(genefilter)
devtools::install_bioc("genefilter")
tt <- colttests(x, y)

pvals <- tt$p.value

# Create an index ind with the column numbers of the predictors that were 
# "statistically significantly" associated with y. Use a p-value cutoff of 0.01
# to define "statistically significantly."

ind <- tt$p.value < 0.01
a <- ifelse(ind == TRUE, 1, 0)
sum(a)

ind <- which(pvals <= 0.01)
x_subset <- x[ ,ind]
fit <- train(x_subset, y, method = "glm")
fit$results


#esempio di t.test
m1 <- sample(c(0,1), 5, replace = TRUE, prob = c(.5,.5))
x1 <- 1:5
x2 <- 6:10
m <- cbind(x1,x2)
class(m)
tot <- rowSums(m)

tt <- t.test(x1,m1)
tt
