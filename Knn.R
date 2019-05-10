set.seed(1)
library(dslabs)
library(caret)
library(tidyverse)


data("heights")
x <- heights$height
y <- heights$sex  #2=Male  1=Female

k <- seq(1, 101, 3)


f1 <- sapply(k, function(k) {
  
  test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE) 
  train_set <- heights[-test_index, ]   
  test_set <- heights[test_index, ]
  
  fit <- knn3(sex ~ height, data = train_set, k = k)   
  y_hat <- predict(fit, test_set, type = "class") %>% factor(levels = levels(y))   
  
  F_meas(data = y_hat, reference = factor(test_set$sex))    
})

max(f1)

plot(k, f1)

plotdat <- cbind(as_data_frame(f1), data_frame(k)) %>% rowid_to_column("id")
plotdat %>% ggplot() + geom_line(aes(x = k, y=value), color = "blue") 
