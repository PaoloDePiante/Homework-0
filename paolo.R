library(dslabs)
library(caret)
library(dplyr) #ci fa usare %>%
library(purrr) #serve per la funzione map_dbl
x <- heights$height
y <- heights$sex
set.seed(2)
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
head(test_index)
test_set <- heights[test_index, ]

# la riga seguente fa il complemento a uno dell'altra meta dei casi p=.5

train_set <- heights[-test_index, ]
dim(test_set)[1]
dim(train_set)[1]

#We will now develop an algorithm using only the training set. 
#Once we are done developing the algorithm, we will freeze it and evaluate it using the test set.
#The simplest way to evaluate the algorithm when the outcomes are categorical is by simply 
#reporting the proportion of cases that were correctly predicted in the test set. This metric 
#is usually referred to as overall accuracy.

#algorittimo a tirare a indovinare

y_hat <- sample(c("Male", "Female"), length(test_index), replace = TRUE)

# ha estratto in totale 525 tra Male e Female (abbiamo tirato a indovinare il sesso )
# in ML le categorie devo essere fattori cosi trasformiamo la Y in FATTORE

y_hat <- sample(c("Male", "Female"), length(test_index), replace = TRUE) %>% factor(levels = levels(test_set$sex))
class(y_hat)

mean(y_hat == test_set$sex) #proportion che ci dice quanti del test_set$sex corrisponde alle Y_hat)

# Proportion bassa?!!
# Facciamo un p? di exploratory data

heights %>% group_by(sex) %>% summarize(mean(height), sd(height), n())

#siccome la media dei maschi ? 69,3 ci tolgo 2*sd e viene 62,08 quindi l'accuracy sale a:

y_hat <- ifelse(x > 62, "Male", "Female") %>% factor(levels = levels(test_set$sex))
mean(y_hat == test_set$sex)

#it is important that we optimize the cutoff using only the training set: the test set is only
#for evaluation.

cutoff <- seq(61, 70)
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train_set$height > x, "Male", "Female") %>% 
    factor(levels = levels(test_set$sex))
  mean(y_hat == train_set$sex)
})

plot(cutoff, accuracy)
lines(cutoff, accuracy)

max(accuracy)

best_cutoff <- cutoff[which.max(accuracy)]
best_cutoff

#Now we can now test this cutoff on our test set to make sure our accuracy is not overly optimistic

y_hat <- ifelse(test_set$height > best_cutoff, "Male", "Female") %>% factor(levels = levels(test_set$sex))
#y_hat <- factor(y_hat)
mean(y_hat == test_set$sex)

#Confusion Matrix 
table(predicted = y_hat, actual = test_set$sex)

#ci sono troppe femmine che sono state riconosciute maschi...
# questo per l'elevato numero di maschi nei dati originari

prev <- mean(y == "Male")
prev

# This can actually be a big problem in machine learning. If your training 
# data is biased in some way, you are likely to develop algorithms that are 
# biased as well. There are several metrics that we can use to evaluate an 
# algorithm in a way that prevalence does not cloud our assessment, and these 
# can all be derived from the confusion matrix. A general improvement to using
# overall accuracy is to study SENSITIVITY and SPECIFICITY separately.



