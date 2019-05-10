library(tidyverse)
library(caret)
#install.packages("e1071")
library(dbplyr)
install.packages("scatterplot3d") # Install
library("scatterplot3d") # load


RowData <- read.csv2("C:\\Users\\PADEP.CORPDOM\\Desktop\\RowData.csv")
glm_fit <- RowData %>% 
  glm(RestingPulse ~ Smokes + Weight, data=., family = "binomial")

test_set <- data.frame("RestingPulse"="", "Smokes"=1,"Weight"=180)
test_set2 <- read.csv2("C:\\Users\\PADEP.CORPDOM\\Desktop\\RowData2.csv")

p_hat_logit <- predict(glm_fit, newdata = test_set, type = "response")
p_hat_logit2 <- predict(glm_fit, newdata = test_set2, type = "response")

lm(RestingPulse ~ Smokes + Weight, data=RowData)
lm_fit <- lm(RestingPulse ~ Smokes + Weight, data=RowData)

p_hat_logit #il risultato è 1-la Fitted probability in Minitab con P(Low) Low =0
p_hat_logit2

y_hat_logit <- ifelse(p_hat_logit > 0.5, "High", "Low") 
y_hat_logit2 <- ifelse(p_hat_logit2 > 0.5, "High", "Low") 


test_set_pop <- test_set %>% mutate(RestingPulse = y_hat_logit)
test_set_pop2 <- test_set2 %>% mutate(RestingPulse = ifelse(p_hat_logit2 > 0.5, 1, 0)) 


# matrice con la Y attesa
test_set_pop2

Previsione <- paste("Il risultato è: ", y_hat_logit)
Previsione

#setdiff(RowData, test_set_pop2)

confusionMatrix(factor(RowData$RestingPulse), factor(test_set_pop2$RestingPulse))

#Graph

scatterplot3d(RowData$Weight, RowData$Smokes, p_hat_logit2)


