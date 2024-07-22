library(readxl)
CC <- read_excel("C:/Users/nshai/OneDrive/Desktop/churn_clean.xlsx", 
                 +     sheet = "churn_clean")

library(ggplot2)
library(tidyverse)
library(hexbin)

t_test_results <- t.test(MonthlyCharge ~ Churn, data = CC)
t_test_results

#boxplot univariate of continuous variables

ggplot(CC, aes(x= "",y = MonthlyCharge ))+
  geom_boxplot() +
  theme_minimal()

ggplot(CC, aes(x= "",y =Tenure ))+
  geom_boxplot() +
  theme_minimal()
#bar graph for univariate categorical variables
ggplot(CC, aes(x= Contract ))+
  geom_bar() +
  theme_minimal()

ggplot(CC, aes(x= InternetService ))+
  geom_bar() +
  theme_minimal()

#analysis of bivariate continuous data
ggplot(CC, aes(x= Tenure,y = MonthlyCharge ))+
  geom_hex() +
  theme_minimal()

#analysis of bivariate categorical data
mosaicplot(table(CC$Churn, CC$InternetService)) 


