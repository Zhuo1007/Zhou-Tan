install.packages("rjags")
library(rjags)
library(coda)
install.packages("ggplot2")
library(ggplot2)
install.packages("patchwork")  
library(patchwork)

#Install data
install.packages("MASS")  
library(MASS)
install.packages("dplyr")
library(dplyr)
data("attitude")
str(attitude)

mean(attitude$rating)
sd(attitude$rating)

#Standardization,model fit
attitude_scaled <- attitude %>%
  mutate(across(-c(rating), ~ scale(.)[,1]))
X <- as.matrix(attitude_scaled %>% dplyr:: select(-rating))
y <- attitude_scaled$rating
n <- nrow(X)
p <- ncol(X)

#OLM
mod <- lm(y~X)




