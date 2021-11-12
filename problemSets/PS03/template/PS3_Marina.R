
### Problem Set 3 Answers

# Marina Schenkel 

#load data
library(tidyverse)
df <- read_csv ("C:\\Users\\marin\\Dropbox\\PhD Political Science\\02 Quantitative methods I\\StatsI_Fall2021\\datasets\\incumbents_subset.csv")

## Question 1 

#1
reg1 <- lm (data=df, voteshare ~ difflog)
summary(reg1) #checking results of the regression

#2
ggplot(data = df, aes(x = difflog, y = voteshare)) +
  geom_point(alpha = 0.5) + #scatterplot
  geom_smooth(method = "lm") + #regression line
  theme_minimal()

#3
resid1 <- reg1$residuals

#4
# voteshare(Y) = 0.579031(Intercept) + 0.041666 x difflog(X1) 


## Question 2

#1 
reg2 <- lm (data=df, presvote ~ difflog)
summary(reg2) #checking results of the regression

#2
ggplot(data = df, aes(x = difflog, y = presvote)) +
  geom_point(alpha = 0.5) + #scatterplot
  geom_smooth(method = "lm", colour = "green") + #regression line
  theme_minimal()

#3
resid2 <- reg2$residuals

#4
# presvote(Y) = 0.507583(Intercept) + 0.023837 x difflog(X1)  

## Question 3

#1
reg3 <- lm (data=df, voteshare ~ presvote)
summary(reg3) #checking results of the regression

#2
ggplot(data = df, aes(x = presvote, y = voteshare)) +
geom_point(alpha = 0.5) + #scatterplot
  geom_smooth(method = "lm", colour = "orange") + #regression line
  theme_minimal()

#3
# voteshare (Y) = 0.441330 (Intercept) + 0.388018 x voteshare(X1) 


## Question 4

#1
reg4 <- lm(data=df, resid1 ~ resid2)
summary(reg4) #checking results of the regression

#2
df_resid <- as.data.frame(cbind(resid1, resid2)) # data frame of residuals from both regressions

ggplot(data = df_resid, aes(x = resid2, y = resid1)) +
  geom_point(alpha = 0.5) + #scatterplot
  geom_smooth(method = "lm", colour= "red") + #regression line
  theme_minimal()

#3
# Residuals from regression 1(Y) = -5.207e-18 (Intercept) + 2.569e-01 x Residuals from regression 2(X1)

## Question 5

#1
reg5 <- lm (data=df, voteshare ~ difflog + presvote)
summary(reg5)#checking results of the regression


#2
# Incumbemnt's vote share (Y) = 0.4486442 (Interecept) + 0.0355431 x difflog (X1) + 0.2568770 x presvote (X2)


#3 
summary(reg4) # Residual standard error: 0.07338 on 3191 degrees of freedom
summary (reg5) # Residual standard error: 0.07339 on 3190 degrees of freedom
