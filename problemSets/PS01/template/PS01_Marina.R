#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
lapply(c("stringr"),  pkgTest)

lapply(c(),  pkgTest)

# set working directory
setwd("C:\\Users\\marin\\Dropbox\\PhD Political Science\\02 Quantitative methods I\\StatsI_Fall2021\\problemSets\\PS01\\template")


#####################
# Problem 1
#####################

y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)


#1. Find a 90% confidence interval for the average student IQ in the school.

IQ_xbar <- mean(y)
IQ_s <- sd(y)
IQ_n <- length(y)
z <- qnorm(0.95) # 90% CI - alpha = .10 -> z(alpha/2) = z(0.05)
lower_limit <- IQ_xbar - z * (IQ_s /sqrt(IQ_n))
upper_limit <- IQ_xbar + z * (IQ_s/sqrt(IQ_n))
print(c(lower_limit,upper_limit))

#2. Next, the school counselor was curious whether the average student IQ in her school
#is higher than the average IQ score (100) among all the schools in the country.


#Using the same sample, conduct the appropriate hypothesis test with alpha = 0:05.


test <- t.test(y, mu = 100, conf.level = 0.95, alternative= "greater")

test$conf.int
test$p.value

#We cannot reject the null hypothesis because p-value (-0.7215383) of the test is higher than the significance level (0.05).
#The average of the IQ score of the 25 students is not significantly greater than average IQ score (100) among all the schools in the country.



#####################
# Problem 2
#####################

expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2021/main/datasets/expenditure.txt", header=T)

#Please plot the relationships among Y, X1, X2, and X3? What are the correlations
#among them (you just need to describe the graph and the relationships among them)?

#scatterplots
attach(expenditure)

p1 <- plot(X1, Y)
#There is a positive association, possibly linear and relatively strong, between the variables Y and X1, with potential outliers.

p2 <- plot (X2, Y)
#There is an apparent scattered relation around a smooth curve (non-linear) and not very strong relationship between variables Y and X2.

p3 <- plot (X3, Y)
#There is no apparent relationship between the variables Y and X3, possibly a positive non-linear.

p4 <- plot(X1, X2)
#There is no apparent relationship between the variables X1 and X2, data points are scattered.

p5 <- plot(X1, X3)
#There is a stronger positive between variables X1 and X3 relation with potential outliers.

p6 <- plot(X2, X3)
#There is no apparent association between the variables X2 and X3, the data points are scattered distributed.  

par(mfrow=c(2,3))


#Please plot the relationship between Y and Region? On average, which region has the
#highest per capita expenditure on housing assistance?
plot(expenditure$Region, expenditure$Y,
     main = "Plot of relationship between Y and Region",
     xlab = "Region",
     ylab = "Expenditure on housing assistance")

#On average, the states of the region 4 (West) have a higher per capita expenditure on shelters/housing assistance.


#. Please plot the relationship between Y and X1? Describe this graph and the rela-
#  tionship. Reproduce the above graph including one more variable Region and display
#different regions with different types of symbols and colors.

library(ggplot2)
plot_1 <- ggplot(expenditure, aes(x = X1, y = Y))+
  geom_point() +
  theme_minimal() +
  labs(title="Plot of relationship between Y and X1",
       x ="Per capita personal income in state", y = "Per capita expenditure on housing")
plot_1

#There is an apparent positive relation between the variables, 
#the higher the per capita personal income in the state, 
#the higher the per capita expenditure on housing assistance by the state.


plot_2 <- ggplot(expenditure, aes(x = X1, y = Y))+
  geom_point(aes(color = Region, shape = Region))+
  theme_minimal()+
  labs(title="Plot of relationship between Y and X1 by Region",
       x ="Per capita personal income in state", y = "Per capita expenditure on housing")
plot_2


