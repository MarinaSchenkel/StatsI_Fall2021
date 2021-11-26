
##Problem Set 4

#Question 1

install.packages("car")
library(car)
data(Prestige)
help(Prestige)

Prestige$professional <- ifelse(Prestige$type=="prof",1,0)

# (b) 

reg1 <- lm (data=Prestige, prestige ~ income + professional + income:professional )
summary(reg1)

#(c) Write the prediction equation based on the result.


# Prestige (y) = 21.142(intercept) + 0.003*income + 37.781*professional - 0.002(income*professional)

#(d) Interpret the coeffcient for income.


# one dollar increase on the average income of incumbents increase on average 0.003 the prestige score
# holding constant the profession type and the interaction of professional and income(??)



#(e) Interpret the coeffcient for professional.

# an individual with an occupation type classified as professional
# increases on average 37.781 the prestige score, holding income constant 


#(f) What is the effect of a $1,000 increase in income on prestige score for professional
#occupations? In other words, we are interested in the marginal effect of income when
#the variable professional takes the value of 1. Calculate the change in ^y associated
#with a $1,000 increase in income based on your answer for (c).


y_1000income <- 21.142 + 0.003*1000 + 37.781 + (-0.002*1000)

y_2000income <- 21.142 + 0.003*2000 + 37.781 + (-0.002*2000)

y_3000income <- 21.142 + 0.003*3000 + 37.781 + (-0.002*3000)

y_3000income-y_2000income
y_2000income-y_1000income 


#(g) What is the effect of changing one's occupations from non-professional to professional
#when her income is $6,000? We are interested in the marginal effect of professional
#jobs when the variable income takes the value of 6; 000. Calculate the change in ^y
# on your answer for (c).

y_nonprof <- 21.142 + 0.003*6000 + (37.781*0) + (-0.002*(6000*0))

y_prof <- 21.142 + 0.003*6000 + (37.781*1) + (-0.002*(6000*1))

y_prof-y_nonprof

#Question 2: Political Science

#(a) Use the results from a linear regression to determine whether having these yard signs
#in a precinct affects vote share (e.g., conduct a hypothesis test with  = :05).

# Hypotheses: H0 : B1 = 0 vs. Ha : B1 != 0

t_stat1 <- (0.042-0)/0.016 #Calculate t-statistic
df <- 131 -3 # calculate degrees of freedom = # of obs. - # of variables 

p_value1 <- 2*pt(abs(t_stat1), df, lower.tail = F)

#0.009728528

#precinct randomly assigned to have the sign against McAuliffe posted 
#has a postive and significant effect on the proportion of the vote that went 
#to McAuliff's opponent Ken Cuccinelli (p-value 0.0097 < 0.05)


#(b) Use the results to determine whether being next to precincts with these yard signs
#affects vote share (e.g., conduct a hypothesis test with alfa = :05).

# Hypotheses: H0 : B2 = 0 vs. Ha : B2 != 0

t_stat2 <- (0.042-0)/0.013 #Calculate t-statistic
p_value2 <- 2*pt(abs(t_stat2), df, lower.tail = F)

#0.00156946

#precinct that was adjacent to a precinct in the treatment group 
#has a postive and significant effect on the proportion of the vote that went 
#to McAuliff's opponent Ken Cuccinelli (p-value 0.0016 < 0.05)



#(c) Interpret the coeffcient for the constant term substantively.

# On average the expected proportion of the vote that went to McAuliff'2 opponent
# is 0.302 when the Precinct assigned lawn signs and the Precinct adjacent to lawn signs are zero


#(d) Evaluate the model fit for this regression. 
#What does this tell us about the importance
#of yard signs versus other factors that are not modeled?

# Approximately 9.4% of the variation in the Precinct assigned lawn signs and 
# in the Precinct adjacent to lawn signs explains the variation of the 
# proportion of the vote that went to McAuliff's opponent Ken Cuccinelli.
# This show us that there are other factors that influence the vote share not included as a covariant in the model 



                                