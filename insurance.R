read.csv("insurance.csv")
insurance <- read.csv("insurance.csv")

##Checking for null values
which(is.na(insurance))

insurance

###When charges depend on age of client
model1 <- lm(charges ~ age, data=insurance)
summary(model1)

###When charges depend on the number of children a client has
model2 <- lm(charges ~ children, data=insurance)
summary(model2)
##Model 2 has a higher RSE but a lower Rsquared and adjusted Rsquared

###When charges depend on age and number of children of the client
model3 <- lm(charges ~ age + children, data=insurance)
summary(model3)
##0BSERVATIONS: comparing model 3 to 1 and 2
## In model3 the RSE reduces meaning the model is assumed to perform better then model1 and 2
## R squared and adjusted R squared also increase meaning the proportion of variability of Y(charges) is higher in model 3

###When charges depend on age,number of children and intersection between age and number of children
model4 <- lm(charges ~ age + children + age:children, data=insurance)
summary(model4)
##OBSERVATION: On introducing the intersection between age and children there is no change in RSE
##Rsquared increases a bit but adjusted Rsquared decreases

###When charges depend on the sex of client
model5 <- lm(charges ~ sex, data=insurance)
summary(model5)

###When charges depend on the bmi of the client
model6 <- lm(charges ~ bmi, data=insurance)
summary(model6)

cor(insurance$bmi, insurance$charges)
cor(insurance$children, insurance$charges)
cor(insurance$smoker, insurance$charges)


###When charges depends on whether a client smokes or not
model7 <- lm(charges ~ smoker, data=insurance)
summary(model7)

###When charges depends on the region
model8 <- lm(charges ~ region, data=insurance)
summary(model8)
 

###When charges depend on the all the independent variables 
model9 <- lm(charges~ age + children + sex + bmi + region + smoker, data=insurance)
summary(model9)
##Assuming that charges depends on all other variables we get the lowest RSE
##A model is assumed to perform better with less RSE
## Rsquared and adjusted Rsquared are the highest as compared to other models
##From adjusted Rsquared we can see that this model has the highest proportion of variability


