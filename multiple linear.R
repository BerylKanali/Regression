library(MASS)
lm.fit <- lm(medv ~ lstat + age, data=Boston)
summary(lm.fit)
## lstat:age is the interction between age and lstat
model1 <- lm(medv ~ lstat + age + lstat:age, data=Boston)
summary(model1)

model3 <- lm(medv ~ lstat + lstat:age, data=Boston)
summary(model3)
