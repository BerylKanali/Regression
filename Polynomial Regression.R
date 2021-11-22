library(MASS)
lm.fit <-lm(medv ~ lstat, data=Boston)
summary(lm.fit)
plot(Boston$lstat, Boston$medv)

lm.fit<- lm(medv ~ lstat+ I(lstat^2), data=Boston)
summary(lm.fit)
 
model2 <- lm(medv ~ lstat, data=Boston)
par(mfrow=c(2,2))
plot(model2)

model5 <-lm(medv ~ poly(lstat,5) , data=Boston)
summary(model5)
par(mfrow=c(2,2))
plot(model5)

