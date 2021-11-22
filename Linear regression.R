##load library
library(MASS)

df= data.frame(Boston$lstat, Boston$medv)

###correlation between variables
cor(df$Boston.lstat, df$Boston.medv)

length(which(is.na(df)))

lm.fit= lm(Boston.medv~Boston.lstat, data=df)

summary(lm.fit)

plot(df$Boston.lstat, df$Boston.medv, pch=20)
abline(lm.fit, col="red", lwd=3)

par(mfrow=c(2,2))
plot(lm.fit)

