## Reading in files
df <- read.csv("/home/dennis/R/MMU/Lecture/3/data/insurance.csv")

## have a peak at the data
head(df)
summary(df)

### Impute missing values

## age
df$age <- ifelse(is.na(df$age),
                      ave(df$age, FUN = function(x) median(x, na.rm=TRUE)),
                      df$age)

## bmi
df$bmi <- ifelse(is.na(df$bmi),
                 ave(df$bmi, FUN = function(x) median(x, na.rm=TRUE)),
                 df$bmi)

## children
table(df$children)
val <- unique(df$children[!is.na(df$children)])
my_mode <- val[which.max(tabulate(match(df$children, val)))]
df$children[is.na(df$children)] <- my_mode

# Encoding categorical variables
df$sex <- factor(df$sex,
                     levels = c("female", "male"),
                     labels = c(1, 2))

df$smoker <- factor(df$smoker,
                     levels = c("yes", "no"),
                     labels = c(1, 2))

df$region <- factor(df$region,
                        levels = c("northeast", "northwest", "southeast", "southwest"),
                        labels = c(1, 2, 3, 4))


# Split the data to test and training set
library(caTools)
set.seed(54)
split <- sample.split(df$charges, SplitRatio = 0.8)
training_set <- subset(df, split == TRUE)
test_set <- subset(df, split == FALSE)

## Feature Selection
library(leaps)

regfit.bwd <- regsubsets(charges ~ ., data=training_set, nvmax=15, method = "backward")

reg.summary <- summary(regfit.bwd)

names(reg.summary)

# Examine R-Squared
reg.summary$rsq

reg.summary$adjr2

#-- R-Squared statistics increases monotically as more variables are included.

par( mfrow = c (2 ,2))

plot(reg.summary$rss , xlab =" Number of Variables " , ylab =" RSS " , type ="l")

plot(reg.summary$adjr2 , xlab =" Number of Variables " , ylab =" Adjusted R-Squared " , type ="l")

plot(reg.summary$cp , xlab =" Number of Variables " , ylab ="Cp" , type ="l")

plot(reg.summary$bic , xlab =" Number of Variables " , ylab ="bic" , type ="l")

which.min(reg.summary$bic)

points(3, reg.summary$bic[3], col='red', cex=2, pch=20)

# Check coefficient estimates

coef(regfit.bwd, 3)

# Fit MLR
model.fit <- lm(charges ~ age + bmi + smoker, data = training_set)
summary(model.fit)

# Predict new results
y_pred <- predict(model.fit, newdata = test_set)

test_set$pred_charges <- y_pred

### ALTERNATIVE MODELS

## Ridge Regression
library(glmnet)
ridge.mod <- glmnet(charges)



