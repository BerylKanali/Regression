### Loading required libraries

## sudo apt-get install r-cran-curl r-cran-openssl r-cran-xml2
## install.packages(c("Rcpp","tidyverse"))
library(tidyverse)          # Pipe operator (%>%) and other commands
library(caret)              # Random split of data/cross validation
library(olsrr)              # Heteroscedasticity Testing (ols_test_score)
## install.packages("olsrr")
library(car)                # Muticolinearity detection (vif)
library(broom)              # Diagnostic Metric Table (augment)


# Loading Data set
data = read.csv("advertising.csv" , header = T)

# Inspection of top 6-rows of data
head(data)

# Inspection of bottom 6-rows of data
tail(data)

# Getting Structure of whole data set
str(data)

# Checking Outliers
boxplot(data)

### The above plot shows that two outliers are present in the variable "Newspaper". 

# Removing Outliers
data <- data[-which(data$Newspaper %in% boxplot.stats(data$Newspaper)$out),]

# Again Checking Outliers
boxplot(data)

# Checking Missing Values
table(is.na(data))

### The above output shows that there is no missing value in the given data set.

# Creating scatter plot matrix 
pairs(data , upper.panel = NULL)

### This output shows that -
  
### 1. No or very low linear relationship between TV and Radio variable.
### 2. Low linear relationship between TV and Newspaper variable.
### 3. Moderate linear relationship between Radio and Newspaper variable.
### 4. High linear relationship between TV and Sales , Radio and Sales , Newspaper and Sales.
### 5. A small curvilinear relationship is also present between TV and Sales as well as Radio and Sales.

# Scatter Plot between TV and Sales
plot(data$TV , data$Sales)

### Notice, there is a small curvilinear relationship between TV and Sales.

# Scatter Plot between Radio and Sales
plot(data$Radio , data$Sales)

### Notice, there is a curvilinear relationship between Radio and Sales.

# Scatter Plot between Newspaper and Sales
plot(data$Newspaper , data$Sales)

### Low linear relationship between Newspaper and Sales variable.

# Randomly Split the data into training and test set
set.seed(1)
training.samples <- data$Sales %>% createDataPartition(p = 0.8, list = FALSE)
train.data  <- data[training.samples, ]
test.data <- data[-training.samples, ]

### Fitting Simple Linear Regression

# Fitting Sales ~ TV
sm1 <- lm(Sales ~ TV , data = train.data)

# Take a look on summary of the model
summary(sm1)

#### From the above output, you must notice that:
  
### 1. Created model is statistically significant since p-value <<< 0.05 (see in the last line of output)
### 2. From the coefficients section, it is clear that both coefficients (slope and intercept) are statistically significant since p-value <<< 0.05
### 3. This model with TV as predictor explains approximately 81% variability of target (Sales).
### 4. Residual standard error for the model is 2.28

# Fitting Sales ~ Radio
sm2 <- lm(Sales ~ Radio , data = train.data)

# Take a look on summary of the model
summary(sm2)

#### From the above output, you must notice that -
  
### 1. Created model is statistically significant since p-value << 0.05 (see in the last line of output)
### 2. From the coefficients section, it is clear that both coefficients (slope and intercept) are statistically significant since p-value << 0.05
### 3. This model with TV as predictor explains approximately 14% variability of target (Sales).
### 4. Residual standard error for the model is 4.95

# Fitting Sales ~ Newspaper
sm3 <- lm(Sales ~ Newspaper , data = train.data)

# Take a look on summary of the model
summary(sm3)

#### From the above output, you must notice that -
### 1. Created model is statistically significant since p-value < 0.05 (see in the last line of output)
### 2. From the coefficients section, it is clear that both coefficients (slope and intercept) are statistically significant since p-value < 0.05
### 3. This model with TV as predictor explains approximately 4% variability of target (Sales).
### 4. Residual standard error for the model is 5.21

### We have obtained that Simple Linear Regression Model with TV as predictor is explaining
### more variability of target (Sales).

# Scatter plot with Simple Linear Regression Line
plot(train.data$TV , train.data$Sales)

# Adding Regression Line
abline(lm(train.data$Sales ~ train.data$TV) , col = "blue")

### One problem occurs here : The above plot shows that it is not feasible to predict Sales
### only on the basis of a single predictor due to more variability in the Sales. Also, if we
### use single predictor then we completely neglect the effect of rest two other predictors on Sales, 
### that may not be the case in real. So, why not extend this model ?

#### Fitting Multiple Linear Regression with Diagnostic Plot

#### Extending Simple Linear Regression Model using Forward Selection Method

# Fitting MLR model with predictors TV and Radio 
mm1 <- lm(Sales ~ TV + Radio , data = train.data)

# Take a look on summary of the model
summary(mm1)

### Well, From the above output, notice that -
  
### 1. Created model is statistically significant since p-value <<< 0.05 (see in the last line of output)
### 2. From the coefficients section, it is clear that both coefficients (slopes and intercept) are statistically significant since p-value <<< 0.05
### 3. This model with TV and Radio as predictors explains approximately 90% variability of target (Sales) that is a better indication with respect to the model with TV alone as predictor.
### 4. Residual standard error for the model is 1.693

### But we must test, whether this improvement in Adjusted R-squared is statistically significant ?
  
### i.e., we want to test the null hypothesis
  ## H0 : The improvement in Adjusted R-squared is not statistically significant 
  ## H1 : The improvement in Adjusted R-squared is statistically significant.
### For this testing, we use ANOVA (Analysis of Variance) technique

# Performing ANOVA to test the above stated null hypothesis
anova(sm1 , mm1)

### In the above output, Notice the value in the last column of second row. This value (2.2e-16) 
### indicates the p-value for testing null hypothesis. Since this value is extremely less than 0.05, 
### hence we have sufficient evidence from the data to reject the null hypothesis and accept the alternative.
### That's why the improvement in Adjusted R-squared is statistically significant.

### Hence, Adopt the model Sales ~ 0.05531 TV + 0.102164 Radio at this stage.

# Extending further the MLR including the predictor Newspaper
mm2 <- lm(Sales ~ TV + Radio + Newspaper , data = train.data)

# Take a look on summary of the model
summary(mm2)

### From the above output and using the information from previously fitted model, Notice that:
### From the coefficients section of the above output, It is clear that Newspaper predictor is 
### not statistically significant for the model due to p-value (0.69) > 0.05
### Adjusted R-squared has been reduced 89.9 to 89.8
### Residual standard error has been increased from 1.715 to 1.72
### Although, the created model is statistically significant since p-value <<< 0.05 (see in the last line of output)
### We have sufficient evidence from the data for not to include the Newspaper as predictor in the model.

#### Diagnostic Plots
### Checking Linearity Assumption

# Residual Plot
plot(mm1, 1)

### Notice from the above plot:
### Red line is approximately horizontal and linear which indicates that Linearity assumption holds well.
### Residual fluctuates in a random manner inside a band drawn between Residuals = -4 to +4 which indicates 
### that the fitted model is good for prediction to some extent. Why to some extent ?
### Because the points 131 and 151 may be potential outliers since they are very far from other points. 
### But we know that Large residuals (as in our case with 131 and 151 data points) could also indicate that 
### either the variance is not constant (heterosceadasticity) or the true relationship between target and predictors is nonlinear. 
### These possibilities should be investigated before the points are considered outliers.

# Score Test for Heteroscedasticity
ols_test_score(mm1)

### From the last line of the above output, It is clear that p-value is greater than the significance level 0.05. 
### Hence, we may accept the null hypothesis and conclude that the variance is homogeneous. i.e., Homoscedasticity

# Checking effect of Auto-correlation
durbinWatsonTest(mm1)

### From the above output, It is clear that p-value (0.326) > 0.05 , Hence, we may accept the null hypothesis and 
### conclude that there is no autocorrelation between errors. i.e., Errors are uncorrelated.

#### Checking Multicolinearity -
  
### Generally, Variance Inflation Factor is used to detect Multicolinearity. 
### As a rule of thumb, VIF greater than 5 or 10 represents Multicolinearity.

# Detecting Multicolinearity
vif(mm1)

### Note that Variance inflation factor for both predictors are less than 5 (as a rule of thumb) , 
### Hence there is no multicolinearity between predictors.

#### Checking Normality Assumption
### Shapiro Wilk Test is generally used to check normality assumption.

# Checking Normality of Errors
shapiro.test(mm1$residuals)

### Normality does not hold since p-value < 0.05
### Just plot histogram for residuals to get an idea about the pattern of distribution

# Plotting Histogram for Residuals
hist(mm1$residuals)

### We see that there is some problem with left tail. It may be due to the data points 131 
### and 151 as pointed out earlier.

#### Fitting Orthogonal Polynomial Regression with Diagnostic Plot 

# Fitting second order orthogonal polynomial model in two variables to avoid multicolinearity
pm1 <- lm(Sales ~ poly(TV , 2) + poly(Radio , 2) + TV:Radio  , data = train.data)

# Take a look on summary of the model
summary(pm1)

### From the above output, Notice that -
### Created model is statistically significant since p-value <<< 0.05 (see in the last line of output)
### From the coefficients section, it is clear that all coefficients are statistically significant since p-value <<< 0.05
### This second order orthogonal polynomial model explains 93.28% variability of target (Sales) that is a better 
### indication with respect to the multiple linear regression model with TV and Radio as predictor.
### Residual standard error for the model is 1.403

### Checking Whether this improvement in Adjusted R-squared is statistically significant -
### H0 : The improvement in Adjusted R-squared is not statistically significant.
### H1 : The improvement in Adjusted R-squared is statistically significant.

# Performing ANOVA to test the above stated null hypothesis
anova(mm1 , pm1)

### In the above output, the value (3.559e-13) indicates the p-value for testing null hypothesis. Since this value is extremely less than 0.05, 
### hence we have sufficient evidence from the data to reject the null hypothesis and accept the alternative.
### That's why the improvement in Adjusted R-squared is statistically significant.
### Hence, Adopt the second order orthogonal polynomial model at this stage.

# Fitting third order (orthogonal) polynomial model in two variables to avoid multicolinearity
pm2 <- lm(Sales ~ poly(TV , 3) + poly(Radio , 3) + TV:Radio  , data = train.data)

# Take a look on summary of the model
summary(pm2)

### It is clear from the coefficients section of the above output that third order of TV predictor is not statistically 
### significant (p-value > 0.05). Hence, Don't include this term in the model.

# Fitting third order (orthogonal) polynomial model in two variables to avoid multicolinearity but after removing third order of TV predictor
pm3 <- lm(Sales ~ poly(TV , 2) + poly(Radio , 3) + TV:Radio  , data = train.data)

# Take a look on summary of the model
summary(pm3)

### From the above output and using the information from second order orthogonal polynomial model stored in R-object pm2, Notice that:
### Created model is statistically significant since p-value <<< 0.05 (see in the last line of output)
### From the coefficients section, it is clear that all coefficients are statistically significant since p-value <<< 0.05
###This third order orthogonal polynomial model in two variables after removing third order of TV predictor explains 92.93% variability of 
### target (Sales) that is a better indication with respect to the second order orthogonal polynomial regression model.
### Residual standard error for the model is 1.377

# Performing ANOVA to test the above stated null hypothesis
anova(pm1 , pm3)

### In the above output, Notice the value in the last column of second row. This value (0.009299) indicates the p-value for 
### testing null hypothesis. Since this value is extremely less than 0.05, hence we have sufficient evidence from the data to 
### reject the null hypothesis and accept the alternative.
### That's why the improvement in Adjusted R-squared is statistically significant.
### Hence, Adopt the third order orthogonal polynomial model without third order of TV predictor at this stage.

#### Diagnostic Plots
### Checking Linearity Assumption 

# Residual Plot
plot(pm3 , 1)

### Notice from the above plot:
### Red line is approximately horizontal and linear at Residuals = 0 which indicates that Linearity assumption holds well.
### Residual fluctuates in a random manner inside a band drawn between Residuals = -4 to +4 which indicates that the fitted 
### model is good for prediction to some extent. Why to some extent ?
### Because again we see that the point 131 (Note : 151 is now with the other points) may be potential outlier since this is very far from other points.

#### Checking Homoscedasticity Assumption

# Score Test for Heteroscedasticity
ols_test_score(pm3)

### Errors have constant variance, p-value > 0.05

#### Checking Auto-correlation Assumption

# Checking effect of Auto-correlation
durbinWatsonTest(pm3)

### Errors are uncorrelated.

#### Checking Normality Assumption

# Checking Normality of Errors
shapiro.test(pm3$residuals)

### Errors are normally distributed.

#### Checking Multicolinearity 

# Detecting Multicolinearity
vif(pm3)

### Note that all values in the last column of the above output are less than 5 (as a rule of thumb) 
### Hence there is no multicolinearity.

#### Removing Observation number 131 from train data set -
### Now we have only a choice that Delete the observation number 131 from the train data set as it has large 
### residual (See : Residual Plot for pm3 object) and check whether Adjusted R-squared improves significantly. 
### If yes, then remove it otherwise include observation number 131 too.

# Creating Diagnostic metrics Table for model pm3
dm = augment(pm3)

### Notice from the above output that -
### This table consists of information on different diagnostic metrics such as Residuals (column - 9), 
### cooks distance (column - 12) and Studentized Residuals (column - 13) and many more.
### I will use last column of the above table to delete observation number 131.

# Checking minimum value of last column (Studentized Residuals)
min(dm$.std.resid)
max(dm$.std.resid)

### The above value of Studentized Residual is less than -3 (Rule of thumb), Hence it indicates an outlier. 
### So just remove that observation as follows:

# Checking the index of that observation in train data
which(dm$.std.resid  %in% min(dm$.std.resid))

### The above output indicates that the outlier is at index 105 in train data set. Just check the complete information
### about that row as follows -

# Info. about 98th row of train data set
train.data[103,]

### This is our target. We have to remove it from our train data set.

# Removing 98th row of outlier
train.data1 = train.data %>% filter(train.data$Sales !=  1.6)

# Checking number of rows in old train data set
nrow(train.data)

# Checking number of rows in new train data set (train.data1)
nrow(train.data1)

### Now, again fit the same orthogonal polynomial model as is stored in pm3 but 
### using the data stored in R-object train.data1 -

# Fitting third order orthogonal polynomial model in two variables TV and Radio but after removing third order of TV predictor using train.data1
pm4 <- lm(Sales ~ poly(TV , 2) + poly(Radio , 3) + TV:Radio  , data = train.data1)

# Take a look on summary of the model
summary(pm4)

### From the above output and using the information from second order orthogonal polynomial 
### model stored in R-object pm3, Notice that:
### Created model pm4 is statistically significant since p-value <<< 0.05 (see in the last line of output)
### From the coefficients section, it is clear that all coefficients are statistically significant since p-value <<< 0.05
### This polynomial model after removing the outlier explains 93.84% variability of target (Sales) that is a better 
### indication with respect to the polynomial regression model stored in R-object pm3.
### Residual standard error for the model is 1.325

### Now, Check all other assumptions in a quick:

# Linearity Assumption
plot(pm4 ,1)

# Homoscedasticity Assumption 
ols_test_score(pm4)

# Autocorrelation Assumption 
durbinWatsonTest(pm4)

# Normality Assumption
shapiro.test(pm4$residuals)

# Multicolinearity Assumption
vif(pm4)

### All assumptions have been satisfied now.

### Checking outliers again by creating Diagnostic metric table for model pm4

# Creating Diagnostic metric table for model pm4
dm1 = augment(pm4)

# Checking minimum and maximum value of Studentized Residuals
min(dm1$.std.resid)
max(dm1$.std.resid)

### The above output shows that Studentized Residuals are not greater than 3 (rule of thumb) 
### in absolute value. Hence, there are no potential outliers.

#### Making Predictions 

# Making Predictions
prediction = pm4 %>% predict(test.data)

# Checking performance by calculating R2 , RMSE and MAE
data.frame( R2 = R2(prediction, test.data$Sales),
            RMSE = RMSE(prediction, test.data$Sales),
            MAE = MAE(prediction, test.data$Sales))





























