# READING THE DATA 
senic <- read.csv("C:/Users/nehal/Desktop/MTU/MTU/Courses/MA4710 - RA/MathsFinalProject/SENIC.csv")
head(senic)
install.packages("dplyr")
library(dplyr)
# Getting the required data frame for Analysis
df <- senic
View(df)
# Creating the design matrix of X
Y <- matrix(df[,1], ncol=1)
X <- as.matrix(df[,-1])
X <- cbind(1,X)
colnames(X)[1] <- 'Intercept'
colnames(Y)[1] <- 'Y'
View(X)
View(Y)

## Exploratory Data Analysis
# Creating the histogram of the target and predictor variables
install.packages('ggplot2')
library(ggplot2)
# Building the histogram
ggplot(data=df, aes(df$Y)) +
  geom_histogram(aes(y =..density..), fill = "red") +
  geom_density()
ggplot(data=df, aes(df$X1)) +
  geom_histogram(aes(y =..density..), fill = "blue") +
  geom_density()
ggplot(data=df, aes(df$X2)) +
  geom_histogram(aes(y =..density..), fill = "green") +
  geom_density()
ggplot(data=df, aes(df$X3)) +
  geom_histogram(aes(y =..density..), fill = "black") +
  geom_density()
ggplot(data=df, aes(df$X4)) +
  geom_histogram(aes(y =..density..), fill = "purple") +
  geom_density()
ggplot(data=df, aes(df$X5)) +
  geom_histogram(aes(y =..density..), fill = "blue") +
  geom_density()
ggplot(data=df, aes(df$X6)) +
  geom_histogram(aes(y =..density..), fill = "green") +
  geom_density()
ggplot(data=df, aes(df$X7)) +
  geom_histogram(aes(y =..density..), fill = "black") +
  geom_density()
ggplot(data=df, aes(df$X8)) +
  geom_histogram(aes(y =..density..), fill = "purple") +
  geom_density()
ggplot(data=df, aes(df$X9)) +
  geom_histogram(aes(y =..density..), fill = "blue") +
  geom_density()
ggplot(data=df, aes(df$X10)) +
  geom_histogram(aes(y =..density..), fill = "green") +
  geom_density()

# Getting the Summary Statistics using the psych package
install.packages('psych')
library(psych)
psych::describe(df)
# Creating the correlation matrix
cor_df <- cor(df)
cor_df
library(GGally)
#generate the pairs plot
ggpairs(df)
# Visualizing the correlation matrix i.e., getting a scatter plot matrix
pairs(df)
# Getting the box plots to look for the outliers
library(reshape)
senicData <- melt(df)
boxplot <- ggplot(senicData, aes(factor(variable), value))
boxplot + geom_boxplot() + facet_wrap(~variable, scale="free")

# Added-Variable Plots 
install.packages('car')
library(car)
# Fitting the model
lmfit <- lm(Y ~ . , data = df)
avPlots(lmfit)


####################################################################
# For scaling, however no change was visible
summary(lmfit)
library(plyr)
library(readr)
library(ggplot2)
library(GGally)
library(dplyr)
library(mlbench)

library(caret)

preproc2 <- preProcess(df, method=c("range"))
norm2 <- predict(preproc2, df)
summary(norm2)
lmfit_norm <- lm(Y ~ . , data = norm2)
vif(lmfit_norm)

############################################################################################
# Model fitting 

# Fit a multiple linear regression model 

lmfit.full <- lm(Y ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10, data=df)
summary(lmfit.full)
anova(lmfit.full)

# Multicollinearity check

summary(lmfit.full)
cor(df)   ###### The signs of some estimates from the output and the correlation matrix are different. 
anova(lmfit.full)

# Check Variance Inflation Factor

vif(lmfit.full)

#  Consider other regression models

lmfit1 <- lm(Y ~ X1 + X2 + X3 + X4 +  X6 + X7 + X8 + X9 + X10, data=df)
lmfit2 <- lm(Y ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 +  X9 + X10, data=df)
summary(lmfit1)
summary(lmfit2)
# Comparison between the full model and the reduced models 
anova(lmfit1, lmfit.full)
anova(lmfit2, lmfit.full)
# VIF on the reduced models 
vif(lmfit1) 
vif(lmfit2)

#################### Model Selection (Step wise Regression) #####################

#### Install packages for the model selection ####
install.packages("leaps")
install.packages("HH")
install.packages("StepReg")
#### Load HH, leaps, and StepReg packages ####
library(leaps)
library(HH)
library(StepReg)
par(mfrow=c(1,1))
#### Stepwise Regression ####
#### Adjusted R2 ####
b = bestsubset(data=df,y="Y",select="adjRsq",best=10)
print(b)
stepwise(data=df,y="Y",select="adjRsq")
plot(b[,1:2])

#### Cp ####
b = bestsubset(data=df,y="Y",select="CP",best=10)
print(b)
stepwise(data=df,y="Y",select="CP")
plot(b[,1:2])

#### AIC ####
b = bestsubset(data=df,y="Y",select="AIC",best=10)
print(b)
stepwise(data=df,y="Y",select="AIC")
plot(b[,1:2])
#### BIC ####
b = bestsubset(data=df,y="Y",select="BIC",best=10)
print(b)
stepwise(data=df,y="Y",select="BIC")
plot(b[,1:2])


#### Final Model? ####
lmfit_reduced_final <- lm(Y ~ X1 + X2 + X4 + X7 + X8 + X9 + X10 + X5 + (X1 + X2 + X4  + X8 + X9 + X10 + X5)* X7, data=df)
reduced.lmfit <- lm(Y ~ X1 + X2 + X4 + X7 + X8 + X9 + X10, data=df)
summary(reduced.lmfit)

######### Assumption Checking #################

# Regression Diagnostics #

res <- rstudent(reduced.lmfit)
fitted.y <- fitted(reduced.lmfit)

######## Residual Plots ##########

par(mfrow=c(2,2))
plot(res ~ df$X1, xlab="X1", ylab="Residual", main="Residuals vs. X1")
abline(h=0)
plot(res ~ df$X2, xlab="X2", ylab="Residual", main="Residuals vs. X2")
abline(h=0)
plot(res ~ df$X4, xlab="X4", ylab="Residual", main="Residuals vs. X4")
abline(h=0)
plot(res ~ df$X7, xlab="X7", ylab="Residual", main="Residuals vs. X7")
abline(h=0)
plot(res ~ df$X8, xlab="X8", ylab="Residual", main="Residuals vs. X8")
abline(h=0)
plot(res ~ df$X9, xlab="X9", ylab="Residual", main="Residuals vs. X9")
abline(h=0)
plot(res ~ df$X10, xlab="X10", ylab="Residual", main="Residuals vs. X10")
abline(h=0)

plot(res ~ fitted.y, xlab="Fitted value", ylab="Residual", main="Residuals vs. Fitted Values")
abline(h=0)

library(GGally)
#generate the pairs plot
ggpairs(df)

######### Normality ###########

qqnorm(res);qqline(res)
shapiro.test(res)
hist(res, main="Historgram of residuals",
     xlab="Residaul",probability = TRUE)
lines(seq(-3,3,length.out = 1000),dnorm(seq(-3,3,length.out = 1000)))


########
 # Checking normality using the Shapiro test
shapiro.test(res)


### Checking the constancy of variance using Breush Pagan test
library(lmtest) 
bptest(reduced.lmfit)


### Checking the independence using the Darwin- Watson test
library(lmtest)
dwtest(reduced.lmfit)

############# Detecting outliers ##########


# Detection methods of influential points
# Install a package, called "olsrr" 
install.packages("olsrr")
library(olsrr)
# DFFITS 
ols_plot_dffits(reduced.lmfit)
# Cook's D 
ols_plot_cooksd_chart(reduced.lmfit)
# DFBETAS 
ols_plot_dfbetas(reduced.lmfit)
cooks.distance(reduced.lmfit)
dffits(reduced.lmfit)
dfbetas(reduced.lmfit)


## Testing Multicollinearity 
# Check Variance Inflation Factor
vif(reduced.lmfit)

######### Transformation #########
library(EnvStats)
boxcox.summary <- boxcox(reduced.lmfit, optimize=TRUE)
lambda <- boxcox.summary$lambda
trans.Y <- df$Y^lambda
df <- cbind(df,trans.Y)
######### Re-fitting a model using the transformed response variable. ##########
boxcox.lmfit <- lm(trans.Y ~ X1 + X2 + X4 + X7 + X8 + X9 + X10 , data=df)
summary(boxcox.lmfit)
boxcox.res <- rstudent(boxcox.lmfit)
boxcox.fitted.y <- fitted(boxcox.lmfit)
######### Multicollinearity ##########
vif(boxcox.lmfit) ### OK ###
######## Residual Plots ##########
par(mfrow=c(2,2))
plot(boxcox.res ~ df$X1, xlab="X1", ylab="Residual", main="Residuals vs. X1")
abline(h=0)
plot(boxcox.res ~ df$X2, xlab="X2", ylab="Residual", main="Residuals vs. X2")
abline(h=0)
plot(boxcox.res ~ df$X4, xlab="X4", ylab="Residual", main="Residuals vs. X4")
abline(h=0)
plot(boxcox.res ~ df$X7, xlab="X7", ylab="Residual", main="Residuals vs. X7")
abline(h=0)
plot(boxcox.res ~ df$X8, xlab="X8", ylab="Residual", main="Residuals vs. X8")
abline(h=0)
plot(boxcox.res ~ df$X9, xlab="X9", ylab="Residual", main="Residuals vs. X9")
abline(h=0)
plot(boxcox.res ~ df$X10, xlab="X10", ylab="Residual", main="Residuals vs. X10")
abline(h=0)
plot(boxcox.res ~ boxcox.fitted.y, xlab="Box-Cox Fitted value", ylab="Residual", main="Residuals vs. Fitted Values")
abline(h=0)
library(GGally)
#generate the pairs plot
ggpairs(df)
######### Constancy of Error Variances #########
library(lmtest) 
bptest(boxcox.lmfit)
######### Normality ###########
qqnorm(boxcox.res);
qqline(boxcox.res)
shapiro.test(boxcox.res)
######### Final Model ##########
final.lmfit <- boxcox.lmfit
summary(final.lmfit)
anova(final.lmfit)






