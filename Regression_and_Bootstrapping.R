# declare libs
library(dplyr)
library(tidyverse)
library(car)
library(boot)

# import data file 
forest_fire <- read.csv(
  "path\\ForestFireInfo.csv", 
  header=T)

####                              QUESTION 1                              ####

###                                 PART A                                ###
# Are rain and temp (individually and joint) important for area?

model1 <- lm(area ~ rain+temp+wind+month+wind*month, data=forest_fire)
model1
anova(model1)
# rain is not significant predictor for area
# temp is significant predictor for area
# interaction of rain * temp is not a significant predictor for area


###                                 PART B                                ###
# evaluate the residuals. Are there any potential issues?

# Residual plot for homoscedasticity
residualPlots(model1)
plot(x=model1$fitted.values, y=model1$residuals, cex=0.5, pch=16, col="red", 
     cex.lab=1, cex.axis=1, xlab="Fitted values",
     ylab="Residuals")
abline(h=0)

summary(lm(abs(model1$residuals) ~ model1$fitted.values))

# plot a histogram of the residuals to investigate normality of errors
hist(model1$residuals, xlab="Residuals", main="",
     col="grey", cex.lab=1, cex.axis=1.5) 

# QQ normal plot of the residuals to evaluate normality of errors
qqnorm(model1$residuals, main="", pch=16, col="red", cex=0.5, cex.lab=1, 
       cex.axis=1.5)
abline(0,1, col="blue", lwd=2) # The 45 degree straight line (y=x)

# Formal test for normality of errors. Shapiro-Wilk normality test 
shapiro.test(model1$residuals) 

# Upon investigating, the residuals appear to be distributed non-normally
  # Residual plot of e_i vs fitted values are not distributed  
    # within a horizontal band (h=0). This is evidence that errors 
    # are hetergeneous
  # Histogram- skewed, outliers present
  # QQ norm- outliers present
  # Shapiro-Wilks test for normailty shows siginificant evidence that 
    # residuals are not distributed normally.

# This model is concerning. 
# Potential outliers, heterscedasticity, non-normality of errors


####                              QUESTION 2                              ####

###                                 PART A                                ###
#	Perform a bootstrapping regression analysis for the data set in Q1. 
  #Particularly, “area” as outcome, and rain, temp, wind, month as predictors.

# fit linear model
forest_fire$month <- as.factor(forest_fire$month) # month is a categorical var
model2 <- lm(area ~ rain+temp+wind+month, data=forest_fire)
model1
model2
boot

# 1.	Obtain the bootstrap estimates for the regression coefficients of the four 
  # predictors.
# 2.	Obtain the bootstrap standard errors for these coefficients.

betahat.boot <- Boot(model2, R=2000)
betahat.boot
summary(model2)
bootsummary <- summary(betahat.boot)

# Calculating R-squared
rsq_function <- function(formula, data, indices) {
  d <- data[indices,]  # Allows boot to select a sample
  fit <- lm(formula, data=d)  # Fit the regression model
  return(summary(fit)$r.square)  # Return the R-squared of the model
}

# Perform bootstrapping with 2000 replicates, calculate R-squared
boot(data=forest_fire, statistic=rsq_function, R=2000, 
             formula=area ~ rain+temp+wind+month)

# Finding significance
# T = b1'*V11^-1*b1
# T*b -= t(z*b)
# T*b = (b*b1 - b1)'V*b,11^-1(b*b1-b1)
# \hat{p*} = #(T*b >= T)/r
rsq_function <- function(formula, data, indices) {
  d <- data[indices,]  # Allows boot to select a sample
  fit <- lm(formula, data=d)  # Fit the regression model
  return(summary(fit)$r.square)  # Return the R-squared of the model
}

# Define function to calculate fitted regression coefficients
coef_function <- function(formula, data, indices) {
  d <- data[indices,]
  fit <- lm(formula, data=d)
  return(coef(fit))
}

# Perform bootstrapping for each coefficient
reps_coef <- boot(data=forest_fire, statistic=coef_function,
                  R=2000, formula=area~rain+temp+wind+month)
reps_coef
# Perform bootstrapping with 2000 replicates, calculate R-squared
boot(data=forest_fire, statistic=rsq_function, R=2000, 
     formula=area ~ rain+temp+wind+month)

# 3.	Construct the bootstrap confidence intervals for these coefficients.
confint(betahat.boot)
hist(betahat.boot) 
# rain shows a concerning plot


# Bootstrap for the estimated residual standard deviation:
sigmahat.boot <- Boot(model2, R=2000, f=sigmaHat, labels="sigmaHat")
summary(sigmahat.boot)
confint(sigmahat.boot)


###                                 PART B                                ###
# Compare the results in a) with the corresponding 
 # values obtained via the approaches discussed in our lectures 

# MDOEL 1
# Summary of regression results
summary(model2)
confint(model2)

#create V and b matrices
V1 <- as.matrix(vcov(model2))
b1 <- as.matrix(coefficients(model2))
tb1 <- t(b1)
iV1 <- solve(V1)

Ts <- t(b1) %*% solve(V1) %*% b1

# Ti = bi'*Vi,i^-1*bi
Tsm <- data.frame(
Ts0= tb1[1,1] * iV1[1,1] * b1[1,1],
Ts1= tb1[1,2] * iV1[2,2] * b1[2,1],
Ts2= tb1[1,3] * iV1[3,3] * b1[3,1],
Ts3= tb1[1,4] * iV1[4,4] * b1[4,1],
Ts4= tb1[1,5] * iV1[5,5] * b1[5,1],
Ts5= tb1[1,6] * iV1[6,6] * b1[6,1],
Ts6= tb1[1,7] * iV1[7,7] * b1[7,1], 
Ts7= tb1[1,8] * iV1[8,8] * b1[8,1],
Ts8= tb1[1,9] * iV1[9,9] * b1[9,1],
Ts9= tb1[1,10] * iV1[10,10] * b1[10,1],
Ts10= tb1[1,11] * iV1[11,11] * b1[11,1], 
Ts11= tb1[1,12] * iV1[12,12] * b1[12,1],
Ts12= tb1[1,13] * iV1[13,13] * b1[13,1])

# MODEL 2
# Summary of regression results
summary(betahat.boot)
confint(betahat.boot)
#does not include 0:
  #temp
  #monthjun

#create V and b matrices
model2 <- lm(area ~ rain+temp+wind+month, data=forest_fire)
boot <- Boot(model2, R=2000)
boot
bb1 <- as.matrix(boot[,2])
bb1
b1
Vb <- as.matrix(vcov(boot)) #(X'X)^-1 * sigma^2

Tb <- t(bb) %*% solve(Vb) %*% (bb) #error, near-singluar
boot$t[,1]
#use lower tolerance
iVb <- solve(Vb,tol = 1e-20)
Tb <- t(bb) %*% Vb_inv %*% (bb)
tbb <- t(bb)
bb1 <- na.omit(bb1)
bb1 <- (bb1-b1)
tbb <- t(bb1-b1)
bb

# Tbi = bi'*Vi,i^-1*bi
Tbm <- data.frame(
Tb0= tbb[1,1] * iVb[1,1] * bb[1,1], 
Tb1= tbb[1,2] * iVb[2,2] * bb[2,1],
Tb2= tbb[1,3] * iVb[3,3] * bb[3,1], 
Tb3= tbb[1,4] * iVb[4,4] * bb[4,1],
Tb4= tbb[1,5] * iVb[5,5] * bb[5,1], 
Tb5= tbb[1,6] * iVb[6,6] * bb[6,1],
Tb6= tbb[1,7] * iVb[7,7] * bb[7,1], 
Tb7= tbb[1,8] * iVb[8,8] * bb[8,1],
Tb8= tbb[1,9] * iVb[9,9] * bb[9,1],
Tb9= tbb[1,10] * iVb[10,10] * bb[10,1],
Tb10= tbb[1,11] * iVb[11,11] * bb[11,1],
Tb11= tbb[1,12] * iVb[12,12] * bb[12,1],
Tb12= tbb[1,13] * iVb[13,13] * bb[13,1])
Tbm
Tstat <- data.frame(cbind(t(Tsm),t(Tbm)))

# significance
Tstat <- Tstat %>% 
  mutate(valid=(Tstat[,2]>=Tstat[,1])) %>% 
  mutate(p_val=case_when(valid == TRUE ~ Tstat[,1]/2000))
Tstat

sum(abs(boot$t[,2]-1) > abs(boot$t0[2]-1))/(1+boot$R)
