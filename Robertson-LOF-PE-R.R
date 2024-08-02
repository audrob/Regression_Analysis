# Lack of Fit and Pure Errors

# Declare Libs
library(car)
library(dplyr)
library(tidyverse)

# Import data file

diabetes <- read.table("path\\data.txt", header=T)
summary(diabetes)

# Simple linear regression between hba1c and fbg for diabetics
# Show parameter estimations

d1 <- diabetes %>% 
  filter(hba1c>6.5)

r1 <- lm(hba1c~fbg, data=d1)
summary(r1)

# Confidence intervals of regression model parameter estimates

confint(r1)

# Plot the confidence intervals for the regression and the future predicted 
#   values
tt = order(d1$fbg)
conf_lim = predict(r1, interval="confidence")
pred_lim = predict(r1, interval="prediction")
matplot(d1$fbg[tt], cbind(conf_lim, pred_lim[,-1])[tt,], type="l", 
        ylab="Predicted hbA1c", xlab="Fasting Blood Glucose", lty=c(1, 2, 2, 3, 3),
        col=c(1, 2, 2, 3, 3))

# create intervals for fbg data and subset into new var
d1$int_mp <- cut(d1$fbg, breaks=c(-Inf,4,5,6.5,8,10,Inf), 
                        labels = c(3.5,4.5,5.75,7.25,9,10.5))
d1$int_mp <- as.numeric(as.character(d1$int_mp))
r1i <- lm(hba1c~int_mp,data=d1)
summary(r1i)
                              

#Test assumptions
# 1. Plot to examine LOF

plot(d1$fbg,d1$hba1c,xlab="Fasting Blood Glucose",ylab="Hemoglobin A1c", 
     cex=0.5, pch=19, col='black', cex.axis=1, cex.lab=1)
abline(r1$coef, lwd=2, col=2)

plot(x=r1$fitted.values, y=r1$residuals, cex=0.5, pch=16, col="red", cex.lab=1,
     cex.axis=1, xlab="Fitted values",
     ylab="Residuals")
abline(h=0)


# Plot data with fbg separated into intervals to demonstrate

plot(d1$int_mp,d1$hba1c,xlab="Fasting Blood Glucose, Intervals",ylab="Hemoglobin A1c", 
     cex=0.5, pch=19, col='black', cex.axis=1, cex.lab=1)
abline(r1i$coef, lwd=2, col=2)

plot(x=r1i$fitted.values, y=r1i$residuals, cex=0.5, pch=16, col="red", cex.lab=1,
     cex.axis=1, xlab="Fitted values",
     ylab="Residuals")
abline(h=0)


residualPlots(r1)

summary(lm(abs(r1$residuals) ~ r1$fitted.values))
#the residual plots show patterns.
#model should be adjusted/transformed.


# LOF anova to examine pure errors
ra1i <- lm(hba1c~factor(int_mp), data=d1)
anova(r1i,ra1i)


# 2. Homoscedasticity of errors
# Levene's test wtih mean
summary(d1$hba1c);summary(d1$fbg)
## Median and mean are approximately equivalent for hba1c
## Levene's test for medians is appropriate

# Levene's test with median
leveneTest(d1$hba1c, as.factor(d1$int_mp), center=median)



# 4. Normality of Errors

hist_data <- hist(r1$residuals, xlab="Residuals", main="", 
                  col="grey", cex.lab=1, cex.axis=1)
hist_data
# Define x and y values to use for normal curve
x_values <- seq(min(r1$residuals), max(r1$residuals), length = 100)
y_values <- dnorm(x_values, mean = mean(r1$residuals), sd = sd(r1$residuals))
y_values <- y_values * diff(hist_data$mids[1:2]) * length(r1$residuals)
# Overlay normal curve on histogram
lines(x_values, y_values, lwd = 2, col = 'red')

#relatively symmetric curve
qqnorm(r1$residuals, main="", pch=19, col="black", cex=0.5, cex.lab=1, cex.axis=1)
abline(0,1, col="red", lwd=2)
#appears reasonably normal
shapiro.test(r1$residuals) 
#p-value is significant, reject the null that the residuals are normal

#ANOVA for model significance
anova(r1)
ss_reg <- sum((r1$fitted.values - mean(d1$hba1c))^2); ss_reg
ms_reg <- (ss_reg/1);ms_reg

ss_residual <- sum((d1$hba1c - r1$fitted.values)^2); ss_residual
ms_residual <- (ss_residual/(nobs(r1)-2));ms_residual

ss_total <- sum((d1$hba1c- mean(d1$hba1c))^2); ss_total

F_Reg <- (ms_reg/ms_residual);F_Reg

r_squared <-1-ss_residual/ss_total; r_squared
adj_r_squared <-1-(1-r_squared)*((nrow(d1)-1)/(nrow(d1)-length(r1$coefficients)-1)); adj_r_squared


#Part B
#linear model between hba1c and fbg for all subjects
#Parameter estimates
r2 <- lm(hba1c~fbg, data=diabetes)
summary(r2)

# Confidence intervals of regression model parameter estimates

confint(r2)

# Plot the confidence intervals for the regression and the future predicted 
#   values
tt = order(diabetes$fbg)
conf_lim = predict(r2, interval="confidence")
pred_lim = predict(r2, interval="prediction")
matplot(diabetes$fbg[tt], cbind(conf_lim, pred_lim[,-1])[tt,], type="l", 
        ylab="Predicted hbA1c", xlab="Fasting Blood Glucose", lty=c(1, 2, 2, 3, 3),
        col=c(1, 2, 2, 3, 3))

#Create intervals
diabetes$int_2 <- cut(diabetes$fbg, breaks=c(-Inf, 4, 4.5, 5, 5.5, 6, 6.5, 7, 
                                             7.5, 8, 8.5, 9, 9.5, 10, Inf),
                      labels = c(4,4.25,4.75,5.25,5.75, 6.25, 6.75, 7.25, 
                                 7.75, 8.25, 8.75, 9.25, 9.75, 10),
                      include.lowest=T)
diabetes$int_2 <- as.numeric(as.character(diabetes$int_2))
r2i <- lm(hba1c~int_2, data=diabetes)


# Regression hba1c ~ fbg with fbg being a factor type of variable
r2a <- lm(hba1c~factor(fbg), data=diabetes)
r2ai <- lm(hba1c~factor(int_2), data=diabetes)

#plot data to examine LOF
plot(diabetes$fbg,diabetes$hba1c,xlab="Fasting Blood Glucose",ylab="Hemoglobin A1c", 
     cex=0.5, pch=19, col='black', cex.axis=1, cex.lab=1)
abline(r1$coef, lwd=2, col=2)

plot(x=r2$fitted.values, y=r2$residuals, cex=0.5, pch=16, col="red", cex.lab=1,
     cex.axis=1, xlab="Fitted values",
     ylab="Residuals")
abline(h=0)

#LOF with interval data
summary(r2i)
anova(r2i)
#Plot Interval data to demonstrate
plot(diabetes$int_2,diabetes$hba1c,xlab="Fasting Blood Glucose, Intervals",ylab="Hemoglobin A1c", 
     cex=0.5, pch=19, col='black', cex.axis=1, cex.lab=1)
abline(r1$coef, lwd=2, col=2)

plot(x=r2i$fitted.values, y=r2i$residuals, cex=0.5, pch=16, col="red", cex.lab=1,
     cex.axis=1, xlab="Fitted values",
     ylab="Residuals")
abline(h=0)

# Comparison between two models in order to study pure errors LOF
anova(r2, r2a)
anova(r2i, r2ai)


#ANOVA test of linear model
anova(r2)

#Part C
# linear model for hba1c = fbg + tg
r3 <- lm(hba1c ~ fbg + tg, data=diabetes)
summary(r3)

#confidence intervals for paremeter estimates

confint(r3)

# Plot the confidence intervals for the regression and the future predicted 
#   values
tt = order(diabetes$fbg, diabetes$tg)
conf_lim = predict(r3, interval="confidence")
pred_lim = predict(r3, interval="prediction")
matplot(diabetes$fbg[tt], cbind(conf_lim, pred_lim[,-1])[tt,], type="l", 
        ylab="Predicted hbA1c", xlab="Predictors", lty=c(1, 2, 2, 3, 3),
        col=c(1, 2, 2, 3, 3))

# ANOVA for predictor significance
anova(r3)

# ANOVA for model significance
ss_reg2 <- sum((r3$fitted.values - mean(diabetes$hba1c))^2); ss_reg2
ms_reg2 <- (ss_reg2/1);ms_reg2

ss_residual2 <- sum((diabetes$hba1c - r3$fitted.values)^2); ss_residual2
ms_residual2 <- (ss_residual2/(nobs(r3)-2));ms_residual2

ss_total2 <- sum((diabetes$hba1c- mean(diabetes$hba1c))^2); ss_total

F_Reg2 <- (ms_reg2/ms_residual2);F_Reg2

# Calculate R-squared
r_squared2 <-(ss_reg2/ss_total2)
adj_r_squared2 <-1-(1-r_squared2)*
  ((nrow(diabetes)-1)/(nrow(diabetes)-length(r3$coefficients)+1-1))

r_squared2
adj_r_squared2

