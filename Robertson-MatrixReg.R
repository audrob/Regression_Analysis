#Robertson BIOS-7060 HW 2

# Declare Libs
library(car)
library(dplyr)
library(tidyverse)
library(MethComp)


# Question 1: Perform Deming Regression on HW2_data1.txt + Regular regression

## import data file

data1 <- read.table("C:/Users/arobe/OneDrive/Desktop/DataSP24/HW2_data1.txt", 
                    header=T)
summary(data1)

# Variables: yy, xx, and x_star
## xx is observed values, x_star is unobserved true values
xx <- data1$xx
yy <- data1$yy
x_star <- data1$x_star


d1dem <- Deming(x=xx, y=yy);d1dem #y*=a+bx*
d1lm <- lm(yy~xx, data=data1);d1lm#y=a+bx or y+a+b(x*+eta)
summary(d1lm)
anova(d1lm)
var(d1lm$residuals)

d1lms <- lm(yy~x_star, data=data1);d1lms #y=a+bx* or y=a+b(x-eta)
summary(d1lms)
var(d1lms$residuals)
anova(d1lms)

eta <- xx-x_star
sd(eta)
cor(x_star,eta) # rho is not large
r <- sd(eta)/sd(xx);r # r is not large. <1
var(x_star)/var(xx)

## Plot data with the two classical regression lines
plot(xx, yy)
abline(d1lm) #r=0
ir <- coef(d1lm)
abline(-ir[1]/ir[2],1/ir[2])
abline(Deming(x=data1$xx, y=data1$yy,sdr=2)[1:2],col="red")
abline(Deming(x=data1$xx, y=data1$yy,sdr=10)[1:2],col="blue")

## Comparing classical regression and "Deming extreme"
summary(d1lm)
Deming(x=data1$xx, y=data1$yy,vr=1000000)





# Question 2, matrix operations

## input matrices
A <- matrix(c(1,2,3, 1,0,1), nrow=3, ncol=2)
B <- matrix(c(1,2, 1,0), nrow=2, ncol=2)
C <- matrix(c(1,2,3, 1,0,1, 1,1,0), nrow=3, ncol=3);C
D <- matrix(c(1,2,3, 1,0,1, 1,1,2), nrow=3, ncol=3)

## a) find detC and detD
det(C)
det(D)

## b) find AB
A %*% B

## c) find the inverse of B
solve(B)


#Question 3: Diabetes Study

## import data file

data2 <- read.table("C:/Users/arobe/OneDrive/Desktop/DataSP24/HW2_data2.txt", 
                    header=T)
summary(data2)

## 1) Find parameter estimates
d2lm <- lm(hba1c~fbg+tg,data=data2)
summary(d2lm)

### matrix method
### b = (X'X)^-1 * X'Y

X_Y <- matrix(c(7622,55937,13830), nrow=3, ncol=1)
X_Xin <- matrix(c(0.0091,-0.00085,-0.0014, 
                  -0.00085,0.00015,-0.00011, 
                  -0.0014,-0.00011,0.0012), nrow=3, ncol=3)
b <- X_Xin * 1.66
b
cov(b)

## 2) Find variances for b1, b2. Find their correlation coefficient.
## V(b)=(X'X)^-1 * sigma^2
r1 <- summary(d2lm)

r1$cov.unscaled # The unscaled matrix, (X'X)^(-1)

r1$cov.unscaled # Scaled matrix, (X'X)^(-1) * sigma^2
vcov(d2lm) # Scaled matrix again, (X'X)^(-1) * sigma^2

### Matrix method. 

### find Var of b1 and b2, cov(b1,b2), and corr(b1,b2)
var <- 1.66 #given s^2

Vb1 <- 0.000249
Vb2 <- 0.001992
covb1b2 <- -0.0001826
corrb1b2 <- (covb1b2/sqrt(Vb1*Vb2))
corrb1b2

# ANOVA table for regression
anova(d2lm)

# 95% confidence interval for parameters 
confint(d2lm)



# 3) find SS2 (partial SS) and SS1 (sequential SS)
anova(d2lm) #SS1 Sequential SS for order 1
summary(d2lm)
d2lmb <- lm(hba1c ~ tg + fbg, data=data2)
anova(d2lmb) #SS1 Sequential SS for order 2
summary(d2lmb)

SS2 <- Anova(d2lm, type='II') #SS2 Partial SS
as.data.frame(SS2)
Anova(d2lmb, type='II') #SS2 is same for order 2
