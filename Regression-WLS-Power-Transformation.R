# Regressions Analysis: OLS, WLS, & Power Transformation

# Declare packages needed
library(car)
library(dplyr)
library(tidyverse)
library(ggplot2)

# Adjust the path to your Java installation
options(java.home = "C:\\Program Files\\Java\\jre-1.8") 
library(xlsx)
library(rJava)

library(nlme)
library(MASS)

####                             Question 1                                ####

# Market Share Data of a product for 36 months
# Y = market share of product
# X1 = price of product
# X2 = Discount Promotion. 0 = no discount, 1 = discount in effect

## import data file
# header not transferable so column names added manually
data1 <- read.table("path\\market_share.txt", 
                    skip=1, 
                    col.names = c('ID','Market_share','Price',
                                  'Discount','Promotion','Month','Year'))


###                                   a)                                    ###
# Conduct regression analysis for Y versus X1 X2 and interaction X1*X2
  # between X1 and X2.

# Dummy variables already coded

# Fit linear model
data1_lm <- lm(Market_share ~ Price + Discount + Price*Discount, data=data1)

## Summarize
summary(data1_lm)
data1_anova <- anova(data1_lm)
write.xlsx(data1_anova,
           file="path\\HW3_1a.xlsx",
           sheetName = 'HW31a',
           col.names = T)
# Market Share = 3.1075 - 0.2956[Price] + 0.4154[Discount] + 0.0008[Price*Discount]
# Y = 3.1075 - 0.2956[X1] + 0.4154[X2]+ 0.0008[X1X2]
# Price is a significant predictor of market share


###                                   b)                                    ###
# Create the residual vs predicted value plot, using different colors 
  # for subjects with different X2 values.

# Create data frame for plot
data1_plot <- data.frame(
  Predicted = predict(data1_lm),
  Residuals = residuals(data1_lm),
  Discount = data1$Discount
)

# Use ggplot to create plot
ggplot(data1_plot, aes(Predicted, Residuals, color=factor(Discount)))+
  geom_point(shape=16) +
  geom_hline(yintercept=0) +
  labs(title = "Residuals versus Predicted Values",
       x = "Predicted Values", y = "Pearson Residuals",
       colour = "Discount") +
  scale_colour_manual(name="Discount",
                      values=c('red','blue')) 


# Create data frame for plot (studentized residuals)
data1_plots <- data.frame(
  Predicted = predict(data1_lm),
  sResiduals = studres(data1_lm),
  Discount = data1$Discount
)

# Use ggplot to create plot
ggplot(data1_plots, aes(Predicted, sResiduals, color=factor(Discount)))+
  geom_point(shape=16) +
  geom_hline(yintercept=0) +
  labs(title = "Residuals versus Predicted Values",
       x = "Predicted Values", y = "Pearson Residuals",
       colour = "Discount") +
  scale_colour_manual(name="Discount",
                      values=c('red','blue')) 

###                                   c)                                    ###
#	Calculate the variance of residuals for subjects with X2 = 0, 

# subset data
data1_no_discount <- data1 %>%
  select(Market_share, Price, Discount) %>%
  filter(Discount == '0')

#fit linear model and put residuals into data frame
no_discount_resid <- data.frame(Residuals= residuals(lm(Market_share ~ 
                                     Price + Discount + Price*Discount,
                                   data=data1_no_discount)))
# export
write.xlsx(no_discount_resid,
           file="path\\HW3_1c1.xlsx",
           sheetName = 'HW31c1',
           col.names = T)


# and the variance of residuals for subjects with X2 = 1.

# subset data
data1_discount <- data1 %>%
  select(Market_share, Price, Discount) %>%
  filter(Discount == '1')


# fit linear model and put residuals into data frame
discount_resid <- data.frame(Residuals= residuals(lm(Market_share ~ 
                                     Price + Discount + Price*Discount, 
                                   data=data1_discount)))
# export
write.xlsx(discount_resid,
           file="path\\HW3_1c2.xlsx",
           sheetName = 'HW31c2',
           col.names = T)


###                                   d)                                    ###
# Conduct a WLS analysis for Y vs X1, X2, and interaction between X1 & X2, 
  # using the calculated residual variances for determining the weights.

#create new data frame with residual variance and weights=1/residual variance

data1_WLS <- data1 %>%
  mutate(
    var_res = ifelse(Discount == '0',var(no_discount_resid),
                     var(discount_resid))) %>%
  mutate(
    weights=(1/var_res)
  )

# WLS can be conducted using "lm" function, with an option for weight specified by the
# option "weights". weights = 1/var(resid)
data1_lm_WLS <- lm(Market_share ~ Price + Discount + Price*Discount, 
                   data=data1_WLS,  
                   weights=weights)


###                                   e)                                    ###
# Summarize WLS analysis
summary(data1_lm_WLS)

# anova and export
data1_WLS_anova <- as.data.frame(anova(data1_lm_WLS))
write.xlsx(data1_WLS_anova,
           file="path\\HW3_1e.xlsx",
           sheetName = 'HW31e',
           col.names = T)
# Market Share = 3.753 - 0.2956[Price] + 0.4154[Discount] + 0.0008[Price*Discount]
# Y = 3.1075 - 0.2956[X1] + 0.4154[X2]+ 0.0008[X1X2]
# Discount is a significant predictor of Market share

# and re-plot the residual plot 
# (with studentized residuals vs predicted). 
# Create data frame for plot
data1_WLS_plot <- data.frame(
  Predicted = predict(data1_lm_WLS),
  sResiduals = studres(data1_lm_WLS),
  Discount = data1_WLS$Discount
)

# Use ggplot to create plot
ggplot(data1_WLS_plot, aes(Predicted, sResiduals, color=factor(Discount)))+
  geom_point(shape=16) +
  geom_hline(yintercept=0) +
  labs(title = "Studentized Residuals versus Predicted Values",
       x = "Predicted Values", y = "Studentized Residuals",
       colour = "Discount") +
  scale_colour_manual(name="Discount",
                      values=c('red','blue')) 
# (Qualitatively) describe whether there is still evidence for issues in 
  # the variance of residuals.

# Plot shows increased residual variance where Discount = 0



####                             Question 2                                ####
# “cars.txt” contains data about car speed and the distance needed to stop. 

## import data file
data2 <- read.table("path\\cars.txt", 
                    header=T)
str(data2)

###                                   a)                                    ###

# Create a scatterplot between the distance and car speed.
ggplot(data2, aes(speed, dist)) +
  geom_point(shape=16) +
  labs(title = "Distance versus Car Speed",
       x = "Speed", y = "Distance")

###                                   b)                                    ###

# Conduct a simple linear regression between distance and speed.

data2_lm <- lm(dist ~ speed, data = data2)
summary(data2_lm)

# anova and export
data2_anova <- as.data.frame(anova(data2_lm))
write.xlsx(data2_anova,
           file="path\\HW3_2a.xlsx",
           sheetName = 'HW32a',
           col.names = T)
data2
###                                   c)                                    ###

# Conduct a power transformation on distance, and determine the best value 
  # for lambda.
# Box-Cox transformation, needing the package "MASS"
boxcox <- boxcox(data2_lm, plotit=T) # default range for lambda: (-2, 2)
lambda <- boxcox$x[which.max(boxcox$y)]
cat("lambda =",lambda)
# lambda ~ 0.42


# transform using boxcox V
dist_gm <- (nrow(data2)^(-1)) * (sum(log10(data2$speed)))
print(dist_gm)

data2_t <- data2 %>%
  mutate(dist_t = (dist^(lambda)-1)/(lambda*dist_gm^(lambda-1)))

# fit linear model
data2_t_lm <- lm(dist_t ~ speed, data=data2_t)
summary(data2_t_lm)

# anova and export
data2_t_anova <- anova(data2_t_lm)
write.xlsx(data2_t_anova,
           file="path\\HW3_2c.xlsx",
           sheetName = 'HW32c',
           col.names = T)

# scatter plot of transformed data
ggplot(data2_transform, aes(speed, dist_t)) +
  geom_point(shape=16) +
  labs(title = "Distance versus Car Speed",
       subtitle = "Transformed Response (Distance)",
       x = "Speed", y = "Distance")

###                                   d)                                    ###

# Conduct a transformation on both distance and speed, and conduct regression 
  # analysis on the transformed values.

# Transform both sides of the model

## Y^lambda = f(X,Beta)^lambda
# Fit linear regression model with both transformed distance and speed
data2_t2 <- data.frame(speed_t=((data2$speed)^(lambda)),
                       dist_t=((data2$dist)^(lambda)))
data2_t2_lm <- lm(dist_t ~ speed_t, data = data2_t2)
summary(data2_t2_lm)

# anova and export
data2_t2_anova <- as.data.frame(anova(data2_t2_lm))
write.xlsx(data2_t2_anova,
           file="path\\HW3_2d.xlsx",
           sheetName = 'HW32d',
           col.names = T)

#scatter plot of the transformed data
ggplot(data2_t2, aes(speed_t, dist_t)) +
  geom_point(shape=16) +
  labs(title = "Distance versus Car Speed",
       subtitle = "Transformed Response (Distance) and Explanatory (Speed)",
       x = "Speed", y = "Distance")

## log(Y) ~ log(X). Power Transformation
# Fit linear regression model with both transformed distance and speed
data2_t2 <- data.frame(speed_t=(log(data2$speed)),
                       dist_t=(log(data2$dist)))
data2_t2_lm <- lm(dist_t ~ speed_t, data = data2_t2)
summary(data2_t2_lm) # This transformation fits better than the lambda transformtion


# anova and export
data2_t2_anova <- as.data.frame(anova(data2_t2_lm))
write.xlsx(data2_t2_anova,
           file="path\\HW3_2d.xlsx",
           sheetName = 'HW32d',
           col.names = T)

#scatter plot of the transformed data
ggplot(data2_t2, aes(speed_t, dist_t)) +
  geom_point(shape=16) +
  labs(title = "Distance versus Car Speed",
       subtitle = "Transformed Response (Distance) and Explanatory (Speed)",
       x = "Speed", y = "Distance")

###                                   e)                                    ###

#	Compare the regression models in b, c, d, in terms of R^2
# Extract R-squared values for the original and transformed regression models
r_squared_original <- summary(data2_lm)$r.squared
r_squared_transformed <- summary(data2_t_lm)$r.squared
r_squared_both_transformed <- summary(data2_t2_lm)$r.squared

cat("R-squared of original linear model:",r_squared_original)
cat("R-squared of model with transformed response:",r_squared_transformed)
cat("R-squared of model with both sides transformed:",r_squared_both_transformed)

# b: R^2 = 0.6511. 
## 65.11% perecent of the variation in distance can be explained by this model.

# c: R^2 = 0.7126
## 71.26% perecent of the variation in distance can be explained by this model.

# d: R^2 = 0.7148
## 71.48% perecent of the variation in distance can be explained by this model.
