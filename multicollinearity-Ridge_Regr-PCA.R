#import data

credit <- read.csv(
  "path\\Credit.csv", header=T)

#import dplyr for management
library(dplyr)

### DIAGNOSE multicolinearity

#select quantitative vars
data <- credit %>%
  select(Income, Limit, Rating, Cards, Age, Education, Balance)


# Correlation among X's
round(cor(data, use="complete.obs"), 3) #Limit and rating highly correlated

#VIF using faraway
library(faraway)
g <- lm(Balance ~ ., data)
x <- as.matrix(data[,-7])
vif(x) #Limit and Rating have a VIF>10

# Eigenvalue for X'X
e0 = eigen(t(x) %*% x)
e = e0$values
round(e, 3)

# Condition index
sqrt(e[1]/e)

# Decomposition of variation of estimators
xsvd = svd(x)
v = xsvd$v
# The i-th column in the returned matrix represents the decomposition
#   of Var(b_i)
apply(v, 1, 
      function(x) {
        t = x^2/e
        return( round(t/sum(t), 3) )
      } 
)

#Remove Rating
g <- lm(Balance ~ Income+Limit+Cards+Age+Education, data)
x <- as.matrix(data[,c(-3,-7)])
vif(x) #All VIF under 10

# Eigenvalue for X'X
e0 = eigen(t(x) %*% x)
e = e0$values
round(e, 3)

# Condition index
sqrt(e[1]/e)

# Decomposition of variation of estimators
library(MASS)
xsvd = svd(x)
v = xsvd$v
# The i-th column in the returned matrix represents the decomposition
#   of Var(b_i)
apply(v, 1, 
      function(x) {
        t = x^2/e
        return( round(t/sum(t), 3) )
      } 
)

# Ridge regression
library(MASS)
gr <- lm.ridge(Balance ~., credit,lambda = seq(0,0.1,0.001))

# Plot of the coefficients vs lambda
matplot(gr$lambda, t(gr$coef), type="l", xlab=expression(lambda),
        ylab = expression(hat(beta)))
abline(h=0,lwd=2)

# Select the lambda value, based on various methods
select(gr)



# Create Dummy Vars
credit2 <- credit %>%
  mutate(O=case_when(Own=='No' ~ 0,
                     Own=='Yes' ~ 1),
         S=case_when(Student=='No' ~ 0,
                     Student=='Yes' ~ 1),
         M=case_when(Married=='No' ~ 0,
                     Married=='Yes' ~ 1),
         R1=case_when(Region=='East' ~ 0,
                      Region=='South' ~ 1,
                      Region=='West' ~ 0),
         R2=case_when(Region=='East' ~ 0,
                      Region=='South' ~ 0,
                      Region=='West' ~ 1))

# Regression Model
g <- lm(Balance~ Income+Limit+Cards+Age+Education+Own+Student+Married+Region, credit)
summary(g)

gd <- lm(Balance~ Income+Limit+Cards+Age+Education+O+S+M+R1+R2, credit2)
summary(gd)
credit2
x <- as.matrix(credit2[,c(-7:-11)])
vif(x) 

x <- as.matrix(credit2[,c(-3,-7:-11)])
vif(x) 

# Principal components regression
# Eigenvalues and eigenvectors at the original scale
x <- as.matrix(data[,-7])
e <- eigen(t(x) %*% x)
round(e$vec, 3)

# Eigenvalues/eigenvectors for the correlation matrix
e <- eigen(cor(x))
round(e$vec, 3)
round(e$val, 3)

# Elbow plot
plot(e$val, type="l") #elbow at 4. 3 PCs

# Principal components for scaled x
nx <- scale(x)
enx <- nx %*% e$vec
enx
# PC regression using all PCs
g1 <- lm(data$Balance ~ enx)
summary(g1)

# PC regression using the first two PCs
g2 <- lm(data$Balance ~ enx[, 1] + enx[,2])
summary(g2)

g3 <- lm(data$Balance ~ enx[, 1] + enx[,2] + enx[,3] + enx[,4] )
summary(g3)

model <- lm(Balance ~ Income + (Age-Rating) +  + Age, data=data)
summary(model)

