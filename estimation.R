library(MASS)
library(irr)

# Set parameters
n <- 200
b <- c(1,2,3,4)

# Generate random data
age <- rpois(n,65)
med <- rpois(n,2)
weight <- round(rnorm(n,mean=60,sd=10))
sex <- rbinom(n,1,.5)

# Generate random risk level according to linear model
risk <- b[1]*age+b[2]*med+b[3]*weight+b[4]*sex+rt(n,1)
risk <- round(4*(risk-min(risk))/(max(risk)-min(risk)))+1

# Convert risk level number to letter (A to E)
risk <- factor(risk, ordered = TRUE)
levels(risk) <- letters[5:1]

# Convert sex variable to factor
sex <- factor(sex)
levels(sex) <- c("Female", "Male")

# Create data frame and separate it into two subset
pharma_scale <- data.frame(risk = risk , age = age, med = med, weight = weight, sex = sex)
train <- pharma_scale[1:150,]
test <- pharma_scale[151:200,]

# Ordered logit on train data
train.plr <- polr(risk ~ age + med + weight + sex,  method = "logistic", data = train)
summary(train.plr)

# Predict model on test data
test.pred <-ordered(predict(train.plr, test), letters[5:1])

# Test Cohen's kappa
kappa2(data.frame(test$risk,test.pred))