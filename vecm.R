library(tsDyn)
library(vars)
library(tseries)
library(foreign)
library(readxl)

# Load data
canada <- read_excel("~/Projects/pharma/canada.xlsx", 
                     sheet = "Dataset", col_types = c("blank", "numeric", "numeric", "numeric", 
                                                      "numeric", "numeric", "numeric", "numeric"))
# Create time series with data
canada <- ts(canada, start = 1997, end = 2015.75, frequency = 4)

# Plot data
plot(canada)

# Augmented Dickey-Fuller to see if it is I(1)
adf.test(canada[,"invest_construction"])
adf.test(diff(canada[,"invest_construction"])) #I(1)

# Augmented Dickey-Fuller to see if it is I(1)
adf.test(canada[,"interest_rate"])
adf.test(diff(canada[,"interest_rate"])) #I(1)

# Augmented Dickey-Fuller to see if it is I(1)
adf.test(canada[,"gdp"])
adf.test(diff(canada[,"gdp"])) #I(1)

# Augmented Dickey-Fuller to see if it is I(1)
adf.test(canada[,"unemployment_rate"])
adf.test(diff(canada[,"unemployment_rate"])) #I(1)

# Create data subset with variable of interest
Y <- canada[, c("invest_construction", "interest_rate", "gdp", "unemployment_rate", "price_index")]
plot(Y)

# Test to determine best lag
VARselect(Y, lag.max = 4)

# Johansen Cointegration test to determine number of contintegration relationship (Note K = lag found with VARselect)
summary(ca.jo(Y, K = 3))

# VECM with r = number of cointegration rel. and lag = best lag
vecm.jo <-VECM(Y, r = 1, lag = 3, estim="ML")

# Predict values ahead
vecm.pred <- predict(vecm.jo, n.ahead = 20)

# Plot value of investment in construction with predicted value
plot(canada[,"invest_construction"], xlim = c(1997,2020))
lines(seq(from = 2016, by = 0.25, length.out = 20), vecm.pred[,"invest_construction"], lty = 2, col = 2)

# Print summary of VECM [to print to latex use toLatex(summary(vecm.jo))]
summary(vecm.jo)

