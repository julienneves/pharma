library(tsDyn)
library(vars)

data(finland)
#Fit a VECM with Engle-Granger 2OLS estimator:
vecm.eg<-lineVar(finland, lag=2)

#Fit a VECM with Johansen MLE estimator:
vecm.jo<-VECM(finland, lag=2, estim="ML")

ols <- lm(finland)


##export to Latex
toLatex(vecm.eg)
toLatex(summary(vecm.eg))
options("show.signif.stars"=FALSE)
toLatex(summary(vecm.eg), parenthese="Pvalue")
options("show.signif.stars"=TRUE)