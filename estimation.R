library(MASS)
library(irr)
library(pROC)
library(readxl)
library(plyr)
Nevi <- read_excel("C:/Users/Server/Downloads/Nevi.xlsx")

# Convert risk level number to letter (A to E)
Nevi$`Pertinence Clinique` <- factor(Nevi$`Pertinence Clinique`, ordered = TRUE)
levels(Nevi$`Pertinence Clinique`) <- letters[5:1]

Nevi$`Pertinence Clinique` <- revalue(Nevi$`Pertinence Clinique`, c("a"="High", "b"="High", "c"="Medium", "d"= "Low", "e"= "Low"))

# Convert sex variable to factor
Nevi$sexe <- factor(Nevi$sexe)
levels(Nevi$sexe) <- c("Female", "Male")

formula <- `Pertinence Clinique` ~ age + sexe + `Dossier onco` + `# Rx ` + `# Rx parenteral ` + `# Rx a Risque ` + `# Rx QT` + `Creatinemie `

train <- Nevi[1:100,]
test <- Nevi[101:135,]

# Ordered logit on train data
train.plr <- polr(formula,  method = "logistic", data = train, Hess = TRUE)
summary(train.plr)

# Predict model on test data
test.pred <- predict(train.plr, test)

# Test Cohen's kappa
kappa2(data.frame(test.pred, test$`Pertinence Clinique`))


