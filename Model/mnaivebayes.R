##
# Install packages, if not
##
if(!("naivebayes" %in% rownames(installed.packages()))) { install.packages("naivebayes") }
library(naivebayes)

##
# Select and format data
##
datasetp <- read.csv("C:/Users/KE/OneDrive/Development/Data Science/Shiny/Titanic2/Dataset/train_prelim2.csv", 
                     header = TRUE, stringsAsFactors = FALSE)
datasetf <- FormatData4Model(datasetp, job = "bc", model = "naive_bayes", target = "Survived")

rps_var <- "Survived"
dep_vars <- colnames(datasetf)[!(colnames(datasetf) %in% rps_var)]

##
# Train naivebayes model
##
fmr <- as.formula(paste(rps_var,"~", paste(dep_vars, collapse="+")))
mdl <- naivebayes::naive_bayes(formula = fmr, data = datasetf[1:691,], laplace = 0)
plot(mdl, ask = FALSE)

##
# Predict value
##
probs <- predict(mdl, datasetf[692:891,], type = "prob")
prob <- probs[,"1"]
pred <- as.numeric(prob > 0.5)
acc <- sum(pred == datasetp[692:891,]$Survived, na.rm = TRUE)/nrow(datasetp[692:891,])