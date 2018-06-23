##
# Select and format data
##
datasetp <- read.csv("C:/Users/KE/OneDrive/Development/Data Science/Projects/Titanic/Dataset/train_prelim2.csv", 
                     header = TRUE, stringsAsFactors = FALSE)
datasetf <- FormatData4Model(datasetp[,-1], 
                             as.data.frame(datasetp[,1], stringAsfactors = FALSE),
                             job = "bc", model = "decision_tree")

# train data
train_data <- cbind.data.frame(datasetf$target, datasetf$predictors)
colnames(train_data) <- c("Target", colnames(datasetf$predictors))
fmr <- as.formula(paste("Target","~", paste(colnames(datasetf$predictors), collapse="+")))
mdl <- naivebayes::naive_bayes(formula = fmr, data = train_data, laplace = 0)
plot(mdl, ask = FALSE)
naivebayes::tables(mdl)

##
# Predict value
##
probs <- predict(mdl, train_data, type = "prob")
prob <- probs[,"1"]
pred <- as.numeric(prob > 0.5)
