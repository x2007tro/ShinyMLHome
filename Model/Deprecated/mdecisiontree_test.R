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

mdl <- rpart::rpart(fmr, data = train_data, method = "class", 
                    control = rpart::rpart.control(minsplit = 20, cp = 0.01, maxdepth = 30))
print.table(mdl$cptable)
x <- rpart.plot::rpart.plot(mdl, faclen = -1, type = 4, extra = "auto")
x <- mdl$variable.importance

##
# Prune a tree model
##
bestcp <- mdl$cptable[which.min(mdl$cptable[,"xerror"]),"CP"]
mdl_pruned <- prune(mdl, cp = bestcp)
rpart::printcp(mdl_pruned)
rpart.plot::rpart.plot(mdl_pruned, faclen = -1, type = 4, extra = "auto")

##
# Predict value
##
probs <- predict(mdl, train_data, type = "prob")
prob <- probs[,"1"]
pred <- as.numeric(prob > 0.5)
acc <- sum(pred == datasetp[692:891,]$Survived)/nrow(datasetp[692:891,])
