##
# Install package, if not
##
if(!("rpart" %in% rownames(installed.packages()))) { install.packages("rpart") }
if(!("rpart.plot" %in% rownames(installed.packages()))) { install.packages("rpart.plot") }
library(rpart)
library(rpart.plot)

##
# Select and format data
##
datasetp <- read.csv("C:/Users/KE/OneDrive/Development/Data Science/Shiny/Titanic2/Dataset/train_prelim2.csv", 
                     header = TRUE, stringsAsFactors = FALSE)
datasetf <- FormatData4Model(datasetp, model = "decision_tree", target = "Survived")

rps_var <- "Survived"
dep_vars <- colnames(datasetf)[!(colnames(datasetf) %in% rps_var)]

##
# Train the tree model
##
fmr <- as.formula(paste(rps_var,"~", paste(dep_vars, collapse="+")))

mdl <- rpart::rpart(fmr, data = datasetf[1:691,], method = "class", 
                    control = rpart::rpart.control(minsplit = 20, cp = 0.01, maxdepth = 30))
print.table(mdl$cptable)
rpart.plot::rpart.plot(mdl, faclen = -1, type = 4, extra = "auto")
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
probs <- predict(mdl_pruned, datasetf[692:891,], type = "prob")
prob <- probs[,"1"]
pred <- as.numeric(prob > 0.5)
acc <- sum(pred == datasetp[692:891,]$Survived)/nrow(datasetp[692:891,])