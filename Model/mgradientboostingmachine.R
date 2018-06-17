##
# Install package, if not
##
if(!("gbm" %in% rownames(installed.packages()))) { install.packages("gbm") }
if(!("rpart" %in% rownames(installed.packages()))) { install.packages("rpart") }
if(!("rpart.plot" %in% rownames(installed.packages()))) { install.packages("rpart.plot") }
library(gbm)
library(rpart)
library(rpart.plot) 

##
# Select and format data
##
datasetp <- read.csv("../Dataset/train_prelim2.csv", 
                     header = TRUE, stringsAsFactors = FALSE)
datasetf <- FormatData4Model(datasetp, model = "decision_tree", target = "Survived")

rps_var <- "Survived"
dep_vars <- colnames(datasetf)[!(colnames(datasetf) %in% rps_var)]

##
# Train the tree model
##
fmr <- as.formula(paste(rps_var,"~", paste(dep_vars, collapse="+")))
mdl <- gbm::gbm(formula = fmr, data = datasetf[1:691,], 
                n.trees = 100, interaction.depth = 1, n.minobsinnode = 10,
                shrinkage = 0.001, bag.fraction = 0.5, train.fraction = 1,
                cv.fold = 0, keep.data = TRUE)
gbm::gbm.perf(mdl, plot.it = TRUE)
