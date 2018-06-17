##
# Install package, if not
##
if(!("randomForest" %in% rownames(installed.packages()))) { install.packages("randomForest") }
if(!("RRF" %in% rownames(installed.packages()))) { install.packages("RRF") }
library(randomForest)
library(RRF)

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
mdl_rf <- randomForest::randomForest(
  formula = fmr, data = datasetf[1:691,], 
  xtest = datasetf[692:891,2:ncol(datasetf)], ytest = datasetf[692:891,1],
  mtry = floor(sqrt(nrow(datasetf))), ntree = 500, importance = TRUE
)
err <- mdl_rf$err.rate
xerr <- mdl_rf$test$err.rate
mdl_lc <- data.frame(ntree = 1:500,
                     train_err = err[,1], 
                     test_err = xerr[,1])
plt <- FitPlot("RandomForest", "Classification", mdl_lc, "ntree", "train_err", "test_err")
print(plt)
                                  
