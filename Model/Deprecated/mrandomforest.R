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
datasetf <- FormatData4Model(datasetp, job = "bc", model = "decision_tree", target = "Survived")

rps_var <- "Survived"
dep_vars <- colnames(datasetf)[!(colnames(datasetf) %in% rps_var)]

##
# Train the tree model
##
fmr <- as.formula(paste(rps_var,"~", paste(dep_vars, collapse="+")))
mdl_rf <- randomForest::randomForest(
  formula = fmr, data = datasetf[1:691,], keep.forest = TRUE,
  xtest = datasetf[692:891,2:ncol(datasetf)], ytest = datasetf[692:891,1],
  mtry = floor(sqrt(nrow(datasetf))), ntree = 500, importance = TRUE
)
plot(mdl_rf)   # useless

# retrieve a tree from random forest
tr <- randomForest::getTree(mdl_rf, k = 1, labelVar = TRUE)

# importance and dependence plot
imp <- randomForest::importance(mdl_rf)
randomForest::varImpPlot(mdl_rf)
vu <- randomForest::varUsed(mdl_rf, by.tree = TRUE)
# impvar <- rownames(imp)[order(imp[, 1], decreasing=TRUE)]
# op <- par(mfrow=c(2, 3))
# for (i in seq_along(impvar)) {
#   partialPlot(mdl_rf, datasetf[1:691,], impval[i], xlab=impvar[i],
#               main=paste("Partial Dependence on", impvar[i]),
#               ylim=c(30, 70))
# }
# par(op)

# feature selection plot
result <- rfcv(datasetf[692:891,2:ncol(datasetf)], datasetf[692:891,1], cv.fold=5)
with(result, plot(n.var, error.cv, log="x", type="o", lwd=2))

# plot learning curve
err <- mdl_rf$err.rate
xerr <- mdl_rf$test$err.rate
mdl_lc <- data.frame(ntree = 1:500,
                     train_err = err[,1], 
                     test_err = xerr[,1])
plt <- FitPlot("RandomForest", "Classification", mdl_lc, "ntree", "train_err", "test_err")
print(plt)

# Prediction
probs <- predict(mdl_rf, datasetf[692:891,], type = "prob")
prob <- probs[,2]
pred <- as.numeric(prob > 0.5)
acc <- sum(pred == datasetp[692:891,]$Survived)/nrow(datasetp[692:891,])
