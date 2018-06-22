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
mdl_rrf <- RRF::RRF(
  formula = fmr, data = datasetf[1:691,], keep.forest = TRUE,
  xtest = datasetf[692:891,2:ncol(datasetf)], ytest = datasetf[692:891,1],
  mtry = floor(sqrt(nrow(datasetf))), ntree = 500, importance = TRUE,
  coefReg = 1, flagReg = 0
)
plot(mdl_rrf)   # useless

# retrieve a tree from random forest
tr <- RRF::getTree(mdl_rrf, k = 1, labelVar = TRUE)

# importance and dependence plot
imp <- RRF::importance(mdl_rrf)
RRF::varImpPlot(mdl_rrf)
vu <- RRF::varUsed(mdl_rrf, by.tree = TRUE)
impvar <- rownames(imp)[order(imp[, 1], decreasing=TRUE)]
op <- par(mfrow=c(2, 3))
for (i in seq_along(impvar)) {
  RRF::partialPlot(mdl_rrf, datasetf[1:691,], impvar[i], xlab=impvar[i],
                   main=paste("Partial Dependence on", impvar[i]),
                   ylim=c(30, 70))
}
par(op)

# feature selection plot
result <- rrfcv(datasetf[692:891,2:ncol(datasetf)], datasetf[692:891,1], cv.fold=5)
with(result, plot(n.var, error.cv, log="x", type="o", lwd=2))

# plot learning curve
err <- mdl_rrf$err.rate
xerr <- mdl_rrf$test$err.rate
rrf_mdl_lc <- data.frame(ntree = 1:500,
                     train_err = err[,1], 
                     test_err = xerr[,1])
rrf_plt <- FitPlot("RRF", "Classification", rrf_mdl_lc, "ntree", "train_err", "test_err")
print(rrf_plt)

# Prediction
rrf_probs <- predict(mdl_rrf, datasetf[692:891,], type = "prob")
rrf_prob <- rrf_probs[,2]
rrf_pred <- as.numeric(rrf_prob > 0.5)
rrf_acc <- sum(rrf_pred == datasetp[692:891,]$Survived)/nrow(datasetp[692:891,])
