datasetp <- read.csv("C:/Users/KE/OneDrive/Development/Data Science/Projects/Titanic/Dataset/train_prelim2.csv", 
                     header = TRUE, stringsAsFactors = FALSE)
datasetf <- FormatData4Model(datasetp[,-1], 
                             as.data.frame(datasetp[,1], stringAsfactors = FALSE),
                             job = "bc", model = "decision_tree")

# train data
train_data <- cbind.data.frame(datasetf$target, datasetf$predictors)
colnames(train_data) <- c("Target", colnames(datasetf$predictors))
fmr <- as.formula(paste("Target","~", paste(colnames(datasetf$predictors), collapse="+")))

mdl_rrf <- RRF::RRF(
  formula = fmr, data = train_data[1:691,], keep.forest = TRUE,
  xtest = train_data[692:891,2:ncol(train_data)], ytest = train_data[692:891,1],
  mtry = floor(sqrt(ncol(train_data))), ntree = 500, importance = TRUE,
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
#par(mfrow=c(1, 1))
for (i in seq_along(impvar)) {
  RRF::partialPlot(mdl_rrf, train_data[1:691,], impvar[i], xlab=impvar[i],
                   main=paste("Partial Dependence on", impvar[i]),
                   ylim=c(30, 70))
}


# feature selection plot
result <- RRF::rrfcv(train_data[692:891,2:ncol(train_data)], train_data[692:891,1], cv.fold=5)
with(result, plot(n.var, error.cv, log="x", type="o", lwd=2))

# plot learning curve
err <- mdl_rrf$err.rate
xerr <- mdl_rrf$test$err.rate
rrf_mdl_lc <- data.frame(ntree = 1:500,
                     train_err = err[,1], 
                     test_err = xerr[,1])
rrf_plt <- FitPlot("RRF", "Classification", rrf_mdl_lc, "ntree", "train_err", "test_err")
print(rrf_plt)

