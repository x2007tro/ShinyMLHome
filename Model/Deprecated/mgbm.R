##
# Install package, if not
##
if(!("gbm" %in% rownames(installed.packages()))) { install.packages("gbm") }
if(!("dismo" %in% rownames(installed.packages()))) { install.packages("dismo") }
library(gbm)
library(dismo)

##
# Select and format data
##
datasetp <- read.csv("../Dataset/train_prelim2.csv", 
                     header = TRUE, stringsAsFactors = FALSE)
datasetf <- FormatData4Model(datasetp, job = "bc", model = "gbm", target = "Survived")
x <- DataInspection(datasetf)

rps_var <- "Survived"
dep_vars <- colnames(datasetf)[!(colnames(datasetf) %in% rps_var)]

##
# Train the tree model
##
fmr <- as.formula(paste(rps_var,"~", paste(dep_vars, collapse="+")))
set.seed(1234)
mdl <- dismo::gbm.step(datasetf, dep_vars, rps_var, tree.complexity = 1,
                       learning.rate = 0.01, bag.fraction = 0.75, n.folds = 10, 
                       family = "bernoulli", n.trees = 50, 
                       step.size = n.trees, max.trees = 10000,
                       tolerance.method = "auto", tolerance = 0.001, plot.main = TRUE, plot.folds = FALSE,
                       verbose = TRUE, silent = FALSE, keep.fold.models = FALSE, keep.fold.vector = FALSE,
                       keep.fold.fit = FALSE)
