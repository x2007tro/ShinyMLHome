##
# Install package, if not
##
if(!("ada" %in% rownames(installed.packages()))) { install.packages("ada") }
if(!("rpart" %in% rownames(installed.packages()))) { install.packages("rpart") }
if(!("rpart.plot" %in% rownames(installed.packages()))) { install.packages("rpart.plot") }
library(ada)
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
mdl <- ada::ada(formula = fmr, data = datasetf[1:691,], type = "discrete", 
                iter = 50, nu = 1, bag.frac = 0.5, verbose = TRUE,
                control = rpart::rpart.control(minsplit = 20, cp = 0.01, maxdepth = 30))
mdl2 <- ada::addtest(mdl, test.x = datasetf[692:891,2:ncol(datasetf)], test.y = datasetf[692:891,1])

# Plot the tree (total iter trees)
mdl_trees <- mdl2$model$trees
rpart.plot::rpart.plot(mdl_trees[[1]], faclen = -1, type = 4, extra = "auto")

# Plot the learning curve
mdl_lc <- as.data.frame(mdl2$model$errs)
mdl_lc$iter <- 1:50
plt <- FitPlot("AdaBoost", "Classification", mdl_lc, "iter", "train.err", "test.errs")
print(plt)
# Other functions
print(mdl2)
summary(mdl2)

##
# Predict value
##
probs <- predict(mdl2, datasetf[692:891,], type = "prob")
prob <- probs[,2]
pred <- as.numeric(prob > 0.5)
acc <- sum(pred == datasetp[692:891,]$Survived)/nrow(datasetp[692:891,])