##
# Select and format data
##
datasetp <- read.csv("C:/Users/KE/OneDrive/Development/Data Science/Projects/Titanic/Dataset/train_prelim2.csv", 
                     header = TRUE, stringsAsFactors = FALSE)
datasetf <- FormatData4Model(datasetp[,-1], 
                             as.data.frame(paste0("cat_",datasetp[,1]), stringAsfactors = FALSE),
                             job = "bc", model = "decision_tree")

# train data
train_data <- cbind.data.frame(datasetf$target, datasetf$predictors)
colnames(train_data) <- c("Target", colnames(datasetf$predictors))
fmr <- as.formula(paste("Target","~", paste(colnames(datasetf$predictors), collapse="+")))

mdl <- ada::ada(formula = fmr, data = train_data[1:691,], type = "discrete", 
                iter = 50, nu = 1, bag.frac = 0.5, verbose = TRUE,
                control = rpart::rpart.control(minsplit = 20, cp = 0.01, maxdepth = 30))
mdl2 <- ada::addtest(mdl, test.x = train_data[692:891,2:ncol(train_data)], test.y = train_data[692:891,1])

# Plot the tree (total iter trees)
mdl_trees <- mdl2$model$trees
rpart.plot::rpart.plot(mdl_trees[[1]], faclen = -1, type = 4, extra = "auto")
var_imp <- mdl_trees[[1]]$variable.importance


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
probs <- predict(mdl2, train_data, type = "prob")
probs <- as.data.frame(probs)
colnames(probs) <- levels(label)
pred_fac <- predict(mdl2, train_data, type = "vector")