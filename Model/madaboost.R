##
# Install package, if not
##
if(!("ada" %in% rownames(installed.packages()))) { install.packages("ada") }
if(!("rpart" %in% rownames(installed.packages()))) { install.packages("rpart") }
if(!("rpart.plot" %in% rownames(installed.packages()))) { install.packages("fastAdaboost") }
library(ada)
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
mdl <- ada::ada(formula = fmr, data = datasetf, type = "discrete", 
                iter = 50, nu = 1, bag.frac = 0.5, verbose = TRUE,
                control = rpart::rpart.control(minsplit = 20, cp = 0.01, maxdepth = 30))
# Plot the tree (total iter trees)
rpart.plot::rpart.plot(mdl$model$trees[[1]], faclen = -1, type = 4, extra = "auto")
plot
