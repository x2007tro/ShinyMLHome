# The following two commands remove any previously installed H2O packages for R.
if ("package:h2o" %in% search()) { detach("package:h2o", unload=TRUE) }
if ("h2o" %in% rownames(installed.packages())) { remove.packages("h2o") }

# Next, we download, install and initialize the H2O package for R.
if(!("h2o" %in% rownames(installed.packages()))) {
  install.packages("h2o", repos=(c("http://s3.amazonaws.com/h2o-release/h2o/master/1497/R", getOption("repos"))))
}
library(h2o)
localH2O <- h2o.init()

##
# Select and format data
##
datasetp <- read.csv("../Dataset/train_prelim2.csv", 
                     header = TRUE, stringsAsFactors = FALSE)
datasetf <- FormatData4Model(datasetp, job = "bc", model = "gbm_h2o", target = "Survived")

rps_var <- "Survived"
dep_vars <- colnames(datasetf)[!(colnames(datasetf) %in% rps_var)]

##
# Train the tree model
##
mdl <- h2o.gbm(x = dep_vars, y = rps_var, training_frame = as.h2o(datasetf[1:691,]), distribution = "AUTO")
h2o.accuracy(h2o.performance(mdl, newdata = as.h2o(datasetf[692:891,]))) 

##
# Train the model with early stopping
##
gbm <- h2o.gbm(
  x = dep_vars, 
  y = rps_var, 
  training_frame = as.h2o(datasetf[1:691,]), 
  validation_frame = as.h2o(datasetf[692:891,]),
  nfolds = 0,
  score_each_iteration = TRUE,
  
  ## more trees is better if the learning rate is small enough 
  ## here, use "more than enough" trees - we have early stopping
  ntrees = 10000,                                                            
  
  ## early stopping once the validation error doesn't improve by at least 0.01% for 5 consecutive scoring events
  stopping_rounds = 10, stopping_tolerance = 1e-4, stopping_metric = "AUTO", 
  
  ## smaller learning rate is better (this is a good value for most datasets, but see below for annealing)
  learn_rate = 0.01, learn_rate_annealing = 0.995,
  
  ## depth of the tree and mini child # in one node
  max_depth = 10, min_rows = 10,
  
  ## sample 80% of rows per tree
  sample_rate = 0.8, sample_rate_per_class = NULL,                                                    
  
  ## sample 80% of columns per split
  col_sample_rate = 0.8, col_sample_rate_per_tree = 1,                                                  
  
  ## fix a random number generator seed for reproducibility
  seed = 1234,                                                             
  
  ## score every 10 trees to make early stopping reproducible (it depends on the scoring interval)
  score_tree_interval = 10                                               
)

##
# Plot
##
train_err_hist <- mdl@model$scoring_history
mdl_sumry <- mdl@model$model_summary
var_imp <- mdl@model$variable_importances
plot(mdl)

##
# Prediction
##
pred_h2o <- h2o.predict(mdl, as.h2o(datasetf[692:891,]))
probs <- as.data.frame(pred_h2o)
prob <- probs[,3]
pred <- as.numeric(prob > 0.5)
acc <- sum(pred == datasetp[692:891,]$Survived)/nrow(datasetp[692:891,])