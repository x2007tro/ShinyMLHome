
# Next, we download, install and initialize the H2O package for R.
if(!("h2o" %in% rownames(installed.packages()))) {
  install.packages("h2o", repos=(c("http://s3.amazonaws.com/h2o-release/h2o/master/1497/R", getOption("repos"))))
}
library(h2o)
localH2O <- h2o.init()

##
# Select and format data
##
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

##
# Train the model with early stopping
##
gbm <- h2o.gbm(
  x = colnames(train_data), 
  y = "Target", 
  training_frame = as.h2o(train_data), 
  validation_frame = as.h2o(train_data),
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
train_err_hist <- gbm@model$scoring_history
# mdl_sumry <- gbm@model$model_summary  # useless
var_imp <- gbm@model$variable_importances[,c("variable","percentage")]
plot(gbm)

##
# Prediction
##
pred_h2o <- h2o.predict(gbm, as.h2o(train_data[,-1]))
probs <- as.data.frame(pred_h2o)
prob <- probs[,3]
pred <- as.numeric(prob > 0.5)
acc <- sum(pred == datasetp[692:891,]$Survived)/nrow(datasetp[692:891,])