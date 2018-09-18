##
# Package installer
##

# for msft r open only
checkpoint::checkpoint("2018-08-25")

# shiny app
if(!("shiny" %in% rownames(installed.packages()))) { install.packages("shiny") }
if(!("DT" %in% rownames(installed.packages()))) { install.packages("DT") }
if(!("shinythemes" %in% rownames(installed.packages()))) { install.packages("shinythemes") }

# utilities
if(!("devtools" %in% rownames(installed.packages()))) { install.packages("devtools") }
if(!("corrplot" %in% rownames(installed.packages()))) { install.packages("corrplot") }
if(!("ggplot2" %in% rownames(installed.packages()))) { install.packages("ggplot2") }
if(!("dplyr" %in% rownames(installed.packages()))) { install.packages("dplyr") }
if(!("robustbase" %in% rownames(installed.packages()))) { install.packages("robustbase") }
if(!("e1071" %in% rownames(installed.packages()))) { install.packages("e1071") }
if(!("zeallot" %in% rownames(installed.packages()))) { install.packages("zeallot") }
if(!("ggpubr" %in% rownames(installed.packages()))) { install.packages("ggpubr") }
if(!("Matrix" %in% rownames(installed.packages()))) { install.packages("Matrix") }
if(!("foreach" %in% rownames(installed.packages()))) { install.packages("foreach") }
if(!("rBayesianOptimization" %in% rownames(installed.packages()))) { install.packages("rBayesianOptimization", dependencies = TRUE) }
if(!("RODBC" %in% rownames(installed.packages()))) { install.packages("RODBC") }
if(!("DBI" %in% rownames(installed.packages()))) { install.packages("DBI") }
if(!("odbc" %in% rownames(installed.packages()))) { install.packages("odbc") }
if(!("DMwR" %in% rownames(installed.packages()))) { install.packages("DMwR") }
if(!("ROSE" %in% rownames(installed.packages()))) { install.packages("ROSE") }
if(!("ROCR" %in% rownames(installed.packages()))) { install.packages("ROCR") }

# machine learning
if(!("caret" %in% rownames(installed.packages()))) { devtools::install_github('topepo/caret/pkg/caret') }
if(!("keras" %in% rownames(installed.packages()))) { devtools::install_github('rstudio/keras') }
if(!("xgboost" %in% rownames(installed.packages()))) { install.packages("xgboost") }
if(!("ada" %in% rownames(installed.packages()))) { install.packages("ada") }
if(!("rpart" %in% rownames(installed.packages()))) { install.packages("rpart") }
if(!("rpart.plot" %in% rownames(installed.packages()))) { install.packages("rpart.plot") }
if(!("nnet" %in% rownames(installed.packages()))) { install.packages("nnet") }
if(!("naivebayes" %in% rownames(installed.packages()))) { install.packages("naivebayes") }
if(!("randomForest" %in% rownames(installed.packages()))) { install.packages("randomForest") }
if(!("RRF" %in% rownames(installed.packages()))) { install.packages("RRF") }
if(!("h2o" %in% rownames(installed.packages()))) {
  install.packages("h2o", repos=(c("http://s3.amazonaws.com/h2o-release/h2o/master/1497/R", getOption("repos"))))
}

# load libraries
library(shiny)
library(DT)
library(shinythemes)

library(devtools)
library(corrplot)
library(ggplot2)
library(dplyr)
library(zeallot)
library(Matrix)
library(ggpubr)
library(rBayesianOptimization)
library(RODBC)
library(odbc)
library(DBI)

library(caret)
library(keras)
library(xgboost)
library(ada)
library(rpart)
library(rpart.plot) 
library(nnet)
library(naivebayes)
library(randomForest)
library(RRF)
library(h2o)
# localH2O <- h2o.init()

# install keras if not installed
# install_keras(
#   method = "conda",
#   tensorflow = "default")

##
# Directory parameters
##

#
# Load trading class depending on OS
#
if(R.Version()$os == "linux-gnu"){
  root_dir <- paste0("/home/",Sys.info()["user"], "/projects/")
  proj_dir <- paste0(root_dir, "Data Science/GoogleAnalyticsCustomerRevenuePrediction/")
  shiny_dir <- paste0(root_dir,"ShinyMLHome/")
} else {
  dev_dir <- paste0("C:/Users/",Sys.info()["user"], "/OneDrive/Development/")
  root_dir <- paste0(dev_dir, "Data Science/Projects/")
  proj_dir <- paste0(root_dir, "GoogleAnalyticsCustomerRevenuePrediction/")
  shiny_dir <- paste0("C:/Github/ShinyMLHome/")
}

##
# Load helpers
##
source(paste0(shiny_dir, "Helper/ml_helper.R"))
source(paste0(shiny_dir, "Helper/seed.R"))
source(paste0(shiny_dir, "Model/mtensorflow.R"))
source(paste0(shiny_dir, "Model/mxgbtree.R"))
source(paste0(shiny_dir, "Model/mregression.R"))
source(paste0(shiny_dir, "Model/mdecisiontree.R"))
source(paste0(shiny_dir, "Model/mnaivebayes.R"))
source(paste0(shiny_dir, "Model/madaboost.R"))
source(paste0(shiny_dir, "Model/mrandomforest.R"))
source(paste0(shiny_dir, "Model/mgbmh2o.R"))
source(paste0(shiny_dir, "Model/mlightgbm.R"))
#source(paste0(proj_dir, "Helper/feature_manipulation.R"))

##
# UI parameters
##
tp_wid_nar <- 2
tp_wid_med <- 4
tp_wid_hlf <- 6
tp_wid_wde <- 8

##
# To be deleted
## 
file_dir_field_width <- "200px"
blotter_field_default_width <- "100px"
# end of to be deleted

##
# General parameters
##
jt <- c("bc", "mc", "rg")
image_dim <- c(640, 480)   # width, height
proj_db_name <- "GoogleAnalyticsCustomerRevenuePrediction"
db_par_tbl <- "input01_parameters"
db_target_map <- "input02_target_map"
db_target_src <- "sample_target"
db_predictors_src <- "sample_predictors01_scaled_ohe"

##
# initialize project variable
# global_pars <- ReadDataFromSSviaCS(proj_db_name, db_par_tbl)
# ini_proj <- global_pars[1, "Project"]
# ini_job <- global_pars[1, "Job"]
# ini_vds <- global_pars[1, "ValidationDatasetSize"]
# ini_cvrep <- global_pars[1, "CVRep"]

##
# model parameters
##
mdl_db_name <- "MachineLearningAlgorithm"
mdl_db_avl_mdls <- "* Input 02 : Available Models *"
mdl_db_unv_mp <- "* Input 05 : Universal Model Parameters *"
mdl_db_bs_mp <- "* Input 09 : Bayesian Search Parameters *"
# mdl_db_xgbt_mp <- "* Input 10 : Xgbtree Parameters *"
# mdl_db_tf_mp <- "* Input 11 : Tensorflow Parameters *"
# mdl_db_rg_mp <- "* Input 12 : Regression Parameters *"
# mdl_db_dt_mp <- "* Input 13 : Decision Tree Parameters *"
# mdl_db_nb_mp <- "* Input 14 : Naive Bayes Parameters *"
# mdl_db_ab_mp <- "* Input 15 : Adaptive Boosting Parameters *"
# mdl_db_rf_mp <- "* Input 16 : Random Forest Parameters *"
# mdl_db_gbm_mp <- "* Input 17 : GBM H2O Parameters *"
mdl_db_lgbm_mp <- "* Input 18 : Light GBM Parameters *"

model_output_specs <- ReadDataFromSSviaCS(mdl_db_name, mdl_db_avl_mdls)
all_models <- model_output_specs$model
unv_pars <- ReadDataFromSSviaCS(mdl_db_name, mdl_db_unv_mp)
bs_pars <- ReadDataFromSSviaCS(mdl_db_name, mdl_db_bs_mp)
# xgbt_pars <- ReadDataFromSSviaCS(mdl_db_name, mdl_db_xgbt_mp)
# tf_pars <- ReadDataFromSSviaCS(mdl_db_name, mdl_db_tf_mp)
# dt_pars <- ReadDataFromSSviaCS(mdl_db_name, mdl_db_dt_mp)
# nb_pars <- ReadDataFromSSviaCS(mdl_db_name, mdl_db_nb_mp)
# rg_pars <- ReadDataFromSSviaCS(mdl_db_name, mdl_db_rg_mp)
# ab_pars <- ReadDataFromSSviaCS(mdl_db_name, mdl_db_ab_mp)
# rf_pars <- ReadDataFromSSviaCS(mdl_db_name, mdl_db_rf_mp)
# gbm_pars <- ReadDataFromSSviaCS(mdl_db_name, mdl_db_gbm_mp)
lgbm_pars <- ReadDataFromSSviaCS(mdl_db_name, mdl_db_lgbm_mp)

##
# Parameters for data panel
##
format_options <- c("Scale", "OnehotEncoding")

##
# Parameters for model ensemble
##
med_options <- c("prob", "pred")