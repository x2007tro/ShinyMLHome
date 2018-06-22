##
# Package installer
##
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

# machine learning
if(!("caret" %in% rownames(installed.packages()))) { devtools::install_github('topepo/caret/pkg/caret') }
if(!("keras" %in% rownames(installed.packages()))) { devtools::install_github('rstudio/keras') }
if(!("xgboost" %in% rownames(installed.packages()))) { install.packages("xgboost") }
if(!("ada" %in% rownames(installed.packages()))) { install.packages("ada") }
if(!("rpart" %in% rownames(installed.packages()))) { install.packages("rpart") }
if(!("rpart.plot" %in% rownames(installed.packages()))) { install.packages("rpart.plot") }
if(!("nnet" %in% rownames(installed.packages()))) { install.packages("nnet") }
if(!("naivebayes" %in% rownames(installed.packages()))) { install.packages("naivebayes") }

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

library(caret)
library(keras)
library(xgboost)
library(ada)
library(rpart)
library(rpart.plot) 
library(nnet)
library(naivebayes)

# install keras if not installed
# install_keras(
#   method = "conda",
#   tensorflow = "default")

##
# Directory parameters
##
dev_dir <- paste0("C:/Users/",Sys.info()["user"], "/OneDrive/Development/")
root_dir <- paste0(dev_dir, "Data Science/Projects/")
proj_dir <- paste0(root_dir, "Titanic/")
shiny_dir <- paste0("C:/Users/Ke/Desktop/Data Science/ShinyMLHome/")

##
# Load helpers
##
source(paste0(shiny_dir, "Helper/ml_helper.R"))
source(paste0(shiny_dir, "Helper/seed.R"))
source(paste0(shiny_dir, "Model/mtensorflow.R"))
source(paste0(shiny_dir, "Model/mxgbtree.R"))
source(paste0(shiny_dir, "Model/mreg.R"))
source(paste0(shiny_dir, "Model/mdecisiontree.R"))
source(paste0(shiny_dir, "Model/mnaivebayes.R"))

##
# UI parameters
##
tp_wid_nar <- 2
tp_wid_med <- 4
tp_wid_hlf <- 6
tp_wid_wde <- 8
file_dir_field_width <- "700px"

##
# General parameters
##
blotter_field_default_width <- "90px"
#def_label <- "Survived"
#jt <- c("bc", "mc", "reg")
bs_pars <- c("kappa", "eps", "nrounds")
bs_pars_def <- c(1, 0, 10)
bs_pars_rng <- c("0 - 1 (1)", "0 - 1 (0)", "0 - inf (10)")
#all_models <- c("tensorflow", "xgbtree", "regression")
image_dim <- c(640, 480)   # width, height
db_path <- "C:/Users/Ke/OneDrive/Development/Data Science/Projects/Titanic/Dataset/train.accdb"
db_target_src <- "000_030_Target"
db_target_map <- "* Input 02 : Target Map *"
db_predictors_src <- "Predictors R01"

##
# model parameters
##
mdl_db_path <- paste0(shiny_dir, "app.accdb")
mdl_db_avl_mdls <- "* Input 02 : Available Models *"
mdl_db_unv_mp <- "* Input 05 : Universal Model Parameters *"
mdl_db_bs_mp <- "* Input 09 : Bayesian Search Parameters *"
mdl_db_xgbt_mp <- "* Input 10 : Xgbtree Model Parameters *"
mdl_db_tf_mp <- "* Input 11 : Tensorflow Model Parameters *"
mdl_db_dt_mp <- "* Input 13 : Decision Tree Parameters *"
mdl_db_nb_mp <- "* Input 14 : Naive Bayes Parameters *"

model_output_specs <- ReadDataFromADB(mdl_db_path, mdl_db_avl_mdls)
all_models <- model_output_specs$model
unv_pars <- ReadDataFromADB(mdl_db_path, mdl_db_unv_mp)
bs_pars <- ReadDataFromADB(mdl_db_path, mdl_db_bs_mp)
xgbt_pars <- ReadDataFromADB(mdl_db_path, mdl_db_xgbt_mp)
tf_pars <- ReadDataFromADB(mdl_db_path, mdl_db_tf_mp)
dt_pars <- ReadDataFromADB(mdl_db_path, mdl_db_dt_mp)
nb_pars <- ReadDataFromADB(mdl_db_path, mdl_db_nb_mp)

##
# Parameters for data panel
##
format_options <- c("Scale", "OnehotEncoding")

##
# Parameters for tensorflow panel
##
tf_pars <- c("nlayers", "units", "reg_l1", "reg_l2", "drop_rate", "tf_nrounds")
tf_pars_def <- c(3, 10, 0, 0, 0, 300)
tf_pars_rng <- c("0 - inf (3)", "0 - inf (10)", "0 - 1 (0)", "0 - 1 (0)", "0 - 1 (0)", "0 - inf (300)")
tf_pars_hint <- c("", "", "", "", "", "")

##
# Parameters for xgbtree panel
##
xgbt_pars <- c("eta", "max_depth", "gamma", "min_child_weight",
               "subsample", "colsample_bytree", "xgbt_nrounds")
xgbt_pars_def <- c(0.3, 6, 0, 1, 1, 1, 300)
xgbt_pars_rng <- c("0 - 1 (0.3)", "0 - inf (6)", "0 - inf (0)", "0 - inf (1)", 
                   "0 - 1 (1)", "0 - 1 (1)", "0 - inf (300)")
xgbt_pars_hint <- c("0.01 - 0.2", "3 - 10", "", "", 
                   "0.5 - 1", "0.5 - 1", "")

##
# Parameters for model ensemble
##
med_options <- c("prob", "pred")