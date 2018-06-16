##
# Directory parameters
##
dev_dir <- paste0("C:/Users/",Sys.info()["user"], "/OneDrive/Development/")
root_dir <- paste0(dev_dir, "R/Independant Projects/DeepLearning/")
proj_dir <- paste0(root_dir, "Titanic/")
shiny_dir <- paste0(dev_dir, "Shiny/MLHome/")
setwd(proj_dir)

##
# Load helpers
##
source(paste0(shiny_dir, "Helper/ml_helper.R"))
source(paste0(shiny_dir, "Helper/seed.R"))
source(paste0(shiny_dir, "Model/mtensorflow.R"))
source(paste0(shiny_dir, "Model/mxgbtree.R"))
source(paste0(shiny_dir, "Model/mreg.R"))

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
def_label <- "Survived"
jt <- c("bc", "mc", "reg")
bs_pars <- c("kappa", "eps", "nrounds")
bs_pars_def <- c(1, 0, 10)
bs_pars_rng <- c("0 - 1 (1)", "0 - 1 (0)", "0 - inf (10)")
all_models <- c("tensorflow", "xgbtree", "regression")
image_dim <- c(640, 480)   # width, height

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