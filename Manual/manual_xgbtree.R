##
# Manual training for xgbtree model
##

##
# first things first, parameters
mlh_dir <- paste0("C:/Users/",Sys.info()["user"],"/Desktop/Data Science/ShinyMLHome/")
dataset_nm <- c("hcdr_sample","hcdr_full")[1]
rmv_fs <- c("appl_process_weekday")
run_test <- FALSE

##
# training parameters
tuning_pars <- expand.grid(
  eta = c(0.01),
  gamma = c(0),
  max_depth = c(30),
  min_child_weight = c(1),
  subsample = c(1),
  colsample_bytree = c(1),
  nrounds = c(100),
  stopping_rounds = c(100)
)

##
# global parameters
static_pars <- data.frame(
  graph_output = FALSE,
  save_mod = FALSE,
  save_pred = FALSE,
  save_res = FALSE,
  stringsAsFactors = FALSE
)
all_models <- c("xgbtree")

##
# Load required source files and data
# source(paste0(mlh_dir, "global.R"))
load(paste0(proj_dir, dataset_nm, ".RData"))

##
# Manipulation
##

##
# Feature removal
ifelse(length(rmv_fs) == 0, 
       prdctrs1 <- sample_predictors,
       prdctrs1 <- sample_predictors %>% select(-one_of("appl_process_weekday")))

##
# Scale and OHE
prdctrs1_peek <- DataInspection(prdctrs1)
num_cols <- prdctrs1_peek[prdctrs1_peek$class != "character","feature"]
chr_cols <- prdctrs1_peek[prdctrs1_peek$class == "character","feature"]

data_scaled <- DataScale(num_cols, prdctrs1, rep_na = TRUE, rep_na_with = 0)
data_scaled_ohe <- OHE(chr_cols, data_scaled)
prdctrs2_peek <- DataInspection(data_scaled_ohe)   # info only

##
# Format data for the model
fmtd_data <- FormatData4Model(
  prdctrs = data_scaled_ohe,
  tgt = sample_target, 
  tgt_map = tgt_map,
  job = "bc",
  model = "xgbtree"
)

##
# Train
br <- GridSearchXgbtree2(
  proj = "HCDR",
  model_name = "xgbtree",
  dataset = fmtd_data$predictors,
  labels = fmtd_data$target,
  job = "bc",
  val_size = 20000,
  cv_rep = 1,
  mdl_pars = tuning_pars,   # data.frame
  stc_pars = static_pars,    # list
  tgt_map = tgt_map
)

##
# Present result
score_board <- res$score_board
cong_matrix <- res$valdn_results[[1]][[1]]$cf

##
# Variable importance
raw_meat <- xgboost::xgb.importance(model = res[[1]][[1]])
features <- colnames(fmtd_data$predictors)
var_imp <- data.frame(
  variable = features[as.numeric(raw_meat$Feature)+1],
  importance = raw_meat$Gain,
  stringsAsFactors = FALSE
)

##
# Plot learning curve
mdl_lc <- res[[1]][[1]]$evaluation_logFitPlot("xgboost tree", input$cgen_job_type, mdl_lc, "iter", "train_error", "test_error")

##
# Run test data
##
if(run_test){
  
  
  
}