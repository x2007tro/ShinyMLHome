##
# Manual training for xgbtree model
##

# ##
# # Saving data
# ##
# full_testdata <- ReadDataFromSSviaCS("HomeLoanDefaultRisk_Aggr", "aggr_test_predictors01")
# full_predictors <- ReadDataFromSSviaCS("HomeLoanDefaultRisk_Aggr", "aggr_train_predictors01")
# full_target <- ReadDataFromSSviaCS("HomeLoanDefaultRisk_Aggr", "aggr_train_target")
# tgt_map <- ReadDataFromSSviaCS("HomeLoanDefaultRisk", "input02_target_map")
# save(full_predictors, full_target, full_testdata, tgt_map, file = "hcdr_full04.RData")
# 
# ##
# # first things first, parameters
# if(R.Version()$os == "linux-gnu"){
#   mlh_dir <- paste0("/home/",Sys.info()["user"],"/projects/ShinyMLHome/")
# } else {
#   mlh_dir <- paste0("C:/Users/",Sys.info()["user"],"/Desktop/Data Science/ShinyMLHome/")
# }
# n_val_size <- 0.1
# dataset_nm <- c("hcdr_sample","hcdr_full04")[2]
# rmv_fs <- c('km_SK_ID_CURR')
# 
# run_test <- FALSE
# smote_flag <- FALSE
# 
# ##
# # Load required source files and data
# source(paste0(mlh_dir, "global.R"))
# setwd(proj_dir)
# load(paste0(proj_dir, dataset_nm, ".RData"))
# 
# ##
# # Manipulation
# ##
# 
# ##
# # Take sample
# full_target <- full_target[, , drop = FALSE]
# full_predictors <- full_predictors[, , drop = FALSE]
# 
# #full_predictors <- AddFeatures(full_predictors, do = TRUE)
# #full_testdata <- AddFeatures(full_testdata, do = TRUE)
# 
# ##
# # Take validation set
# allrows <- 1:nrow(full_target)
# set.seed(123)
# val_idx <- sample(allrows, floor(n_val_size*nrow(full_target)), replace = FALSE)
# rem_idx <- allrows[!(allrows %in% val_idx)]
# 
# rem_target <- full_target[rem_idx, , drop = FALSE]
# rem_predictors <- full_predictors[rem_idx,]
# 
# val_target <- full_target[val_idx, , drop = FALSE]
# val_predictors <- full_predictors[val_idx,]
# 
# rm(full_target, full_predictors)
# gc()
# 
# ##
# # Scale and OHE training data
# prdctrs_train <- FinalTouch(rem_predictors[,], rmv_fs)
# train_peek1 <- prdctrs_train$peek1
# train_peek2 <- prdctrs_train$peek2
# rm(rem_predictors)
# 
# ##
# # SMOTE data so the data can be balanced
# if(smote_flag){
#   bd_tmp <- cbind.data.frame(rem_target, prdctrs_train$coredata, stringsAsFactors = FALSE)
#   bd_fnl <- ROSE::ROSE(StrTarget ~., data = bd_tmp,
#                        N = floor(1.5 * sum(bd_tmp$StrTarget == unique(bd_tmp$StrTarget)[1])),
#                        p = 0.25,
#                        seed = 1)$data
# 
#   prdctrs_train$coredata <- bd_fnl[,-1]
#   rem_target <- bd_fnl[,1,drop=FALSE]
#   rm(bd_tmp, bd_fnl)
#   gc()
# }
# 
# ##
# # Format training data
# fmtd_train <- FormatData4Model(
#   prdctrs = prdctrs_train$coredata,
#   tgt = rem_target,
#   tgt_map = tgt_map,
#   job = "bc",
#   model = "lightgbm"
# )
# rm(prdctrs_train, rem_target)
# 
# ##
# # Scale and OHE validation data
# ##
# prdctrs_val <- FinalTouch(val_predictors[,], rmv_fs)
# rm(val_predictors)
# gc()
# 
# ##
# # Format validation data
# fmtd_vald <- FormatData4Model(
#   prdctrs = prdctrs_val$coredata,
#   tgt = val_target[,,drop = FALSE],
#   tgt_map = tgt_map,
#   job = "bc",
#   model = "lightgbm"
# )
# rm(prdctrs_val, val_target)
# gc()
# 
# ##
# # Scale and OHE
# prdctrs_test <- FinalTouch(full_testdata, rmv_fs)
# test_peek1 <- prdctrs_test$peek1
# test_peek2 <- prdctrs_test$peek2
# test_id <- full_testdata$km_SK_ID_CURR
# 
# save(fmtd_train, fmtd_vald, prdctrs_test, test_id, tgt_map, file = "fmtd_data04.RData")

##
# Continue point
setwd(proj_dir)
load("fmtd_data04.RData")
load("lightgbm_feats04.RData")
rm(prdctrs_test)
gc()

n_feats_sltd <- 700     # max number of feats 910

##
# Select features
prdctrs_sltd <- fmtd_train$predictors[,feats_all[1:n_feats_sltd]]
tgts <- fmtd_train$target
rm(fmtd_train)
gc()

##
# global parameters
static_pars <- list(
  graph_output = FALSE,
  save_mod = TRUE,
  save_pred = FALSE,
  save_res = FALSE
)

##
# training parameters
tuning_pars <- expand.grid(
  boosting = c(1),     # 0 - gbdt, 1 - dart
  num_leaves = c(31),
  min_data_in_leaf = c(300),
  max_depth = c(50),
  bagging_fraction = c(0.7),
  bagging_freq = c(0),
  feature_fraction = c(0.7),
  max_bin = c(255),
  learning_rate = c(0.075),
  num_iterations = c(500),
  lambda_l1 = c(5),
  lambda_l2 = c(5),
  min_gain_to_split = c(0),
  early_stopping_round = c(0),
  num_threads = c(6),
  drop_rate = c(0.1),
  max_drop = c(50),
  skip_drop = c(0.5)
)

##
# Train
br <- GridSearchLgbm2(
  proj = "HCDR",
  model_name = "lightgbm",
  dataset = prdctrs_sltd,
  labels = tgts,
  job = "bc",
  val_size = floor(length(tgts)/20),
  cv_rep = 1,
  mdl_pars = tuning_pars,   # data.frame
  stc_pars = static_pars,    # list
  tgt_map = tgt_map
)

print(br$holdout_results[[1]][[1]])

# ##
# # Present result
# score_board <- br$score_board
# conf_matrix <- as.data.frame(br$valdn_results[[1]][[1]]$cf)
# 
# ##
# # Variable importance
# var_imp <- lightgbm::lgb.importance(br$models[[1]][[1]])
# var_imp_plot <- lightgbm::lgb.plot.importance(var_imp, top_n = 10)

##
# Plot learning curve
eval_res <- data.frame(
  iter = 1:length(br$models[[1]][[1]]$record_evals$train$binary_logloss$eval),
  train_logloss = unlist(br$models[[1]][[1]]$record_evals$train$binary_logloss$eval),
  test_logloss = unlist(br$models[[1]][[1]]$record_evals$test$binary_logloss$eval)
)
mdl_lc <- FitPlot("lightgbm tree", "bc",
                  eval_res, "iter", "train_logloss", "test_logloss")
mdl_lc
rm(fmtd_train)
gc()

##
# Predict
val_prdctrs_sltd <- fmtd_vald$predictors[, feats_all[1:n_feats_sltd]]
val_tgts <- fmtd_vald$target
rm(fmtd_vald)
gc()

val_pred <- predict(br$models[[1]][[1]], val_prdctrs_sltd)
rocr_pred <- ROCR::prediction(val_pred, val_tgts)
rocr_perf <- ROCR::performance(rocr_pred, measure = "auc")
print(rocr_perf@y.values)

##
# Run test data
##
if(TRUE){
  
  rm(fmtd_train, fmtd_vald)
  load("fmtd_data04.RData")
  rm(fmtd_train, fmtd_vald)
  gc()
  
  test_prdctrs_sltd <- prdctrs_test$coredata[, feats_all[1:n_feats_sltd]]
  rm(prdctrs_test)
  gc()
  
  ##
  # Load model
  #load(paste0(proj_dir, "Output/Model/", "model_lightgbm_20180827-220338.RData"))
  pred_res <- predict(br$models[[1]][[1]], as.matrix(test_prdctrs_sltd))
  pred_res2 <- as.data.frame(pred_res)
  
  out <- data.frame(SK_ID_CURR = test_id,
                    TARGET = pred_res2[,1],
                    stringsAsFactors = FALSE)
  write.csv(out, paste0(proj_dir, "Submission/",
                        "run_result_", format(Sys.Date(), "%Y%m%d"), "_", format(Sys.time(), "%H%M"),
                        ".csv"), 
            row.names = FALSE)
}

# source("/home/tli/projects/ShinyMLHome/Manual/manual_lightgbm.R")