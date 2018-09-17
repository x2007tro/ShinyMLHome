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
# save(full_predictors, full_target, full_testdata, tgt_map, file = "hcdr_full05.RData")
# 
# ##
# # first things first, parameters
# if(R.Version()$os == "linux-gnu"){
#   mlh_dir <- paste0("/home/",Sys.info()["user"],"/projects/ShinyMLHome/")
# } else {
#   mlh_dir <- paste0("C:/Github/ShinyMLHome/")
# }
# dataset_nm <- c("hcdr_sample","hcdr_full05")[2]
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
# # ##
# # # clean up tc manual features
# # tmp <- colnames(tc_manual_v2)
# # tmp1 <- substr(tmp, 3, nchar(tmp)-1)
# # colnames(tc_manual_v2) <- paste0("tc2_", tmp1)
# # colnames(tc_manual_v2)[1] <- "km_SK_ID_CURR"
# # tc_manual_v2_num <- data.frame(lapply(tc_manual_v2,as.numeric))
# # 
# # full_predictors_tmp <- dplyr::inner_join(full_predictors, tc_manual_v2_num, by = "km_SK_ID_CURR")
# # full_test_tmp <- dplyr::inner_join(full_testdata, tc_manual_v2_num, by = "km_SK_ID_CURR")
# # 
# # full_predictors <- full_predictors_tmp
# # full_test <- full_test_tmp
# # rm(full_predictors_tmp, full_test_tmp)
# 
# ##
# # Take sample
# full_target <- full_target[, , drop = FALSE]
# full_predictors <- full_predictors[, , drop = FALSE]
# 
# ##
# # Add some average features
# full_predictors$km_score_3p <- rowMeans(
#   full_predictors[,c("km_score_from_dpd_avg", "km_score_from_dpd_max", "km_score_from_dpd_min")], 
#   na.rm = FALSE
# )
# 
# full_predictors$km_score_4p <- rowMeans(
#   full_predictors[,c("km_score_3p", "km_sim_credit_score")], 
#   na.rm = FALSE
# )
# 
# full_predictors$km_avg_exsrc <- rowMeans(
#   full_predictors[,c("tc_EXT_SOURCE_3", "tc_EXT_SOURCE_2", "tc_EXT_SOURCE_1")], 
#   na.rm = FALSE
# )
# 
# ##
# # Scale and OHE training data
# prdctrs_train <- FinalTouch(full_predictors[,], rmv_fs)
# train_peek1 <- prdctrs_train$peek1
# train_peek2 <- prdctrs_train$peek2
# rm(full_predictors)
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
# load("tmp_train.RData")
# 
# ##
# # Format training data
# fmtd_train <- FormatData4Model(
#   prdctrs = prdctrs_train$coredata,
#   tgt = full_target,
#   tgt_map = tgt_map,
#   job = "bc",
#   model = "lightgbm"
# )
# rm(prdctrs_train, full_target)
# gc()
# 
# ##
# # Scale and OHE
# 
# ##
# # Add some average features
# full_testdata$km_score_3p <- rowMeans(
#   full_testdata[,c("km_score_from_dpd_avg", "km_score_from_dpd_max", "km_score_from_dpd_min")], 
#   na.rm = FALSE
# )
# 
# full_testdata$km_score_4p <- rowMeans(
#   full_testdata[,c("km_score_3p", "km_sim_credit_score")], 
#   na.rm = FALSE
# )
# 
# full_testdata$km_avg_exsrc <- rowMeans(
#   full_testdata[,c("tc_EXT_SOURCE_3", "tc_EXT_SOURCE_2", "tc_EXT_SOURCE_1")], 
#   na.rm = FALSE
# )
# 
# prdctrs_test <- FinalTouch(full_testdata, rmv_fs)
# test_peek1 <- prdctrs_test$peek1
# test_peek2 <- prdctrs_test$peek2
# test_id <- full_testdata$km_SK_ID_CURR
# rm(full_testdata)
# 
# ##
# # Format training data
# fmtd_test <- FormatData4Model(
#   prdctrs = prdctrs_test$coredata,
#   tgt = c(),
#   tgt_map = tgt_map,
#   job = "bc",
#   model = "lightgbm"
# )
# 
# save(fmtd_train, fmtd_test, test_id, tgt_map, file = "fmtd_data05.RData")

##
# Continue point
setwd(proj_dir)
load("fmtd_data05.RData")
load("all_imp_feats.RData")
rm(fmtd_test)
gc()

n_feats_sltd <- 700     # max number of feats 728

##
# Select features
prdctrs_sltd <- fmtd_train$predictors[, all_feats[1:n_feats_sltd]]
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
  num_leaves = c(100),
  min_data_in_leaf = c(300),    # always use 300
  max_depth = c(25),
  bagging_fraction = c(0.7),
  bagging_freq = c(0),
  feature_fraction = c(0.7),
  max_bin = c(255),
  learning_rate = c(0.01),     # then compare with 0.01 result
  num_iterations = c(8216),
  lambda_l1 = c(20),
  lambda_l2 = c(20),
  min_gain_to_split = c(0),
  early_stopping_round = c(0),
  num_threads = c(-1),
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
  val_size = 100,
  holdout_size = 100,
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
# all_feats <- var_imp$Feature
# save(all_feats, file = "all_imp_feats.RData")

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

##
# Reset train data
rm(fmtd_train)
gc()

##
# Run test data
##
if(TRUE){
  
  rm(fmtd_train, fmtd_vald)
  load("fmtd_data05.RData")
  rm(fmtd_train, fmtd_vald)
  gc()
  
  test_prdctrs_sltd <- fmtd_test$predictors
  rm(fmtd_test)
  gc()
  
  ##
  # Load model
  # load(paste0(proj_dir, "Output/Model/", mdl_nm))
  pred_res <- predict(br$models[[1]][[1]], test_prdctrs_sltd[,all_feats[1:n_feats_sltd]])
  pred_res2 <- as.data.frame(pred_res)
  
  out <- data.frame(SK_ID_CURR = test_id,
                    TARGET = pred_res2[,1],
                    stringsAsFactors = FALSE)
  write.csv(out, paste0(proj_dir, "Submission/",
                        "run_result_", format(Sys.Date(), "%Y%m%d"), "_", format(Sys.time(), "%H%M"),
                        ".csv"), 
            row.names = FALSE)
  
  rm(prdctrs_sltd, test_prdctrs_sltd)
  gc()
}

# source("/home/tli/projects/ShinyMLHome/Manual/manual_lightgbm.R")