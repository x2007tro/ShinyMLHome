##
# Manual training for lightgbm model
##

# ##
# # first thing first, parameters
# options(scipen=999)
# if(R.Version()$os == "linux-gnu"){
#   mlh_dir <- paste0("/home/",Sys.info()["user"],"/projects/ShinyMLHome/")
# } else {
#   mlh_dir <- paste0("C:/Github/ShinyMLHome/")
# }
# dataset_nm <- c("dataset")[1]
# rmv_fs <- c("sessionId", "visitId")  # features to be excluded for training
# 
# ##
# # Load required source files and data
# source(paste0(mlh_dir, "global.R"))
# source(paste0(proj_dir, "Helper/", "results_aggregation.R"))
# setwd(proj_dir)
# 
#
# ##
# # Manipulation
# ##
# 
# load(paste0(proj_dir, "./Dataset/", dataset_nm, ".RData"))
#
# # 1.\ retrieve train target to replace NA with 0 and change it to numeric values
# full_target <- train_final[, "totals_transactionRevenue", drop = FALSE] %>% 
#   dplyr::mutate(target = ifelse(is.na(totals_transactionRevenue), 0, totals_transactionRevenue)) %>% 
#   dplyr::select(target)
# 
# # 2.\ create target map
# tgt_map <- data.frame(
#   StrTarget = unique(target$target),
#   NumTarget = as.numeric(unique(target$target)),
#   stringsAsFactors = FALSE
# )
# 
# # 3.\ obtain id for train and test set
# train_id <- train_final[, "fullVisitorId", drop = FALSE]
# test_id <- test_final[, "fullVisitorId", drop = FALSE]
# 
# # 4.\ remove target and id from train and test dataset
# train1 <- train_final %>% dplyr::select(-dplyr::one_of(c("totals_transactionRevenue", "fullVisitorId"))) %>% 
#   dplyr::mutate(am_i_train = 1)
# test1 <- test_final %>% dplyr::select(-fullVisitorId) %>% 
#   dplyr::mutate(am_i_train = 0)
# train_n_test <- rbind.data.frame(train1, test1)
# train_n_test_peek <- DataInspection(train_n_test)
# 
# # 5.\ feature engineering to train_n_test dataset
# #
# # serious feature engineering code needed
# #
# train_n_test_new <- train_n_test[, 
#   train_n_test_peek[(train_n_test_peek$class == "character" & 
#                       train_n_test_peek$value_cnt <= 50) | train_n_test_peek$class != "character", "feature"]]
# 
# # 6.\ Scale and OHE the full dataset
# prdctrs <- FinalTouch(train_n_test_new[,], 
#                       c(rmv_fs, 
#                         train_n_test_peek[train_n_test_peek$value_cnt == 1, "feature"]))
# train_peek1 <- prdctrs$peek1
# train_peek2 <- prdctrs$peek2
# # rm(train_n_test_new)      # if running out of memory, delete used dataset
# 
# # 7.\ separate training and test dataset
# full_train <- prdctrs$coredata[prdctrs$coredata$am_i_train == 1, ] %>% 
#   dplyr::select(-am_i_train)
# 
# full_test <- prdctrs$coredata[prdctrs$coredata$am_i_train == 0, ] %>% 
#   dplyr::select(-am_i_train)
# 
# # 8.\ Save data for model training
# save(full_train, full_target, tgt_map, full_test, train_id, test_id, file = "./Dataset/dataset_engineered01.RData")
#
# ##
# # data formatting
# ##
# 
# # 1.\ load data
# load("./Dataset/dataset_engineered01.RData")
# 
# # 2.\ setup flags
# job <- c("bc", "mc", "rg")[3]
# run_test <- FALSE
# smote_flag <- FALSE
# 
# # 3.\ smote data if necessary
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
# # 4.\ Format training data
# fmtd_train <- FormatData4Model(
#   prdctrs = full_train,
#   tgt = full_target,
#   tgt_map = tgt_map,
#   job = job,
#   model = "lightgbm"
# )
# rm(full_train, full_target)
# gc()
# 
# # 5.\ Format test data
# fmtd_test <- FormatData4Model(
#   prdctrs = full_test,
#   tgt = c(),
#   tgt_map = tgt_map,
#   job = job,
#   model = "lightgbm"
# )
# rm(full_test)
# gc()
# 
# # 6.\ Save formatted data
# save(fmtd_train, tgt_map, fmtd_test, train_id, test_id, job, file = "./Dataset/dataset_2btrained01.RData")

##
# Train the model
##

# 1.\ Load final train data
# load("./Dataset/dataset_2btrained01.RData")

# 2.\ Static parameters mainly for I/O purpose
static_pars <- list(
  graph_output = FALSE,
  save_mod = TRUE,
  save_pred = FALSE,
  save_res = FALSE
)

# 3.\ Setup model parameters
tuning_pars <- expand.grid(
  boosting = c(0),     # 0 - gbdt, 1 - dart
  num_leaves = c(100),
  min_data_in_leaf = c(300),    # always use 300
  max_depth = c(25),
  bagging_fraction = c(1),
  bagging_freq = c(0),
  feature_fraction = c(1),
  max_bin = c(255),
  learning_rate = c(0.01),     # then compare with 0.01 result
  num_iterations = c(100),
  lambda_l1 = c(0),
  lambda_l2 = c(0),
  min_gain_to_split = c(0),
  early_stopping_round = c(0),
  num_threads = c(-1),
  drop_rate = c(0.1),    # only used in dart
  max_drop = c(50),      # only used in dart
  skip_drop = c(0.5)     # only used in dart
)

# 4.\ train the model
br <- GridSearchLgbm2(
  proj = "GACRP",
  model_name = "lightgbm",
  dataset = fmtd_train$predictors[,],
  labels = fmtd_train$target[],
  job = job,
  val_size = 10000,
  holdout_size = 10000,
  cv_rep = 1,
  mdl_pars = tuning_pars,   # data.frame
  stc_pars = static_pars,    # list
  tgt_map = tgt_map
)

# 5.\ print holdout result
print(paste0("Holdout result is ", br$holdout_results[[1]][[1]]))

# 6.\ present results

### 6.1\ score board
score_board <- br$score_board

### 6.2\ Variable importance
var_imp <- lightgbm::lgb.importance(br$models[[1]][[1]])
var_imp_plot <- lightgbm::lgb.plot.importance(var_imp, top_n = 10)
all_feats <- var_imp$Feature
save(all_feats, file = "all_imp_feats.RData")

### 6.3\ Confusion matrix (Optional) and learning curve
if(job == "bc"){
  ### Confusion matrix
  conf_matrix <- as.data.frame(br$valdn_results[[1]][[1]]$cf)
  
  ### Learning curve
  eval_res <- data.frame(
    iter = 1:length(br$models[[1]][[1]]$record_evals$train$binary_logloss$eval),
    train_logloss = unlist(br$models[[1]][[1]]$record_evals$train$binary_logloss$eval),
    test_logloss = unlist(br$models[[1]][[1]]$record_evals$test$binary_logloss$eval)
  )
  mdl_lc <- FitPlot("lightgbm tree", "bc",
                    eval_res, "iter", "train_logloss", "test_logloss")
} else if (job == "rg") {
  ### Learning curve
  eval_res <- data.frame(
    iter = 1:length(br$models[[1]][[1]]$record_evals$train$l2$eval),
    train_l2 = unlist(br$models[[1]][[1]]$record_evals$train$l2$eval),
    test_l2 = unlist(br$models[[1]][[1]]$record_evals$test$l2$eval)
  )
  mdl_lc <- FitPlot("lightgbm tree", job,
                    eval_res, "iter", "train_l2", "test_l2")
} else {
  # do nothing
}
mdl_lc

##
# Generate test data
##
if(run_test){
  
  # 1.\ Run prediction
  pred_res <- predict(br$models[[1]][[1]], fmtd_test$predictors)
  
  # 2.\ Results aggregation
  out <- GACRPAggRevByFullVisitorID(
    visitor_id = test_id$fullVisitorId,
    proj_rev = pred_res
  )
  
  # 3.\ Write results to file
  write.csv(out, paste0(proj_dir, "Submission/",
                        "run_result_", format(Sys.Date(), "%Y%m%d"), "_", format(Sys.time(), "%H%M"),
                        ".csv"), 
            row.names = FALSE)
  
  gc()
}