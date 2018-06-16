##
#
# Four main functions
# 1.\ Train
# 2.\ CrossVal
#
##

##
# keras helper
##
library(xgboost)
library(caret)
library(Matrix)
library(zeallot)
library(ggplot2)
library(dplyr)
library(ggpubr)

##
# function TrainTF
##
TrainReg2 <- function(proj_nm = "",
                      split_id = 1,
                      job = c("bc", "mc", "rg"),  # binary class., multi class., regression
                      tr_dataset = data.frame(f1=character(0)),
                      tr_labels = c(),
                      tr_labelsf = c(),
                      tr_idx = c(),
                      val_dataset = data.frame(f1=character(0)),
                      val_labels = c(),
                      val_labelsf = c(),
                      val_idx = c(),
                      save_pred = TRUE,
                      save_model = FALSE,
                      output = FALSE,
                      output_dir = ""){
  
  ##
  # Input validationcolsample_bytree
  ##
  mdl_pn <- proj_nm
  mdl_si <- split_id
  mdl_job <- match.arg(job)
  
  if(nrow(tr_dataset) == 0){
    print("Error: training dataset has no data!")
  } else {
    mdl_trds <- tr_dataset
  }
  if(length(tr_labels) == 0){
    print("Error: training lables has no data!")
  } else {
    mdl_trl <- tr_labels
    trls <- unique(mdl_trl)
    if(length(trls) != 2 & mdl_job == "bc") print("Error: more than two training labels exist for binary classification!")
  }
  mdl_trlf <- tr_labelsf
  
  if(nrow(val_dataset) == 0){
    print("Error: validation dataset has no data!")
  } else {
    mdl_vads <- val_dataset
  }
  if(length(val_labels) == 0){
    print("Error: validation lables has no data!")
  } else {
    mdl_val <- val_labels
    vals <- unique(mdl_val)
    if(length(vals) != 2 & mdl_job == "bc") print("Error: more than two validation labels exist for binary classification!")
  }
  mdl_valf <- val_labelsf
  
  mdl_opt <- output
  mdl_optd <- output_dir
  
  ##
  # Train xgbtree network
  ##
  
  # Set model specific parameters
  if(mdl_job == "bc"){
    mdl_tp <- "glm"
  } else if (mdl_job == "mc"){
    mdl_tp <- "glm"
  } else if (mdl_job == "rg"){
    mdl_tp <- "lm"
  } else {
    print("Error: undefined training job type!")
  }
  
  # cross validation parameters
  cv_kf_n <- 5
  cv_kf_rep <- 1
  
  # CV trainControl
  ctrl_pars <- caret::trainControl(
    method = "repeatedcv",
    number = cv_kf_n,
    repeats = cv_kf_rep,
    savePredictions = TRUE,
    classProbs = TRUE,
    verboseIter = TRUE,
    allowParallel = FALSE
  )
  
  # CV train
  mdl <- caret::train(x = mdl_trds,
                      y = mdl_trlf,
                      method = mdl_tp,
                      trControl = ctrl_pars)
  
  # predict train data
  trp <- PredictMe(mdl, mdl_trds, mdl_trl, mdl_job, caret = TRUE)
  valp <- PredictMe(mdl, mdl_vads, mdl_val, mdl_job, caret = TRUE)
  
  # calculate confusion matrix
  tr_cmtx <- caret::confusionMatrix(as.factor(trp$pred), as.factor(mdl_trl))
  val_cmtx <- caret::confusionMatrix(as.factor(valp$pred), as.factor(mdl_val))
  
  # save prediction
  if(save_pred){
    pred_df <- data.frame(
      index = c(tr_idx, val_idx),
      prob = c(trp$prob, valp$prob),
      pred = c(trp$pred, valp$pred)
    )
    CreateDirIfNotExist(paste0(mdl_optd, "/Prediction"))
    write.csv(pred_df, file = paste0(mdl_optd, "/Prediction/pred_regression_",
                                     format(Sys.Date(),"%Y%m%d"),"-",
                                     format(Sys.time(),"%H%M%S"),".csv"), row.names = FALSE)
  }
  
  # save prediction
  if(save_model){
    CreateDirIfNotExist(paste0(mdl_optd, "/Model"))
    save(mdl, file = paste0(mdl_optd, "/Model/model_regression_",
                            format(Sys.Date(),"%Y%m%d"),"-",
                            format(Sys.time(),"%H%M%S"),".RData"))
  }
  
  # construct evaluation score board
  sb <- data.frame(
    proj = mdl_pn,
    spt_id = mdl_si,
    job = mdl_job,
    loss = "n/a",
    accuracy = valp$accr,
    stringsAsFactors = FALSE
  )
  
  # plot loss and accuracy graphs (not used for regression)
  if(mdl_opt){
    # errs <- mdl$evaluation_log
    # x <- FitPlot("Regression", "Error", errs, "iter", "train_error", "test_error")
    # z1 <- ggpubr::ggtexttable(sb[,1:round(ncol(sb)/2)], rows = NULL, theme = ttheme("mOrange"))
    # z2 <- ggpubr::ggtexttable(sb[,round(ncol(sb)/2):ncol(sb)], rows = NULL, theme = ttheme("mOrange"))
    # 
    # CreateDirIfNotExist(paste0(mdl_optd, "/Graph"))
    # ggpubr::ggarrange(z1, z2, x, ncol = 1, nrow = 3) %>% 
    #   ggpubr::ggexport(filename = paste0(mdl_optd, "/Graph/regression for proj '", mdl_pn, "' - ",
    #                                      "loss (", round(sb$loss, 4), ") - acc (", round(sb$acc, 4), ") - ",
    #                                      format(Sys.Date(),"%Y%m%d"),"-",
    #                                      format(Sys.time(),"%H%M%S"),".png"))
  }
  
  # create output
  res <- list(
    model = mdl, 
    score_board = sb,
    tr_cmtx = tr_cmtx$table,
    val_cmtx = val_cmtx$table
  )
  
  return(res)
}

##
# Cross validation
##
CrossValReg2 <- function(proj = "",
                         dataset,
                         labelsf,
                         labels,
                         job = c("bc", "mc", "rg"),
                         val_size = 100,
                         cv_rep = 5,
                         intercept = 0,
                         save_pred = FALSE,
                         save_model = FALSE){
  
  # assign local variables
  cv_proj <- proj
  cv_ds <- dataset   # used
  cv_ls <- labels   # used
  cv_lsf <- labelsf
  cv_jb <- match.arg(job)   # used
  cv_val_sz <- val_size   # used
  cv_cv_sd <- 1234   # used
  cv_cv_rep <- cv_rep   # used
  cv_icpt <- intercept
  
  # split data
  all_res <- lapply(1:cv_cv_rep, function(i){
    ##
    # Split training and validation set
    ##
    splt_sd <- cv_cv_sd + 100*i
    allrows <- 1:nrow(cv_ds)
    set.seed(splt_sd)
    valdn_idx <- sample(allrows, cv_val_sz, replace = FALSE)
    train_idx <- allrows[!(allrows %in% valdn_idx)]
    ripe_strain <- cv_ds[train_idx,]
    ripe_valdn <- cv_ds[valdn_idx,]
    target_strain <- cv_ls[train_idx]
    target_valdn <- cv_ls[valdn_idx]
    targetf_strain <- cv_lsf[train_idx]
    targetf_valdn <- cv_lsf[valdn_idx]
    
    ##
    # parameters for model training
    ##
    input_jb <- cv_jb
    input_icpt <- cv_icpt
    
    ##
    # apply model
    ##
    mdl <- TrainReg2(proj_nm = cv_proj,
                     split_id = splt_sd,
                     job = input_jb,
                     tr_dataset = ripe_strain,
                     tr_labels = target_strain,
                     tr_labelsf = targetf_strain,
                     tr_idx = train_idx,
                     val_dataset = ripe_valdn,
                     val_labels = target_valdn,
                     val_labelsf = targetf_valdn,
                     val_idx = valdn_idx,
                     save_pred = save_pred,
                     save_model = save_model,
                     output = FALSE,
                     output_dir = "Output")
    
    return(mdl)
  })
  
  ##
  # Aggragate cross validation results
  ##
  res <- purrr::map(all_res,2)
  cv_res <- dplyr::bind_rows(res) 
  cv_res <- cv_res %>% 
    dplyr::group_by(proj, job) %>% 
    summarise(avg_loss = -1,
              std_loss = -1,
              avg_acc = mean(accuracy, na.rm = TRUE),
              std_acc = ifelse(n() == 1, -1, format(sd(accuracy, na.rm = TRUE), digits = 2)))
  
  return(list(
    run_res = cv_res,
    tr_cmtx = all_res[[cv_cv_rep]]$tr_cmtx,
    val_cmtx = all_res[[cv_cv_rep]]$val_cmtx
  ))
}