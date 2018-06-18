##
#
# Four main functions
# 1.\ Train
# 2.\ CrossVal
# 3.\ GridSearch
# 4.\ BayesianSearch
# 5.\ CaretCV
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
TrainXgbT2 <- function(proj_nm = "",
                       split_id = 1,
                       job = c("bc", "mc", "rg"),  # binary class., multi class., regression
                       n = 1,
                       eta = 0.3,
                       gamma = 0,
                       max_depth = 6,
                       min_child_weight = 1,
                       subsample = 1,
                       colsample_bytree = 1,
                       iter_n = 20,
                       tr_dataset = data.frame(f1=character(0)),
                       tr_labels = c(),
                       tr_idx = c(),
                       val_dataset = data.frame(f1=character(0)),
                       val_labels = c(),
                       val_idx = c(),
                       save_pred = TRUE,
                       save_model = TRUE,
                       output = FALSE,
                       output_dir = ""){
  
  ##
  # Input validationcolsample_bytree
  ##
  mdl_pn <- proj_nm
  mdl_si <- split_id
  mdl_job <- match.arg(job)
  mdl_rd <- n
  mdl_eta <- eta
  mdl_gm <- gamma
  mdl_md <- max_depth
  mdl_mcw <- min_child_weight
  mdl_ss <- subsample
  mdl_cs_bt <- colsample_bytree
  mdl_ep <- iter_n
  
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
  mdl_opt <- output
  mdl_optd <- output_dir
  
  ##
  # Train xgbtree network
  ##
  
  # Set model specific parameters
  if(mdl_job == "bc"){
    mdl_lsf <- "binary:logistic"
  } else if (mdl_job == "mc"){
    mdl_lsf <- "multi:softprob"
  } else if (mdl_job == "rg"){
    mdl_lsf <- "reg:linear"
  } else {
    print("Error: undefined training job type!")
  }
  
  # Start building model
  
  # Create DMatrix
  dm_strain <- xgboost::xgb.DMatrix(data = mdl_trds, label = mdl_trl)
  dm_valdn <- xgboost::xgb.DMatrix(data = mdl_vads, label = mdl_val)
  
  mdl_pars <- list(
    eta = mdl_eta,
    gamma = mdl_gm,
    max_depth = mdl_md,
    min_child_weight = mdl_mcw,
    subsample = mdl_ss,
    colsample_bytree = mdl_cs_bt
  )
  
  # train model parameters
  if(mdl_job == "mc"){
    # Training
    mdl <- xgboost::xgb.train(params = mdl_pars,
                              data = dm_strain,
                              booster = "gbtree",
                              objective = mdl_lsf,
                              nrounds = mdl_ep,
                              early_stopping_rounds = 10,
                              num_class = length(unique(mdl_trl)),
                              watchlist = list(train=dm_strain, test=dm_valdn),
                              verbose = 1)
  } else {
    # Training
    mdl <- xgboost::xgb.train(params = mdl_pars,
                              data = dm_strain,
                              booster = "gbtree",
                              objective = mdl_lsf,
                              nrounds = mdl_ep,
                              early_stopping_rounds = 10,
                              watchlist = list(train=dm_strain, test=dm_valdn),
                              verbose = 1)
  }
  
  # predict train data
  trp <- PredictMe(mdl, mdl_trds, mdl_trl, mdl_job)
  valp <- PredictMe(mdl, mdl_vads, mdl_val, mdl_job)
  
  # save prediction
  if(save_pred){
    pred_df <- data.frame(
      index = c(tr_idx, val_idx),
      prob = c(trp$prob, valp$prob),
      pred = c(trp$pred, valp$pred)
    )
    CreateDirIfNotExist(paste0(mdl_optd, "/Prediction"))
    write.csv(pred_df, file = paste0(mdl_optd, "/Prediction/pred_xgbtree_",
                                     format(Sys.Date(),"%Y%m%d"),"-",
                                     format(Sys.time(),"%H%M%S"),".csv"), row.names = FALSE)
  }
  
  # save prediction
  if(save_model){
    CreateDirIfNotExist(paste0(mdl_optd, "/Model"))
    save(mdl, file = paste0(mdl_optd, "/Model/model_xgbtree_",
                            format(Sys.Date(),"%Y%m%d"),"-",
                            format(Sys.time(),"%H%M%S"),".RData"))
  }
  
  # construct evaluation score board
  sb <- data.frame(
    proj = mdl_pn,
    spt_id = mdl_si,
    job = mdl_job,
    eta = mdl_eta,
    gamma = mdl_gm,
    max_depth = mdl_md,
    min_child_weight = mdl_mcw,
    subsample = mdl_ss,
    colsample_bytree = mdl_cs_bt,
    iteration = mdl_ep,
    loss = "n/a",
    accuracy = valp$accr,
    stringsAsFactors = FALSE
  )
  
  # plot loss and accuracy graphs
  if(mdl_opt){
    errs <- mdl$evaluation_log
    x <- FitPlot("xgboost tree", "Error", errs, "iter", "train_error", "test_error")
    z1 <- ggpubr::ggtexttable(sb[,1:round(ncol(sb)/2)], rows = NULL, theme = ttheme("mOrange"))
    z2 <- ggpubr::ggtexttable(sb[,(round(ncol(sb)/2)+1):ncol(sb)], rows = NULL, theme = ttheme("mOrange"))
    
    CreateDirIfNotExist(paste0(mdl_optd, "/Graph"))
    ggpubr::ggarrange(z1, z2, x, ncol = 1, nrow = 3) %>% 
      ggpubr::ggexport(filename = paste0(mdl_optd, "/Graph/xbgtree for proj '", mdl_pn, "' - ",
                                         "round (", mdl_rd, ") - ",
                                         "loss (", "na", ") - acc (", round(sb$acc, 4), ") - ",
                                         format(Sys.Date(),"%Y%m%d"),"-",
                                         format(Sys.time(),"%H%M%S"),".png"),
                       width = image_dim[1], height = image_dim[2])
  }
  
  # create output
  res <- list(
    model = mdl, 
    score_board = sb
  )
  
  return(res)
}

##
# Cross validation
##
CrossValXgbT2 <- function(proj = "",
                          dataset,
                          labels,
                          job = c("bc", "mc", "rg"),
                          n = 1,
                          val_size = 100,
                          cv_rep = 5,
                          eta = 0.001,
                          gamma = 0.1,
                          max_depth = 3,
                          min_child_weight = 1,
                          subsample = 10,
                          colsample_bytree = 10,
                          iter_n = 20,
                          save_pred,
                          save_model){
  
  # assign local variables
  cv_proj <- proj
  cv_ds <- dataset   # used
  cv_ls <- labels   # used
  cv_jb <- match.arg(job)   # used
  cv_val_sz <- val_size   # used
  cv_cv_sd <- 1234   # used
  cv_cv_rep <- cv_rep   # used
  cv_eta <- eta
  cv_gm <- gamma
  cv_md <- max_depth
  cv_mcw <- min_child_weight
  cv_ss <- subsample
  cv_cs_bt <- colsample_bytree
  cv_ep <- iter_n   # used
  
  # split data
  res <- lapply(1:cv_cv_rep, function(i){
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
    
    ##
    # parameters for model training
    ##
    input_jb <- cv_jb
    input_eta <- cv_eta
    input_gm <- cv_gm
    input_md <- cv_md
    input_mcw <- cv_mcw
    input_ss <- cv_ss
    input_cs_bt <- cv_cs_bt
    input_itn <- cv_ep
    
    ##
    # apply model
    ##
    mdl <- TrainXgbT2(proj_nm = cv_proj,
                      split_id = splt_sd,
                      job = input_jb,
                      n = n,
                      eta = input_eta,
                      gamma = input_gm,
                      max_depth = input_md,
                      min_child_weight = input_mcw,
                      subsample = input_ss,
                      colsample_bytree = input_cs_bt,
                      iter_n = input_itn,
                      tr_dataset = ripe_strain,
                      tr_labels = target_strain,
                      tr_idx = train_idx,
                      val_dataset = ripe_valdn,
                      val_labels = target_valdn,
                      val_idx = valdn_idx,
                      save_pred = save_pred,
                      save_model = save_model,
                      output = TRUE,
                      output_dir = "Output")
    
    return(mdl$score_board)
  })
  
  ##
  # Aggragate cross validation results
  ##
  cv_res <- dplyr::bind_rows(res) 
  cv_res <- cv_res %>% 
    dplyr::group_by(proj, job, eta, gamma, max_depth, min_child_weight,
                    subsample, colsample_bytree, iteration) %>% 
    summarise(avg_loss = -1,
              std_loss = -1,
              avg_acc = mean(accuracy, na.rm = TRUE),
              std_acc = ifelse(n() == 1, -1, format(sd(accuracy, na.rm = TRUE), digits = 2))
    )
  
  return(cv_res)
}

##
# GridSearch
##
GridSearchXgbT2 <- function(proj = "",
                            dataset,
                            labels,
                            job = c("bc", "mc", "rg"),
                            val_size = 100,
                            cv_rep = 5,
                            xgbt_pars = data.frame(f1=character(0)),
                            save_pred,
                            save_model){
  # assign local variables.
  gs_proj <- proj
  gs_ds <- dataset   # used
  gs_ls <- labels   # used
  gs_jb <- match.arg(job)   # used
  gs_val_sz <- val_size   # used
  gs_cv_rep <- cv_rep   # used
  gs_ly_pars <- xgbt_pars   # used
  
  if(nrow(gs_ly_pars) > 0){
    res2 <- lapply(1:nrow(gs_ly_pars), function(i){
      gs_eta <- gs_ly_pars[i, "eta"]
      gs_gm <- gs_ly_pars[i, "gamma"]
      gs_md <- gs_ly_pars[i, "max_depth"]
      gs_mcw <- gs_ly_pars[i, "min_child_weight"]
      gs_ss <- gs_ly_pars[i, "subsample"]
      gs_cs_bt <- gs_ly_pars[i, "colsample_bytree"]
      gs_iter_n <- gs_ly_pars[i, "xgbt_nrounds"]
      
      # train and fit the model
      res <- CrossValXgbT2(proj = gs_proj,
                           dataset = gs_ds,
                           labels = gs_ls,
                           job = gs_jb,
                           n = i,
                           val_size = gs_val_sz,
                           cv_rep = gs_cv_rep,
                           eta = gs_eta,
                           gamma = gs_gm,
                           max_depth = gs_md,
                           min_child_weight = gs_mcw,
                           subsample = gs_ss,
                           colsample_bytree = gs_cs_bt,
                           iter_n = gs_iter_n,
                           save_pred = save_pred,
                           save_model = save_model)
    })
      
    res3 <- dplyr::bind_rows(res2)
  } else {
    print("Error: no parameters for tuning!")
    res3 <- xgbt_pars
  }
  return(res3)
}

##
# BayesianSearch
##
BayesianSearchXgbT2 <- function(proj = "",
                                dataset,
                                labels,
                                job = c("bc", "mc", "rg"),
                                val_size = 100,
                                cv_rep = 5,
                                xgbt_pars = list(),
                                bayes_ini_grid = NULL,
                                bayes_nrounds = 30,
                                bayes_kappa = 1,
                                bayes_eps = 0,
                                save_pred){
  # assign local variables
  bs_proj <- proj
  bs_ds <- dataset   # used
  bs_ls <- labels   # used
  bs_jb <- match.arg(job)   # used
  bs_val_sz <- val_size   # used
  bs_cv_rep <- cv_rep   # used
  bs_ly_pars <- xgbt_pars   # used
  
  # define optimization function
  BsOpUtil <- function(eta, gamma, max_depth, 
                       min_child_weight, subsample, 
                       colsample_bytree, xgbt_nrounds){
    tr_res <- CrossValXgbT2(proj = paste0(bs_proj,"-","bayesian"),
                            dataset = bs_ds,
                            labels = bs_ls,
                            job = bs_jb,
                            n = 1,
                            val_size = bs_val_sz,
                            cv_rep = bs_cv_rep,
                            eta = eta,
                            gamma = gamma,
                            max_depth = floor(max_depth),
                            min_child_weight = floor(min_child_weight),
                            subsample = subsample,
                            colsample_bytree = colsample_bytree,
                            iter_n = floor(xgbt_nrounds),
                            save_pred = save_pred,
                            save_model = FALSE)

    acc <- tr_res$avg_acc
    
    return(list(Score = acc, Pred = 0))
  }
  
  # fit bayesian function
  if(length(xgbt_pars) != 0){
    set.seed(1111)
    res <- rBayesianOptimization::BayesianOptimization(
      FUN = BsOpUtil,
      bounds = xgbt_pars,
      init_grid_dt = bayes_ini_grid, 
      init_points = 2, 
      n_iter = bayes_nrounds,
      acq = "ucb", 
      kappa = bayes_kappa, 
      eps = bayes_eps,
      verbose = TRUE
    )
    res <- res$History
  } else {
    print("Warning: parameters tuning list is empty for bayesian search!")
    res <- data.frame(f1=character(0))
  }
  return(res)
}

##
# CV using caret package
##
CaretCVXgbT2 <- function(proj = "",
                         dataset,
                         labels,
                         job = c("bc", "mc", "rg"),
                         val_size = 100,
                         cv_rep = 5,
                         xgbt_pars = data.frame(f1=character(0)),
                         save_pred){

  # cross validation parameters
  nr <- nrow(dataset)
  cv_kf_n <- floor(nr/val_size)
  cv_kf_rep <- cv_rep
  
  # CV trainControl
  ctrl_pars <- caret::trainControl(
    method = "repeatedcv",
    number = cv_kf_n,
    repeats = cv_kf_rep,
    savePredictions = save_pred,
    classProbs = TRUE,
    verboseIter = TRUE,
    allowParallel = FALSE
  )
  
  # CV train
  set.seed(442)
  labelsf <- as.factor(labels)
  levels(labelsf) <- c("No", "Yes")
  xgbcv_model <- caret::train(x = dataset,
                              y = labelsf,
                              method = "xgbTree",
                              trControl = ctrl_pars,
                              tuneGrid = xgbt_pars)
  
  best_par_from_cv <- xgbcv_model$bestTune
  return(best_par_from_cv)
}


















