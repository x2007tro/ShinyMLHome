##
#
# Four main functions
# 1.\ Train
# 2.\ CrossVal
#
##

##
# GridSearch
##
GridSearchAdaBoost2 <- function(proj = "",
                               model_name,
                               dataset,
                               labels,
                               job = c("bc", "mc", "rg"),
                               val_size = 100,
                               cv_rep = 5,
                               mdl_pars,
                               stc_pars){
  # assign local variables.
  gs_proj <- proj
  gs_ds <- dataset   # used
  gs_ls <- labels   # used
  gs_jb <- match.arg(job)   # used
  gs_val_sz <- val_size   # used
  gs_cv_rep <- cv_rep   # used
  gs_ly_pars <- mdl_pars   # used
  
  if(nrow(gs_ly_pars) > 0){
    res2 <- lapply(1:nrow(gs_ly_pars), function(i){
      mdl_par <- gs_ly_pars[i,, drop = FALSE]
      
      ##
      # train and fit the model
      #
      # return four items:
      # 
      # 1. aggregated&averaged score_board
      # 2. list of models for each cross validation
      # 3. list of train results for each cross validation
      # 4. list of validation results for each cross validation
      #
      res <- CrossValAdaBoost2(proj = gs_proj,
                              model_name = model_name,
                              dataset = gs_ds,
                              labels = gs_ls,
                              job = gs_jb,
                              n = i,
                              val_size = gs_val_sz,
                              cv_rep = gs_cv_rep,
                              mdl_pars = mdl_par,
                              stc_pars = stc_pars)
    })
    
    # Extract result
    score_board_prelim <- purrr::map(res2, 1)
    score_board <- dplyr::bind_rows(score_board_prelim)
    models <- purrr::map(res2, 2)
    train_results <- purrr::map(res2, 3)
    valdn_results <- purrr::map(res2, 4)
    
    res <- list(
      score_board = score_board,
      models = models,
      train_results = train_results,
      valdn_results = valdn_results
    )
  } else {
    print("Error: no parameters for tuning!")
    res <- gs_ly_pars
  }
  return(res)
}

##
# Cross validation
##
CrossValAdaBoost2 <- function(proj = "",
                             model_name,
                             dataset,
                             labels,
                             job = c("bc", "mc", "rg"),
                             n,
                             val_size = 100,
                             cv_rep = 5,
                             mdl_pars,
                             stc_pars){
  
  # assign local variables
  cv_proj <- proj
  cv_ds <- dataset   # used
  cv_ls <- labels   # used
  cv_jb <- match.arg(job)   # used
  cv_val_sz <- val_size   # used
  cv_cv_sd <- 1234   # used
  cv_cv_rep <- cv_rep   # used
  cv_mdl_pars <- mdl_pars
  cv_stc_pars <- stc_pars
  
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
    
    # trainset
    ripe_strain <- cv_ds[train_idx,]
    ripe_valdn <- cv_ds[valdn_idx,]
    
    # target
    target_strain <- cv_ls[train_idx]
    target_valdn <- cv_ls[valdn_idx]
    
    ##
    # apply model
    #
    # return a list with four items
    #
    # 1. mdl 
    # 2. score_board 
    # 3. train_result 
    # 4. valdn_result
    #
    mdl <- TrainAdaBoost2(proj_nm = cv_proj,
                         model_name,
                         split_id = splt_sd,
                         job = cv_jb,
                         tr_dataset = ripe_strain,
                         tr_labels = target_strain,
                         tr_idx = train_idx,
                         val_dataset = ripe_valdn,
                         val_labels = target_valdn,
                         val_idx = valdn_idx,
                         mdl_pars = cv_mdl_pars,
                         stc_pars = cv_stc_pars,
                         output_dir = "Output")
    
    return(mdl)
  })
  
  ##
  # Aggragate cross validation results
  ##
  res <- purrr::map(all_res,2)
  cv_res <- dplyr::bind_rows(res) 
  cv_res <- cv_res %>% 
    dplyr::group_by(proj, job, max_depth, min_leaf_size, min_info_gain2split,
                    learning_rate, data_subset, num_of_trees) %>% 
    summarise(
      avg_na_perc = mean(na_perc, na.rm = TRUE),
      avg_loss = -1,
      std_loss = -1,
      avg_acc = mean(accuracy, na.rm = TRUE),
      std_acc = ifelse(n() == 1, -1, format(sd(accuracy, na.rm = TRUE), digits = 2))
    )
  
  return(list(
    score_board = cv_res,
    model_list = purrr::map(all_res, 1),
    train_res_list = purrr::map(all_res, 3),
    valdn_res_list = purrr::map(all_res, 4)
  ))
}

##
# function TrainTF
##
TrainAdaBoost2 <- function(proj_nm = "",
                          model_name,
                          split_id = 1,
                          job = c("bc", "mc", "rg"),  # binary class., multi class., regression
                          tr_dataset = data.frame(f1=character(0)),
                          tr_labels = c(),
                          tr_idx = c(),
                          val_dataset = data.frame(f1=character(0)),
                          val_labels = c(),
                          val_idx = c(),
                          mdl_pars,
                          stc_pars,
                          output_dir){
  
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
  mdl_optd <- output_dir
  
  ##
  # Train decision tree model
  ##
  mdl <- CoreTrainAdaBoost2(x = mdl_trds,
                            y = mdl_trl,
                            x_val = mdl_vads,
                            y_val = mdl_val,
                            pars = mdl_pars,
                            job = mdl_job)
  ##
  # predict train data - return three/five items
  #
  # 1.\ Probability of each class (bc and mc only) 
  # 2.\ Prediction
  # 3.\ Accurary
  # 4.\ Prediction NA
  # 5.\ Confusion matrix (bc only)
  #
  trp <- PredictMe(mdl, mdl_trds, mdl_trl, mdl_job, model_name = model_name)
  valp <- PredictMe(mdl, mdl_vads, mdl_val, mdl_job, model_name = model_name)
  
  # Save prediction
  pred_df <- data.frame(
    index = c(tr_idx, val_idx),
    prob = rbind(trp$prob, valp$prob),
    pred = c(trp$pred, valp$pred)
  )
  SavePrediction(pred_df, mdl_optd, model_name, stc_pars$save_pred)
  
  # save model
  SaveModel(mdl, mdl_optd, model_name, stc_pars$save_mod)
  
  # construct evaluation score board
  sb <- data.frame(
    proj = mdl_pn,
    spt_id = mdl_si,
    job = mdl_job,
    max_depth = mdl_pars[1, "max_depth"], 
    min_leaf_size = mdl_pars[1, "min_child_weight"], 
    min_info_gain2split = mdl_pars[1, "cp"], 
    learning_rate = mdl_pars[1, "nu"],
    data_subset = mdl_pars[1, "bag_frac"],
    num_of_trees = mdl_pars[1, "nrounds"],
    na_perc = valp$na_pred,
    loss = "n/a",
    accuracy = valp$accr,
    stringsAsFactors = FALSE
  )
  
  # plot loss and accuracy graphs (not used for decision tree)
  if(stc_pars$graph_output){
    # errs <- mdl$evaluation_log
    # x <- FitPlot("decision tree", "Error", errs, "iter", "train_error", "test_error")
    # z1 <- ggpubr::ggtexttable(sb[,1:round(ncol(sb)/2)], rows = NULL, theme = ttheme("mOrange"))
    # z2 <- ggpubr::ggtexttable(sb[,round(ncol(sb)/2):ncol(sb)], rows = NULL, theme = ttheme("mOrange"))
    # 
    # CreateDirIfNotExist(paste0(mdl_optd, "/Graph"))
    # ggpubr::ggarrange(z1, z2, x, ncol = 1, nrow = 3) %>% 
    #   ggpubr::ggexport(filename = paste0(mdl_optd, "/Graph/decision tree for proj '", mdl_pn, "' - ",
    #                                      "loss (", round(sb$loss, 4), ") - acc (", round(sb$acc, 4), ") - ",
    #                                      format(Sys.Date(),"%Y%m%d"),"-",
    #                                      format(Sys.time(),"%H%M%S"),".png"))
  }
  
  # create output
  res <- list(
    model = mdl, 
    score_board = sb,
    train_res = trp,
    valdn_res = valp
  )
  
  return(res)
}

##
# Core decision tree train
##
CoreTrainAdaBoost2 <- function(x, y, x_val, y_val, pars, 
                              job = c("bc", "mc", "rg")){
  
  ##
  # Train the model
  ##
  train_data <- cbind.data.frame(y, x)
  colnames(train_data) <- c("Target", colnames(x))
  fmr <- as.formula(paste("Target","~", paste(colnames(x), collapse="+")))
  
  mdl <- ada::ada(formula = fmr, data = train_data, type = "discrete", 
                  iter = pars[1, "nrounds"], 
                  nu = pars[1, "nu"], 
                  bag.frac = pars[1, "bag_frac"], verbose = TRUE,
                  control = rpart::rpart.control(
                    maxdepth = pars[1, "max_depth"], 
                    minsplit = pars[1, "min_child_weight"],
                    cp = pars[1, "cp"])
                  )
  mdl2 <- ada::addtest(mdl, test.x = x_val, test.y = y_val)
  
  return(mdl2)
}