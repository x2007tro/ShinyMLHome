##
#
# Four main functions
# 1.\ Train
# 2.\ CrossVal
#
##

##
# BayesianSearch
##
BayesianSearchGBMH2O2 <- function(proj = "",
                              model_name,
                              dataset,
                              labels,
                              job = c("bc", "mc", "rg"),
                              sd = 0,   # 0 means no control over tf results
                              val_size = 100,
                              cv_rep = 5,
                              mdl_pars,
                              stc_pars,
                              bs_pars){
  # assign local variables
  bs_proj <- proj
  bs_ds <- dataset   # used
  bs_ls <- labels   # used
  bs_jb <- match.arg(job)   # used
  bs_sd <- sd    # used
  bs_val_sz <- val_size   # used
  bs_cv_rep <- cv_rep   # used
  
  # define optimization function
  BsOpUtil <- function(max_depth, min_child_weight, learn_rate, learn_rate_annealing, 
                       sample_rate, col_sample_rate, nrounds, stopping_rounds){
    ##
    # create parameter
    mdl_pars_bayesian <- data.frame(
      max_depth = floor(max_depth), 
      min_child_weight = floor(min_child_weight), 
      learn_rate = learn_rate, 
      learn_rate_annealing = learn_rate_annealing,
      sample_rate = sample_rate,
      col_sample_rate = col_sample_rate,
      nrounds = floor(nrounds),
      stopping_rounds = floor(stopping_rounds),
      stringsAsFactors = FALSE
    )
    
    ##
    # run cv main algorithm
    tr_res <- CrossValGBMH2O2(
      proj = paste0(bs_proj,"-","bayesian"),
      model_name = model_name,
      dataset = bs_ds,
      labels = bs_ls,
      job = bs_jb,
      n = 1,
      val_size = bs_val_sz,
      cv_rep = bs_cv_rep,
      mdl_pars = mdl_pars_bayesian,
      stc_pars = stc_pars)
                          
    acc <- mean(tr_res$score_board$avg_acc, na.rm = TRUE)  # alough only one row
    
    return(list(Score = acc, Pred = 0))
  }
  
  # fit bayesian function
  if(length(mdl_pars) != 0){
    set.seed(1111)
    res <- rBayesianOptimization::BayesianOptimization(
      FUN = BsOpUtil,
      bounds = mdl_pars,
      init_grid_dt = bs_pars$ini_grid, 
      init_points = 2, 
      n_iter = bs_pars$nrounds,
      acq = "ucb", 
      kappa = bs_pars$kappa, 
      eps = bs_pars$eps,
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
# GridSearch
##
GridSearchGBMH2O2 <- function(proj = "",
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
      res <- CrossValGBMH2O2(proj = gs_proj,
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
CrossValGBMH2O2 <- function(proj = "",
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
    mdl <- TrainGBMH2O2(proj_nm = cv_proj,
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
    dplyr::group_by(proj, job, max_depth, min_leaf_size, learning_rate,
                    learning_rate_reduction, data_subset, feature_subset,
                    num_of_trees, early_stop) %>% 
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
TrainGBMH2O2 <- function(proj_nm = "",
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
  mdl <- CoreTrainGBMH2O2(x = mdl_trds,
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
    learning_rate = mdl_pars[1, "learn_rate"], 
    learning_rate_reduction = mdl_pars[1, "learn_rate_annealing"],
    data_subset = mdl_pars[1, "sample_rate"],
    feature_subset = mdl_pars[1, "col_sample_rate"],
    num_of_trees = mdl_pars[1, "nrounds"],
    early_stop = mdl_pars[1, "stopping_rounds"],
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
CoreTrainGBMH2O2 <- function(x, y, x_val, y_val, pars, 
                              job = c("bc", "mc", "rg")){
  
  ##
  # Prepare data
  ##
  train_data <- cbind.data.frame(y, x)
  colnames(train_data) <- c("Target", colnames(x))
  
  val_data <- cbind.data.frame(y_val, x_val)
  colnames(val_data) <- c("Target", colnames(x_val))
  
  ##
  # Train the model with early stopping
  ##
  mdl <- h2o.gbm(
    x = colnames(train_data), 
    y = "Target", 
    training_frame = as.h2o(train_data), 
    validation_frame = as.h2o(val_data),
    nfolds = 0,
    score_each_iteration = TRUE,
    
    ## more trees is better if the learning rate is small enough 
    ## here, use "more than enough" trees - we have early stopping
    ntrees = pars[1, "nrounds"],                                                            
    
    ## early stopping once the validation error doesn't improve by at least 0.01% for 10 consecutive scoring events
    stopping_rounds = pars[1, "stopping_rounds"], stopping_tolerance = 1e-4, stopping_metric = "AUTO", 
    
    ## smaller learning rate is better (this is a good value for most datasets, but see below for annealing)
    learn_rate = pars[1, "learn_rate"], learn_rate_annealing = pars[1, "learn_rate_annealing"],
    
    ## depth of the tree and mini child # in one node
    max_depth = pars[1, "max_depth"], min_rows = pars[1, "min_child_weight"],
    
    ## sample 80% of rows per tree
    sample_rate = pars[1, "sample_rate"], sample_rate_per_class = NULL,                                                    
    
    ## sample 80% of columns per split
    col_sample_rate = pars[1, "col_sample_rate"], col_sample_rate_per_tree = 1,                                                  
    
    ## fix a random number generator seed for reproducibility
    seed = 1111,                                                             
    
    ## score every 10 trees to make early stopping reproducible (it depends on the scoring interval)
    score_tree_interval = 10                                               
  )
  
  return(mdl)
}