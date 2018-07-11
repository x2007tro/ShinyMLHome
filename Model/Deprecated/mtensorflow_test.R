##
#
# Four main functions
# 1.\ Train
# 2.\ CrossVal
# 3.\ GridSearch
# 4.\ BayesianSearch
#
##

##
# keras helper
##
library(keras)
library(zeallot)
library(ggplot2)
library(dplyr)
library(ggpubr)

##
# function TrainTF
##
TrainTF2 <- function(proj_nm = "",
                     split_id = 1,
                     tf_seed = 0,    # 0 means don't set seed
                     job = c("bc", "mc", "rg"),  # binary class., multi class., regression
                     n = 1,
                     nlayers = 3,
                     layers_pars = list(),
                     learning_rate = 0.001,
                     metric = c("accuracy"),
                     iter_n = 20,
                     batch_n = 512,
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
  # Input validation
  ##
  mdl_pn <- proj_nm
  mdl_si <- split_id
  mdl_sd <- tf_seed
  mdl_job <- match.arg(job)
  mdl_rd <- n
  if(nlayers != length(layers_pars)){
    print("Error: layers parameters don't match the number of layers!")
  } else {
    mdl_ly_n <- nlayers
    mdl_ly_pars <- layers_pars
  }
  mdl_lr <- learning_rate
  mdl_mtc <- metric
  mdl_ep <- iter_n
  mdl_bs <- batch_n
  
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
  # Train tensorflow network
  ##
  
  # Set seed for reproduicible results
  if(mdl_sd != 0) use_session_with_seed(mdl_sd)
  
  # Set model specific parameters
  if(mdl_job == "bc"){
    mdl_oply_act <- "sigmoid"
    mdl_lsf <- "binary_crossentropy"
  } else if (mdl_job == "mc"){
    mdl_oply_act <- "softmax"
    mdl_lsf <- "categorical_crossentropy"
  } else if (mdl_job == "rg"){
    trl_max <- max(trls)
    trl_min <- min(trls)
    if(trl_min >= 0 & trl_max <= 1){
      mdl_oply_act <- "sigmoid"
      mdl_lsf <- "binary_crossentropy"
    } else {
      mdl_oply_act <- ""
      mdl_lsf <- "mse"
    }
  } else {
    print("Error: undefined training job type!")
  }
  
  # Start building model
  mdl <- keras::keras_model_sequential()
  uts_cln <- c()
  act_cln <- c()
  l1_cln <- c()
  l2_cln <- c()
  dr_cln <- c()
  if(nlayers < 2){
    print("Error: at least two layers are required!")
  } else {
    # Input layer configuration
    mdl_iply_pars <- mdl_ly_pars[[1]]
    mdl <- mdl %>% 
      layer_dense(units = mdl_iply_pars$units, activation = mdl_iply_pars$act, 
                  kernel_regularizer = regularizer_l1_l2(l1 = mdl_iply_pars$reg_l1, l2 = mdl_iply_pars$reg_l2),
                  input_shape = dim(mdl_trds)[[2]]) %>% 
      layer_dropout(rate = mdl_iply_pars$drop_rate)
    
    # collecting parameters
    uts_cln <- c(uts_cln, mdl_iply_pars$units)
    act_cln <- c(act_cln, mdl_iply_pars$act)
    l1_cln <- c(l1_cln, mdl_iply_pars$reg_l1)
    l2_cln <- c(l2_cln, mdl_iply_pars$reg_l2)
    dr_cln <- c(dr_cln, mdl_iply_pars$drop_rate)
    
    # Inner layer configuration
    mdl_inly_n <- mdl_ly_n - 2
    if(mdl_inly_n != 0){
      for(i in 1:mdl_inly_n){
        mdl_inly_pars <- mdl_ly_pars[[1+i]]
        mdl <- mdl %>% 
          layer_dense(units = mdl_inly_pars$units, activation = mdl_inly_pars$act, 
                      kernel_regularizer = regularizer_l1_l2(l1 = mdl_inly_pars$reg_l1, l2 = mdl_inly_pars$reg_l2)) %>% 
          layer_dropout(rate = mdl_inly_pars$drop_rate)
        
        # collecting parameters
        uts_cln <- c(uts_cln, mdl_inly_pars$units)
        act_cln <- c(act_cln, mdl_inly_pars$act)
        l1_cln <- c(l1_cln, mdl_inly_pars$reg_l1)
        l2_cln <- c(l2_cln, mdl_inly_pars$reg_l2)
        dr_cln <- c(dr_cln, mdl_inly_pars$drop_rate)
      }
    }
  
    # Output layer configuration
    mdl_oply_pars <- mdl_ly_pars[[length(mdl_ly_pars)]]
    if(mdl_oply_act == ""){
      mdl <- mdl %>% layer_dense(units = mdl_oply_pars$units)
    } else {
      mdl <- mdl %>% layer_dense(units = mdl_oply_pars$units, activation = mdl_oply_act)
    }
    
    # collecting parameters
    uts_cln <- c(uts_cln, mdl_inly_pars$units)
    act_cln <- c(act_cln, mdl_oply_act)
    l1_cln <- c(l1_cln, -1)
    l2_cln <- c(l2_cln, -1)
    dr_cln <- c(dr_cln, -1)
  }
  
  # Compile the model
  mdl %>% keras::compile(
    optimizer = keras::optimizer_rmsprop(lr = mdl_lr),
    loss = mdl_lsf,
    metrics = mdl_mtc
  )
  
  fitrs <- mdl %>% keras::fit(
    x = mdl_trds,
    y = mdl_trl,
    epochs = mdl_ep,
    batch_size = mdl_bs,
    validation_data = list(mdl_vads, mdl_val)
  )
  
  # calculate score
  score <- mdl %>% keras::evaluate(
    x = mdl_vads,
    y = mdl_val
  )
  
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
    write.csv(pred_df, file = paste0(mdl_optd, "/Prediction/pred_tensorflow_",
                                     format(Sys.Date(),"%Y%m%d"),"-",
                                     format(Sys.time(),"%H%M%S"),".csv"), row.names = FALSE)
  }
  
  # save prediction
  if(save_model){
    CreateDirIfNotExist(paste0(mdl_optd, "/Model"))
    save(mdl, file = paste0(mdl_optd, "/Model/model_tensorflow_",
                            format(Sys.Date(),"%Y%m%d"),"-",
                            format(Sys.time(),"%H%M%S"),".RData"))
  }
  
  # construct evaluation score board
  sb <- data.frame(
    proj = mdl_pn,
    spt_id = mdl_si,
    tf_seed = mdl_sd,
    job = mdl_job,
    layers = mdl_ly_n,
    unit = paste0(uts_cln, collapse = " ,"),
    activation = paste0(act_cln, collapse = " ,"),
    l1_reg = paste0(l1_cln, collapse = " ,"),
    l2_reg = paste0(l2_cln, collapse = " ,"),
    drop_rate = paste0(dr_cln, collapse = " ,"),
    learning_rate = mdl_lr,
    iteration = mdl_ep,
    loss = score$loss,
    accuracy = score$acc,
    stringsAsFactors = FALSE
  )
  
  # plot loss and accuracy graphs
  if(mdl_opt){
    fitmet <- fitrs$metrics
    fitmet$iter <- 1:mdl_ep
    fitmet_df <- dplyr::bind_cols(fitmet)
    x <- FitPlot("Tensorflow via Keras", "Loss", fitmet_df, "iter", "loss", "val_loss")
    y <- FitPlot("Tensorflow via Keras", "Accuracy", fitmet_df, "iter", "acc", "val_acc")
    z1 <- ggpubr::ggtexttable(sb[,1:round(ncol(sb)/2)], rows = NULL, theme = ttheme("mOrange"))
    z2 <- ggpubr::ggtexttable(sb[,(round(ncol(sb)/2)+1):ncol(sb)], rows = NULL, theme = ttheme("mOrange"))
    
    CreateDirIfNotExist(paste0(mdl_optd, "/Graph"))
    ggpubr::ggarrange(z1, z2, x, y, ncol = 1, nrow = 4) %>% 
      ggpubr::ggexport(filename = paste0(mdl_optd, "/Graph/tensorflow for proj '", mdl_pn, "' - ",
                                         "round (", mdl_rd, ") - ",
                                         "loss (", round(sb$loss, 4), ") - acc (", round(sb$acc, 4), ") - ",
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
CrossValTF2 <- function(proj = "",
                        dataset,
                        labels,
                        job = c("bc", "mc", "rg"),
                        n = i,
                        tf_sd = 0,   # 0 means no control over tf results
                        val_size = 100,
                        cv_rep = 5,
                        nlayers = 3,
                        units = 5,
                        act = "relu",    # shouldn't change
                        reg_l1 = 0,
                        reg_l2 = 0,
                        drop_rate = 0,
                        learning_rate = 0.001,
                        iter_n = 20,
                        batch_size = 512,
                        save_pred = FALSE,
                        save_model = FALSE){
  
  # assign local variables
  cv_proj <- proj
  cv_ds <- dataset   # used
  cv_ls <- labels   # used
  cv_jb <- match.arg(job)   # used
  cv_tf_sd <- tf_sd    # used
  cv_val_sz <- val_size   # used
  cv_cv_sd <- 1234   # used
  cv_cv_rep <- cv_rep   # used
  cv_ly_n <- nlayers   # used
  cv_uts <- units 
  cv_act <- act
  cv_rl1 <- reg_l1
  cv_rl2 <- reg_l2
  cv_dr <- drop_rate
  cv_lr <- learning_rate   # used
  cv_ep <- iter_n   # used
  cv_bs <- batch_size   # used
  
  # split data
  res <- lapply(1:cv_cv_rep, function(i){
    ##
    # Split training and validation set
    ##
    cv_rep <- cv_cv_rep
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
    input_tfsd <- cv_tf_sd
    input_jb <- cv_jb
    input_lr <- cv_lr
    input_mtc <- c("accuracy")  # accuary for classification and mae for regression
    input_itn <- cv_ep
    input_bn <- cv_bs
    
    # construct layer parameters
    input_lay_pars <- list()
    for(j in 1:cv_ly_n){
      if(j < cv_ly_n){   # input and inner layers
        res <- list(
          units = cv_uts, act = cv_act, reg_l1 = cv_rl1, reg_l2 = cv_rl2, drop_rate = cv_dr
        )
      } else {   # output layers
        res <- list(
          units = if(input_jb == "mc") v = length(unique(cv_ls)) else v = 1, 
          act = NA, reg_l1 = NA, reg_l2 = NA, drop_rate = NA   # last layer only uts matters
        )
      }
      input_lay_pars[[j]] <- res
    }
    
    ##
    # apply model
    ##
    mdl <- TrainTF2(proj_nm = cv_proj,
                    split_id = splt_sd,
                    tf_seed = input_tfsd,
                    job = input_jb,
                    n = n,
                    nlayers = length(input_lay_pars),
                    layers_pars = input_lay_pars,
                    learning_rate = input_lr,
                    metric = input_mtc,
                    iter_n = input_itn,
                    batch_n = input_bn,
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
    dplyr::group_by(proj, tf_seed, job, layers, unit, activation,
                    l1_reg, l2_reg, drop_rate, learning_rate, iteration) %>% 
    summarise(avg_loss = format(mean(loss, na.rm = TRUE), digits = 2),
              std_loss = ifelse(n() == 1, -1, format(sd(loss, na.rm = TRUE), digits = 2)),
              avg_acc = mean(accuracy, na.rm = TRUE),
              std_acc = ifelse(n() == 1, -1, format(sd(accuracy, na.rm = TRUE), digits = 2))
    )
  
  return(cv_res)
}

##
# GridSearch
##
GridSearchTF2 <- function(proj = "",
                          dataset,
                          labels,
                          job = c("bc", "mc", "rg"),
                          tf_sd = 0,   # 0 means no control over tf results
                          val_size = 100,
                          cv_rep = 5,
                          layers_pars = data.frame(f1=character(0)),
                          learning_rate = 0.001,
                          batch_size = 512,
                          save_pred = FALSE,
                          save_model = FALSE){
  # assign local variables
  gs_proj <- proj
  gs_ds <- dataset   # used
  gs_ls <- labels   # used
  gs_jb <- match.arg(job)   # used
  gs_tf_sd <- tf_sd    # used
  gs_val_sz <- val_size   # used
  gs_cv_rep <- cv_rep   # used
  gs_ly_pars <- layers_pars   # used
  gs_lr <- learning_rate   # used
  gs_bs <- batch_size   # used
  
  if(nrow(gs_ly_pars) > 0){
    res2 <- lapply(1:nrow(gs_ly_pars), function(i){
      gs_ly_n <- gs_ly_pars[i, "nlayers"]
      gs_uts <- gs_ly_pars[i, "units"]
      gs_rl1 <- gs_ly_pars[i, "reg_l1"]
      gs_rl2 <- gs_ly_pars[i, "reg_l2"]
      gs_dr <- gs_ly_pars[i, "drop_rate"]
      gs_iter_n <- gs_ly_pars[i, "tf_nrounds"]
      
      # train and fit the model
      res <- CrossValTF2(proj = gs_proj,
                         dataset = gs_ds,
                         labels = gs_ls,
                         job = gs_jb,
                         n = i,
                         tf_sd = gs_tf_sd,   # 0 means no control over tf results
                         val_size = gs_val_sz,
                         cv_rep = gs_cv_rep,
                         nlayers = gs_ly_n,
                         units = gs_uts,
                         act = "relu",    # shouldn't change
                         reg_l1 = gs_rl1,
                         reg_l2 = gs_rl2,
                         drop_rate = gs_dr,
                         learning_rate = gs_lr,
                         iter_n = gs_iter_n,
                         batch_size = gs_bs,
                         save_pred = save_pred)
    })
    res3 <- dplyr::bind_rows(res2)
  } else {
    print("Error: no parameters for tuning!")
    res3 <- layers_pars
  }
  return(res3)
}

##
# BayesianSearch
##
BayesianSearchTF2 <- function(proj = "",
                              dataset,
                              labels,
                              job = c("bc", "mc", "rg"),
                              tf_sd = 0,   # 0 means no control over tf results
                              val_size = 100,
                              cv_rep = 5,
                              layers_pars = list(),
                              learning_rate = 0.001,
                              batch_size = 512,
                              bayes_ini_grid = NULL,
                              bayes_nrounds = 30,
                              bayes_kappa = 1,
                              bayes_eps = 0,
                              save_pred = FALSE){
  # assign local variables
  bs_proj <- proj
  bs_ds <- dataset   # used
  bs_ls <- labels   # used
  bs_jb <- match.arg(job)   # used
  bs_tf_sd <- tf_sd    # used
  bs_val_sz <- val_size   # used
  bs_cv_rep <- cv_rep   # used
  bs_ly_pars <- layers_pars   # used
  bs_lr <- learning_rate   # used
  bs_bs <- batch_size   # used
  
  # define optimization function
  BsOpUtil <- function(nlayers, units, reg_l1, reg_l2, drop_rate, tf_nrounds){
    tr_res <- CrossValTF2(proj = paste0(bs_proj,"-","bayesian"),
                          dataset = bs_ds,
                          labels = bs_ls,
                          job = bs_jb,
                          n = 1,
                          tf_sd = bs_tf_sd,   # 0 means no control over tf results
                          val_size = bs_val_sz,
                          cv_rep = bs_cv_rep,
                          nlayers = floor(nlayers),
                          units = floor(units),
                          act = "relu",    # shouldn't change
                          reg_l1 = reg_l1,
                          reg_l2 = reg_l2,
                          drop_rate = drop_rate,
                          learning_rate = bs_lr,
                          iter_n = floor(tf_nrounds),
                          batch_size = bs_bs,
                          save_pred = save_pred)
    acc <- tr_res$avg_acc
    
    return(list(Score = acc, Pred = 0))
  }
  
  # fit bayesian function
  if(length(layers_pars) != 0){
    set.seed(1111)
    res <- rBayesianOptimization::BayesianOptimization(
      FUN = BsOpUtil,
      bounds = layers_pars,
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





















