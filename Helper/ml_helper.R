##
# Data inspection
##
DataInspection <- function(dataset){
  res <- lapply(1:ncol(dataset), function(i){
    max_row <- nrow(dataset)
    subset <- dataset[,i]
    cls <- class(subset)[1]
    if(cls == "numeric"){ subset <- round(subset, 2) }
    has_val <- min(length(subset[subset!=""]),
                   length(subset[!is.na(subset)]))
    no_val <- max_row - has_val
    vals <- paste0(paste0(unique(subset)[1:min(length(unique(subset)), 10)], collapse = ","), "...")
    
    res <- data.frame(feature = colnames(dataset)[i],
                      class = cls,
                      has_value = has_val,
                      miss_value = no_val,
                      value_cnt = length(unique(subset)),
                      values = vals,
                      stringsAsFactors = FALSE)
    return(res)
  })
  fnl_res <- dplyr::bind_rows(res)
  return(fnl_res)
}

##
# One hot encoding
##
OHEOneCol <- function(col_nm, dataset){
  vals_l1 <- dataset[,col_nm]
  vals_l1[is.na(vals_l1) | vals_l1 == ""] <- paste0(col_nm, "_unknown")
  vals_l2 <- vals_l1
  
  ##
  # perform replacement
  vals_l2 <- gsub(",", "_", vals_l2)
  vals_l2 <- gsub(" ", "_", vals_l2)
  vals_l2 <- gsub("-", "_", vals_l2)
  vals_l2 <- gsub(" / ", "_", vals_l2)
  vals_l2 <- gsub("/", "_", vals_l2)
  
  vals <- unique(vals_l2)
  
  res <- matrix(0, nrow = nrow(dataset), ncol = length(vals))
  colnames(res) <- paste0(col_nm, "_", vals)
  
  for(i in 1:nrow(dataset)){
      res[i, paste0(col_nm, "_", vals_l2[i])] <- 1
  }
  
  gc()
  return(as.data.frame(res, stringsAsFactors = FALSE))
}

OHE <- function(col_nms, dataset){
  if(length(col_nms) == 0){
    dataset_new <- dataset
  } else {
    dataset_remain <- dataset[,!(colnames(dataset) %in% col_nms)]
    res <- lapply(col_nms, OHEOneCol, dataset)
    res_all <- dplyr::bind_cols(res)
    dataset_new <- cbind(dataset_remain, res_all)
  }
  
  return(dataset_new)
}

##
# DataScale
##
DataScaleOneCol <- function(col_nm, dataset, rep_na, rep_na_with){
  vals <- dataset[,col_nm]
  if(is.numeric(vals) | is.integer(vals)){
    if(max(abs(vals), na.rm = TRUE) > 1){
      rnk <- rank(vals, ties.method = "average", na.last = TRUE)
      unif_converter <- function(x){(x-min(x, na.rm = TRUE))/(max(x, na.rm = TRUE)-min(x, na.rm = TRUE))}
      res <- unif_converter(rnk)
      
      res <- round(res, 5)
    } else {
      res <- vals
    }
    if(rep_na) res[is.na(res)] <- rep_na_with
  } else {
    res <- vals
    print(paste0("Warning: Feature ", col_nm, " is not scaled due to non-numeric format!"))
  }
  return(as.data.frame(res, stringsAsFactors = FALSE))
}

DataScale <- function(col_nms, dataset, rep_na = FALSE, rep_na_with = 0){
  if(length(col_nms) == 0){
    dataset_new <- dataset
  } else {
    dataset_remain <- dataset[,!(colnames(dataset) %in% col_nms)]
    res <- lapply(col_nms, DataScaleOneCol, dataset, rep_na, rep_na_with)
    res_all <- dplyr::bind_cols(res)
    colnames(res_all) <- col_nms
    dataset_new <- cbind(dataset_remain, res_all)
  }
  
  return(dataset_new)
}

##
# Format data based on the model
##
FormatData4Model <- function(prdctrs, tgt, tgt_map,
                             job = c("bc", "mc", "rg"),
                             model = all_models){
  
  mdl_nm <- match.arg(model)
  tgt_map_used <- FALSE
  tgt_nm <- colnames(tgt)[1]
  tgt <- tgt[,1]
  
  # -- target manipulation --
  # if job = regression, 
  # - target would be numerical;
  # else,
  # - target would be factor for most models except for xgbtree and tensorflow
  #
  if(length(tgt) == 0){
    new_tgt <- c()
  } else {
    if(job == "rg"){
      # all targets has to be numerical
      new_tgt <- as.numeric(tgt) * 1.0
    } else {
      if(mdl_nm == "regression" | mdl_nm == "naive_bayes" | mdl_nm == "decision_tree" | 
         mdl_nm == "ada_boost" | mdl_nm == "random_forest" | mdl_nm == "gbm_h2o"){
        # if above models, target should be factors
        new_tgt <- as.factor(tgt)
      } else if (mdl_nm == "xgbtree" | mdl_nm == "tensorflow" | mdl_nm == "lightgbm"){
        # if xgbtree, target should be numerical
        new_tgt <- SwapTargetType(tgt = tgt, tgt_map = tgt_map, from_nm = "StrTarget", to_nm = "NumTarget")
        tgt_nm <- "NumTarget"
        tgt_map_used <- TRUE
      } else {
        new_tgt <- tgt
      }
    }
  }
  
  # -- Predictors manipulation --
  # if certain model, character has to be converted to factor,
  # else if xgbtree, data has to be matrix,
  # else, keep the data unchanged
  #
  
  if(mdl_nm == "regression" | mdl_nm == "naive_bayes" | mdl_nm == "decision_tree" | 
     mdl_nm == "ada_boost" | mdl_nm == "random_forest" | mdl_nm == "gbm_h2o"){
    # Format charater to factor and everything numeric
    for(i in 1:ncol(prdctrs)){
      tmp <- prdctrs[,i]
      if(class(tmp)[1] == "character"){
        tmp_mod <- as.factor(tmp)
      } else if(class(tmp)[1] == "factor") {
        tmp_mod <- tmp
      } else {
        tmp_mod <- as.numeric(tmp) * 1.0
      }
      prdctrs[,i] <- tmp_mod
    }
    new_prdctrs <- prdctrs
  } else if (mdl_nm == "xgbtree" | mdl_nm == "tensorflow" | mdl_nm == "lightgbm") {
    # data has to be matrix - force formatting
    prdctrs2 <- ToNumeric(colnames(prdctrs), prdctrs)
    new_prdctrs <- as.matrix(prdctrs2) * 1.0
    colnames(new_prdctrs) <- colnames(prdctrs2)
  } else {
    new_prdctrs <- prdctrs
  }
  
  res <- list(
    target = new_tgt,
    target_name = tgt_nm,
    predictors = new_prdctrs,
    target_map_used = tgt_map_used
  )
  
  return(res)
}

##
# Plot functions for fitting visualization
##
FitPlot <- function(model, type, data, iter_col, tr_col, vl_col){
  x <- ggplot(data, aes(x = data[[iter_col]])) +                    
    geom_line(aes(y = data[[tr_col]], color = "training")) +  
    geom_line(aes(y = data[[vl_col]], color = "validation")) +
    xlab("Iteration") + ylab(type) +
    ggtitle(paste0(type, " plot for model - ", model)) +
    labs(caption = paste0("Plot produced on ", Sys.time()))
  return(x)
}

##
# Create directory if not exist
##
CreateDirIfNotExist <- function(my_dir){
  ifelse(
    !dir.exists(my_dir),
    dir.create(my_dir, recursive = TRUE),
    my_dir
  )
}

##
# Help functions
##
CreateParRange <- function(type = c("exact", "grid", "bayesian"), beg = 0, end = 0, inc = 0){
  t <- match.arg(type)
  if(t == "exact"){
    res <- c((beg+end)/2)
  } else {
    if(end < beg | inc > (end - beg)){
      res <- c(0)
    } else {
      if(t == "grid"){
        if(beg == end){
          res <- c(beg)
        } else {
          if(inc == 0){
            res <- c(beg, end)
          } else {
            res <- seq(beg, end, inc)
          }
        }
      } else if (t == "bayesian") {
        if(beg >= end){
          res <- c(beg, beg + 0.1)
        } else {
          res <- c(beg, end)
        }
      } else {
        res <- c(0)
      }
    }
  }
}

##
# Prediction
##
PredictMe <- function(model, data, label = c(), job, 
                      model_name = all_models, tgt_map = data.frame(f1=character(0))){
  if(model_name == "decision_tree"){
    # -- decision tree
    # if rg, no probability
    # else, there is prob
    if(job == "bc" | job == "mc"){
      probs <- predict(model, data, type = "prob")
      pred_fac <- predict(model, data, type = "class")
      pred <- as.character(pred_fac)
      # if bc, calculate confusion matrix
      if(job == "bc"){
        cf <- caret::confusionMatrix(pred_fac, label)
      } else {
        cf <- data.frame(f1 = character(0))
      }
    } else if (job == "rg") {
      probs <- data.frame(f1 = character(0))
      pred <- predict(model, train_data, type = "vector")
      cf <- data.frame(f1 = character(0))
    } else {
      # do nothing
    }
  } else if(model_name == "naive_bayes"){
    # -- naive bayes
    # if rg, empty data.frame as naive_bayes cannot be used for regression
    # else, there is prob
    if(job == "bc" | job == "mc"){
      probs <- predict(model, data, type = "prob")
      pred_fac <- predict(model, data, type = "class")
      pred <- as.character(pred_fac)
      # if bc, calculate confusion matrix
      if(job == "bc"){
        cf <- caret::confusionMatrix(pred_fac, label)
      } else {
        cf <- data.frame(f1 = character(0))
      }
    } else if (job == "rg") {
      probs <- data.frame(f1 = character(0))
      pred <- data.frame(f1 = character(0))
      cf <- data.frame(f1 = character(0))
    } else {
      # do nothing
    }
  } else if(model_name == "regression"){
    # -- regression
    # if rg, no probability
    # else, there is prob
    if(job == "bc"){
      ##
      # manipulate probs matrix
      prob <- predict(model, data, type = "response")
      probs <- data.frame(x1 = 1 - prob, x2 = prob)
      colnames(probs) <- levels(label)
      
      ##
      # manipulate pred vector
      pred <- prob
      pred <- sapply(1:length(pred), function(i){ 
        if(pred[i] <= 0.5) levels(label)[1] else levels(label)[2]
      })
      pred_fac <- as.factor(pred)
      
      ##
      # confusion matrix
      cf <- caret::confusionMatrix(pred_fac, label)
    } else if (job == "mc") {
      probs <- predict(model, data, type = "raw")   # might need special treatment later
      pred_fac <- predict(model, data, type = "class")
      pred <- as.character(pred_fac)
    } else if (job == "rg") {
      probs <- data.frame(f1 = character(0))
      pred <- predict(model, train_data, type = "response")
      cf <- data.frame(f1 = character(0))
    } else {
      # do nothing
    }
  } else if(model_name == "ada_boost"){
    # -- ada boost
    # if rg, not available
    # else, there is prob
    if(job == "bc"){
      ##
      # computer probability and prediction
      probs <- predict(model, data, type = "prob")
      probs <- as.data.frame(probs)
      colnames(probs) <- levels(label)
      pred_fac <- predict(model, data, type = "vector")
      pred <- as.character(pred_fac)
      
      ##
      # confusion matrix
      cf <- caret::confusionMatrix(pred_fac, label)
    } else {
      print("Only bc is supported for Adapative Boosting!")
      probs <- data.frame(f1 = character(0))
      pred <- data.frame(f1 = character(0))
      cf <- data.frame(f1 = character(0))
    }
  } else if(model_name == "random_forest"){
    # -- random forest
    # if rg, predict class
    # else, there is prob
    if(job == "bc" | job == "mc"){
      probs <- predict(model, data, type = "prob")
      pred_fac <- predict(model, data, type = "response")
      pred <- as.character(pred_fac)
      # if bc, calculate confusion matrix
      if(job == "bc"){
        cf <- caret::confusionMatrix(pred_fac, label)
      } else {
        cf <- data.frame(f1 = character(0))
      }
    } else if (job == "rg") {
      probs <- data.frame(f1 = character(0))
      pred <- predict(model, data, type = "response")
      cf <- data.frame(f1 = character(0))
    } else {
      # do nothing
    }
  } else if(model_name == "gbm_h2o"){
    # -- gbm h20
    pred_all <- h2o.predict(model, as.h2o(data))
    pred_all <- as.data.frame(pred_all)
    if(job == "bc" | job == "mc"){
      probs <- pred_all[,-1]
      pred <- pred_all[,1]
      pred_fac <- as.factor(pred)
      # if bc, calculate confusion matrix
      if(job == "bc"){
        cf <- caret::confusionMatrix(pred_fac, label)
      } else {
        cf <- data.frame(f1 = character(0))
      }
    } else if (job == "rg") {
      probs <- data.frame(f1 = character(0))
      pred <- pred_all[,1]
      cf <- data.frame(f1 = character(0))
    } else {
      # do nothing
    }
  } else if(model_name == "xgbtree"){
    # -- xgbtree model
    ori_label <- label
    label <- SwapTargetType(label, tgt_map, "NumTarget", "StrTarget")
	  label <- as.factor(label)
    if(job == "bc"){
      ##
      # computer probability and prediction
      prob <- predict(model, data, reshape = FALSE)
      probs <- data.frame(x1 = 1 - prob, x2 = prob)
      colnames(probs) <- levels(label)
      
      ##
      # manipulate pred vector
      pred <- prob
      pred <- sapply(1:length(pred), function(i){ 
        if(pred[i] <= 0.5) levels(label)[1] else levels(label)[2]
      })
      pred_fac <- as.factor(pred)
      
      ##
      # confusion matrix
      cf <- caret::confusionMatrix(pred_fac, label)
    } else if (job == "mc") {
	    ##
      # computer probability and prediction
      probs <- predict(model, data, reshape = TRUE)
	    colnames(probs) <- levels(label)
	    pred <- sapply(1:nrow(probs), function(i){
        x <- probs[i,]
        levels(label)[which(x == max(x))]
      })
	    cf <- data.frame(f1 = character(0))
	} else if (job == "rg") {
      probs <- data.frame(f1 = character(0))
      pred <- predict(model, data, reshape = FALSE)
      cf <- data.frame(f1 = character(0))
    } else {
      # do nothing
    }
  } else if(model_name == "lightgbm"){
    # -- xgbtree model
    ori_label <- label
    label <- SwapTargetType(label, tgt_map, "NumTarget", "StrTarget")
    label <- as.factor(label)
    if(job == "bc"){
      ##
      # computer probability and prediction
      prob <- predict(model, data, reshape = FALSE)
      probs <- data.frame(x1 = 1 - prob, x2 = prob)
      colnames(probs) <- levels(label)
      
      ##
      # manipulate pred vector
      pred <- prob
      pred <- sapply(1:length(pred), function(i){ 
        if(pred[i] <= 0.5) levels(label)[1] else levels(label)[2]
      })
      pred_fac <- as.factor(pred)
      
      ##
      # confusion matrix
      cf <- caret::confusionMatrix(pred_fac, label)
    } else if (job == "mc") {
      ##
      # computer probability and prediction
      probs <- predict(model, data, reshape = TRUE)
      colnames(probs) <- levels(label)
      pred <- sapply(1:nrow(probs), function(i){
        x <- probs[i,]
        levels(label)[which(x == max(x))]
      })
      cf <- data.frame(f1 = character(0))
    } else if (job == "rg") {
      probs <- data.frame(f1 = character(0))
      pred <- predict(model, data, reshape = FALSE)
      cf <- data.frame(f1 = character(0))
    } else {
      # do nothing
    }
  } else if(model_name == "tensorflow"){
    # -- tensorflow model
    ori_label <- label
    label <- SwapTargetType(label, tgt_map, "NumTarget", "StrTarget")
    label <- as.factor(label)
    if(job == "bc"){
      ##
      # computer probability and prediction
      prob <- keras::predict_proba(model, data)
      probs <- data.frame(x1 = 1 - prob, x2 = prob)
      colnames(probs) <- levels(label)
      
      ##
      # manipulate pred vector
      pred <- keras::predict_classes(model, data)
      pred <- SwapTargetType(pred[,1], tgt_map, "NumTarget", "StrTarget")
      pred_fac <- as.factor(pred)
	  
	    ##
      # confusion matrix
      cf <- caret::confusionMatrix(pred_fac, label)
    } else if (job == "mc") {
	    ##
      # computer probability and prediction
      probs <- keras::predict_proba(model, data)
      colnames(probs) <- levels(label)
      
      ##
      # manipulate pred vector
      pred <- keras::predict_classes(model, data)
      pred <- SwapTargetType(pred[,1], tgt_map, "NumTarget", "StrTarget")
      pred_fac <- as.factor(pred)
	    cf <- data.frame(f1 = character(0))
	  } else if (job == "rg") {
      probs <- data.frame(f1 = character(0))
      pred <- keras::predict(model, data)
      cf <- data.frame(f1 = character(0))
    } else {
      # do nothing
    }
  } else {
    
  }

  if(job == "bc" | job == "mc") {
    # calc accurary
    accr_tmp <- sum(as.numeric(pred == label), na.rm = TRUE)/length(pred)
  } else {
    # calc RMSE
    accr_tmp <- sqrt(sum((pred - ori_label)^2)/length(ori_label))
  }
  res <- list(
    prob = probs,
    pred = pred,
    accr = accr_tmp,
    na_pred = sum(as.numeric(is.na(pred)))/length(pred),
    cf = cf$table
  )
  return(res)
}

##
# Swap target type
##
SwapTargetType <- function(tgt, tgt_map, from_nm, to_nm){
  if(is.vector(tgt)){
    # create a data.frame
    tgt_df <- data.frame(f1 = tgt, stringsAsFactors = FALSE)
    colnames(tgt_df) <- from_nm
    
    # join/merge
    tmp_tgt <- dplyr::inner_join(tgt_df, tgt_map, by = from_nm)
    new_tgt <- tmp_tgt[,to_nm]
  } else {
    print("Target is not swapped due to tgt is not a vector!")
    new_tgt <- tgt
  }
  return(new_tgt)
}

##
# Save prediction
##
SavePrediction <- function(df, output_dir, name, flag){
  if(flag){
    CreateDirIfNotExist(paste0(output_dir, "/Prediction"))
    write.csv(df, file = paste0(
      output_dir, "/Prediction/pred_", name, "_",
      format(Sys.Date(),"%Y%m%d"),"-",
      format(Sys.time(),"%H%M%S"),".csv"), row.names = FALSE)
  }
}

##
# Save model
##
SaveModel <- function(mdl, output_dir, name, flag){
  if(flag){
    CreateDirIfNotExist(paste0(output_dir, "/Model"))
    save(mdl, file = paste0(output_dir, "/Model/model_", name, "_",
                            format(Sys.Date(),"%Y%m%d"),"-",
                            format(Sys.time(),"%H%M%S"),".RData"))
  }
}

##
# Save results
##
SaveResults <- function(df, output_dir, name, flag){
  if(flag){
    CreateDirIfNotExist(paste0("Output/Text"))
    write.csv(df, file = paste0("Output/Text/tuning_", name, "_",
                                format(Sys.Date(),"%Y%m%d"),"-",
                                format(Sys.time(),"%H%M%S"),
                                ".csv"), row.names = FALSE) 
  }
}

##
# Change data type from non-numeric to integer
##
ToNumericOneCol <- function(col_nm, dataset){
  vals_l1 <- dataset[,col_nm]
  cls <- class(vals_l1)[1]
  
  if(cls == "character"){
    vals <- unique(vals_l1)
    
    lookup <- data.frame(
      key = vals,
      num_val = 1:length(vals),
      stringsAsFactors = FALSE
    )
    
    ori_val <- data.frame(
      key = vals_l1, 
      stringsAsFactors = FALSE
    )
    
    new_vals <- dplyr::inner_join(ori_val, lookup, by = "key")
    new_val <- new_vals$num_val
  } else {
     new_val <- vals_l1
  }
  
  return(as.data.frame(new_val, stringsAsFactors = FALSE))
}

ToNumeric <- function(col_nms, dataset){
  res <- lapply(col_nms, ToNumericOneCol, dataset)
  res_df <- dplyr::bind_cols(res)
  colnames(res_df) <- col_nms
  
  return(res_df)
}

##
# measure categorial data correlation
##
GKTau2 <- function(x,y){
  #
  #  First, compute the IxJ contingency table between x and y
  #
  Nij <- table(x,y,useNA = "ifany")
  #
  #  Next, convert this table into a joint probability estimate
  #
  PIij <- Nij/sum(Nij)
  #
  #  Compute the marginal probability estimates
  #
  PIiPlus = apply(PIij,MARGIN=1,sum)
  PIPlusj = apply(PIij,MARGIN=2,sum)
  #
  #  Compute the marginal variation of y
  #
  Vy <- 1 - sum(PIPlusj^2)
  #
  #  Compute the expected conditional variation of y given x
  #
  InnerSum <- apply(PIij^2,MARGIN=1,sum)
  VyBarx <- 1 - sum(InnerSum/PIiPlus)
  #
  #  Compute and return Goodman and Kruskal's tau measure
  #
  tau = (Vy - VyBarx)/Vy
  return(tau)
}

##
# GKTau table construction
##
GKTauMatrix <- function(col_nms, dataset){
  n <- length(col_nms)
  res <- matrix(rep(1, n*n), nrow = n)
  colnames(res) <- col_nms
  rownames(res) <- col_nms
  combs <- t(combn(col_nms, 2))
 
  for(i in 1:nrow(combs)){
    x <- combs[i,1]
    y <- combs[i,2]
    res[x, y] <- GKTau2(dataset[,x], dataset[,y])
    res[y, x] <- GKTau2(dataset[,y], dataset[,x])
  }
  
  return(res)
}

##
# Remove unneeded data and scale and ohe the data
##
FinalTouch <- function(dataset, rmv_fs){
  ##
  # Feature removal
  if(length(rmv_fs) == 0) prdctrs1_t <- dataset else prdctrs1_t <- dataset %>% select(-one_of(rmv_fs))
  
  ##
  # Scale and OHE
  prdctrs1_t_peek <- DataInspection(prdctrs1_t)
  num_cols <- prdctrs1_t_peek[prdctrs1_t_peek$class != "character","feature"]
  chr_cols <- prdctrs1_t_peek[prdctrs1_t_peek$class == "character","feature"]
  
  data_scaled_t <- DataScale(num_cols, prdctrs1_t, rep_na = TRUE, rep_na_with = 0)
  data_scaled_ohe_t <- OHE(chr_cols, data_scaled_t)
  prdctrs2_t_peek <- DataInspection(data_scaled_ohe_t)   # info only
  
  res <- list(
    peek1 = prdctrs1_t_peek,
    peek2 = prdctrs2_t_peek,
    coredata = data_scaled_ohe_t
  )
  
  return(res)
}

##
# Return an active connection to DB
##
ConnAccess <- function(db_path){
  conn_string <- paste("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=",db_path,sep="")
  conn <- RODBC::odbcDriverConnect(conn_string)
  return(conn)
}

##
# Read a table from access db
##
ReadDataFromADB <- function(db_path, tbl_name){
  conn <- ConnAccess(db_path)
  df <- RODBC::sqlFetch(conn, tbl_name, stringsAsFactors = FALSE)
  RODBC::odbcClose(conn)
  return(df)
}

##
# Write a table to access db
##
WriteDataToADB <- function(db_path, data, tbl_name){
  conn <- ConnAccess(db_path)
  df <- RODBC::sqlSave(conn, data, tablename = tbl_name, append = TRUE, rownames = FALSE,
                       colnames = FALSE)
  RODBC::odbcClose(conn)
  return(df)
}

##
# List all tables and queries
##
ListTblsFromADB <- function(db_path){
  conn <- ConnAccess(db_path)
  dfs_all <- RODBC::sqlTables(conn, tableType = "TABLE")
  dfs_tn <- dfs_all$TABLE_NAME
  RODBC::odbcClose(conn)
  return(unlist(dfs_tn))
}

##
# Return an active connection to SQL server
##
ConnSqlServer <- function(dsn_nm){
  conn <- DBI::dbConnect(drv = odbc::odbc(), dsn = dsn_nm)
  return(conn)
}

##
# Read a table from access db
##
ReadDataFromSS <- function(dsn_nm, tbl_name){
  conn <- ConnSqlServer(dsn_nm)
  df <- DBI::dbReadTable(conn, tbl_name)
  DBI::dbDisconnect(conn)
  return(df)
}

##
# Write a table to access db
##
WriteDataToSS <- function(dsn_nm, data, tbl_name){
  conn <- ConnSqlServer(dsn_nm)
  df <- DBI::dbWriteTable(conn, name = tbl_name, value = data,
                          append = FALSE, overwrite = TRUE, row.names = FALSE)
  DBI::dbDisconnect(conn)
  return(df)
}

##
# List all tables and queries
##
ListTblsFromSS <- function(dsn_nm){
  conn <- ConnSqlServer(dsn_nm)
  dfs_tn <- DBI::dbListTables(conn, schema = "dbo")
  DBI::dbDisconnect(conn)
  
  return(dfs_tn)
}

##
# Connect to SQL server using connection string
##
ConnSqlServerViaCS <- function(dbn){
  srv <- "192.168.2.120,3773"  # kmpka123.ddns.net
  id <- "dspeast2"
  pwd <- "yuheng"
  
  conn <- DBI::dbConnect(drv = odbc::odbc(),
                         Driver = "ODBC Driver 17 for SQL Server",
                         Server = srv,
                         Database = dbn,
                         UID = id,
                         PWD = pwd)
  return(conn)
}

##
# Read a table from sel server db
##
ReadDataFromSSviaCS <- function(dbn, tbl_name){
  conn <- ConnSqlServerViaCS(dbn)
  df <- DBI::dbReadTable(conn, tbl_name)
  DBI::dbDisconnect(conn)  
  return(df)
}

##
# Write a table to sql server db
##
WriteDataToSSviaCS <- function(dbn, data, tbl_name, apd = FALSE){
	conn <- ConnSqlServerViaCS(dbn)
  df <- DBI::dbWriteTable(conn, name = tbl_name, value = data,
                          append = apd, overwrite = !apd, row.names = FALSE)
	DBI::dbDisconnect(conn) 				  
  return(df)
}

##
# List all tables and queries
##
ListTblsFromSSviaCS <- function(dbn){
  conn <- ConnSqlServerViaCS(dbn)
  dfs_tn <- DBI::dbListTables(conn, scheme = "dbo")
  DBI::dbDisconnect(conn)
  return(dfs_tn)
}