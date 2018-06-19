##
# Data inspection
##
DataInspection <- function(dataset){
  res <- lapply(1:ncol(dataset), function(i){
    max_row <- nrow(dataset)
    subset <- dataset[,i]
    cls <- class(subset)[1]
    has_val <- min(length(subset[subset!=""]),
                   length(subset[!is.na(subset)]))
    no_val <- max_row - has_val
    vals <- paste0(unique(subset)[1:min(length(unique(subset)), 10)], collapse = ",")
    
    res <- data.frame(feature = colnames(dataset)[i],
                      class = cls,
                      has_value = has_val,
                      miss_value = no_val,
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
  vals <- unique(vals_l2)
  
  res <- matrix(0, nrow = nrow(dataset), ncol = length(vals))
  colnames(res) <- vals
  
  for(i in 1:nrow(dataset)){
      res[i, vals_l2[i]] <- 1
  }
  
  return(as.data.frame(res, stringsAsFactors = FALSE))
}

OHE <- function(col_nms, dataset, ex_col_nms = c()){
  if(length(ex_col_nms) == 0){
    rev_col_nms <- col_nms
  } else {
    rev_col_nms <- col_nms[!(col_nms %in% ex_col_nms)]
  }
  
  dataset_remain <- dataset[,!(colnames(dataset) %in% rev_col_nms)]
  res <- lapply(col_nms, OHEOneCol, dataset)
  res_all <- dplyr::bind_cols(res)
  dataset_new <- cbind(dataset_remain, res_all)
  
  return(as.matrix(dataset_new))
}

##
# DataScale
##
DataScaleOneCol <- function(col_nm, dataset, rep_na, rep_na_with){
  vals <- dataset[,col_nm]
  if(is.numeric(vals) | is.integer(vals)){
    if(rep_na) vals[is.na(vals)] <- rep_na_with
    res <- scale(vals, mean(vals, na.rm = TRUE), sd(vals, na.rm = TRUE))
  } else {
    res <- vals
    print(paste0("Warning: Feature ", col_nm, " is not scaled due to non-numeric format!"))
  }
  return(as.data.frame(res, stringsAsFactors = FALSE))
}

DataScale <- function(col_nms, dataset, rep_na = FALSE, rep_na_with = 0, ex_col_nms = c()){
  if(length(ex_col_nms) == 0){
    rev_col_nms <- col_nms
    res <- lapply(rev_col_nms, DataScaleOneCol, dataset, rep_na, rep_na_with)
    res_all <- dplyr::bind_cols(res)
    dataset_new <- res_all
    colnames(dataset_new) <- rev_col_nms
  } else {
    rev_col_nms <- col_nms[!(col_nms %in% ex_col_nms)]
    res <- lapply(rev_col_nms, DataScaleOneCol, dataset, rep_na, rep_na_with)
    res_all <- dplyr::bind_cols(res)
    dataset_new <- res_all
    colnames(dataset_new) <- rev_col_nms
    
    dataset_remain <- dataset[,!(colnames(dataset) %in% rev_col_nms)]
    dataset_new <- cbind(dataset_remain, dataset_new)
  }
  
  return(dataset_new)
}

##
# Format data based on the model
##
FormatData4Model <- function(dataset,
                             job = c("bc", "mc", "rg"),
                             model = c("regression",
                                       "naive_bayes",
                                       "decision_tree",
                                       "random_forest",
                                       "ada_boost",
                                       "gbm",
                                       "xgbtree",
                                       "tensorflow"),
                             target = ""){
  
  ds <- dataset
  mdl_nm <- match.arg(model)
  
  if(mdl_nm == "regression" | mdl_nm == "naive_bayes" | mdl_nm == "decision_tree" | 
     mdl_nm == "ada_boost" | mdl_nm == "random_forest" | mdl_nm == "gbm_h2o"){
    # Format charater to factor and everything numeric
    for(i in 1:ncol(ds)){
      tmp <- ds[,i]
      if(colnames(ds)[i] == target){
        if(job == "rg"){
          tmp_mod <- tmp
        } else {
          tmp_mod <- as.factor(tmp)
        }
      } else {  
        if(class(tmp)[1] == "character"){
          tmp_mod <- as.factor(tmp)
        } else if(class(tmp)[1] == "factor") {
          tmp_mod <- tmp
        } else {
          tmp_mod <- as.numeric(tmp)
        }
      }
      ds[,i] <- tmp_mod
    }
    res <- ds
  } else if (mdl_nm == "xgbtree" | mdl_nm == "tensorflow") {
    # do nothing
    res <- ds
  } else {
    # not specified
    res <- ds
  }

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
PredictMe <- function(model, data, label = c(), job, caret = FALSE){
  if(caret == TRUE){
    if(job == "bc"){
      probs <- predict(model, data, type = "prob")
      prob <- probs$fc_1
      pred <- as.numeric(prob > 0.5)
    } else if (job == "mc"){
      probs <- predict(model, data, type = "prob")
      pred <- sapply(1:nrow(probs), function(i){
        x <- probs[i,]
        which(x == max(x))
      })
    } else if (job == "reg"){
      prob <- -1
      pred <- predict(model, data)
    } else {
      # do nothing so far
    }
  } else {
    if(job == "bc"){
      prob <- predict(model, data, type = "response")
      pred <- as.numeric(prob > 0.5)
    } else if (job == "mc"){
      prob <- predict(model, data, type = "response")
      pred <- sapply(1:nrow(prob), function(i){
        x <- prob[i,]
        which(x == max(x))
      })
    } else if (job == "reg"){
      prob <- -1
      pred <- predict(model, data)
    } else {
      # do nothing so far
    }
  }
  
  if(length(label) == 0){
    res <- list(
      prob = prob,
      pred = pred,
      accr = 0
    )
  } else {
    res <- list(
      prob = prob,
      pred = pred,
      accr = mean(pred == label)
    )
  }
  
  return(res)
}

##
# Change data type from non-numeric to integer
##
ToNumericOneCol <- function(col_nm, dataset){
  vals_l1 <- dataset[,col_nm]
  cls <- class(vals_l1)[1]
  
  #if(cls == "character"){
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
  # } else {
  #   new_val <- vals_l1
  # }
  
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