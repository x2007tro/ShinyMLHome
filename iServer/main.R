#
# Shiny server
#
mainServer <- function(input, output, session) {

  ##
  # Setup root directory if user selects different path
  ##
  source(paste0(shiny_dir, "iServer/root_dir.R"), local = TRUE)
  
  ##
  # Data upload tab
  ##
  source(paste0(shiny_dir, "iServer/data_upload.R"), local = TRUE)
  
  ##
  # Data selection tab
  ##
  source(paste0(shiny_dir, "iServer/data_select.R"), local = TRUE)
  
  ##
  # Data format tab
  ## 
  source(paste0(shiny_dir, "iServer/data_format.R"), local = TRUE)
  
  ##############################################################################################################
  # Regression model
  ##############################################################################################################
  
  ##
  # Model regression train
  ##
  source(paste0(shiny_dir, "iServer/model_regression_grid.R"), local = TRUE)
  
  ##############################################################################################################
  # Naive Bayes model
  ##############################################################################################################
  
  ##
  # Model regression train
  ##
  source(paste0(shiny_dir, "iServer/model_navbay_grid.R"), local = TRUE)
  
  ##############################################################################################################
  # Decision tree model
  ##############################################################################################################
  
  ##
  # Model regression train
  ##
  source(paste0(shiny_dir, "iServer/model_dectree_grid.R"), local = TRUE)
  
  ##############################################################################################################
  # ada boost model
  ##############################################################################################################
  
  ##
  # Adaboost grid Search
  ##
  source(paste0(shiny_dir, "iServer/model_adaboost_grid.R"), local = TRUE)
  
  ##
  # Adaboost bayesian Search
  ##
  source(paste0(shiny_dir, "iServer/model_adaboost_bayesian.R"), local = TRUE)
  
  ##############################################################################################################
  # random forest model
  ##############################################################################################################
  
  ##
  # random forest grid Search
  ##
  source(paste0(shiny_dir, "iServer/model_randomforest_grid.R"), local = TRUE)
  
  ##
  # random forest bayesian Search
  ##
  source(paste0(shiny_dir, "iServer/model_randomforest_bayesian.R"), local = TRUE)
  
  ##############################################################################################################
  # gbm h2o model
  ##############################################################################################################
  
  ##
  # GBM h2o grid Search
  ##
  source(paste0(shiny_dir, "iServer/model_gbmh2o_grid.R"), local = TRUE)
  
  ##
  # GBM h2o bayesian Search
  ##
  source(paste0(shiny_dir, "iServer/model_gbmh2o_bayesian.R"), local = TRUE)
  
  ##############################################################################################################
  # Tensorflow model
  ##############################################################################################################
  
  ##
  # Tensorflow grid Search
  ##
  source(paste0(shiny_dir, "iServer/model_tensorflow_grid.R"), local = TRUE)
  
  ##
  # Tensorflow bayesian Search
  ##
  source(paste0(shiny_dir, "iServer/model_tensorflow_bayesian.R"), local = TRUE)
  
  ##############################################################################################################
  # Xgboost tree model
  ##############################################################################################################
  
  ##
  # Xgbtree grid Search
  ##
  source(paste0(shiny_dir, "iServer/model_xgbtree_grid.R"), local = TRUE)
  
  ##
  # Xgbtree bayesian Search
  ##
  source(paste0(shiny_dir, "iServer/model_xgbtree_bayesian.R"), local = TRUE)
  
  ##############################################################################################################
  # Model ensemble
  ##############################################################################################################
  
  ##
  # Model ensemble data setup
  ##
  source(paste0(shiny_dir, "iServer/model_ensemble_data.R"), local = TRUE)
  
  ##
  # Model ensemble weighted
  ##
  source(paste0(shiny_dir, "iServer/model_ensemble_weighted.R"), local = TRUE)
  
  ##
  # Model ensemble majority vote
  ##
  source(paste0(shiny_dir, "iServer/model_ensemble_vote.R"), local = TRUE)
  
  ##
  # Model ensemble weighted
  ##
  source(paste0(shiny_dir, "iServer/model_ensemble_cycle.R"), local = TRUE)
  
  ##
  # util pdf viewer
  ##
  source(paste0(shiny_dir, "iServer/util_png_viewer.R"), local = TRUE)
  
  ##
  # util corr viewer
  ##
  source(paste0(shiny_dir, "iServer/util_corr_viewer.R"), local = TRUE)
}
