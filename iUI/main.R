##
# Source all ui files
##
ui_files <- c("data_upload", "data_select", "data_format",
              #"model_tensorflow_par", "model_tensorflow_grid", "model_tensorflow_bayesian", "model_tensorflow_test",
              #"model_xgbtree_par", "model_xgbtree_grid", "model_xgbtree_bayesian", "model_xgbtree_test",
              "model_regression_par", "model_regression_grid", 
              "model_navbay_par", "model_navbay_grid", "model_navbay_plot",
              "model_dectree_par", "model_dectree_grid", "model_dectree_plot",
              "model_adaboost_par", "model_adaboost_grid", "model_adaboost_bayesian", "model_adaboost_plot",
              "model_randomforest_par", "model_randomforest_grid", "model_randomforest_bayesian", "model_randomforest_plot",
              "model_gbmh2o_par", "model_gbmh2o_grid", "model_gbmh2o_bayesian", "model_gbmh2o_plot",
              "model_ensemble_data", "model_ensemble_weighted", "model_ensemble_vote", "model_ensemble_cycle",
              "util_png_viewer", "util_corr_viewer",
              "develop", "conf_gen", "conf_tensorflow", "conf_xgbtree")
lapply(ui_files, function(f){
  source(paste0(shiny_dir, "iUI/", f, ".R"), local = FALSE)
})

##
# Shiny ui
##
mainUI <- fluidPage(theme = shinythemes::shinytheme("united"),
  
  # css style
  tags$head(
    includeCSS("mlhome_style.css")
  ),
  
  navbarPage(
    "Machine Learning Home",
    ##
    # Data tab
    ##
    tabPanel(
      "Data",
      tabsetPanel(
        tp_data_upload,
        tp_data_select,
        tp_data_format
      )
    ),
    ##
    # Model tab
    ##
    tabPanel(
      "Model",
      navlistPanel(
        ##
        # Basic
        ##
        "Basic",
        widths = c(2, 10),
        # regression
        tabPanel(
          "Regression",
          tabsetPanel(
            tp_model_regression_par,
            tp_model_regression_grid
          )
        ),
        # Naive Bayes model
        tabPanel(
          "Naive Bayes (bc and mc)",
          tabsetPanel(
            tp_model_navbay_par,
            tp_model_navbay_grid,
            tp_model_navbay_plot
          )
        ),
        
        ##
        # Tree models
        ##
        "Tree",
        # Decision Tree
        tabPanel(
          "Decision Tree",
          tabsetPanel(
            tp_model_dectree_par,
            tp_model_dectree_grid,
            tp_model_dectree_plot
          )
        ),
        # Adaptive Boost
        tabPanel(
          "Adapative Boosting (bc only)",
          tabsetPanel(
            tp_model_adaboost_par,
            tp_model_adaboost_grid,
            tp_model_adaboost_bayesian,
            tp_model_adaboost_plot
          )
        ),
        # Random Forest
        tabPanel(
          "Random Forest",
          tabsetPanel(
            tp_model_randomforest_par,
            tp_model_randomforest_grid,
            tp_model_randomforest_bayesian,
            tp_model_randomforest_plot
          )
        ),
        # GBM H2O
        tabPanel(
          "H2O Gradient Boosting Tree",
          tabsetPanel(
            tp_model_gbmh2o_par,
            tp_model_gbmh2o_grid,
            tp_model_gbmh2o_bayesian,
            tp_model_gbmh2o_plot
          )
        ),
        # XbgTree model
        tabPanel(
          "Extreme Gradient Boosting Tree",
          tabsetPanel(
            #tp_model_xgbtree_par,
            #tp_model_xgbtree_grid,
            #tp_model_xgbtree_bayesian
          )
        ),
        
        ##
        # Neural Nets
        ##
        "Deep Learning",
        tabPanel(
          # Tensorflow model
          "Tensorflow",
          tabsetPanel(
            #tp_model_tensorflow_par,
            #tp_model_tensorflow_grid,
            #tp_model_tensorflow_bayesian
          )
        ),
        tabPanel(
          # NNets H2O
          "H2O NNets",
          tabsetPanel(
            
          )
        ),
        
        ##
        # Ensembled models
        ##
        "Ensemble",
        tabPanel(
          # Stacking
          "Stacking",
          tabsetPanel(
            tp_model_ensemble_data,
            tp_model_ensemble_weighted,
            tp_model_ensemble_vote,
            tp_model_ensemble_cycle
          )
        )
        # tabPanel(
        #   # Bagging
        #   "Bagging",
        #   tabsetPanel(
        #     # to be filled later
        #   )
        # )
      )
    ),
    ##
    # utility
    ##
    tabPanel(
      "Utility",
      tabsetPanel(
        tp_util_png_viewer,
        tp_util_corr_viewer
      )
    ),
    ##
    # Model development done and in progress
    ##
    tabPanel(
      "Development",
      div_develop
    ),
    ##
    # Model configuration
    ##
    tabPanel(
      "Configuration",
      tabsetPanel(
        tp_conf_gen,
        tp_conf_tensorflow,
        tp_conf_xgbtree
      )
    )
  )
  # End of navbarpage
)
