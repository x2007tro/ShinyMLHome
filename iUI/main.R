##
# Source all ui files
##
ui_files <- c("style","data_upload", "data_select", "data_format",
              "model_tensorflow_par", "model_tensorflow_grid", "model_tensorflow_bayesian", "model_tensorflow_test",
              "model_xgbtree_par", "model_xgbtree_grid", "model_xgbtree_bayesian", "model_xgbtree_test",
              "model_regression_train", 
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
    style
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
        #tp_data_add,
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
        # Standalone models
        ##
        "Standalone",
        widths = c(2, 10),
        # regression
        tabPanel(
          "Regression",
          tabsetPanel(
            tp_model_regression_train
            #tp_model_regression_test
          )
        ),
        # XbgTree model
        tabPanel(
          "XgbTree",
          tabsetPanel(
            tp_model_xgbtree_par,
            tp_model_xgbtree_grid,
            tp_model_xgbtree_bayesian
            #tp_model_xgbtree_test
          )
        ),
        # Tensorflow model
        tabPanel(
          "Tensorflow",
          tabsetPanel(
            tp_model_tensorflow_par,
            tp_model_tensorflow_grid,
            tp_model_tensorflow_bayesian
            #tp_model_tensorflow_test
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
