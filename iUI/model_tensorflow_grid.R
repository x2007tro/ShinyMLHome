tp_model_tensorflow_grid <- tabPanel(
  "Training Grid Search",
  fluidRow(
    column(
      tags$h5("CONFIGURATION"),
      width = tp_wid_nar,
      radioButtons("tf_gs_save_res", "Save Results to CSV", choices = c("y", "n"), selected = "n"),
      radioButtons("tf_gs_save_pred", "Save Prediction", choices = c("y", "n"), selected = "n"),
      radioButtons("tf_gs_save_mod", "Save Model", choices = c("y", "n"), selected = "n"),
      actionButton("tf_gs_search", "Search", width = blotter_field_default_width),
      textOutput("tf_gs_message")
    ),
    column(
      tags$h5("SEARCH RESULTS"),
      width = 12 - tp_wid_nar,
      dataTableOutput("tf_gs_search_result"),
      tags$h5("OUTPUT LOCATION"),
      uiOutput("tf_gs_graph")
    )
  )
)
