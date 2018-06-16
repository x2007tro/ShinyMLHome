tp_model_regression_train <- tabPanel(
  "Train",
  fluidRow(
    column(
      tags$h5("PARAMETER"),
      width = tp_wid_nar,
      numericInput("mrt_icpt", "Intercept (reg only)", value = 0, width = "120px"),
      radioButtons("mrt_save_res", "Save Results to CSV", choices = c("y", "n"), selected = "n"),
      radioButtons("mrt_save_pred", "Save Prediction", choices = c("y", "n"), selected = "n"),
      radioButtons("mrt_save_mod", "Save Model", choices = c("y", "n"), selected = "n"),
      actionButton("mrt_run", "Run"),
      textOutput("mrt_run_msg")
    ),
    column(
      tags$h5("RESULT"),
      width = 12 - tp_wid_nar,
      DT::dataTableOutput("mrt_accr"),
      br(),
      DT::dataTableOutput("mrt_cfmtx")
    )
  )
)