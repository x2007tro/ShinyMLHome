tp_model_ensemble_weighted <- tabPanel(
  "Weighted",
  fluidRow(
    column(
      width = tp_wid_nar,
      tags$h5("PARAMETER"),
      uiOutput("mew_par_selector"),
      actionButton("mew_run", "Run", width = blotter_field_default_width)
    ),
    column(
      width = 12 - tp_wid_nar,
      column(
        width = 6,
        tags$h5("PREDICTION DETAILS"),
        DT::dataTableOutput("mew_new_pred")
      ),
      column(
        width = 6,
        tags$h5("TRAIN RESULTS"),
        textOutput("mew_tr_accr"),
        DT::dataTableOutput("mew_tr_cfmtx"),
        br(),
        tags$h5("VALIDATION RESULTS"),
        textOutput("mew_val_accr"),
        DT::dataTableOutput("mew_val_cfmtx")
      )
    )
  )
)