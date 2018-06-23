tp_model_adaboost_bayesian <- tabPanel(
  "Bayesian Search",
  fluidRow(
    column(
      tags$div(class = "title_wrapper", tags$h5(class = "title_content_lg", "Option")),
      width = tp_wid_nar,
      br(),
      ##
      # load universal parameters for all models
      ##
      lapply(1:nrow(bs_pars), function(i){
        numericInput(paste0("mabg_", bs_pars[i, "par"]), label = bs_pars[i, "desc"],
                     value = bs_pars[i, "default"])
      }),

      actionButton("mabg_run", "Run"),
      textOutput("mabg_run_msg")
    ),
    column(
      width = 12 - tp_wid_nar,
      tags$div(
        class = "frame_wrapper",
        tags$div(class = "title_wrapper", tags$h5(class = "title_content_lg", " Score Board")),
        tags$div(class = "content_wrapper", DT::dataTableOutput("mabg_sb"))
      ),
      br(),
      tags$div(
        class = "frame_wrapper",
        tags$div(class = "title_wrapper", tags$h5(class = "title_content_lg", " Confusion Matrix")),
        tags$div(class = "content_wrapper", uiOutput("mabg_cfmtx"))
      ),
      br(),
      tags$div(
        class = "frame_wrapper",
        tags$div(class = "title_wrapper", tags$h5(class = "title_content_lg", " Variable Importance")),
        tags$div(class = "content_wrapper", uiOutput("mabg_varimp"))
      )
    )
  )
)