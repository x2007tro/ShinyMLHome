tp_model_dectree_train <- tabPanel(
  "Train",
  fluidRow(
    column(
      tags$h5("PARAMETER"),
      width = tp_wid_nar,

      ##
      # load universal parameters for all models
      ##
      lapply(1:nrow(unv_pars), function(i){
        radioButtons(paste0("mdtt_", unv_pars[i, "par"]), unv_pars[i, "desc"], 
                     choices = c(unv_pars[i, "choice1"], unv_pars[i, "choice2"]), 
                     selected = unv_pars[i, "default"])
      }),

      actionButton("mdtt_run", "Run"),
      textOutput("mdtt_run_msg")
    ),
    column(
      tags$h5("RESULT"),
      width = 12 - tp_wid_nar,
      DT::dataTableOutput("mdtt_sb"),
      br(),
      tags$h5("CONFUSION MATRIX"),
      uiOutput("mdtt_cfmtx"),
      br(),
      tags$h5("VARIABLE IMPORTANCE"),
      uiOutput("mdtt_varimp"),
      br(),
      tags$h5("CP TABLE"),
      uiOutput("mdtt_cpt")
    )
  )
)