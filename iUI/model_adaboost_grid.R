tp_model_adaboost_grid <- tabPanel(
  "Grid Search",
  fluidRow(
    column(
      tags$div(class = "title_wrapper", tags$h5(class = "title_content_lg", "Option")),
      width = tp_wid_nar,
      br(),
      ##
      # load universal parameters for all models
      ##
      lapply(1:nrow(unv_pars), function(i){
        radioButtons(paste0("mabg_", unv_pars[i, "par"]), unv_pars[i, "desc"], 
                     choices = c(unv_pars[i, "choice1"], unv_pars[i, "choice2"]), 
                     selected = unv_pars[i, "default"])
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