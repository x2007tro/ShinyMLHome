tp_model_adaboost_bayesian <- tabPanel(
  "Bayesian Search",
  fluidRow(
    column(
      tags$div(class = "title_wrapper", tags$h5(class = "title_content_lg", "Option")),
      width = tp_wid_nar,
      br(),
      ##
      # load bayesian parameters for all models
      ##
      lapply(1:nrow(bs_pars), function(i){
        numericInput(paste0("mabb_", bs_pars[i, "par"]), label = bs_pars[i, "desc"],
                     value = bs_pars[i, "default"])
      }),
      
      ##
      # load universal parameters for all models
      ##
      lapply(1:nrow(unv_pars), function(i){
        radioButtons(paste0("mabb_", unv_pars[i, "par"]), unv_pars[i, "desc"], 
                     choices = c(unv_pars[i, "choice1"], unv_pars[i, "choice2"]), 
                     selected = unv_pars[i, "default"])
      }),

      actionButton("mabb_run", "Run"),
      textOutput("mabb_run_msg")
    ),
    column(
      width = 12 - tp_wid_nar,
      tags$div(
        class = "frame_wrapper",
        tags$div(class = "title_wrapper", tags$h5(class = "title_content_lg", " Score Board")),
        tags$div(class = "content_wrapper", DT::dataTableOutput("mabb_sb"))
      )
    )
  )
)