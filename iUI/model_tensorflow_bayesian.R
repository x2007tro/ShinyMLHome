tp_model_tensorflow_bayesian <- tabPanel(
  "Training Bayesian Search",
  fluidRow(
    column(
      tags$h5("CONFIGURATION"),
      width = tp_wid_med,
      tags$div("Bayesian search specifications"),
      lapply(1:length(bs_pars), function(i){
        pnm <- bs_pars[i]
        fluidRow(
          column(
            width = 12,
            tags$div(
              class = "nm_fields",
              textInput(paste0(pnm, "_name"), label = "name", value = pnm)
            ),
            tags$div(
              class = "par_fields",
              numericInput(paste0(pnm, "_val"), label = "value", value = bs_pars_def[i])
            ),
            tags$div(
              class = "hint_fields",
              textInput(paste0(pnm, "_hint"), label = "hint", value = bs_pars_rng[i])
            )
          )
        )
      }),
      radioButtons("tf_bs_save_res", "Save Results to csv", choices = c("y", "n"), selected = "n"),
      radioButtons("tf_bs_save_pred", "Save Prediction", choices = c("y", "n"), selected = "n"),
      actionButton("tf_bs_search", "Search", width = blotter_field_default_width),
      textOutput("tf_bs_message")
    ),
    column(
      tags$h5("SEARCH RESULTS"),
      width = 12 - tp_wid_med,
      dataTableOutput("tf_bs_search_result"),
      tags$h5("OUTPUT LOCATION"),
      uiOutput("tf_bs_graph")
    )
  )
)