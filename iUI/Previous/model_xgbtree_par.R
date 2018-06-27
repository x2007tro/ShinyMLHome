tp_model_xgbtree_par <- tabPanel(
  "Parameters",
  fluidRow(
    column(
      #tags$h5("Parameter"),
      br(),
      width = 12,
      lapply(1:length(xgbt_pars), function(i){
        pnm <- xgbt_pars[i]
        fluidRow(
          column(
            width = 12,
            tags$div(
              class = "nm_fields",
              textInput(paste0(pnm, "_name"), label = "name", value = pnm)
            ),
            tags$div(
              class = "par_fields",
              numericInput(paste0(pnm, "_beg"), label = "begin", value = xgbt_pars_def[i])
            ),
            tags$div(
              class = "par_fields",
              numericInput(paste0(pnm, "_end"), label = "end", value = xgbt_pars_def[i])
            ), 
            tags$div(
              class = "par_fields",
              numericInput(paste0(pnm, "_inc"), label = "increment", value = 0)
            ),
            tags$div(
              class = "hint_fields",
              textInput(paste0(pnm, "_rng"), label = "range", value = xgbt_pars_rng[i])
            ),
            tags$div(
              class = "hint_fields",
              textInput(paste0(pnm, "_hint"), label = "hint", value = xgbt_pars_hint[i])
            )
          )
        )
      })
    )
  )
)