tp_model_randomforest_par <- tabPanel(
  "Parameters",
  fluidRow(
    column(
      width = 12,
      tags$div(class = "title_wrapper", tags$h5(class = "title_content_lg", " Parameters")),
      lapply(1:nrow(rf_pars), function(i){
        pnm <- paste0("mrfp_", rf_pars[i, "par"])
        fluidRow(
          column(
            width = 12,
            tags$div(
              class = "nm_fields",
              textInput(paste0(pnm, "_name"), label = "name", value = rf_pars[i, "par"])
            ),
            tags$div(
              class = "par_fields",
              numericInput(paste0(pnm, "_beg"), label = "begin", value = rf_pars[i, "default"])
            ),
            tags$div(
              class = "par_fields",
              numericInput(paste0(pnm, "_end"), label = "end", value = rf_pars[i, "default"])
            ), 
            tags$div(
              class = "par_fields",
              numericInput(paste0(pnm, "_inc"), label = "increment", value = 0)
            ),
            tags$div(
              class = "hint_fields",
              textInput(paste0(pnm, "_rng"), label = "range", value = rf_pars[i, "range"])
            ),
            tags$div(
              class = "hint_fields",
              textInput(paste0(pnm, "_hint"), label = "hint", value = rf_pars[i, "hint"])
            )
          )
        )
      })
    )
  )
)