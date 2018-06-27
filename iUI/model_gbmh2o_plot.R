tp_model_gbmh2o_plot <- tabPanel(
  "Plot",
  fluidRow(
    column(
      width = 12,
      tags$div(class = "title_wrapper", tags$h5(class = "title_content_lg", "Learning Curve")),
      uiOutput("mgbmpl_lc")
    )
  )
)