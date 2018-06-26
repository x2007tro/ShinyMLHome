tp_model_randomforest_plot <- tabPanel(
  "Plot",
  fluidRow(
    column(
      width = 12,
      tags$div(class = "title_wrapper", tags$h5(class = "title_content_lg", "Learning Curve")),
      uiOutput("mrfpl_lc")
    )
  ),
  fluidRow(
    column(
      width = 12,
      tags$div(class = "title_wrapper", tags$h5(class = "title_content_lg", "Cross Val Feature Importance")),
      uiOutput("mrfpl_cv")
    )
  ),
  fluidRow(
    column(
      width = 12,
      tags$div(class = "title_wrapper", tags$h5(class = "title_content_lg", "Partial Dependence")),
      uiOutput("mrfpl_pd")
    )
  )
)