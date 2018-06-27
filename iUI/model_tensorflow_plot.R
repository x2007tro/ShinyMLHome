tp_model_tensorflow_plot <- tabPanel(
  "Plot",
  fluidRow(
    column(
      width = 12,
      tags$div(class = "title_wrapper", tags$h5(class = "title_content_lg", "Tree")),
      uiOutput("mtfpl_tree")
    )
  ),
  fluidRow(
    column(
      width = 12,
      tags$div(class = "title_wrapper", tags$h5(class = "title_content_lg", "Learning Curve")),
      uiOutput("mtfpl_lc")
    )
  ),
  fluidRow(
    column(
      width = 12,
      tags$div(class = "title_wrapper", tags$h5(class = "title_content_lg", "Deepness")),
      uiOutput("mtfpl_dn")
    )
  )
)