tp_model_adaboost_plot <- tabPanel(
  "Plot",
  fluidRow(
    column(
      width = 12,
      uiOutput("mabp_tree_pick")
    )
  ),
  fluidRow(
    column(
      width = 12,
      tags$div(class = "title_wrapper", tags$h5(class = "title_content_lg", "Tree")),
      uiOutput("mabp_tree")
    )
  ),
  fluidRow(
    column(
      width = 12,
      tags$div(class = "title_wrapper", tags$h5(class = "title_content_lg", "Learning Curve")),
      uiOutput("mabp_lc")
    )
  )
)