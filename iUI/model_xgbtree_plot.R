tp_model_xgbtree_plot <- tabPanel(
  "Plot",
  fluidRow(
    column(
      width = 12,
      tags$div(class = "title_wrapper", tags$h5(class = "title_content_lg", "Tree")),
      uiOutput("mxgbtpl_tree")
    )
  ),
  fluidRow(
    column(
      width = 12,
      tags$div(class = "title_wrapper", tags$h5(class = "title_content_lg", "Learning Curve")),
      uiOutput("mxgbtpl_lc")
    )
  ),
  fluidRow(
    column(
      width = 12,
      tags$div(class = "title_wrapper", tags$h5(class = "title_content_lg", "Deepness")),
      uiOutput("mxgbtpl_dn")
    )
  )
)