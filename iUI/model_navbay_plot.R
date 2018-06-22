tp_model_navbay_plot <- tabPanel(
  "Plot",
  fluidRow(
    column(
      width = 12,
      tags$div(class = "title_wrapper", tags$h5(class = "title_content_lg", "Model Plot")),
      uiOutput("mnbp_orig")
    )
  )
)