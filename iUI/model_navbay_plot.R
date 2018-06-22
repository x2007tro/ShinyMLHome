tp_model_naybay_plot <- tabPanel(
  "Tree plot",
  fluidRow(
    column(
      width = 12,
      tags$div(class = "title_wrapper", tags$h5(class = "title_content_lg", " Rpart Tree")),
      uiOutput("mdtp_orig")
    )
  )
)