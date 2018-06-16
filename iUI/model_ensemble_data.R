##
# Data setup for ensemble model
##
tp_model_ensemble_data <- tabPanel(
  "Data",
  fluidRow(
    column(
      tags$h5("SETUP"),
      width = 12,
      column(
        width = 11,
        fileInput(
          "med_upload", 
          "Select prediction data",
          multiple = TRUE,
          accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")
        )
      ),
      column(
        width = 1
      )
    )
  ),
  fluidRow(
    column(
      tags$h5("PREVIEW"),
      width = 12,
      column(
        width = 6,
        DT::dataTableOutput("med_prob")
      ),
      column(
        width = 6,
        DT::dataTableOutput("med_pred")
      )
    )
  )
)