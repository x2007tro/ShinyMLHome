tp_model_ensemble_cycle <- tabPanel(
  "Cycle",
  fluidRow(
    column(
      width = 6,
      tags$h5("STEP 1. Save data."),
      textInput("mec_save_to", label = "Save to", 
                value = paste0(proj_dir, "Dataset/lowlevelprediction1.csv"), 
                width = file_dir_field_width),
      actionButton("mec_save", "Save", width = blotter_field_default_width),
      textOutput("mec_save_message"),
      br(),
      tags$h5("STEPA 2. Run standalone model with saved data.")
    ),
    column(
      width = 6,
      tags$h5("PREVIEW"),
      DT::dataTableOutput("mec_preview")
    )
  )
  
)