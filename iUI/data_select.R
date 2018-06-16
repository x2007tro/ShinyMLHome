tp_data_select <- tabPanel(
  "Selection",
  fluidRow(
    column(
      tags$h5("FEATURES"),
      width = 4,
      uiOutput("dss_features")
    ),
    column(
      tags$h5("SAVE"),
      width = 8,
      tags$div(
        style="padding:0px, margin:0px, height:50%",
        textInput("dss_nname", label = NULL, 
                  value = paste0(proj_dir, "Dataset/train_prelim.csv"), 
                  width = file_dir_field_width)
      ),
      tags$div(
        style="padding:0px, margin:0px, height:50%",
        actionButton("dss_save", "Save", width = blotter_field_default_width),
        textOutput("dss_save_message")
      )
    )   
  ),
  br(), 
  fluidRow(
    column(
      tags$h5("OVERVIEW"),
      width = 3,
      DT::dataTableOutput("dss_ovw")
    ),
    column(
      tags$h5("DATASET"),
      width = 7,
      DT::dataTableOutput("dss_dts")
    ),
    column(
      tags$h5("LABELS"),
      width = 2,
      DT::dataTableOutput("dss_lbs")
    )
  )
)