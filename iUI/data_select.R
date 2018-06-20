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
                  value = "Predictors R**", 
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
      tags$h5("PREDICTORS"),
      width = tp_wid_wde,
      DT::dataTableOutput("dss_dts")
    )
  ),
  fluidRow(
    column(
      tags$h5("SPECS"),
      width = tp_wid_hlf,
      DT::dataTableOutput("dss_ovw")
    ),
    column(
      tags$h5("TARGET"),
      width = tp_wid_nar,
      DT::dataTableOutput("dss_lbs")
    )
  )
)