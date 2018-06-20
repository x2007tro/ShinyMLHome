tp_data_format <- tabPanel(
  "Format",
  fluidRow(
    column(
      tags$h5("FEATURES"),
      width = 4,
      selectInput("dsf_format_choice", label = "Options", choices = format_options,
                  multiple = TRUE, selectize = TRUE, selected = format_options),
      uiOutput("dsf_ex_cols"),
      actionButton("dsf_format", "Format", width = blotter_field_default_width)
    ),
    column(
      tags$h5("SAVE"),
      width = 8,
      tags$div(
        style="padding:0px, margin:0px, height:50%",
        textInput("dsf_nname", label = NULL, 
                  value = "Predictors R**", 
                  width = file_dir_field_width)
      ),
      tags$div(
        style="padding:0px, margin:0px, height:50%",
        actionButton("dsf_save", "Save", width = blotter_field_default_width),
        textOutput("dsf_save_message")
      )
    )   
  ),
  br(),
  fluidRow(
    column(
      tags$h5("PREDICTORS"),
      width = tp_wid_wde,
      DT::dataTableOutput("dsf_dts")
    ) 
  ),
  fluidRow(
    column(
      tags$h5("SPECS"),
      width = tp_wid_hlf,
      DT::dataTableOutput("dsf_ovw")
    ),
    column(
      tags$h5("TARGET"),
      width = tp_wid_nar,
      DT::dataTableOutput("dsf_lbs")
    )
  )
)