tp_model_xgbtree_test <- tabPanel(
  "Test",
  fluidRow(
    column(
      tags$h5("DATASET"),
      width = tp_wid_med,
      fileInput(
        "mxgbtt_upload", 
        NULL,
        multiple = FALSE,
        accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")
      ),
      uiOutput("mxgbtt_ex_cols_ui")
    ),
    column(
      tags$h5("RUN AND SAVE"),
      width = 12 - tp_wid_med,
      tags$div(
        style="padding:0px, margin:0px, height:50%",
        textInput("mxgbtt_nname", label = NULL, 
                  value = paste0(proj_dir, "Output/Prediction/xgbtree_test_pred.csv"), 
                  width = file_dir_field_width)
      ),
      tags$div(
        style="padding:0px, margin:0px, height:50%",
        actionButton("mxgbtt_run_save", "Save", width = blotter_field_default_width),
        textOutput("mxgbtt_save_message")
      )
    )
  )
)