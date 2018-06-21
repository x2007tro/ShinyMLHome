tp_conf_gen <- tabPanel(
  "General",
  br(),
  fluidRow(
    column(
      width = 8,
      textInput("cgen_root_dir", "root dirctory", value = root_dir, width = "700px"),
      textInput("cgen_db_path", "access database", value = db_path, width = "700px"),
      textInput("cgen_db_tgt_src", "target table", value = db_target_src, width = "150px"),
      textInput("cgen_db_tgt_map", "target map", value = db_target_map, width = "150px"),
      textInput("cgen_proj_name", "project name", value = "Titanic", width = "150px"),
      selectInput("cgen_job_type", "job type", choices = jt, multiple = FALSE, selectize = TRUE, selected = jt[1], width = "150px"),
      numericInput("cgen_val_size", "validation dataset size", value = 100, width = "150px"),
      numericInput("cgen_cv_rep", "random validation rep", value = 4, width = "150px"),
      textInput("cgen_label_field", "label field", value = def_label, width = "150px")
    )
  )
)