##
# Feature selection display
##
output$dsf_ex_cols <- renderUI({
  specs <- dataset()$specs
  tb_scale <- specs[specs[,"class"] == "integer" | specs[,"class"] == "numeric","feature"]
  tb_ohe <- specs[specs[,"class"] == "character","feature"]
  list(
    selectInput("dsf_scale", label = "to be scaled", 
                choices = specs$feature,
                multiple = TRUE, selectize = TRUE,
                selected = tb_scale),
    selectInput("dsf_ohe", label = "to be OHE", 
                choices = specs$feature,
                multiple = TRUE, selectize = TRUE,
                selected = tb_ohe)
  )
})

##
# Format data upon request
##
observeEvent(input$dsf_format, {
  # format data
  fmt <- input$dsf_format_choice
  ds_fmtd <- dataset()$predictors
  ds_final <<- dataset()$predictors
  ds_peek <- dataset()$specs
  
  for(i in 1:length(fmt)){
    if(fmt[i] == format_options[1]){  # Scale
      ds_fmtd <- DataScale(input$dsf_scale, ds_fmtd, rep_na = TRUE, rep_na_with = 0)
    } else if (fmt[i] == format_options[2]){  # OneHotEncoding
      ds_fmtd <- OHE(input$dsf_ohe, ds_fmtd)
    } else (
      print("Error invalid format options")
    )
  }
  ds_fmtd_peek <- DataInspection(ds_fmtd)
  ds_final <<- ds_fmtd
  
  ##
  # Display data peek
  ##
  output$dsf_ovw <- DT::renderDataTable({
    DT::datatable(
      ds_fmtd_peek, 
      options = list(
        pageLength = 10,
        orderClasses = TRUE,
        searching = TRUE,
        paging = TRUE,
        scrollX = 400,
        scrollY = 400,
        scrollCollapse = TRUE),
      rownames = FALSE
    )
  })
  
  ##
  # Display detailed data
  ##
  output$dsf_dts <- DT::renderDataTable({
    DT::datatable(
      ds_fmtd, 
      options = list(
        pageLength = 10,
        orderClasses = TRUE,
        searching = TRUE,
        paging = TRUE,
        scrollX = 400,
        scrollY = 400,
        scrollCollapse = TRUE),
      rownames = TRUE
    )
  })
  
  ##
  # Display label data
  ##
  output$dsf_lbs <- DT::renderDataTable({
    DT::datatable(
      dataset()$target,
      options = list(
        pageLength = nrow(ds_fmtd_peek),
        orderClasses = FALSE,
        searching = FALSE,
        paging = TRUE,
        scrollX = 400,
        scrollY = 400,
        scrollCollapse = TRUE),
      rownames = FALSE
    )
  })
})

##
# Save data
##
observeEvent(input$dsf_save, {
  ff <- input$dsf_nname
  WriteDataToSSviaCS(input$cgen_db_path, ds_final, input$dss_nname)
  
  msg <- paste0("Data is saved at ",
                format(Sys.Date(),"%Y-%m-%d")," ",
                format(Sys.time(),"%H:%M:%S"))

  # display a message
  output$dsf_save_message <- renderText({ msg })
})