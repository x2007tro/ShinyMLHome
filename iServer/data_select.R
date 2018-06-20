##
# Feature selection display
##
output$dss_features <- renderUI({
  list(
    selectInput("dss_slt_fts", label = NULL, 
                choices = dataset()$specs$feature,
                multiple = TRUE, selectize = TRUE, 
                selected = dataset()$specs$feature),
    actionButton("dss_select", "Select", width = blotter_field_default_width)
  )
})

##
# Featuer selection action
##
observeEvent(input$dss_select, {
  # select target fields
  old_ds <- dataset()$predictors
  new_ds <<- old_ds[,input$dss_slt_fts]
  new_ds_peek <- DataInspection(new_ds)
  
  ##
  # Display data peek
  ##
  output$dss_ovw <- DT::renderDataTable({
    DT::datatable(
      new_ds_peek, 
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
  output$dss_dts <- DT::renderDataTable({
    DT::datatable(
      new_ds, 
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
  # Display label data
  ##
  output$dss_lbs <- DT::renderDataTable({
    DT::datatable(
      dataset()$target,
      options = list(
        pageLength = nrow(new_ds_peek),
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
observeEvent(input$dss_save, {
  ff <- input$dss_nname
  WriteDataToADB(input$cgen_db_path, new_ds, input$dss_nname)
  
  msg <- paste0("Data is saved at ",
                format(Sys.Date(),"%Y-%m-%d")," ",
                format(Sys.time(),"%H:%M:%S"))
  
  # display a message
  output$dss_save_message <- renderText({ msg })
})