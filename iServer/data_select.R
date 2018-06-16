##
# Feature selection display
##
output$dss_features <- renderUI({
  list(
    selectInput("dss_slt_fts", label = NULL, 
                choices = peek()$feature,
                multiple = TRUE, selectize = TRUE, 
                selected = peek()$feature),
    actionButton("dss_select", "Select", width = blotter_field_default_width)
  )
})

##
# Featuer selection action
##
observeEvent(input$dss_select, {
  # select target fields
  old_ds <- data()
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
    ifelse(input$cgen_label_field == "", 
           new_ds_4d <- new_ds,
           new_ds_4d <- new_ds[,!(colnames(new_ds) %in% input$cgen_label_field)])
    DT::datatable(
      new_ds_4d, 
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
    lb_4d <- data.frame(labels = targets()[1:10])
    colnames(lb_4d) <- input$cgen_label_field
    DT::datatable(
      lb_4d, 
      options = list(
        pageLength = 10,
        orderClasses = FALSE,
        searching = TRUE,
        paging = FALSE),
      rownames = FALSE
    )
  })
})

##
# Save data
##
observeEvent(input$dss_save, {
  ff <- input$dss_nname
  fdir <- dirname(ff)
  CreateDirIfNotExist(fdir)
  
  if(substr(ff,(nchar(ff)+1)-3,nchar(ff)) == "csv"){
    req(new_ds)
    write.csv(new_ds, file = ff, row.names = FALSE)
    msg <- paste0("Data is saved at ",
                  format(Sys.Date(),"%Y-%m-%d")," ",
                  format(Sys.time(),"%H:%M:%S"))
  } else {
    msg <- "Data is not saved!"
  }
  
  # display a message
  output$dss_save_message <- renderText({ msg })
})