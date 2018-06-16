##
# Feature selection display
##
output$dsf_ex_cols <- renderUI({
  list(
    selectInput("dsf_no_scale", label = "Numerical but no Scale", 
                choices = peek()$feature,
                multiple = TRUE, selectize = TRUE, 
                selected = input$cgen_label_field),
    selectInput("dsf_no_ohe", label = "Char but no OHE", 
                choices = peek()$feature,
                multiple = TRUE, selectize = TRUE, 
                selected = input$cgen_label_field)
  )
})

##
# Format data upon request
##
observeEvent(input$dsf_format, {
  # format data
  fmt <- input$dsf_format_choice
  ds_fmtd <- data()
  ds_final <<- data()
  ds_peek <- peek()
  
  for(i in 1:length(fmt)){
    if(fmt[i] == format_options[1]){  # Scale
      if(length(input$dsf_no_scale) == 0) {
        ds_fmtd <- DataScale(ds_peek$feature, ds_fmtd, rep_na = TRUE, rep_na_with = 0)
      } else {
        ds_fmtd <- DataScale(ds_peek$feature, ds_fmtd, rep_na = TRUE, rep_na_with = 0, ex_col_nms = input$dsf_no_scale)
      }
    } else if (fmt[i] == format_options[2]){  # OneHotEncoding
      if(length(input$dsf_no_ohe) == 0) {
        ds_fmtd <- OHE(ds_peek[ds_peek[,"class"] == "character","feature"], ds_fmtd)
      } else {
        ds_fmtd <- OHE(ds_peek[ds_peek[,"class"] == "character","feature"], ds_fmtd, ex_col_nms = input$dsf_no_ohe)
      }
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
    ifelse(input$cgen_label_field == "", 
           ds_fmtd_4d <- ds_fmtd,
           ds_fmtd_4d <- ds_fmtd[,!(colnames(ds_fmtd) %in% input$cgen_label_field)])
    DT::datatable(
      ds_fmtd_4d, 
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
  output$dsf_lbs <- DT::renderDataTable({
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
observeEvent(input$dsf_save, {
  ff <- input$dsf_nname
  fdir <- dirname(ff)
  CreateDirIfNotExist(fdir)
  
  if(substr(ff,(nchar(ff)+1)-3,nchar(ff)) == "csv"){
    write.csv(ds_final, file = ff, row.names = FALSE)
    msg <- paste0("Data is saved at ",
                  format(Sys.Date(),"%Y-%m-%d")," ",
                  format(Sys.time(),"%H:%M:%S"))
  } else {
    msg <- "Data is not saved!"
  }
  
  # display a message
  output$dsf_save_message <- renderText({ msg })
})