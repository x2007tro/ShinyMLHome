observeEvent(input$mrt_run, {
  
  # step 2. run regression search
  withProgress(
    message = 'Regression train in progress. ',
    detail = 'This may take a while ...', value = 0, {
      tuning_res <- tryCatch({
        br <- CrossValReg2(
          proj = input$cgen_proj_name,
          dataset = dataset() * 1.0,
          labelsf = targetsf(),
          labels = targets(),
          job = input$cgen_job_type,
          val_size = input$cgen_val_size,
          cv_rep = input$cgen_cv_rep,
          intercept = input$mrt_icpt,
          save_pred = ifelse(input$mrt_save_pred == "y", TRUE, FALSE),
          save_model = ifelse(input$mrt_save_mod == "y", TRUE, FALSE)
        )
        msg <- "regression run success!"
        list(br, msg)
      },
      error=function(cond) {
        print("Here's the original error message:")
        print(cond)
        msg <- "regression run failed!"
        br <- data.frame(result = paste0("regression run failed: ", cond))
        list(br, msg)
      },
      # warning=function(cond) {
      #   msg <- "regression run failed!"
      #   res <- data.frame(result = paste0("regression run failed: ", cond))
      #   list(res, msg)
      # },
      finally={
      })
    })
  
  
  res <- tuning_res[[1]]
  msg <- tuning_res[[2]]
  
  # step 3. if output text results
  if(input$mrt_save_res == "y"){
    CreateDirIfNotExist(paste0("Output/Text"))
    write.csv(res$accr, file = paste0("Output/Text/regression_train_",
                                        format(Sys.Date(),"%Y%m%d"),"-",
                                        format(Sys.time(),"%H%M%S"),
                                        ".csv"), row.names = FALSE)
    
  }
  
  if(msg == "regression run failed!"){
    output$mrt_accr <- DT::renderDataTable({
      DT::datatable(
        res, 
        # options = list(
        #   pageLength = 10,
        #   orderClasses = TRUE,
        #   searching = TRUE,
        #   paging = TRUE,
        #   scrollX = 400,
        #   scrollY = 400,
        #   scrollCollapse = TRUE),
        options = list(dom = "t"),
        rownames = FALSE
      )
    })
  } else {
    # output
    output$mrt_accr <- DT::renderDataTable({
      DT::datatable(
        res$run_res, 
        # options = list(
        #   pageLength = 10,
        #   orderClasses = TRUE,
        #   searching = TRUE,
        #   paging = TRUE,
        #   scrollX = 400,
        #   scrollY = 400,
        #   scrollCollapse = TRUE),
        options = list(dom = "t"),
        rownames = FALSE
      )
    })
    
    output$mrt_cfmtx <- DT::renderDataTable({
      DT::datatable(
        res$val_cmtx, 
        # options = list(
        #   pageLength = 10,
        #   orderClasses = TRUE,
        #   searching = TRUE,
        #   paging = TRUE,
        #   scrollX = 400,
        #   scrollY = 400,
        #   scrollCollapse = TRUE),
        options = list(dom = "t"),
        rownames = FALSE
      )
    }) 
  }
  
  # step 5. output message
  output$mrt_run_msg <- renderText({
    msg
  })
  
})