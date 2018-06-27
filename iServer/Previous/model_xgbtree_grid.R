##
# Xgbtree grid Search
##
observeEvent(input$xgbt_gs_search, {
  
  # Once grid search button is clicked, start searching procedure
  
  # step 1. create a grid search data.frame
  res <- lapply(1:length(xgbt_pars), function(i){
    pnm <- xgbt_pars[i]
    res <- CreateParRange("grid", input[[paste0(pnm, "_beg")]], input[[paste0(pnm, "_end")]], input[[paste0(pnm, "_inc")]])
    return(res)
  })
  names(res) <- xgbt_pars
  tuning_pars <- expand.grid(res)
  
  # step 2. run grid search
  withProgress(
    message = 'Grid search in progress. ',
    detail = 'This may take a while ...', value = 0, {
      tuning_res <- tryCatch({
        gr <- GridSearchXgbT2(
          proj = input$cgen_proj_name,
          dataset = as.matrix(dataset()) * 1.0,
          labels = as.numeric(targets()),
          job = input$cgen_job_type,
          val_size = input$cgen_val_size,
          cv_rep = input$cgen_cv_rep,
          xgbt_pars = tuning_pars,
          save_pred = ifelse(input$xgbt_gs_save_pred == "y", TRUE, FALSE),
          save_model = ifelse(input$xgbt_gs_save_mod == "y", TRUE, FALSE)
        )
        msg <- "grid search success!"
        list(gr, msg)
      },
      error=function(cond) {
        print("Here's the original error message:")
        print(cond)
        gr <- data.frame(result = paste0("grid search failed: ", cond))
        msg <- "grid search failed!"
        list(gr, msg)
      },
      # warning=function(cond) {
      #   msg <- "bayesian search failed!"
      #   gr <- data.frame(result = paste0("grid search failed: ", cond))
      # },
      finally={
        
      })
    })
  
  # step 3. if output text results
  if(input$xgbt_bs_save_res == "y"){
    CreateDirIfNotExist(paste0("Output/Text"))
    write.csv(tuning_res[[1]], file = paste0("Output/Text/xgbTree_grid_",
                                        format(Sys.Date(),"%Y%m%d"),"-",
                                        format(Sys.time(),"%H%M%S"),
                                        ".csv"), row.names = FALSE)
    
  }
  
  # step 4. output results to shiny
  output$xgbt_gs_search_result <- DT::renderDataTable({
    DT::datatable(
      tuning_res[[1]], 
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
  
  # step 5. output message
  output$xgbt_gs_message <- renderText({
    tuning_res[[2]]
  })
  
  # step 6. output Search graphs
  output$xgbt_gs_graph <- renderUI({
    list(
      textInput("xgbt_gs_graphx", NULL, value = paste0(input$cgen_root_dir, input$cgen_proj_name, "/Output/"),
                width = file_dir_field_width)
    )
  })
})