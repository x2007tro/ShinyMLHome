##
# Tensorflow Bayesian Search
##
observeEvent(input$tf_bs_search, {
  
  # Once grid search button is clicked, start searching procedure
  
  # step 1. create a grid search data.frame
  res <- lapply(1:length(tf_pars), function(i){
    pnm <- tf_pars[i]
    res <- CreateParRange("bayesian", input[[paste0(pnm, "_beg")]], input[[paste0(pnm, "_end")]], input[[paste0(pnm, "_inc")]])
    return(res)
  })
  names(res) <- tf_pars
  tuning_pars <- res
  ig <- lapply(1:length(tuning_pars), function(i){
    mean(tuning_pars[[i]])
  })
  names(ig) <- tf_pars 
  
  # step 2. run Bayesian search
  withProgress(
    message = 'Bayesian search in progress. ',
    detail = 'This may take a while ...', value = 0, {
      tuning_res <- tryCatch({
        br <- BayesianSearchTF2(
          proj = input$cgen_proj_name,
          dataset = as.matrix(dataset()),
          labels = as.numeric(targets()),
          job = input$cgen_job_type,
          tf_sd = input$ctf_seed,   # 0 means no control over tf results
          val_size = input$cgen_val_size,
          cv_rep = input$cgen_cv_rep,
          layers_pars = tuning_pars,
          learning_rate = input$ctf_lr,
          batch_size = input$ctf_bs,
          bayes_ini_grid = ig,
          bayes_kappa = input[[paste0(bs_pars[1], "_val")]],
          bayes_eps = input[[paste0(bs_pars[2], "_val")]],
          bayes_nrounds = input[[paste0(bs_pars[3], "_val")]],
          save_pred = ifelse(input$tf_bs_save_pred == "y", TRUE, FALSE)
        )
        msg <- "bayesian search success!"
        list(br, msg)
      },
      error=function(cond) {
        print("Here's the original error message:")
        print(cond)
        msg <- "bayesian search failed!"
        br <- data.frame(result = paste0("bayesian search failed: ", cond))
        list(br, msg)
      },
      # warning=function(cond) {
      #   msg <- "bayesian search failed!"
      #   br <- data.frame(result = paste0("bayesian search failed: ", cond))
      # },
      finally={
      })
    })
  
  # step 3. if output text results
  if(input$tf_bs_save_res == "y"){
    CreateDirIfNotExist(paste0("Output/Text"))
    write.csv(tuning_res[[1]], file = paste0("Output/Text/tensorflow_bayesian_",
                                        format(Sys.Date(),"%Y%m%d"),"-",
                                        format(Sys.time(),"%H%M%S"),
                                        ".csv"), row.names = FALSE)
    
  }
  
  # step 4. output results to shiny
  output$tf_bs_search_result <- DT::renderDataTable({
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
  output$tf_bs_message <- renderText({
    tuning_res[[2]]
  })
  
  # step 6. output Search graphs
  output$tf_bs_graph <- renderUI({
    list(
      textInput("tf_bs_graphx", NULL, value = paste0(input$cgen_root_dir, input$cgen_proj_name, "/Output/"),
                width = file_dir_field_width)
    )
  })
})