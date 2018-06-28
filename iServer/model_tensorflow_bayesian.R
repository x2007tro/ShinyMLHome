observeEvent(input$mtfb_run, {
  ##
  # first thing first
  mdl_nm <- "tensorflow"
  score_board_opt <- model_output_specs[model_output_specs$model == mdl_nm, "score_board"]
  conf_mtrx_opt <- model_output_specs[model_output_specs$model == mdl_nm, "conf_mtrx"]
  var_imp_opt <- model_output_specs[model_output_specs$model == mdl_nm, "var_imp"]
  tree_plot_opt <- model_output_specs[model_output_specs$model == mdl_nm, "tree_plot"]
  cp_table_opt <- model_output_specs[model_output_specs$model == mdl_nm, "cp_table"]
  lc_plot_opt <- model_output_specs[model_output_specs$model == mdl_nm, "learning_curve_plot"]
  tree_pick_ipt <- model_output_specs[model_output_specs$model == mdl_nm, "tree_pick_input"]
  
  # step 1. data formatting
  fmtd_data <- FormatData4Model(
    prdctrs = dataset()$predictors,
    tgt = dataset()$target, 
    tgt_map = dataset()$target_map,
    job = input$cgen_job_type,
    model = mdl_nm
  )
  
  # step 2.1 model specific parameters
  res <- lapply(1:nrow(tf_pars), function(i){
    pnm <- paste0("mtfp_", tf_pars[i, "par"])
    res <- CreateParRange("bayesian", input[[paste0(pnm, "_beg")]], input[[paste0(pnm, "_end")]], input[[paste0(pnm, "_inc")]])
  })
  names(res) <- tf_pars$par
  ##
  # this step differs between grid and bayesian search
  #
  # grid search needs data.frame while bayesian requires list to work
  tuning_pars <- res   
  
  # step 2.2 initial grid for bayesian
  ig <- lapply(1:length(tuning_pars), function(i){
    mean(tuning_pars[[i]])
  })
  names(ig) <- tf_pars$par
  
  # step 2.3 bayesian model parameters
  bayesian_pars <- lapply(1:nrow(bs_pars), function(i){
    res <- input[[paste0("mtfb_", bs_pars[i, "par"])]]
    ifelse(res == "y", TRUE, FALSE)
  })
  names(bayesian_pars) <- bs_pars$par
  bayesian_pars[["ini_grid"]] <- ig  # add initial grid to pars
  
  # step 3. universal model parameters
  static_pars <- lapply(1:nrow(unv_pars), function(i){
    res <- input[[paste0("mtfb_", unv_pars[i, "par"])]]
  })
  names(static_pars) <- unv_pars$par
  
  # step 4. run bayesian search
  withProgress(
    message = paste0(mdl_nm, " train in progress. "),
    detail = 'This may take a while ...', value = 0, {
      tuning_res <- tryCatch({
        br <- BayesianSearchTensorflow2(
          proj = input$cgen_proj_name,
          model_name = mdl_nm,
          dataset = fmtd_data$predictors,
          labels = fmtd_data$target,
          job = input$cgen_job_type,
          val_size = input$cgen_val_size,
          cv_rep = input$cgen_cv_rep,
          mdl_pars = tuning_pars,   # data.frame
          stc_pars = static_pars,    # list
          bs_pars = bayesian_pars    # list
        )
        msg <- paste0(mdl_nm, " run success!")
        list(br, msg)
      },
      error = function(cond) {
        print("Here's the original error message:")
        print(cond)
        msg <- paste0(mdl_nm, " run failed!")
        br <- data.frame(result = paste0(mdl_nm, " run failed: ", cond))
        list(br, msg)
      },
      # warning=function(cond) {
      #   msg <- paste0(mdl_nm, " run failed!")
      #   res <- data.frame(result = paste0(mdl_nm, " run failed: ", cond))
      #   list(res, msg)
      # },
      finally={
      })
    })
  
  # res structure
  # res = list(
  #   score_board (data.frame),
  #   models = list (
  #     parameter set 1 = list (model1, model2, model3, ...),
  #     parameter set 2 = list (model1, model2, model3, ...),
  #     ......
  #   ),
  #   train_results = list (
  #     parameter set 1 = list (
  #        cross val 1 = list (
  #          item 1 = probablity (data.frame),
  #          item 2 = prediction (data.frame),
  #          item 3 = accuracy (numeric),
  #          item 4 = prediction na (numeric),
  #          item 5 = confusion matrix (matrix)
  #        )
  #        cross val 2 = list (
  #          item 1 = probablity (data.frame),
  #          item 2 = prediction (data.frame),
  #          item 3 = accuracy (numeric),
  #          item 4 = prediction na (numeric),
  #          item 5 = confusion matrix (matrix)
  #        )        
  #     )
  #     parameter set 2 ......
  #     ......
  #   ),
  #   valdn_results same structure as train_results
  # )
  # 
  res <- tuning_res[[1]]
  msg <- tuning_res[[2]]
  
  # step 5. if output text results
  SaveResults(res, static_pars$output_dir, mdl_nm, static_pars$save_res)
  
  # step 6. if training run successfully, output
  if(msg != paste0(mdl_nm, " run failed!")){
    ##
    # output scoreboard
    ##
    output$mtfb_sb <- DT::renderDataTable({
      DT::datatable(
        res, 
        options = list(dom = "t"),
        rownames = FALSE
      )
    })
  }
  
  ##
  # step 5. output run message
  ##
  output$mtfb_run_msg <- renderText({
    msg
  })
  
})