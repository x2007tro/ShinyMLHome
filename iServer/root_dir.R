##
# Setup root directory
##
observeEvent({
  input$cgen_root_dir
  input$cgen_proj_name
}, {
  tryCatch({
    setwd(paste0(input$cgen_root_dir, input$cgen_proj_name, "/"))  
  },
  error=function(cond) {
    print("Here's the original error message:")
    print(cond)
  },
  warning=function(cond) {
  },
  finally={
  })
})