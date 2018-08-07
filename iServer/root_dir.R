##
# Setup root directory
##
observeEvent({
  input$cgen_root_dir
  input$cgen_proj_name
}, {
  tryCatch({
    setwd(proj_dir)  
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