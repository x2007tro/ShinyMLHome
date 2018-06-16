##
# Load liabs
##
lib <- c("shiny","DT","ggplot2", "shinythemes")
lapply(lib, function(x){library(x, character.only = TRUE)})

##
# Source server and ui components
##
source("./global.R", local = FALSE)
source(paste0(shiny_dir, "iUI/main.R"))
source(paste0(shiny_dir, "iServer/main.R"))

##
# Launch shiny app
##
shinyApp(
  ui = mainUI,
  server = mainServer
)