# ShinyMLHome
Machine learning home shiny app

# Model addition
To add a new model, do the following steps:

### Preparation 
1. Add a parameters table with the format '* Input # : xxx Parameters *' to app.accdb
1. Copy and paste the .R files below
    + ./iUI/model_xxx_par.R
    + ./iUI (server)/model_xxx_grid.R
    + ./iUI (server)/model_xxx_bayesian.R (if applicable)
    + ./iUI/model_xxx_plot.R (if applicable)
    + ./Model/mxxx.R
2. Add ui file reference to main.R (ui)
3. Add server file reference to main.R (server)

### Update ui files
4. Update ./iUI/model_xxx_par.R
    + 