# ShinyMLHome
Machine learning home shiny app

# Model addition
To add a new model, do the following steps:

### Preparation 
1. Add a parameters table with the format * Input # : xxx Parameters * to app.accdb
    + Add relavant parameters to the table
2. Copy and paste the .R files below
    + ./iUI/model_xxx_par.R
    + ./iUI (server)/model_xxx_grid.R
    + ./iUI (server)/model_xxx_bayesian.R (if applicable)
    + ./iUI/model_xxx_plot.R (if applicable)
    + ./Model/mxxx.R
3. Add ui file reference to main.R (ui)
4. Add server file reference to main.R (server)
5. Add code in ./global.R to read the table created above
6, Add code in ./global.R to load library and ./Model/mxxx.R

### Update ui files
7. Update ./iUI/model_xxx_par.R
    + change parameters variable name
8. Update ./iUI/model_xxx_grid.R
    + change ui component ID
    + add/remove ui output depending on the model
9. Update ./iUI/model_xxx_plot.R (if applicable)
    + change ui component ID
    + add/remove ui output depending on the model

### Update model file
10. Update ./Model/mxxx.R
    + replace the model part of the function names
    + update score_board related codes (two places) to include model specific parameters
    + update function CoreTrainNavBay2 for specific model
11. Update ./Helper/ml_helper.R (It might be very easy to do, refer to package pdf)
    + update function PredictMe
    + update function FormatData4Model
    
### Update server file
12. Update ./iServer/model_xxx_grid.R
    + update ui component ID
    + update parameter variable
    + replace GridSearchxxx2 with new model name
    + update ui component code that is for specific model