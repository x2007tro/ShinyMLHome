# ShinyMLHome
Machine learning home shiny app

# Model addition
To add a new model, do the following steps:

### Preparation
1. Add a parameters table with the format * Input # : xxx Parameters * to app.accdb
    + Add relavant parameters to the table
2. Add ui file reference to main.R (ui)
    + The files don't exist yet, they will be created in step 6 and 7
3. Add server file reference to main.R (server)
    + The files don't exist yet, they will be created in step 6 and 7
4. Add code in ./global.R to read the table created above
5. Add code in ./global.R to load library and ./Model/mxxx.R
    + mxxx.R doesn't exist yet, it will be created in step 6 and 7
6. Copy and paste the .R files below
    + ./iUI/model_xxx_par.R
    + ./iUI (server)/model_xxx_grid.R
    + ./iUI (server)/model_xxx_bayesian.R (if applicable)
    + ./iUI/model_xxx_plot.R (if applicable)
    + ./Model/mxxx.R
7. Rename the above files to reflect the appropriate model name

### Update ui files
7. Update ./iUI/model_xxx_par.R
    + update the tab object name - tp_model_xxx_par
    + update the parameter variable name - xxx_pars
    + update the ui component name - mxxxp
8. Update ./iUI/model_xxx_grid.R
    + update the tab object name - tp_model_xxx_grid
    + update the ui component name - mxxxg
    + add/remove ui output depending on the model
9. Update ./iUI/model_xxx_bayesian.R (if applicable)
    + update the tab object name - tp_model_xxx_bayesian
    + update the ui component name - mxxxb
10. Update ./iUI/model_xxx_plot.R (if applicable)
    + update the ui component name - mxxxb
    + add/remove ui output depending on the model

### Update model file
10. Update ./Model/mxxx.R
    + replace the model part of the function names
    + update score_board related codes (two places) to include model specific parameters
    + update function CoreTrainxxx2 for specific model
    + update bayesian search for specic model
        + update input parameters for function BsOpUtil
        + update field names for data.frame mdl_pars_bayesian
        
11. Update ./Helper/ml_helper.R (It might be very easy to do, refer to package pdf)
    + update function PredictMe
    + update function FormatData4Model
    
### Update server file
12. Update ./iServer/model_xxx_grid.R
    + update model name parameter - mdl_nm
    + update the parameter variable name - xxx_pars
    + update the ui component name - mxxxg/p
    + replace GridSearchxxx2 with new model name
13. Update ./iServer/model_xxx_bayesian.R (if applicable)
    + update model name parameter - mdl_nm
    + update the parameter variable name - xxx_pars
    + update the ui component name - mxxxb/p
    + replace BayesianSearchxxx2 with new model name