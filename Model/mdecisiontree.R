##
# Install tree package, if not
##
if(!("tree" %in% rownames(installed.packages()))) { install.packages("tree") }
library(tree)

##
# Select data and train the model
##
datasetp <- read.csv(file.choose(), row.names = TRUE, stringsAsFactors = FALSE)
datasetf <- FormatData4Model(datasetp, model = "decisiontree")

rps_var <- "Survived"
dep_vars <- colnames(datasetf)[!(colnames(datasetf) %in% rps_var)]
fmr <- as.formula(paste(rps_var,"~", paste(dep_vars, collapse="+")))

mdl <- tree::tree(formula = fmr, data = datasetf, split = "deviance")
mdl_prune <- tree::cv.tree(mdl, FUN = prune.tree)