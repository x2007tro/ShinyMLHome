##
# Install package, if not
##
if(!("devtools" %in% rownames(installed.packages()))) { install.packages("devtools") }
library(devtools)
options(devtools.install.args = "--no-multiarch") # if you have 64-bit R only, you can skip this
install_github("Microsoft/LightGBM", subdir = "R-package")
