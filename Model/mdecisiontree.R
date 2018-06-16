##
# Install tree package, if not
##
if(!("tree" %in% rownames(installed.packages()))) { install.packages("tree") }
if(!("maptree" %in% rownames(installed.packages()))) { install.packages("maptree") }
if(!("rpart" %in% rownames(installed.packages()))) { install.packages("rpart") }
if(!("rpart.plot" %in% rownames(installed.packages()))) { install.packages("rpart.plot") }
library(rpart)
library(rpart.plot)
library(maptree)

##
# Select data and train the model
##
datasetp <- read.csv("C:/Users/kmin/OneDrive/Development/Data Science/Shiny/Titanic2/Dataset/train_prelim2.csv", 
                     header = TRUE, stringsAsFactors = FALSE)
datasetf <- FormatData4Model(datasetp, model = "decision_tree", target = "Survived")

rps_var <- "Survived"
dep_vars <- colnames(datasetf)[!(colnames(datasetf) %in% rps_var)]
fmr <- as.formula(paste(rps_var,"~", paste(dep_vars, collapse="+")))

mdl <- rpart::rpart(fmr, data = datasetf, method = "class", 
                    control = rpart::rpart.control(minsplit = 20, cp = 0.01, maxdepth = 30))
rpart::printcp(mdl)
plot(mdl)
text(mdl)


##
# Prune a tree
##
bestcp <- mdl$cptable[which.min(mdl$cptable[,"xerror"]),"CP"]

# Step3: Prune the tree using the best cp.
mdl_pruned <- prune(mdl, cp = bestcp)
rpart::printcp(mdl_pruned)
#plot(mdl_pruned)
#text(mdl_pruned, cex = 0.8, use.n = TRUE, xpd = TRUE)
rpart.plot::prp(mdl_pruned, faclen = 0, cex = 0.8, extra = 1)

only_count <- function(x, labs, digits, varlen)
{
  paste(x$frame$n)
}

boxcols <- c("pink", "palegreen3")[mdl_pruned$frame$yval]

par(xpd=TRUE)
prp(mdl_pruned, faclen = 0, cex = 0.8, node.fun=only_count, box.col = boxcols)
legend("bottomleft", legend = c("died","survived"), fill = c("pink", "palegreen3"),
       title = "Group")

# 
# mdl <- tree::tree(formula = fmr, data = datasetf, split = "deviance")
# x <- deviance(mdl, detail = FALSE)
# y <- misclass.tree(mdl, detail = TRUE)
# maptree::draw.tree(mdl, nodeinfo = FALSE)
# maptree::group.tree(mdl)
# 
# prd <- predict(mdl, datasetf, type = "vector")    # vector returns prob and class returns label
# 
# mdl_prune <- tree::cv.tree(mdl, FUN = prune.tree)
# prd <- predict(mdl_prune, datasetf, type = "vector")    # vector returns prob and class returns label
# mdl_prune <- tree::prune.tree(mdl, k = NULL, best = 1)
