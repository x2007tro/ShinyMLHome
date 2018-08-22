require(lightgbm)
require(methods)

# We load in the agaricus dataset
# In this example, we are aiming to predict whether a mushroom is edible
data(agaricus.train, package = "lightgbm")
data(agaricus.test, package = "lightgbm")
train <- agaricus.train
test <- agaricus.test

# The loaded data is stored in sparseMatrix, and label is a numeric vector in {0,1}
class(train$label)
class(train$data)

#--------------------Advanced features ---------------------------
# To use advanced features, we need to put data in lgb.Dataset
dtrain <- lgb.Dataset(data = train$data, label = train$label, free_raw_data = FALSE)
dtest <- lgb.Dataset(data = test$data, label = test$label, free_raw_data = FALSE)

#--------------------Using validation set-------------------------
# valids is a list of lgb.Dataset, each of them is tagged with name
valids <- list(train = dtrain, test = dtest)

# To train with valids, use lgb.train, which contains more advanced features
# valids allows us to monitor the evaluation result on all data in the list
print("Train lightgbm using lgb.train with valids")
bst <- lgb.train(data = dtrain,
                 num_leaves = 4,
                 learning_rate = 1,
                 nrounds = 10,
                 valids = valids,
                 nthread = 2,
                 objective = "binary",
                 verbose = 1)


# information can be extracted from lgb.Dataset using getinfo
label = getinfo(dtest, "label")
pred <- predict(bst, test$data)
err <- as.numeric(sum(as.integer(pred > 0.5) != label)) / length(label)
print(paste("test-error=", err))