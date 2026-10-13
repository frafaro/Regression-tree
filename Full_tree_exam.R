
############################################################
# REGRESSION TREES, BAGGING AND RANDOM FORESTS
# Dataset: Carseats (ISLR2)
# Response variable: Sales
# Exam objective: The goal is to predict the Sales variable
# based on all the other information in the dataset
############################################################


# ==========================================================
# 0. DATA PREPARATION AND EXPLORATORY DATA ANALYSIS
# ==========================================================

# Install packages if necessary:
# install.packages(c("ISLR2", "rpart", "rpart.plot",
#                    "randomForest"))

library(ISLR2)
library(rpart)
library(rpart.plot)
library(randomForest)

data("Carseats")

# ==========================================================
# QUESTION 1: TRAIN-TEST SPLIT
# Divide the dataset into a training set (70% of the observations) and a test set.
# ==========================================================

# Set the random seed to reproduce the same split.
set.seed(123456)

n <- nrow(Carseats)

# Select 70% of the observations for training.
n_train <- round(0.70 * n)

# Randomly sample training observation indices.
train_indices <- sample(seq_len(n), size = n_train)

train <- Carseats[train_indices, ]
test  <- Carseats[-train_indices, ]

# Check sample sizes
nrow(train)
nrow(test)

# The split is performed BEFORE model fitting.
# The training set is used to estimate the models.
# The test set is reserved for final evaluation.

# Define a common evaluation function.
# All models will be evaluated using exactly the same metric.

mse <- function(actual, predicted) {
  mean((actual - predicted)^2)
}

rmse <- function(actual, predicted) {
  sqrt(mse(actual, predicted))
}


# ==========================================================
# QUESTION 2: REGRESSION TREE
# Estimate a regression tree based on the training set.
# Calculate the MSE of the predictions on the test set.
# ==========================================================

# A regression tree recursively partitions the predictor space.
# At each split, it searches for a predictor and threshold
# that minimize the residual sum of squares (RSS) in the
# resulting child nodes.

# A regression tree predicts the mean training response
# within the terminal leaf containing the new observation.

# Use a relatively small initial complexity parameter so that
# the tree can grow sufficiently before pruning.
# Cross-validation uses 10 folds.

tree <- rpart(
  Sales ~ .,
  data = train,
  method = "anova",
  ## this are the default control setting
  ## it is possible to stop at "method"
  control = rpart.control( 
    cp = 0.001,
    minsplit = 20,
    minbucket = 7,
    maxdepth = 30,
    xval = 10
  )
)

# Display the fitted tree structure
print(tree)

# Plot the regression tree.
# Each terminal node reports the predicted response,
# and the branches describe the sequence of decision rules.

rpart.plot(
  tree,
  type = 2,
  extra = 101,
  fallen.leaves = TRUE,
  main = "Regression Tree for Carseats Sales"
)

# Predict Sales for observations in the test set.
pred_tree <- predict(tree, newdata = test)

# Evaluate out-of-sample predictive performance.
mse_tree <- mse(test$Sales, pred_tree)
rmse_tree <- sqrt(mse_tree)

mse_tree #4.25
rmse_tree #2.06


# ==========================================================
# QUESTION 3: COST-COMPLEXITY PRUNING
# Prune the tree by selecting the cost/complexity parameter based on cross-validation. 
# Calculate the MSE again on the test set predictions and discuss the results.
# ==========================================================

# A large tree may fit the training data very closely but
# generalize poorly because it captures noise.

# Cost-complexity pruning balances fit against complexity:

# RSS(T) + alpha * |T|

# RSS(T) is the residual sum of squares of the subtree,
# |T| is its number of terminal nodes, and alpha penalizes
# tree size. In rpart, alpha corresponds to the cp parameter.

# Larger cp values favor smaller trees.

# plot the visual representation of the cross validation results
plotcp(tree) #cp = 0.025 best value to prune the tree

tree_pruned<-prune(tree, cp= 0.025)
print(tree_pruned)
# Plot the pruned tree.
rpart.plot(
  tree_pruned,
  type = 2,
  extra = 101,
  fallen.leaves = TRUE,
  main = "Pruned Regression Tree"
)

# Evaluate the pruned model on the SAME test set.
pred_pruned <- predict(tree_pruned, newdata = test)

mse_pruned <- mse(test$Sales, pred_pruned)
rmse_pruned <- sqrt(mse_pruned)

mse_pruned #4.986
rmse_pruned #2.23

# the mean squared error did not improved however we obtained the same result
# reducing the number of split, so the pruned tree is to prefer over the normal
# tree regression


# ==========================================================
# QUESTION 4: BAGGING
# Use the bagging method, calculating the MSE on the test set.
# Discuss the results and the importance of the regressors.
# ==========================================================

# Bagging = Bootstrap Aggregating.
#
# 1. Draw many bootstrap samples from the training set.
# 2. Fit a regression tree to each bootstrap sample.
# 3. Predict with every fitted tree.
# 4. Average their predictions.
#
# Bootstrap samples are generated by sampling training
# observations WITH replacement.
#
# In standard bagging, every predictor is eligible at every
# split. Therefore, mtry equals the number of predictors.

set.seed(1)

p <- ncol(train) - 1 #minus one since the prediction variable is as well a column of the train set

bagging <- randomForest(
  formula = Sales ~ .,
  data = train,
  ntree = 500,
  mtry = p,
  importance = TRUE
)

# Out-of-bag (OOB) error:
# Each bootstrap tree leaves out some training observations.
# Those observations can be predicted by trees that did not
# use them for fitting. Aggregating these predictions gives
# an internal estimate of generalization error.

#it is usefull to see if we used to many trees for the bagging process
#es: in this case also using  ntree = 200 should be good.

plot(
  bagging,
  main = "Bagging: Out-of-Bag Error vs Number of Trees"
)

# Predict on the held-out test set.
pred_bagging <- predict(bagging, newdata = test)

mse_bagging <- mse(test$Sales, pred_bagging)
rmse_bagging <- sqrt(mse_bagging)

mse_bagging #2.477
rmse_bagging #1.574

# For regression, %IncMSE measures the increase in OOB
# prediction error when a predictor is permuted.
# IncNodePurity summarizes reductions in residual sum
# of squares attributable to splits on that predictor.

varImpPlot(
  bagging,
  main = "Bagging: Predictor Importance"
)

# it is possible to say that the shelve location is the most important predictor

# ==========================================================
# QUESTION 5: RANDOM FOREST
# Use a random forest to predict Sales. Calculate the MSE on the test set 
# and discuss the importance of the variables.
# ==========================================================

# Random forests add predictor randomization to bagging.
#
# At each split, a random subset of mtry predictors is
# considered instead of allowing every predictor to compete.
#
# This can reduce the correlation between individual trees.
# Lower correlation can improve the variance reduction
# achieved by averaging many trees.

# Here we use sqrt(p) as a common illustrative choice.
# This is a heuristic, not a universal optimal value
# for regression. Tune mtry using training-only resampling
# when predictive performance is the main objective.

set.seed(1)

mtry_rf <- max(1, floor(sqrt(p)))

RF <- randomForest(
  Sales ~ .,
  data = train,
  ntree = 500,
  mtry = mtry_rf,
  importance = TRUE
)

# Inspect the internal OOB error.
plot(
  RF,
  main = "Random Forest: Out-of-Bag Error vs Number of Trees"
)

# Predict on the held-out test set.
pred_RF <- predict(RF, newdata = test)

mse_RF <- mse(test$Sales, pred_RF)
rmse_RF <- sqrt(mse_RF)

mse_RF #2.65
rmse_RF #1.62

# Examine variable importance.

varImpPlot(
  RF,
  main = "Random Forest: Predictor Importance"
)
# the mse is slowly higher than the bagging method, while the predictor importance show 
# the same hierarchy of importance

# ==========================================================
# QUESTION 6: THE ROLE OF mtry
# ==========================================================

# mtry is the number of randomly selected predictors
# considered as candidates at each split in the forest.
#
# If p is the total number of predictors:
#
# Bagging:       mtry = p
# Random forest: mtry < p, commonly a smaller subset
#
# A smaller mtry introduces more randomness and may reduce
# tree correlation, but it can also weaken individual trees.
# A larger mtry allows stronger splits but may make trees
# more correlated.
#
# The best value depends on the dataset and should ideally
# be selected using cross-validation on the training data.


# ==========================================================
# FINAL COMPARISON OF ALL MODELS:
# coompare the four models, introduce R2 and study the residual error
# ==========================================================

#introducing R2
# R2 is a complementary information of MSE, it does not have the same interpetation
# as for the linear regression.
# if R2 = 1 the prediction is perfect
# if R2 = 0 The model has the same mean squared error as the constant forecast, 
#           which is equal to the mean of the test set.
# id R2 < 0 the model is worse than the benchmark

# Calculate out-of-sample R-squared
r2_test <- function(actual, predicted) {
  1 - sum((actual - predicted)^2) /
    sum((actual - mean(actual))^2)
}

# Calculate test R-squared for each model
r2_tree <- r2_test(test$Sales, pred_tree)
r2_pruned <- r2_test(test$Sales, pred_pruned)
r2_bagging <- r2_test(test$Sales, pred_bagging)
r2_RF <- r2_test(test$Sales, pred_RF)

results <- data.frame(
  Model = c(
    "Regression tree",
    "Pruned tree",
    "Bagging",
    "Random forest"
  ),
  Test_MSE = c(
    mse_tree,
    mse_pruned,
    mse_bagging,
    mse_RF
  ),
  R2_test = c(
    r2_tree,
    r2_pruned,
    r2_bagging,
    r2_RF
  )
)

results$Test_RMSE <- sqrt(results$Test_MSE)

# Order models from lowest to highest test MSE.
results <- results[order(results$Test_MSE), ]
print(results, row.names = FALSE)

# Bar chart comparing test MSE.
barplot(
  results$Test_MSE,
  names.arg = results$Model,
  las = 2,
  col = "lightblue",
  main = "Out-of-Sample Predictive Performance",
  ylab = "Test MSE",
  cex.names = 0.8
)

# Predicted vs observed sales for each model.
# Points closer to the diagonal represent more accurate
# predictions. All panels use the same held-out observations.

predictions <- list(
  "Regression tree" = pred_tree,
  "Pruned tree" = pred_pruned,
  "Bagging" = pred_bagging,
  "Random forest" = pred_RF
)

par(mfrow = c(2, 2))

for (model_name in names(predictions)) {
  plot(
    test$Sales,
    predictions[[model_name]],
    xlab = "Observed Sales",
    ylab = "Predicted Sales",
    main = model_name,
    pch = 19,
    col = "steelblue"
  )
  
  abline(0, 1, lty = 2, col = "red", lwd = 2)
}

par(mfrow = c(1, 1))

# Residual diagnostic plot for the selected model.
# Residuals should ideally be centered around zero without
# systematic patterns or strongly changing variance.

best_model_name <- results$Model[1]

best_predictions <- switch(
  best_model_name,
  "Regression tree" = pred_tree,
  "Pruned tree" = pred_pruned,
  "Bagging" = pred_bagging,
  "Random forest" = pred_RF
)

residuals_best <- test$Sales - best_predictions

plot(
  best_predictions,
  residuals_best,
  xlab = "Predicted Sales",
  ylab = "Residuals",
  main = paste("Residual Diagnostics:", best_model_name),
  pch = 19,
  col = "steelblue"
)

abline(h = 0, lty = 2, col = "red", lwd = 2)
