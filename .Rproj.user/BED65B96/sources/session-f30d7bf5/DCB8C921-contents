# Regression Trees, Bagging and Random Forests in R

## Overview

This project compares four supervised-learning models for predicting
**`Sales`** in the `Carseats` dataset:

1.  **Regression tree** --- a single decision tree.
2.  **Pruned regression tree** --- a smaller tree obtained using
    cost-complexity pruning.
3.  **Bagging** --- an ensemble of bootstrap-sampled regression trees
    whose predictions are averaged.
4.  **Random forest** --- an ensemble that also randomizes the candidate
    predictors considered at each split.

The script is designed as a worked example for an exam or introductory
machine-learning exercise. It walks through train/test splitting, model
fitting, pruning, variable importance, out-of-sample evaluation, and
residual diagnostics.

## Research question

Can `Sales` be predicted from the other variables in `Carseats`, and how
does predictive performance change when moving from a single regression
tree to tree ensembles?

The response is `Sales`; all other columns are used as candidate
predictors through the formula `Sales ~ .`.

## Dataset

`Carseats` is supplied by the R package
[`ISLR2`](https://cran.r-project.org/package=ISLR2). The script loads it
directly, so a separate CSV file is not required.

The dataset contains store-level observations and variables describing
sales, prices, advertising, shelving location, competition, income, age,
and population. The model formula uses all predictors available in the
dataset except `Sales`.

## Requirements

-   R
-   The following R packages:
    -   `ISLR2`
    -   `rpart`
    -   `rpart.plot`
    -   `randomForest`

Install them once if they are not already available:

``` r
install.packages(c("ISLR2", "rpart", "rpart.plot", "randomForest"))
```

Load the script in RStudio or run it from an R session. The dataset is
loaded using `data("Carseats")`.

## Workflow

### 1. Train/test split

The script sets a random seed and assigns approximately 70% of
observations to the training set and the remaining 30% to the test set.

``` r
set.seed(123456)
n <- nrow(Carseats)
n_train <- round(0.70 * n)
train_indices <- sample(seq_len(n), size = n_train)

train <- Carseats[train_indices, ]
test  <- Carseats[-train_indices, ]
```

-   **Training set:** used to fit the models.
-   **Test set:** held out for the final evaluation.
-   **Seed:** makes the random split reproducible, assuming the same
    data and compatible R setup.

All four models are evaluated on the same test observations, which makes
their test metrics directly comparable. The test set should not be used
to choose tuning parameters.

The script defines reusable functions for mean squared error (MSE) and
root mean squared error (RMSE):

\[ `\mathrm{MSE}`{=tex} =
`\frac{1}{n}`{=tex}`\sum`{=tex}\_{i=1}^{n}(y_i-`\hat `{=tex}y_i)^2 \]

\[ `\mathrm{RMSE}`{=tex} = `\sqrt{\mathrm{MSE}}`{=tex} \]

MSE is expressed in squared `Sales` units, while RMSE is expressed in
the original `Sales` units. Lower values indicate more accurate
predictions on the evaluated sample.

### 2. Regression tree

The first model is fitted with `rpart()` using `method = "anova"`, which
specifies a regression tree for a continuous response.

``` r
tree <- rpart(
  Sales ~ .,
  data = train,
  method = "anova",
  control = rpart.control(
    cp = 0.001,
    minsplit = 20,
    minbucket = 7,
    maxdepth = 30,
    xval = 10
  )
)
```

A regression tree recursively splits the predictor space into regions.
At each split, it seeks a predictor and cutoff that reduce the residual
sum of squares (RSS) within the resulting nodes. A terminal node
predicts the mean `Sales` of the training observations that fall into
that node.

The main controls are:

  -----------------------------------------------------------------------
  Parameter                           Meaning
  ----------------------------------- -----------------------------------
  `cp = 0.001`                        Allows a relatively large candidate
                                      tree to grow before pruning
                                      decisions are made. Smaller values
                                      generally permit more splits.

  `minsplit = 20`                     Minimum number of observations in a
                                      node required for an attempted
                                      split.

  `minbucket = 7`                     Minimum number of observations
                                      allowed in a terminal node.

  `maxdepth = 30`                     Maximum depth of the tree.

  `xval = 10`                         Uses 10-fold cross-validation to
                                      estimate the relative error for
                                      candidate subtrees.
  -----------------------------------------------------------------------

The script prints the tree, visualizes it with `rpart.plot()`, predicts
`Sales` for the test set, and calculates test MSE and RMSE.

**How to read the tree:** each internal node shows a decision rule.
Follow the branch whose condition is satisfied until reaching a terminal
node. The value shown in a terminal node is the predicted response for
observations satisfying that path.

### 3. Cost-complexity pruning

A large tree can fit idiosyncrasies in the training data and generalize
poorly. Cost-complexity pruning compares model fit with tree size.
Conceptually, the criterion is:

\[ `\mathrm{RSS}`{=tex}(T) + `\alpha `{=tex}\|T\| \]

where (T) is a subtree, (`\mathrm{RSS}`{=tex}(T)) is its residual sum of
squares, (\|T\|) is its number of terminal nodes, and (`\alpha`{=tex})
penalizes complexity. In `rpart`, the related parameter is called `cp`.

The script plots cross-validation results with `plotcp(tree)` and prunes
the tree with `cp = 0.025`:

``` r
tree_pruned <- prune(tree, cp = 0.025)
```

The value `0.025` is the value used in this script; it is not
automatically selected by `plotcp()`. Inspect the cross-validation
plot/table to justify the choice. A reproducible alternative is to
choose a `cp` according to a clearly stated rule based on the
cross-validation results, using training data only.

The pruned tree is then plotted and evaluated on the same test set as
the original tree.

**Interpretation:** pruning is useful when it achieves a simpler model
with similar or better out-of-sample performance. If test MSE increases
slightly, the pruned model is not more accurate by that metric on this
particular split, although its simplicity may still be desirable. Avoid
claiming that pruning always improves performance.

### 4. Bagging

Bagging stands for **bootstrap aggregating**. It repeatedly draws
bootstrap samples from the training data, fits a tree to each sample,
and averages their predictions.

``` r
p <- ncol(train) - 1

bagging <- randomForest(
  Sales ~ .,
  data = train,
  ntree = 500,
  mtry = p,
  importance = TRUE
)
```

The `randomForest` package implements bagging for regression when `mtry`
equals the total number of predictors, `p`. Each tree is trained on a
bootstrap sample, and the ensemble prediction is the average of the tree
predictions.

-   `ntree = 500`: fits 500 trees.
-   `mtry = p`: all predictors are eligible at each split, as in
    standard bagging.
-   `importance = TRUE`: stores variable-importance measures.

The plotted out-of-bag (OOB) error provides an internal estimate of
prediction error. For each training observation, OOB predictions are
formed from trees whose bootstrap samples did not include that
observation. The script also evaluates the ensemble on the held-out test
set.

The `varImpPlot()` figure displays predictor importance. For regression
forests, common measures include:

-   **`%IncMSE`:** the increase in OOB prediction error after permuting
    a predictor. A larger increase suggests the model relies more on
    that predictor for predictive accuracy.
-   **`IncNodePurity`:** the total reduction in residual sum of squares
    attributed to splits using that predictor.

These are measures of model-based predictive importance, not proof of
causality. Correlated predictors can share or distort importance, and
importance rankings can differ by measure.

### 5. Random forest

Random forests add predictor randomization to bagging. At each split,
only a randomly selected subset of `mtry` predictors is considered. This
can reduce the correlation between individual trees, which can improve
the benefit of averaging.

``` r
mtry_rf <- max(1, floor(sqrt(p)))

RF <- randomForest(
  Sales ~ .,
  data = train,
  ntree = 500,
  mtry = mtry_rf,
  importance = TRUE
)
```

The script uses `floor(sqrt(p))` as an illustrative choice. This is a
heuristic, not a universal optimum for regression. The best `mtry`
depends on the dataset and should ideally be tuned using
cross-validation or resampling within the training set.

The script plots the OOB error, predicts the test observations, computes
MSE and RMSE, and displays variable importance.

**Bagging versus random forest:**

  -----------------------------------------------------------------------
  Feature                 Bagging                 Random forest
  ----------------------- ----------------------- -----------------------
  Bootstrap samples       Yes                     Yes

  Candidate predictors at All predictors          Random subset
  each split              (`mtry = p`)            (`mtry < p`, as used
                                                  here)

  Main source of          Bootstrap samples       Bootstrap samples and
  randomness                                      predictor subsampling

  Main motivation         Reduce variance by      Reduce variance and
                          averaging trees         tree correlation
                                                  through additional
                                                  randomization
  -----------------------------------------------------------------------

A random forest is not guaranteed to outperform bagging on every dataset
or every train/test split.

### 6. Role of `mtry`

Let (p) be the number of predictors.

-   Bagging uses `mtry = p`.
-   The random forest in this script uses `mtry = floor(sqrt(p))`,
    bounded below by 1.

A smaller `mtry` increases randomness and may reduce correlation between
trees, but it can also prevent a strong predictor from being considered
at a particular split. A larger `mtry` gives each split more candidate
predictors, potentially producing stronger individual trees but also
more similar trees. Performance depends on the balance and should be
checked using training-only resampling.

### 7. Final comparison and test-set (R\^2)

The script compares the four models using test MSE, test RMSE, and an
out-of-sample (R\^2):

\[ R\^2\_{`\mathrm{test}`{=tex}} = 1 -
`\frac{\sum_i (y_i-\hat y_i)^2}`{=tex} {`\sum`{=tex}*i
(y_i-`\bar `{=tex}y*{`\mathrm{test}`{=tex}})\^2} \]

Here, (`\bar `{=tex}y\_{`\mathrm{test}`{=tex}}) is the mean of the
observed `Sales` values in the test set.

Interpretation:

-   (R\^2\_{`\mathrm{test}`{=tex}} = 1): perfect predictions.
-   (R\^2\_{`\mathrm{test}`{=tex}} = 0): the model's squared-error total
    equals that of predicting the test-set mean for every observation.
-   (R\^2\_{`\mathrm{test}`{=tex}} \< 0): the model has a larger
    squared-error total than that mean-prediction benchmark.

This is an out-of-sample predictive (R\^2), not necessarily the same
quantity as the in-sample (R\^2) often reported for ordinary
least-squares regression. It can be negative.

The results table is sorted from lowest to highest test MSE. Since all
models use the same test observations, the denominator in the
test-(R\^2) formula is common to all models; therefore, lower test MSE
corresponds to higher test (R\^2).

The bar chart compares test MSE visually. The predicted-versus-observed
panels plot observed `Sales` on the horizontal axis and predictions on
the vertical axis. The dashed diagonal represents perfect prediction:
points closer to it have smaller prediction errors.

### 8. Residual diagnostics

For each test observation, the residual is:

\[ e_i = y_i - `\hat `{=tex}y_i \]

-   Positive residual: the model underpredicted `Sales`.
-   Negative residual: the model overpredicted `Sales`.
-   Residual equal to zero: the prediction was exact for that
    observation.

The script selects the model with the lowest test MSE and plots its
residuals against its predictions. Ideally, residuals are scattered
around zero without a clear systematic pattern or a strong change in
spread across fitted values.

Residuals are meaningful for tree models as well as linear models. A
regression tree does not need to be a linear regression to have
observed-minus-predicted errors. Because the tree predicts a constant
mean within each leaf, its predictions are piecewise constant; the
residuals measure how far individual observations are from those
predictions.

The diagnostic plot is a useful check, not a formal proof that the model
assumptions are satisfied. The selected model is selected using the test
MSE in this script, so the plotted test-set diagnostics are descriptive
rather than an independent final assessment. For a stricter workflow,
select/tune models using training-only resampling and reserve the test
set for the final report.

## Outputs produced by the script

When run from top to bottom, the script:

-   prints training and test sample sizes;
-   prints the fitted regression tree and pruned tree;
-   plots the full and pruned regression trees;
-   reports test MSE and RMSE for each model;
-   plots OOB error against the number of trees for bagging and random
    forest;
-   plots variable importance for both ensembles;
-   prints a comparison table with test MSE, test RMSE, and test (R\^2);
-   displays a bar chart of test MSE;
-   displays four predicted-versus-observed plots;
-   displays a residual plot for the model with the lowest test MSE.

Plots are displayed in the active R graphics device. The script does not
currently save them to image files or export the results table to disk.

## Example results and interpretation

The comments in the script provide approximate example test errors for
the chosen split and settings:

  Model               Example test MSE   Example test RMSE
  ----------------- ------------------ -------------------
  Regression tree                 4.25                2.06
  Pruned tree                    4.986                2.23
  Bagging                        2.477               1.574
  Random forest                   2.65                1.62

Treat these as reference values for the current script, not universal
results. Results can change if the split, tuning parameters, package
versions, or random seeds change. If the printed values differ, use the
values produced by your own run.

For the example above, bagging has the lowest test MSE among the four
fitted models. The random forest is close but has a slightly higher test
MSE on this split. The single tree has higher test error, while pruning
reduces complexity but increases test MSE in this example. The
appropriate conclusion is based on the actual computed results, not on a
general rule that one method must always win.

Variable-importance plots can help identify predictors that the fitted
ensembles use heavily. They do not establish that those variables cause
changes in `Sales`.

## Suggested project structure

``` text
project/
├── README.md
└── regression_trees.R
```

Save the R code in a file such as `regression_trees.R` and place this
README alongside it. The script loads `Carseats` from `ISLR2`, so no
local dataset file is required.

## Reproducibility and methodological notes

1.  **Keep the test set untouched during model selection.** The current
    script evaluates several models on the test set and then identifies
    the best model from those test results. This is useful for a
    teaching exercise, but repeated model selection against the same
    test set can make the reported performance optimistic. Prefer
    cross-validation on the training set for tuning and use the test set
    once for final evaluation.
2.  **The pruning choice is explicit.** `cp = 0.025` is hard-coded.
    Check the cross-validation results from `plotcp(tree)` and document
    the rule used to select the parameter.
3.  **Tune `mtry` if needed.** `sqrt(p)` is a starting point for this
    regression example, not a guarantee of optimal performance.
4.  **OOB error and test error are different.** OOB error is computed
    internally from the training data; test error is computed on the
    held-out test observations.
5.  **Importance is not causation.** Importance indicates predictive
    contribution according to the chosen measure and fitted forest.
6.  **Compare models on the same observations.** This script uses the
    same test set for every model, avoiding comparisons based on
    different test samples.

## Key concepts at a glance

  -----------------------------------------------------------------------
  Concept                             Main idea
  ----------------------------------- -----------------------------------
  Regression tree                     Predicts a response using recursive
                                      splits and terminal-node means.

  Pruning                             Trades some fit for a simpler tree
                                      using a complexity penalty.

  Bagging                             Averages bootstrap-trained trees to
                                      reduce prediction variance.

  Random forest                       Adds random predictor selection at
                                      each split to reduce tree
                                      correlation.

  `mtry`                              Number of candidate predictors
                                      considered at each split.

  OOB error                           Internal error estimate from
                                      training observations omitted from
                                      individual bootstrap samples.

  MSE / RMSE                          Quantify prediction error; lower is
                                      better on the same evaluation
                                      sample.

  Test (R\^2)                         Compares model squared error with a
                                      benchmark that predicts the
                                      test-set mean; may be negative.

  Variable importance                 Measures how strongly predictors
                                      contribute to forest predictions
                                      under a specific importance
                                      definition.

  Residual                            Observed response minus predicted
                                      response.
  -----------------------------------------------------------------------

## References

-   `ISLR2` package and datasets:
    https://cran.r-project.org/package=ISLR2
-   `rpart` package: https://cran.r-project.org/package=rpart
-   `rpart.plot` package: https://cran.r-project.org/package=rpart.plot
-   `randomForest` package:
    https://cran.r-project.org/package=randomForest
