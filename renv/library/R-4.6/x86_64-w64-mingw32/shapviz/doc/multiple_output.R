## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  warning = FALSE,
  message = FALSE,
  fig.height = 5, 
  fig.width = 6,
  fig.align = "center"
)

## -----------------------------------------------------------------------------
library(shapviz)
library(ggplot2)
library(patchwork)
library(xgboost)

params <- list(objective = "multi:softprob", num_class = 3, nthread = 1)
X_pred <- data.matrix(iris[, -5])
dtrain <- xgb.DMatrix(X_pred, label = as.integer(iris[, 5]) - 1, nthread = 1)
fit <- xgb.train(params = params, data = dtrain, nrounds = 50)

# Create "mshapviz" object (logit scale)
(x <- shapviz(fit, X_pred = X_pred, X = iris))

# Contains "shapviz" objects for all classes
all.equal(x[[3]], shapviz(fit, X_pred = X_pred, X = iris, which_class = 3))

# Better names
names(x) <- levels(iris$Species)
x

# SHAP plots
sv_importance(x)
sv_importance(x, bar_type = "stack")  # Same but stacked

sv_dependence(x, v = "Petal.Length", share_y = TRUE) +
  plot_layout(ncol = 1)

## -----------------------------------------------------------------------------
library(shapviz)
library(ggplot2)
library(patchwork)
library(xgboost)

X_pred <- data.matrix(iris[, -1])
dtrain <- xgb.DMatrix(X_pred, label = iris[, 1], nthread = 1)
fit_xgb <- xgb.train(params = list(nthread = 1), data = dtrain, nrounds = 50)

# Create "mshapviz" object and split it into subgroups
shap_xgb <- shapviz(fit_xgb, X_pred = X_pred, X = iris)
x_subgroups <- split(shap_xgb, f = iris$Species)

# SHAP analysis
sv_importance(x_subgroups, bar_type = "stack")

sv_dependence(x_subgroups, v = "Petal.Length", share_y = TRUE) +
  plot_layout(ncol = 1) & 
  xlim(1, 7)

