## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  warning = FALSE,
  message = FALSE,
  fig.width = 5.5, 
  fig.height = 3.5,
  fig.align = "center"
)

## -----------------------------------------------------------------------------
library(shapviz)
library(ggplot2)
library(xgboost)

set.seed(1)

xvars <- c("log_carat", "cut", "color", "clarity")
X <- diamonds |> 
  transform(log_carat = log(carat)) |> 
  subset(select = xvars)
head(X)

# Fit (untuned) model
fit <- xgb.train(
  params = list(learning_rate = 0.1, nthread = 1), 
  data = xgb.DMatrix(data.matrix(X), label = log(diamonds$price), nthread = 1),
  nrounds = 65
)

# SHAP analysis: X can even contain factors
X_explain <- X[sample(nrow(X), 2000), ]
shp <- shapviz(fit, X_pred = data.matrix(X_explain), X = X_explain)

sv_importance(shp, show_numbers = TRUE)
sv_importance(shp, kind = "beeswarm")

## ----fig.width=8.5, fig.height=5.5--------------------------------------------
sv_dependence(shp, v = xvars, share_y = TRUE)  # patchwork object

## -----------------------------------------------------------------------------
sv_waterfall(shp, row_id = 1) +
  theme(axis.text = element_text(size = 11))

## ----fig.height=3-------------------------------------------------------------
sv_force(shp, row_id = 1)

## -----------------------------------------------------------------------------
sv_waterfall(shp, shp$X$color == "D") +
  theme(axis.text = element_text(size = 11))

## ----fig.width=8.5, fig.height=5.5--------------------------------------------
shp_i <- shapviz(
  fit, X_pred = data.matrix(X_explain), X = X_explain, interactions = TRUE
)
sv_dependence(shp_i, v = "log_carat", color_var = xvars, interactions = TRUE)
sv_interaction(shp_i)
sv_interaction(shp_i, kind = "bar")

