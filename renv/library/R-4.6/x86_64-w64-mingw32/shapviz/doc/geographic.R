## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  warning = FALSE,
  message = FALSE,
  fig.height = 5, 
  fig.width = 8,
  fig.align = "center"
)

## -----------------------------------------------------------------------------
library(xgboost)
library(ggplot2)
library(shapviz)

miami <- miami |>
  transform(
    log_living = log(TOT_LVG_AREA),
    log_land = log(LND_SQFOOT),
    log_price = log(SALE_PRC)
  )

x_coord <- c("LATITUDE", "LONGITUDE")
x_nongeo <- c("log_living", "log_land", "structure_quality", "age")
xvars <- c(x_coord, x_nongeo)

# Select training data
set.seed(1)
ix <- sample(nrow(miami), 0.8 * nrow(miami))
train <- miami[ix, ]
X_train <- train[xvars]
y_train <- train$log_price

# Fit XGBoost model
params <- list(learning_rate = 0.2, nthread = 1)
dtrain <- xgb.DMatrix(data.matrix(X_train), label = y_train, nthread = 1)
fit <- xgb.train(params, dtrain, nrounds = 200)

## -----------------------------------------------------------------------------
X_explain <- X_train[1:2000, ]
sv <- shapviz(fit, X_pred = data.matrix(X_explain))
sv_dependence(
  sv, 
  v = c("log_living", "structure_quality", "LONGITUDE", "LATITUDE"), 
  alpha = 0.2
)

# And now the two-dimensional plot of the sum of SHAP values
sv_dependence2D(sv, x = "LONGITUDE", y = "LATITUDE") +
  coord_equal()

## -----------------------------------------------------------------------------
# Extend the feature set
more_geo <- c("CNTR_DIST", "OCEAN_DIST", "RAIL_DIST", "HWY_DIST")
xvars <- c(xvars, more_geo)
X_train <- train[xvars]
dtrain <- xgb.DMatrix(data.matrix(X_train), label = y_train, nthread = 1)

# Build interaction constraint vector and add it to params
ic <- c(
  list(which(xvars %in% c(x_coord, more_geo)) - 1),
  as.list(which(xvars %in% x_nongeo) - 1)
)
params$interaction_constraints <- ic

# Fit XGBoost model
fit <- xgb.train(params, dtrain, nrounds = 200)

# SHAP analysis
X_explain <- X_train[2:2000, ]
sv <- shapviz(fit, X_pred = data.matrix(X_explain))

# Two selected features: Thanks to additivity, structure_quality can be read as 
# Ceteris Paribus
sv_dependence(sv, v = c("structure_quality", "LONGITUDE"), alpha = 0.2)

# Total geographic effect (Ceteris Paribus thanks to additivity)
sv_dependence2D(sv, x = "LONGITUDE", y = "LATITUDE", add_vars = more_geo) +
  coord_equal()

