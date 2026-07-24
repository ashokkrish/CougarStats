## -----------------------------------------------------------------------------
knitr::opts_chunk$set(
  message = FALSE,
  digits = 3,
  collapse = TRUE,
  comment = "#>"
  )
options(digits = 3)
library(recipes)

## -----------------------------------------------------------------------------
library(recipes)

# make a copy for use below
iris <- iris |> mutate(original = Species)

iris_rec <- recipe( ~ ., data = iris)
summary(iris_rec)

## -----------------------------------------------------------------------------
ref_cell <- 
  iris_rec |> 
  step_dummy(Species) |>
  prep(training = iris)
summary(ref_cell)

# Get a row for each factor level
bake(ref_cell, new_data = NULL, original, starts_with("Species")) |> distinct()

## -----------------------------------------------------------------------------
# now make dummy variables with new parameterization
helmert <- 
  iris_rec |> 
  step_dummy(Species, contrasts = "contr.helmert") |>
  prep(training = iris)
summary(helmert)

bake(helmert, new_data = NULL, original, starts_with("Species")) |> distinct()

## -----------------------------------------------------------------------------
iris_int <- 
  iris_rec |>
  step_interact( ~ Sepal.Width:Sepal.Length) |>
  prep(training = iris)
summary(iris_int)

## -----------------------------------------------------------------------------
model.matrix(~ Species*Sepal.Length, data = iris) |> 
  as.data.frame() |> 
  # show a few specific rows
  slice(c(1, 51, 101)) |> 
  as.data.frame()

## -----------------------------------------------------------------------------
# # Must I do this?
# iris_rec |>
#   step_interact( ~ Species_versicolor:Sepal.Length +
#                    Species_virginica:Sepal.Length)

## -----------------------------------------------------------------------------
iris_int <- 
  iris_rec |> 
  step_dummy(Species) |>
  step_interact( ~ starts_with("Species"):Sepal.Length) |>
  prep(training = iris)
summary(iris_int)

## -----------------------------------------------------------------------------
# starts_with("Species")

## -----------------------------------------------------------------------------
# (Species_versicolor + Species_virginica)

## -----------------------------------------------------------------------------
iris_int

## -----------------------------------------------------------------------------
iris_int <- 
  iris_rec |> 
  step_interact( ~ Species:Sepal.Length) |>
  prep(training = iris)
summary(iris_int)

## -----------------------------------------------------------------------------
iris_rec |> 
  step_dummy(Species, one_hot = TRUE) |>
  prep(training = iris) |>
  bake(original, new_data = NULL, starts_with("Species")) |>
  distinct()

## -----------------------------------------------------------------------------
hot_reference <- 
  iris_rec |> 
  step_dummy(Species, one_hot = TRUE) |>
  prep(training = iris) |>
  bake(original, new_data = NULL, starts_with("Species")) |>
  distinct()

hot_reference

hot_helmert <- 
  iris_rec |> 
  step_dummy(Species, one_hot = TRUE, contrasts = "contr.helmert") |>
  prep(training = iris) |>
  bake(original, new_data = NULL, starts_with("Species")) |>
  distinct()

hot_helmert

