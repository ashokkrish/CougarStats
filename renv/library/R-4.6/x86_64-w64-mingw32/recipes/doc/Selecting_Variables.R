## -----------------------------------------------------------------------------
knitr::opts_chunk$set(
  message = FALSE,
  digits = 3,
  collapse = TRUE,
  comment = "#>",
  eval = requireNamespace("modeldata", quietly = TRUE)
  )
options(digits = 3)

## -----------------------------------------------------------------------------
library(recipes)
library(modeldata)

data("penguins")
str(penguins)

rec <- recipe(body_mass_g ~ ., data = penguins)
rec

## -----------------------------------------------------------------------------
summary(rec, original = TRUE)

## -----------------------------------------------------------------------------
summary(rec, original = TRUE)$type

## -----------------------------------------------------------------------------
dummied <- rec |> step_normalize(all_numeric())

## -----------------------------------------------------------------------------
dummied <- rec |> step_normalize(bill_length_mm, bill_depth_mm, 
                                  flipper_length_mm) # or
dummied <- rec |> step_normalize(all_numeric(), - body_mass_g) # or
dummied <- rec |> step_normalize(all_numeric_predictors()) # recommended

## -----------------------------------------------------------------------------
rec |>
  step_dummy(sex) |>
  prep() |>
  juice()

## -----------------------------------------------------------------------------
dummied <- prep(dummied, training = penguins)
with_dummy <- bake(dummied, new_data = penguins)
with_dummy

