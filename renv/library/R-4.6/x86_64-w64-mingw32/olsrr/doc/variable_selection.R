## ----echo=FALSE, message=FALSE------------------------------------------------
library(olsrr)
library(ggplot2)
library(gridExtra)
library(nortest)
library(goftest)

## ----allsub-------------------------------------------------------------------
model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
ols_step_all_possible(model)

## ----bestsub, size='tiny'-----------------------------------------------------
model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
ols_step_best_subset(model)

## ----model--------------------------------------------------------------------
model <- lm(y ~ ., data = surgical)
summary(model)

## ----stepf1-------------------------------------------------------------------
# stepwise forward regression
ols_step_forward_p(model)

## ----stepb--------------------------------------------------------------------
# stepwise backward regression
ols_step_backward_p(model)

## ----include_name-------------------------------------------------------------
ols_step_forward_p(model, include = c("age", "alc_mod"))

## ----include_index------------------------------------------------------------
ols_step_forward_p(model, include = c(5, 7))

## ----output-------------------------------------------------------------------
# adjusted r-square 
ols_step_forward_adj_r2(model)

## ----visualize----------------------------------------------------------------
# adjusted r-square 
k <- ols_step_forward_adj_r2(model)
plot(k)

## ----details------------------------------------------------------------------
# adjusted r-square 
ols_step_forward_adj_r2(model, details = TRUE)

## ----progress-----------------------------------------------------------------
# adjusted r-square 
ols_step_forward_adj_r2(model, progress = TRUE)

## ----hierarchical-------------------------------------------------------------
# hierarchical selection
m <- lm(y ~ bcs + alc_heavy + pindex + enzyme_test + liver_test + age + gender + alc_mod, data = surgical)
ols_step_forward_p(m, 0.1, hierarchical = TRUE)

