## ----include=FALSE------------------------------------------------------------
knitr::opts_chunk$set(fig.width = 7, fig.height = 5, fig.retina = 2)
library(latex2exp)
library(reactable)
library(purrr)
library(dplyr)
library(ggplot2)
library(tibble)

## ----eval=FALSE---------------------------------------------------------------
# # Use raw strings, no need to escape backslashes.
# TeX(r"(\textbf{Euler's identity} is $e^{i\pi} + 1 = 0$.)")

## ----eval=FALSE---------------------------------------------------------------
# # Equivalent to the previous code fragment.
# # Use regular strings, but escape the backslashes.
# TeX("\\textbf{Euler's identity} is $e^{i\\pi} + 1 = 0$.")

## ----plot-formula, fig.height=2, fig.width=5----------------------------------
plot(TeX(r'(A $\LaTeX$ formula: $\frac{2hc^2}{\lambda^5}\frac{1}{e^{\frac{hc}{\lambda k_B T}} - 1}$)'), cex = 2, main = "")

## ----base-plot, warning=FALSE-------------------------------------------------
x <- seq(0, 4, length.out = 100)
alpha <- 1:5

plot(x, xlim = c(0, 4), ylim = c(0, 10), 
     xlab = 'x', ylab = TeX(r'($\alpha  x^\alpha$, where $\alpha \in \{1 \ldots 5\}$)'), 
     type = 'n', main = TeX(r'(Using $\LaTeX$ for plotting in base graphics!)', bold = TRUE))

for (a in alpha) {
  lines(x, a*x^a, col = a)
}

legend('topleft', 
       legend = TeX(sprintf(r'($\alpha = %d$)', alpha)), 
       lwd = 1, 
       col = alpha)

## ----ggplot, warning=FALSE----------------------------------------------------
x <- seq(0, 4, length.out = 100)
alpha <- 1:5
data <- map_df(alpha, ~ tibble(v = .*x^., x = x, alpha = .))

p <- ggplot(data, aes(x = x, y = v, color = as.factor(alpha))) +
    geom_line() + 
    ylab(TeX(r'($\alpha  x^\alpha$, where $\alpha \in 1\ldots 5$)')) +
    ggtitle(TeX(r'(Using $\LaTeX$ for plotting in ggplot2. I $\heartsuit$ ggplot!)')) +
    coord_cartesian(ylim = c(-1, 10)) +
    guides(color = guide_legend(title = NULL)) +
    scale_color_discrete(labels = lapply(sprintf(r'($\alpha = %d$)', alpha), TeX)) 
    # Note that ggplot2 legend labels must be lists of expressions, not vectors of expressions

print(p)

## ----examples, fig.width=7, fig.height=6--------------------------------------
latex2exp_examples(cex = 0.9)

