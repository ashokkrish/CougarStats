## install.packages("remotes")
## remotes::install_github("deepanshu88/shinyDarkmode")
## remotes::install_github('goodekat/ggResidpanel')
## remotes::install_github("rsquaredacademy/olsrr")

## options(conflicts.policy = TRUE)
## library(conflicted)

## Only packages whose functions are called without pkg:: somewhere in the app
## are attached (in their original order, so name masking is unchanged).
## Packages the app calls as pkg::fun() (DescTools, GGally, ggpubr, haven, olsrr
## and others) are loaded on first use; they stay installed by the Dockerfile.
library(bslib)
library(dplyr)
library(DT)
library(generics)
library(ggplot2)
library(htmltools)
## magrittr is not attached: the app's only magrittr function, %>%, is the copy
## that dplyr, plotly and tibble re-export (a re-export was found first before too).
library(plotly)
library(reactable)
library(readr)
library(readxl)
library(shiny)
library(shinyjs)
library(shinyMatrix)
library(shinyvalidate)
library(shinyWidgets)
## sortable (and its sortable::enable_modules() call) was dropped: its only
## user, the drag-and-drop rank_list() in an MLR encoding UI that was never
## shown, is gone. It stays in the Dockerfile.
library(thematic)
library(tibble)

# shinyDarkmode, ggResidpanel and olsrr have been removed/archived from CRAN. 
# So install.packages() silently skips it (no error during the Docker build), 

# but then it fails at runtime when your app tries to library() it.

# As a consequence in our Dockerfile, I have removed 'ggResidpanel', from the 
# install.packages(c(...)) # block and added a one line to the GitHub installs
# section at the bottom of the RUN step

library(shinyDarkmode)

margin <- ggplot2::margin

## The R/ files are loaded by Shiny itself: it sources every R/*.R file
## (alphabetically) after this file, so they are not source()d here.

## MathJax: request the version that https://mathjax.rstudio.com/latest/ redirects
## to (the same files), so the script and each file it loads afterwards (config,
## output jax, fonts) no longer cost an extra redirect round trip.
options(shiny.mathjax.url = "https://mathjax.rstudio.com/2.7.9/MathJax.js")

options(scipen = 999) # options(scipen = 0)
## options(shiny.reactlog = TRUE)

## How many digits to round Critical Values
cvDigits <- 3

# wrap this in HTML() function to output the message
# i.e HTML(uploadDataDisclaimer)
uploadDataDisclaimer <- "<small style='color:#999; display:block; margin-bottom:4px;'>
              <em><b>Note:</b> CougarStats does not store, log, or share any data you upload. 
              All uploaded files exist only for the duration of your session and are permanently deleted when the session ends.
              </em></small>"

boxplotDisclaimer <-   helpText("* Note: Quartiles are calculated by excluding the median on both sides.")

render <- "
{
  option: function(data, escape){return '<div class=\"option\">'+data.label+'</div>';},
  item: function(data, escape){return '<div class=\"item\">'+data.label+'</div>';}
}"

shiny::addResourcePath("www", "www")

## NOTE: advanced understanding of R is required to interpret these results.
## It's not for the faint of heart.
## warning("What follows is the base R conflicts() report: all MASK-ed or MASK-ing symbols are given.",
##         immediate. = TRUE)
## print(conflicts(detail = TRUE))

## NOTE: see #41.
## warning("Following this is the conflicted::conflict_scout() report.",
##         immediate. = TRUE)
## print(conflicted::conflict_scout())

## TODO: reenable this line before deployment.
## conflicted::conflicts_prefer(shinyjs::show, dplyr::filter, dplyr::select)

## See the theming issue brought up in #33; use thematic to attempt to make base
## R graphics compliant with ggplot theming, and to anticipate the impact of
## dark mode.
ggplot2::theme_set(ggplot2::theme_minimal())
thematic_shiny()

## Byte-compile the app's own functions (everything the R/ files define) once
## per R process, in the background, soon after start-up. Otherwise R's JIT
## compiles each large module server on its second call, inside the session of
## the second visitor who opens that tab (about 4 s for statInfrServer alone),
## and every other session waits meanwhile. Compiled functions behave exactly
## like the originals, and the JIT stays on for everything else.
## Shiny sources this file from shiny::loadSupport(), whose `renv` argument is
## the environment the R/ files are sourced into next. The work starts 5 s
## after the server is up (so a visitor who arrives right at start-up, e.g. the
## one who woke the app, gets the page first) and compiles one function per
## turn of Shiny's event loop, largest first, so requests that arrive meanwhile
## are served in between. When the app is started some other way, or in Shiny's
## test mode (shinytest2 drives the app from its first second, and a blocking
## compile would only skew its waits), nothing is scheduled and the JIT works
## as usual.
local({
  if (isTRUE(shiny::getShinyOption("testmode", default = FALSE))) return(invisible())
  for (i in rev(seq_len(sys.nframe()))) {
    if (identical(sys.function(i), shiny::loadSupport)) {
      app_env <- get("renv", envir = sys.frame(i))
      later::later(function() {
        fns <- Filter(function(nm) {
          f <- get(nm, envir = app_env, inherits = FALSE)
          is.function(f) && !is.primitive(f)
        }, ls(app_env, all.names = TRUE))
        size <- vapply(fns, function(nm) {
          as.numeric(utils::object.size(body(get(nm, envir = app_env, inherits = FALSE))))
        }, numeric(1))
        queue <- fns[order(size, decreasing = TRUE)]
        compile_next <- function() {
          if (length(queue) == 0) return(invisible())
          nm <- queue[1]
          queue <<- queue[-1]
          compiled <- tryCatch(compiler::cmpfun(get(nm, envir = app_env, inherits = FALSE)),
                               error = function(e) {
                                 message("Byte-compiling ", nm, " failed (the JIT will handle it): ",
                                         conditionMessage(e))
                                 NULL
                               })
          if (!is.null(compiled)) assign(nm, compiled, envir = app_env)
          later::later(compile_next)
        }
        compile_next()
      }, delay = 5)
      break
    }
  }
})
