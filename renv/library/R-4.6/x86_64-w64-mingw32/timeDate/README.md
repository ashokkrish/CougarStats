<!-- badges: start -->
[![CRANStatusBadge](http://www.r-pkg.org/badges/version/timeDate)](https://cran.r-project.org/package=timeDate)
[![CRAN RStudio mirror downloads](https://cranlogs.r-pkg.org/badges/timeDate)](https://www.r-pkg.org/pkg/timeDate)
[![CRAN RStudio mirror downloads](https://cranlogs.r-pkg.org/badges/grand-total/timeDate?color=blue)](https://r-pkg.org/pkg/timeDate)
<!-- badges: end -->

The R package 'timeDate' provides class 'timeDate' for working with date/time.
The 'timeDate' class fulfils the conventions of the ISO 8601 standard as well as
of the ANSI C and POSIX standards. Beyond these standards it provides the
"Financial Center" concept which allows to handle data records collected in
different time zones and mix them up to have always the proper time stamps with
respect to your personal financial center, or alternatively to the GMT reference
time. It can thus also handle time stamps from historical data records from the
same time zone, even if the financial centers changed day light saving times at
different calendar dates.

Class 'timeInterval', introduced in v4050.111, represents time-date intervals. Suitable
metods are defined for manipulating them, including union, intersection, complement, set
difference and other utilities.

Package `timeDate` is part of the Rmetrics suite of R packages and is developed
on R-forge at
[timeDate](https://r-forge.r-project.org/scm/viewvc.php/pkg/timeDate/?root=rmetrics).
The root of Rmetrics is at [R-forge](https://r-forge.r-project.org/projects/rmetrics).


# Installing timeDate


Install the [latest stable version](https://cran.r-project.org/package=timeDate) of
`timeDate` from CRAN:

    install.packages("timeDate")


You can install the
[development version](https://r-forge.r-project.org/scm/viewvc.php/pkg/timeDate/?root=rmetrics)
of `timeDate` from R-forge:

    install.packages("timeDate", repos = "http://R-Forge.R-project.org")

To report bugs visit [Rmetrics](https://r-forge.r-project.org/projects/rmetrics/).

# Documentation

You can view the documentation of `timeDate` at
[timeDateDoc](https://geobosh.github.io/timeDateDoc/) (html site created with 'pkgdown')
or download the
[reference manual](https://cran.r-project.org/package=timeDate/timeDate.pdf)
of the latest release from CRAN.
