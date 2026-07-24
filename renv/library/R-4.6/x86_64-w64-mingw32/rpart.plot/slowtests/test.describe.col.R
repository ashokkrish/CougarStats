# test.describe.col.R

source("test.prolog.R")

show.prp.palettes() # initialize plot device so we can use palette function

describe.col <- function(col, show.hex=TRUE, check.palette.index=TRUE)
{
    rpart.plot:::describe.col(col, show.hex, check.palette.index)
}
stopifnot(identical(describe.col(1), "#000000 (black)"))
stopifnot(identical(describe.col(2), "#DF536B (near indianred2 #EE6363)"))

stopifnot(identical(describe.col(rgb(0, 1, 0)),  "#00FF00 (green)"))
stopifnot(identical(describe.col(rgb(0, 1, 1)),  "#00FFFF (cyan)"))
stopifnot(identical(describe.col(rgb(.1,.2,.3)), "#1A334D (near darkslategray #2F4F4F)"))

stopifnot(identical(describe.col(col2rgb(3)),         "#61D04F (near palegreen3 #7CCD7C)"))
stopifnot(identical(describe.col(col2rgb("green3")),  "#00CD00 (green3)"))
stopifnot(identical(describe.col(col2rgb("#00CD01")), "#00CD01 (near green3 #00CD00)"))

stopifnot(identical(describe.col("salmon2"), "#EE8262 (salmon2)"))
stopifnot(identical(describe.col("#EE8262"), "#EE8262 (salmon2)"))
stopifnot(identical(describe.col("#EE8260"), "#EE8260 (near salmon2 #EE8262)"))
stopifnot(identical(describe.col("#EE9260"), "#EE9260 (near salmon2 #EE8262)"))

stopifnot(identical(describe.col(0),             "#FFFFFF (white)"))
stopifnot(identical(describe.col(0.0),           "#FFFFFF (white)"))
stopifnot(identical(describe.col(0L),            "#FFFFFF (white)"))
stopifnot(identical(describe.col(NA),            "#FFFFFF (white)"))
stopifnot(identical(describe.col(col2rgb(NA)),   "#FFFFFF (white)"))
stopifnot(identical(describe.col(col2rgb("NA")), "#FFFFFF (white)"))
# col2rgb returns a matrix with 3 rows
stopifnot(identical(describe.col(matrix(c(1,2,3), nrow=3)), "#010203 (near gray1 #030303)"))

expect.err(try(describe.col(1.23)),             "non-integer col is illegal")
expect.err(try(describe.col(c(1,2))),           "only one color is allowed")
expect.err(try(describe.col(c("red", "pink"))), "only one color is allowed")
expect.err(try(describe.col(-1)),               "col -1 is illegal (col must be greater than or equal to 0)")
expect.err(try(describe.col(9)),                "illegal col 9 (only 8 colors in the current palette)")
expect.err(try(describe.col("nonesuch")),       "invalid color name 'nonesuch'")
expect.err(try(describe.col(FALSE)),            "invalid color name 'FALSE'")
expect.err(try(describe.col(col2rgb(FALSE))),   "invalid color name 'FALSE'")

par(old.par)

source("test.epilog.R")
