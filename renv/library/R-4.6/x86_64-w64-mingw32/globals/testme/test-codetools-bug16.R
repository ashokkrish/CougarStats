library(globals)

message("*** codetools::findGlobals() bug #16 ...")

exprs <- list(
 A = quote(x %% `$<-`("abc", 42)),
 B = quote(function() x %% `$<-`("abc", 42))
)

for (name in names(exprs)) {
  expr <- exprs[[name]]
  print(expr)
  globals <- globals::findGlobals(expr)
  print(globals)
  
  diffA <- setdiff(c("%%", "x", "$<-"), globals)
  print(diffA)
  stopifnot(length(diffA) == 0)
  
  diffB <- setdiff(globals, c("%%", "x", "$<-"))
  print(diffB)
  stopifnot(length(diffB) == 0)
}

message("*** codetools::findGlobals() bug #16 ... done")
