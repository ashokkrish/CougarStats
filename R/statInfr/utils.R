prehideTab <- function(taglist, value) {
  selector_li <- r"{ li > a[data-value="%s"] }"
  selector_div <- r"{ div[data-value="%s"] }"
  style <- "display: none;"
  attr <- "data-value"
  tagQuery(taglist)$
    find("div")$
    filter(function(t, i) tagHasAttribute(t, attr) && tagGetAttribute(t, attr) == value)$
    addAttrs(style = style)$
    resetSelected()$
    find("li > a")$
    filter(function(t, i) tagHasAttribute(t, attr) && tagGetAttribute(t, attr) == value)$
    parent()$
    addAttrs(style = style)$
    resetSelected()$
    allTags()
}










## Extended fileInput widgetry; use the add_rule_accepted_file_formats
## function to easily add a rule to each validator
accepted_formats <- c(
  "text/csv",
  "text/comma-separated-values",
  ".csv",
  "text/plain",
  "text/tab-separated-values",
  ".tsv",

  ## R (native)
  ".rds",

  ## SPSS
  ".sav",

  ## Stata
  ".dta",

  ## SAS
  ".sas7bdat",

  ## Excel
  ".xls",
  ".xlsx",

  ## Minitab
  ".mpt",
  ".mpx",
  ".mwx"
)
newFileInput <- function(fileInputId, namespace) {
  stopifnot(is.character(fileInputId) && length(fileInputId) == 1)
  accepted_formats_listing <- "CSV, TSV, RDS, Excel®, Minitab®, SAS®, SPSS®, or Stata® formats are supported"
  tagList(
    uploadDataDisclaimer,
    fileInput(
      inputId = NS(namespace, fileInputId),
      label = strong(sprintf("Upload your data (%s)", accepted_formats_listing)),
      accept = accepted_formats
    )
  )
}
add_rule_accepted_file_formats <- function(validator, fileInputId) {
  do.call(validator$add_rule,
    list(
      fileInputId,
      ~ if (!tolower(tools::file_ext(.$name)) %in% (
        accepted_formats[-(1:3)] |>
          stringr::str_match("[^.]+")
      )[, 1]) {
        sprintf("File format (%s) not accepted.", tools::file_ext(.$name))
      }
    ),
    envir = parent.frame()
  )
}










lessThanInequalGreaterThanChoices123 <-
  c("<" = 1, "≠" = 2, ">" = 3)
