prehideTab <- function(taglist, value) {
  selector_li <- r"{ li > a[data-value="%s"] }"
  selector_div <- r"{ div[data-value="%s"] }"
  style <- "display: none;"
  attr <- "data-value"
  tagQuery(taglist)$find("div")$filter(function(t, i) {
    tagHasAttribute(t, attr) && tagGetAttribute(t, attr) == value
  })$addAttrs(style = style)$resetSelected()$find("li > a")$filter(function(
    t,
    i
  ) {
    tagHasAttribute(t, attr) && tagGetAttribute(t, attr) == value
  })$parent()$addAttrs(style = style)$resetSelected()$allTags()
}
showTabs <- function(input_id, values, select, session) {
  for (value in values) {
    if (!missing(session))
      shiny::showTab(input_id, value, select == value, session)
    else
      shiny::showTab(input_id, value, select == value)
  }
}
hideTabs <- function(input_id, values, session) {
  for (value in values) {
    if (!missing(session))
      shiny::hideTab(input_id, value, session)
    else
      shiny::hideTab(input_id, value)
  }
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
    tags$label(
      style = css(cursor = "pointer !important"),
      `for` = NS(namespace, fileInputId),
      strong(sprintf("Upload your data (%s)", accepted_formats_listing))
    ),
    fileInput(
      inputId = NS(namespace, fileInputId),
      label = NULL,
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
          envir = parent.frame())
}










lessThanInequalGreaterThanChoices123 <-
  c("<" = 1, "≠" = 2, ">" = 3)










## TODO: hack on these so they work for the varied tabsets in StatInfr.
validateResultTabsArgs <- quote({
  if (missing(tabs)) {
    stop("What tabs do you want to hide? Tabs must be specified for use with hideResultTabs.")
  } else if (missing(tabset)) {
    stop("A tabset must be specified for use with hideResultTabs.")
  }
  stopifnot(is.character(tabset) && is.character(tabs))
})
hideResultTabs <- function(tabset, tabs) {
  eval(validateResultTabsArgs)
  if (length(tabs)) for (t in tabs) shiny::hideTab(inputId = tabset, target = t)
}
showResultTabs <- function(tabset, tabs) {
  eval(validateResultTabsArgs)
  if (length(tabs)) for (t in tabs) shiny::showTab(inputId = tabset, target = t)
  ## FIXME: this is a best guess at desired behaviour right now; fix after testing.
  updateTabsetPanel(inputId = tabset, selected = 1)
  ## shinyjs::runjs(sprintf(r"---[
  ##     setTimeout(function() {
  ##       var a = $('#%s a[data-value="Descriptive Statistics"]');
  ##       a.removeClass('active');
  ##       a.tab('show');
  ##       $(window).trigger('resize');
  ##     }, 50);
  ##   ]---",
  ##   session$ns("dsTabset")))
}
goToUploadedDataTab <- function(tabsetId, tab = "Uploaded Data") {
  showTab(inputId = tabsetId, target = tab)
  updateTabsetPanel(inputId = tabsetId, selected = tab)
}

createFileInputEventReactive <- function(input, inputId) {
  quietExcelRead <- function(reader, path, sheet) {
    withCallingHandlers(
      reader(path, sheet = sheet),
      warning = function(w) {
        if (grepl("Coercing boolean to numeric", conditionMessage(w))) {
          invokeRestart("muffleWarning")
        }
      }
    )
  }

  eventReactive(input[[inputId]], {
    input_value <- input[[inputId]]
    ext <- tolower(tools::file_ext(input_value$name))
    path <- input_value$datapath
    ext |>
      switch(
        csv = read_csv(path, show_col_types = FALSE),
        xls = {
          quietExcelRead(read_xls, path, input$dsSheet)
        },
        xlsx = {
          quietExcelRead(read_xlsx, path, input$dsSheet)
        },
        txt = read_tsv(path, show_col_types = FALSE),
        sas7bdat = read_sas(path),
        sav = read_sav(path),
        dta = haven::read_dta(path),
        rds = {
          obj <- readRDS(path)
          validate(need(is.data.frame(obj), ".rds file must contain a data frame."))
          obj
        },
        mtp = read_mtp_helper(path),
        mwx = tryCatch(read_minitab_xml(path), error = validate(sprintf("Unable to read uploaded file; %s support is partial.", ext))),
        mpx = tryCatch(read_minitab_xml(path), error = validate(sprintf("Unable to read uploaded file; %s support is partial.", ext))),
        validate(sprintf("Uploaded data file format (%s) is unsupported.", ext))
      )
  })
}

## ----------------------------------------------------------- #
##     Minitab file readers (authored by Darren Law)           #
## ----------------------------------------------------------- #
## Older Minitab Portable Worksheet (.mtp) – text-based.
read_mtp_helper <- function(path) {
  raw <- foreign::read.mtp(path)
  keep <- raw[vapply(raw, is.numeric, logical(1))]
  validate(need(length(keep) > 0, "No numeric columns found in .mtp file."))
  max_len <- max(vapply(keep, length, integer(1)))
  keep <- lapply(keep, function(v) {
    length(v) <- max_len
    v
  })
  if (is.null(names(keep)) || any(names(keep) == "")) {
    names(keep) <- paste0("V", seq_along(keep))
  }
  as.data.frame(keep, stringsAsFactors = FALSE)
}

## Newer Minitab XML formats (.mwx / .mpx) – best-effort, schema varies.
read_minitab_xml <- function(path) {
  tmp <- tempfile()
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  utils::unzip(path, exdir = tmp)
  xml_files <- list.files(tmp, pattern = "\\.xml$", recursive = TRUE, full.names = TRUE)
  validate(need(length(xml_files) > 0, "Could not find data inside Minitab file. Try exporting to .xlsx."))

  doc <- NULL
  for (f in xml_files) {
    candidate <- try(xml2::read_xml(f), silent = TRUE)
    if (inherits(candidate, "xml_document") &&
        length(xml2::xml_find_all(candidate, "//*[local-name()='Column']")) > 0) {
      doc <- candidate
      break
    }
  }
  validate(need(!is.null(doc), "Could not parse Minitab file. Please export to .xlsx in Minitab."))

  cols <- xml2::xml_find_all(doc, "//*[local-name()='Column']")
  col_data <- lapply(seq_along(cols), function(i) {
    col <- cols[[i]]
    nm <- xml2::xml_attr(col, "Name")
    if (is.na(nm)) nm <- xml2::xml_attr(col, "name")
    if (is.na(nm)) nm <- paste0("C", i)
    cells <- xml2::xml_find_all(col, ".//*[local-name()='Cell' or local-name()='Value' or local-name()='R']")
    vals <- xml2::xml_text(cells)
    list(name = nm, values = vals)
  })

  max_len <- max(vapply(col_data, function(x) length(x$values), integer(1)))
  df_cols <- lapply(col_data, function(x) {
    v <- x$values
    length(v) <- max_len
    nv <- suppressWarnings(as.numeric(v))
    if (sum(is.na(nv)) <= sum(is.na(v))) nv else v
  })
  names(df_cols) <- vapply(col_data, function(x) x$name, character(1))
  as.data.frame(df_cols, stringsAsFactors = FALSE)
}
## NOTE: end of code copied from DescStats.R written by Darren Law.


checkNumeric <- function(data, cols) {
  if (is.null(cols) || length(cols) == 0 || !all(cols %in% colnames(data))) {
    return(FALSE) ## no invalid columns if nothing selected
  }

  dat <- as.data.frame(data)[, cols, drop = FALSE]
  invalid <- any(!sapply(dat, is.numeric))

  return(invalid)
}

getOutliers <- function(sample, sampleName, coef = 1.5) {
  x <- sort(sample)

  if (length(x) %% 2 != 0) {
    x_no_median <- x[-ceiling(length(x) / 2)]
  } else {
    x_no_median <- x
  }

  mid <- length(x_no_median) / 2
  Q1 <- median(x_no_median[1:mid])
  Q2 <- median(x)
  Q3 <- median(x_no_median[(mid + 1):length(x_no_median)])

  IQR <- Q3 - Q1
  lower_fence <- Q1 - coef * IQR
  upper_fence <- Q3 + coef * IQR

  outliers <- x[x < lower_fence | x > upper_fence]

  if (length(outliers) == 0) {
    return(data.frame(sample = character(0), data = numeric(0)))
  } else {
    return(data.frame(sample = sampleName, data = outliers))
  }
}



hypTTestPlot <- function(testStatistic, degfree, critValue, altHypothesis) {
  tTail <- qt(0.999, df = degfree, lower.tail = FALSE)
  tHead <- qt(0.999, df = degfree, lower.tail = TRUE)
  x <- round(seq(from = tTail, to = tHead, by = 0.1), 2)

  if (altHypothesis == "two.sided") {
    CVs <- c(-critValue, critValue)
    RRLabels <- c((-critValue + tTail) / 2, (critValue + tHead) / 2)
  } else {
    CVs <- c(critValue)
    if (altHypothesis == "less") {
      RRLabels <- c((critValue + tTail) / 2)
    } else {
      RRLabels <- c((critValue + tHead) / 2)
    }
  }

  xSeq <- unique(sort(c(x, testStatistic, CVs, RRLabels, 0)))

  df <- data.frame(x = xSeq, y = dt(xSeq, degfree))
  cvDF <- filter(df, x %in% CVs)
  RRLabelsDF <- filter(df, x %in% RRLabels)
  tsDF <- filter(df, x %in% testStatistic)
  centerDF <- filter(df, x %in% c(0))

  htPlot <- ggplot(df, aes(x = x, y = y))

  if (altHypothesis == "two.sided") {
    htPlot <- htPlot + shadeHtArea(df, -critValue, "less") +
      shadeHtArea(df, critValue, "greater")
  } else {
    htPlot <- htPlot + shadeHtArea(df, critValue, altHypothesis)
  }

  htPlot <- htPlot + stat_function(
                       fun = dt,
                       args = list(df = degfree),
                       geom = "density",
                       fill = NA
                     ) +
    theme_void() +
    scale_y_continuous(breaks = NULL) +
    ylab("") +
    xlab("t") +
    geom_segment(
      data = filter(df, x %in% c(0)),
      aes(x = x, xend = x, y = 0, yend = y),
      linetype = "dotted",
      linewidth = 0.75,
      color = "black"
    ) +
    geom_text(
      data = filter(df, x %in% c(0)),
      aes(x = x, y = y / 2, label = "A R"),
      size = 16 / .pt,
      fontface = "bold"
    ) +
    geom_text(
      data = filter(df, x %in% c(0)),
      aes(x = x, y = 0, label = "0"),
      size = 14 / .pt,
      fontface = "bold",
      nudge_y = -.03
    ) +
    geom_segment(
      data = tsDF,
      aes(x = x, xend = x, y = 0, yend = y + .03),
      linetype = "solid",
      linewidth = 1.25,
      color = "#BD130B"
    ) +
    geom_text(
      data = tsDF,
      aes(x = x, y = y, label = x),
      size = 16 / .pt,
      fontface = "bold",
      nudge_y = .075
    ) +
    geom_segment(
      data = cvDF,
      aes(x = x, xend = x, y = 0, yend = y),
      linetype = "solid",
      lineend = "butt",
      linewidth = 1.5,
      color = "#023B70"
    ) +
    geom_text(
      data = cvDF,
      aes(x = x, y = 0, label = x),
      size = 14 / .pt,
      fontface = "bold",
      nudge_y = -.03
    ) +
    geom_text(
      data = RRLabelsDF,
      aes(x = x, y = y, label = "RR"),
      size = 16 / .pt,
      fontface = "bold",
      nudge_y = .03
    ) +
    theme(axis.title.x = element_text(
            size = 16,
            face = "bold.italic"
          ))

  return(htPlot)
}


hypZTestPlot <- function(testStatistic, critValue, altHypothesis) {
  ## normTail = qnorm(0.999, mean = 0, sd = 1, lower.tail = FALSE)
  ## normHead = qnorm(0.999, mean = 0, sd = 1, lower.tail = TRUE)
  ## xSeq = sort(c(normTail, normHead, testStatistic, critValues, 0))

  x <- round(seq(from = -3, to = 3, by = 0.1), 2)

  if (altHypothesis == "two.sided") {
    CVs <- c(-critValue, critValue)
    RRLabels <- c((-critValue + -3) / 2, (critValue + 3) / 2)
  } else {
    CVs <- c(critValue)
    if (altHypothesis == "less") {
      RRLabels <- c((critValue + -3) / 2)
    } else {
      RRLabels <- c((critValue + 3) / 2)
    }
  }

  xSeq <- unique(sort(c(x, testStatistic, CVs, RRLabels, 0)))

  ## if(testStatistic < normTail)
  ## {
  ## normTail = testStatistic
                                        #
  ## } else if(testStatistic > normHead)
  ## {
  ## normHead = testStatistic
  ## }

  df <- distinct(data.frame(x = xSeq, y = dnorm(xSeq, mean = 0, sd = 1)))
  cvDF <- filter(df, x %in% CVs)
  RRLabelsDF <- filter(df, x %in% RRLabels)
  tsDF <- filter(df, x %in% testStatistic)
  centerDF <- filter(df, x %in% c(0))

  htPlot <- ggplot(df, aes(x = x, y = y))

  if (altHypothesis == "two.sided") {
    htPlot <- htPlot + shadeHtArea(df, -critValue, "less") +
      shadeHtArea(df, critValue, "greater")
  } else {
    htPlot <- htPlot + shadeHtArea(df, critValue, altHypothesis)
  }

  htPlot <- htPlot + geom_segment(
                       data = cvDF,
                       aes(x = x, xend = x, y = 0, yend = y),
                       linetype = "solid",
                       lineend = "butt",
                       linewidth = 1.5,
                       color = "#023B70"
                     ) +
    stat_function(
      fun = dnorm,
      geom = "density",
      fill = NA
    ) +
    theme_void() +
    scale_y_continuous(breaks = NULL) +
    ylab("") + xlab("Z") +
    geom_segment(
      data = filter(df, x %in% c(0)),
      aes(x = x, xend = x, y = 0, yend = y),
      linetype = "dotted",
      linewidth = 0.75,
      color = "black"
    ) +
    geom_text(
      data = filter(df, x %in% c(0)),
      aes(x = x, y = y / 2, label = "A R"),
      size = 16 / .pt,
      check_overlap = TRUE,
      fontface = "bold"
    ) +
    geom_text(
      data = filter(df, x %in% c(0)),
      aes(x = x, y = 0, label = "0"),
      size = 14 / .pt,
      fontface = "bold",
      nudge_y = -.03,
      check_overlap = TRUE
    ) +
    geom_segment(
      data = tsDF,
      aes(x = x, xend = x, y = 0, yend = y + .055),
      linetype = "solid",
      linewidth = 1.25,
      color = "#BD130B"
    ) +
    geom_text(
      data = tsDF,
      aes(x = x, y = y, label = x),
      size = 14 / .pt,
      fontface = "bold",
      nudge_y = .075,
      check_overlap = TRUE
    ) +
    geom_text(
      data = cvDF,
      aes(x = x, y = 0, label = x),
      size = 14 / .pt,
      fontface = "bold",
      nudge_y = -.03,
      check_overlap = TRUE
    ) +
    geom_text(
      data = RRLabelsDF,
      aes(x = x, y = y, label = "RR"),
      size = 16 / .pt,
      fontface = "bold",
      nudge_y = .025,
      check_overlap = TRUE
    ) +
    theme(axis.title.x = element_text(size = 16, face = "bold.italic")) +
    coord_cartesian(clip = "off")

  return(htPlot)
}


shadeHtArea <- function(df, critValue, altHypothesis) {
  if (altHypothesis == "less") {
    geom_area(
      data = subset(df, x <= critValue),
      aes(y = y),
      fill = "#023B70",
      color = NA,
      alpha = 0.4
    )
  } else if (altHypothesis == "greater") {
    geom_area(
      data = subset(df, x >= critValue),
      aes(y = y),
      fill = "#023B70",
      color = NA,
      alpha = 0.4
    )
  }
}


printHTPVal <- function(pValue, testStat, alternative, tsValue, pvalSign, reject) {
  if (pValue < 0.0001) {
    pValue <- "P \\lt 0.0001"
  }

  if (alternative == "two.sided") {
    pvalCalc <- paste("2 \\times P(", testStat, "\\, \\gt \\; \\mid", tsValue, "\\mid)")
  } else if (alternative == "greater") {
    pvalCalc <- paste("P(", testStat, "\\, > \\,", tsValue, ")")
  } else {
    pvalCalc <- paste("P(", testStat, "\\, < \\,", tsValue, ")")
  }

  pvalOutput <- tagList(
    p(tags$b("Using P-Value Method:")),
    sprintf(
      "\\(P = %s = %s\\)",
      pvalCalc,
      pValue
    ),
    br(),
    br(),
    sprintf(
      "Since \\( P %s %0.2f \\), %s \\( H_{0}\\).",
      pvalSign,
      SigLvl(),
      reject
    ),
    br(),
    br(),
    br()
  )

  return(pvalOutput)
}


printHTConclusion <- function(region, reject, suffEvidence, altHyp, altHypValue) {
  conclusion <- tagList(
    withMathJax(),
    p(tags$b("Conclusion:")),
    sprintf(
      "At \\( \\alpha = %s \\), since the test statistic falls in the %s region we %s \\(
               H_{0}\\) and conclude that there %s enough statistical evidence to support that \\(%s %s\\).",
      SigLvl(),
      region,
      reject,
      suffEvidence,
      altHyp,
      altHypValue
    ),
    br()
  )

  return(conclusion)
}


createCalculateResetButtonsGroup <- function(ns) {
  if (missing(ns) || !is.function(ns))
    stop("The ns function (ns <- NS(\"module-id\")) was missing! Cannot proceed!")

  actionGroupButtons(
    inputIds = c(ns("calculate"), ns("reset")),
    labels = list(
      tagList(
        span(class = "action-icon", tags$i(class = "fas fa-calculator", role = "presentation", `aria-label` = "calculator icon")),
        span(class = "action-label", "Calculate")
      ),
      tagList(
        span(class = "action-icon", tags$i(class = "fas fa-recycle", role = "presentation", `aria-label` = "recycle icon")),
        span(class = "action-label", "Reset Form")
      )
    ),
    fullwidth = TRUE
  )
}

add_rules <- function(iv, id, ...) {
  lapply(rlang::list2(...), function(rule) iv$add_rule(id, rule))
}
