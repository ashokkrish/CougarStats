## Shared F-distribution plot used by SLR, MLR, and PR ANOVA tabs.
## f_stat and f_crit should already be rounded before passing in.
## y_cap adapts to df1: for df1=1 it clips the near-infinite left spike;
## for df1>=2 it sits above the actual mode so the full curve is visible.
anovaFPlot <- function(f_stat, f_crit, df1, df2) {

  x_start <- 0.05
  x_max   <- if (f_stat > x_start && f_stat < f_crit * 10)
               max(f_crit * 2.5, f_stat * 1.3)
             else
               f_crit * 2.5

  y_cap <- if (df1 > 2) {
    mode_x <- (df1 - 2) / df1 * df2 / (df2 + 2)
    stats::df(mode_x, df1, df2) * 1.15
  } else {
    # df1 <= 2: F density is monotonically decreasing from 0; cap at the curve's
    # value at x_start so pmin() never creates a flat left section.
    stats::df(x_start, df1, df2) * 1.05
  }

  x_curve <- seq(x_start, x_max, length.out = 600)
  y_curve <- stats::df(x_curve, df1, df2)
  y_disp  <- pmin(y_curve, y_cap)
  plot_df <- data.frame(x = x_curve, y = y_disp)
  plot_df <- plot_df[is.finite(plot_df$y), ]

  seg_h       <- y_cap * 0.75
  f_in_range  <- f_stat > x_start && f_stat <= x_max
  f_off_chart <- f_stat > x_max

  ggplot(plot_df, aes(x = x, y = y)) +
    geom_ribbon(data = plot_df[plot_df$x >= f_crit, ],
                aes(ymin = 0, ymax = y),
                fill = "steelblue", alpha = 0.35) +
    geom_line(linewidth = 0.8) +
    annotate("text",
             x = f_crit, y = y_cap * 1.08,
             label = "← AR   ", hjust = 1,
             size = 14 / .pt, fontface = "bold") +
    annotate("text",
             x = f_crit, y = y_cap * 1.08,
             label = "   RR →", hjust = 0,
             size = 14 / .pt, fontface = "bold") +
    annotate("segment",
             x = f_crit, xend = f_crit, y = 0, yend = seg_h,
             linewidth = 1.5, color = "#023B70", linetype = "dashed") +
    {if (f_in_range)
      annotate("segment",
               x = f_stat, xend = f_stat, y = 0, yend = seg_h,
               linewidth = 1.25, color = "#BD130B", linetype = "dashed")
    } +
    annotate("text",
             x = f_crit, y = -y_cap * 0.07,
             label = as.character(f_crit),
             color = "#023B70", fontface = "bold",
             size = 14 / .pt) +
    {if (f_in_range)
      annotate("text",
               x = f_stat, y = -y_cap * 0.07,
               label = as.character(f_stat),
               color = "#BD130B", fontface = "bold",
               size = 14 / .pt)
    } +
    {if (f_off_chart)
      annotate("segment",
               x = x_max * 0.90, xend = x_max * 0.995,
               y = seg_h, yend = seg_h,
               arrow = arrow(length = unit(0.18, "cm"), type = "closed"),
               linewidth = 1.25, color = "#BD130B")
    } +
    {if (f_off_chart)
      annotate("text",
               x = x_max * 0.90, y = seg_h + y_cap * 0.06,
               label = paste0("F = ", as.character(f_stat)),
               color = "#BD130B", fontface = "bold", hjust = 1,
               size = 14 / .pt)
    } +
    coord_cartesian(xlim = c(0, x_max * 1.02),
                    ylim = c(0, y_cap * 1.18),
                    clip = "off") +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(breaks = 0, labels = "0", expand = c(0, 0)) +
    ylab(expression(bold(italic(Density)))) +
    xlab(expression(bold(italic(F)))) +
    theme_classic() +
    theme(
      axis.text.x   = element_blank(),
      axis.ticks.x  = element_blank(),
      axis.text.y   = element_text(size = 13, face = "bold"),
      axis.title.x  = element_text(size = 16, face = "bold.italic",
                                   margin = margin(t = 22)),
      axis.title.y  = element_text(size = 16, face = "bold.italic"),
      axis.line    = element_line(linewidth = 0.8, color = "black"),
      plot.margin  = margin(t = 20, r = 10, b = 45, l = 5)
    )
}

## Format a number for LaTeX: uses value^{exp} notation when the value would
## round to zero at 'digits' decimal places, so fractions never display 0/0.
## Passes literal 0 (not x) for the x==0 case to prevent IEEE 754 negative-zero
## (-0.0) from producing "-0.0000" via sprintf.
fmt_sci_latex <- function(x, digits = 3) {
  if (!is.finite(x)) return(sprintf("%.*f", digits, x))
  if (x == 0)        return(sprintf("%.*f", digits, 0))   # literal 0 avoids -0.0 → "-0.0000"
  rounded <- round(x, digits)
  if (rounded == 0) {
    # x is non-zero but rounds to 0 — show scientific notation instead of "0.0000"
    exp  <- floor(log10(abs(x)))
    mant <- x / 10^exp
    sprintf("%.*f^{%d}", digits, mant, exp)
  } else {
    format(rounded, nsmall = digits, scientific = FALSE)
  }
}

## Format a p-value as a LaTeX relational string for use inside \( ... \)
## Returns "= 0.0234" for ordinary values or "< 0.0001" when below eps.
pval_tex <- function(p, eps = 0.0001) {
  if (p < eps) paste0("< ", format(eps, scientific = FALSE))
  else sprintf("= %0.4f", p)
}

## String List to Numeric List
## Accepts values separated by commas, spaces, tabs, and/or newlines (e.g.
## pasted directly from a spreadsheet column), treating any run of those
## delimiters as a single separator. A value written in plain or scientific
## notation (e.g. 2.5e2, 1E-3, +4) is read as is; any other value has its
## non-numeric characters purged first, as before. NULL, empty or NA input
## (e.g. an input that is not rendered yet) gives numeric(0). A plain number
## too large for a double (a long run of digits) is Inf, as before, so callers
## can report it.
createNumLst <- function(text) {
  if (is.null(text) || length(text) == 0 || is.na(text[1])) return(numeric(0))
  text   <- gsub("[,\t\r\n ]+", ",", as.character(text[1]), perl = TRUE)     #collapse delimiter runs into a single comma
  tokens <- strsplit(text, ",", fixed = TRUE)[[1]]
  isNum  <- grepl("^[+-]?([0-9]+\\.?[0-9]*|\\.[0-9]+)([eE][+-]?[0-9]+)?$", tokens, perl = TRUE)
  values <- rep(NA_real_, length(tokens))
  values[isNum]  <- suppressWarnings(as.numeric(tokens[isNum]))
  values[!isNum] <- suppressWarnings(as.numeric(gsub("[^0-9.-]", "", tokens[!isNum], perl = TRUE))) #purge non-numeric characters
  values[isNum & grepl("[eE]", tokens) & !is.finite(values)] <- NA            #e.g. 1e999 overflows to Inf
  values[!is.na(values)]
}

## NULL/NA-safe: an input that is not rendered yet falls back to the default size.
GetPlotHeight  <- function(plotToggle, pxValue, ui) {

  ifelse(isTRUE(plotToggle == 'in px') && isTRUE(!is.na(pxValue)),
         height <- pxValue,
         height <- 400)

  ifelse(ui,
         return(paste0(height, "px")),
         return(height))
}

GetPlotWidth  <- function(plotToggle, pxValue, ui) {

  if(isTRUE(plotToggle == 'in px') && isTRUE(!is.na(pxValue))) {
    width <- pxValue

    if(ui) {
      width <- paste0(width, "px")
    }
  } else {
    width <- "auto"
  }

  return(width)
}

`%then%` <- function(a, b) {
  if (is.null(a)) b else a
}

copyButton <- function(id, ns) {
  tags$button(
    class = "copy-btn",
    type = "button",
    onclick = sprintf(
      "   navigator.clipboard.writeText(document.getElementById('%s').innerText.trim());
          var btn = this;
          var old = btn.innerText;
          btn.innerText = '✓ Copied';
          setTimeout(function(){
            btn.innerText = old;
          }, 1000);",
      ns(id)
    ),
    "Copy"
  )
}

# Creates an empty R code box display
codeBox <- function(title = "R Code", boxId, outputId, ns) {
  
  div(
    id = ns(paste0(boxId, "Wrapper")),
    class = "code-container",

    div(
      class = "code-header",
      div(title),
      copyButton(boxId, ns)
    ),

    div(
      id = ns(boxId),
      class = "code-body",
      htmlOutput(ns(outputId))
    )
  )
}

# Shows/hides code box. Does nothing on the client if the box is not on the
# page (yet), e.g. while the renderUI that contains it has not rendered.
toggleCodeBox <- function(showBox, boxId, ns) {
  if (length(showBox) == 0 || is.na(showBox[1])) {
    showBox <- FALSE
  }
  runjs(sprintf(
    "(function(){var e=document.getElementById('%s'); if (e) e.style.display='%s';})();",
    ns(paste0(boxId, "Wrapper")),
    if (showBox) "block" else "none"
  ))
}

# Adds colour blue to the argument
codeValue <- function(x) {
  paste0('<span class="code-value">', as.character(x), '</span>')
}

## ------------------------------------------------------------------------ #
## Shared data-upload file readers
## Used by any module with an "Upload Data" option (Descriptive Stats, SLR,
## MLR, etc.) so format support only needs to be maintained in one place.
## ------------------------------------------------------------------------ #

UPLOAD_ACCEPTED_EXTENSIONS <- c("csv", "txt", "xls", "xlsx", "sas7bdat",
                                "sav", "dta", "rds", "mtp", "mwx", "mpx")

## Upload size limits. Shiny already caps the upload itself (5 MB by default),
## but zip containers (.xlsx/.mwx/.mpx), gzip/bzip2/xz streams (.rds, and
## .csv/.txt/.mtp, which the readers decompress transparently) and the
## internally compressed SAS/SPSS formats can expand far beyond that inside
## the single shared R process.
UPLOAD_MAX_UNCOMPRESSED_BYTES <- 200 * 1024^2 # 200 MB once decompressed
UPLOAD_MAX_CELLS              <- 20e6         # rows x columns of the data read

# Size in bytes of an uploaded file once decompressed, counted as a stream
# without keeping it in memory (counting stops once 'limit' is exceeded).
# Uncompressed files return their size on disk. NA when the file cannot be
# inspected; the reader then reports its usual error.
uploadDecompressedSize <- function(path, limit = UPLOAD_MAX_UNCOMPRESSED_BYTES) {
  magic <- readBin(path, "raw", n = 6)
  isZip <- length(magic) >= 4 && magic[1] == as.raw(0x50) && magic[2] == as.raw(0x4b) &&
           ((magic[3] == as.raw(3) && magic[4] == as.raw(4)) ||
            (magic[3] == as.raw(5) && magic[4] == as.raw(6)) ||
            (magic[3] == as.raw(7) && magic[4] == as.raw(8)))
  isStream <- (length(magic) >= 2 && magic[1] == as.raw(0x1f) && magic[2] == as.raw(0x8b)) ||        # gzip
              (length(magic) >= 3 && identical(magic[1:3], as.raw(c(0x42, 0x5a, 0x68)))) ||           # bzip2
              (length(magic) >= 6 && identical(magic[1:6], as.raw(c(0xfd, 0x37, 0x7a, 0x58, 0x5a, 0x00)))) # xz
  if (isZip) {
    entries <- tryCatch(utils::unzip(path, list = TRUE), error = function(e) NULL)
    if (is.null(entries)) return(NA_real_)
    return(sum(as.numeric(entries$Length)))
  }
  if (isStream) {
    return(tryCatch({
      con <- gzfile(path, "rb")
      on.exit(close(con), add = TRUE)
      total <- 0
      repeat {
        chunk <- readBin(con, "raw", n = 4 * 1024^2)
        if (length(chunk) == 0) break
        total <- total + length(chunk)
        if (total > limit) break
        if (total %% (32 * 1024^2) == 0) gc(full = FALSE) # drop counted chunks early on big streams
      }
      total
    }, error = function(e) NA_real_))
  }
  as.numeric(file.size(path))
}

# validate()s that an uploaded file stays within UPLOAD_MAX_UNCOMPRESSED_BYTES
# once decompressed. Can be called by any module before reading an upload.
validateUploadSize <- function(path) {
  size <- uploadDecompressedSize(path)
  validate(need(is.na(size) || size <= UPLOAD_MAX_UNCOMPRESSED_BYTES,
                sprintf("File is too large: it expands to more than %d MB when decompressed.",
                        UPLOAD_MAX_UNCOMPRESSED_BYTES / 1024^2)))
}

uploadTooManyCellsMsg <- sprintf("File is too large: more than %s values (rows x columns).",
                                 format(UPLOAD_MAX_CELLS, big.mark = ",", scientific = FALSE))

# haven readers (SAS/SPSS/Stata) with a cap on rows x columns. The header is
# read first (n_max = 0) so the row limit can be derived from the number of
# columns before any data is allocated; within the cap the result is the same
# as an uncapped read.
readHavenCapped <- function(reader, path) {
  header  <- reader(path, n_max = 0)
  maxRows <- floor(UPLOAD_MAX_CELLS / max(1, ncol(header)))
  dat     <- reader(path, n_max = maxRows + 1)
  validate(need(nrow(dat) <= maxRows, uploadTooManyCellsMsg))
  dat
}

# Silence noisy but harmless readxl warnings (boolean-to-numeric coercions).
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

# Older Minitab Portable Worksheet (.mtp) - text-based.
read_mtp_helper <- function(path) {
  raw <- foreign::read.mtp(path)
  keep <- raw[vapply(raw, is.numeric, logical(1))]
  validate(need(length(keep) > 0, "No numeric columns found in .mtp file."))
  max_len <- max(vapply(keep, length, integer(1)))
  keep <- lapply(keep, function(v) { length(v) <- max_len; v })
  if (is.null(names(keep)) || any(names(keep) == ""))
    names(keep) <- paste0("V", seq_along(keep))
  as.data.frame(keep, stringsAsFactors = FALSE)
}

# Newer Minitab XML formats (.mwx / .mpx) - best-effort, schema varies.
read_minitab_xml <- function(path) {
  tmp <- tempfile()
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  # Only the .xml entries are read below, so only those are extracted.
  entries <- tryCatch(utils::unzip(path, list = TRUE)$Name, error = function(e) character(0))
  xml_entries <- entries[grepl("\\.xml$", basename(entries)) & !grepl("/$", entries)]
  if (length(xml_entries) > 0) utils::unzip(path, files = xml_entries, exdir = tmp)
  xml_files <- list.files(tmp, pattern = "\\.xml$", recursive = TRUE, full.names = TRUE)
  validate(need(length(xml_files) > 0, "Could not find data inside Minitab file. Try exporting to .xlsx."))

  doc <- NULL
  for (f in xml_files) {
    candidate <- try(xml2::read_xml(f), silent = TRUE)
    if (inherits(candidate, "xml_document") &&
        length(xml2::xml_find_all(candidate, "//*[local-name()='Column']")) > 0) {
      doc <- candidate; break
    }
  }
  validate(need(!is.null(doc), "Could not parse Minitab file. Please export to .xlsx in Minitab."))

  cols <- xml2::xml_find_all(doc, "//*[local-name()='Column']")
  col_data <- lapply(seq_along(cols), function(i) {
    col <- cols[[i]]
    nm  <- xml2::xml_attr(col, "Name")
    if (is.na(nm)) nm <- xml2::xml_attr(col, "name")
    if (is.na(nm)) nm <- paste0("C", i)
    cells <- xml2::xml_find_all(col, ".//*[local-name()='Cell' or local-name()='Value' or local-name()='R']")
    vals  <- xml2::xml_text(cells)
    list(name = nm, values = vals)
  })

  max_len <- max(vapply(col_data, function(x) length(x$values), integer(1)))
  df_cols <- lapply(col_data, function(x) {
    v <- x$values; length(v) <- max_len
    nv <- suppressWarnings(as.numeric(v))
    if (sum(is.na(nv)) <= sum(is.na(v))) nv else v
  })
  names(df_cols) <- vapply(col_data, function(x) x$name, character(1))
  as.data.frame(df_cols, stringsAsFactors = FALSE)
}

# Message for an uploaded file that cannot be read (corrupt, binary data, text
# in an unsupported encoding, ...). It names the format but never the server's
# temporary path.
uploadReadErrorMsg <- function(ext) {
  ext <- tolower(ext)
  sprintf("Unable to read this file. Make sure it is a valid %s file%s.",
          if (ext == "txt") "tab-delimited .txt" else paste0(".", ext),
          if (ext %in% c("csv", "txt")) " (text files must be UTF-8 encoded)" else "")
}

# Signals that an uploaded file cannot be read: an error of class
# "uploadReadError" with uploadReadErrorMsg(). It is not a validate() message,
# so a caller with its own text for unreadable files (Statistical Inference,
# Machine Learning) keeps showing that text; the other callers turn it into a
# validate() message (see readUploadedDataFile()).
uploadReadError <- function(ext) {
  stop(structure(class = c("uploadReadError", "error", "condition"),
                 list(message = uploadReadErrorMsg(ext), call = NULL)))
}

# Evaluates 'expr' (a reader call). validate() and req() stop as usual; any
# other error (the reader's own message can contain the temporary path) becomes
# uploadReadError(ext).
withUploadReadError <- function(ext, expr) {
  tryCatch(expr, error = function(e) {
    if (inherits(e, "shiny.silent.error")) stop(e)
    uploadReadError(ext)
  })
}

# TRUE when a text upload (.csv/.txt) is binary data, e.g. an image or a PDF
# renamed .csv: its start (decompressed) contains NUL bytes and it is not
# UTF-16 text (which has a byte-order mark, or NULs in every other byte).
# Such data is not handed to readr, which can crash the whole R process on it.
# A zip archive is left to readr, which reads the archive's first file.
uploadLooksBinary <- function(path) {
  magic <- readBin(path, "raw", n = 4)
  if (length(magic) == 4 && identical(magic, as.raw(c(0x50, 0x4b, 0x03, 0x04)))) return(FALSE)
  b <- tryCatch({
    con <- gzfile(path, "rb")
    on.exit(close(con), add = TRUE)
    readBin(con, "raw", n = 65536)
  }, error = function(e) raw(0))
  nul <- b == as.raw(0)
  if (!any(nul)) return(FALSE)
  if (length(b) >= 2 && (identical(b[1:2], as.raw(c(0xff, 0xfe))) ||
                         identical(b[1:2], as.raw(c(0xfe, 0xff))))) return(FALSE)
  even <- nul[c(TRUE, FALSE)]
  odd  <- nul[c(FALSE, TRUE)]
  !((mean(odd) > 0.9 && mean(even) < 0.1) || (mean(even) > 0.9 && mean(odd) < 0.1))
}

# Text of an uploaded data frame as valid UTF-8. readr returns the bytes of a
# file as they are, so a CSV saved by Excel in Windows-1252 / Latin-1 (e.g. a
# header "café") gives column names and values that are not valid UTF-8; Shiny
# would send them to the browser, which then drops the connection. Such names,
# values and factor levels are converted from Windows-1252 (from Latin-1 for the
# few bytes Windows-1252 leaves undefined); valid UTF-8 text is left as it is.
# If the file had such text and then contains control characters, it is binary
# data (e.g. an image renamed .csv), and this is an error.
uploadTextToUTF8 <- function(dat) {
  converted <- FALSE
  # The converted text, or NULL when 'x' is valid UTF-8 already.
  toUTF8 <- function(x) {
    bad <- !is.na(x) & !validUTF8(x)
    if (!any(bad)) return(NULL)
    converted <<- TRUE
    fixed <- iconv(x[bad], "WINDOWS-1252", "UTF-8")
    undefined <- is.na(fixed)
    fixed[undefined] <- iconv(x[bad][undefined], "latin1", "UTF-8")
    x[bad] <- fixed
    x
  }
  if (!is.null(x <- toUTF8(names(dat)))) names(dat) <- x
  for (j in seq_along(dat)) {
    col <- dat[[j]]
    if (is.character(col)) {
      if (!is.null(x <- toUTF8(col))) dat[[j]] <- x
    } else if (is.factor(col)) {
      if (!is.null(x <- toUTF8(levels(col)))) levels(dat[[j]]) <- x
    }
  }
  if (converted) {
    text <- c(names(dat), unlist(lapply(dat, function(col) {
      if (is.character(col)) col else if (is.factor(col)) levels(col)
    }), use.names = FALSE))
    if (any(grepl("[\\x01-\\x08\\x0B\\x0C\\x0E-\\x1F\\x7F]|\\xC2[\\x80-\\x9F]", text,
                  perl = TRUE, useBytes = TRUE))) {
      stop("binary data")
    }
  }
  dat
}

# Reads an uploaded data file of any format in UPLOAD_ACCEPTED_EXTENSIONS into
# a data frame. 'sheet' is only used for xls/xlsx; callers must validate it
# (e.g. req(sheet %in% readxl::excel_sheets(path))) before invoking this.
# A file that cannot be read signals uploadReadError(); value labels (SPSS,
# Stata, SAS) are dropped, keeping the stored values.
readUploadedDataFile <- function(ext, path, sheet = NULL) {
  validateUploadSize(path)
  dat <- withUploadReadError(ext, switch(tolower(ext),
        csv      = {
          if (uploadLooksBinary(path)) stop("binary data")
          readr::read_csv(path, show_col_types = FALSE)
        },
        xls      = quietExcelRead(readxl::read_xls, path, sheet),
        xlsx     = quietExcelRead(readxl::read_xlsx, path, sheet),
        txt      = {
          if (uploadLooksBinary(path)) stop("binary data")
          readr::read_tsv(path, show_col_types = FALSE)
        },
        sas7bdat = readHavenCapped(haven::read_sas, path),
        sav      = readHavenCapped(haven::read_sav, path),
        dta      = readHavenCapped(haven::read_dta, path),
        rds      = {
          obj <- readRDS(path)
          validate(need(is.data.frame(obj), ".rds file must contain a data frame."))
          obj
        },
        mtp      = read_mtp_helper(path),
        mwx      = read_minitab_xml(path),
        mpx      = read_minitab_xml(path),
        validate("Improper file format")))
  validate(need(as.numeric(NROW(dat)) * max(1, NCOL(dat)) <= UPLOAD_MAX_CELLS, uploadTooManyCellsMsg))
  # Labelled columns (haven_labelled) cannot be shown by DT or used as plain numbers.
  if (any(vapply(dat, inherits, logical(1), what = "haven_labelled"))) {
    dat <- haven::zap_labels(dat)
  }
  withUploadReadError(ext, uploadTextToUTF8(dat))
}

# For shinyvalidate rules on a file input. Evaluates 'expr' (e.g. the reactive
# that calls readUploadedDataFile) and returns the message of a validate() error
# raised by the readers (".rds file must contain a data frame.", "File is too
# large ...", ...) or of uploadReadError(), so it shows under the file input.
# shinyvalidate itself would show "An unexpected error occurred during input
# validation" for it, and a rule that swallows errors would show nothing. NULL
# when 'expr' succeeds or fails silently (req()) or for any other reason, so
# other rules apply as before.
uploadValidationMessage <- function(expr) {
  tryCatch({
    expr
    NULL
  }, error = function(e) {
    msg <- conditionMessage(e)
    if ((inherits(e, "validation") || inherits(e, "uploadReadError")) && nzchar(msg)) msg else NULL
  })
}



# SHADED AREA FUNCTION for SLR outputs

shadeHtArea <- function(df, critValue, altHypothesis) {
  
  if(altHypothesis == 'less') {
    geom_area(data = subset(df, x <= critValue),
              aes(y=y),
              fill = "#023B70",
              color = NA,
              alpha = 0.4)
    
    
  } else if (altHypothesis == 'greater') {
    geom_area(data = subset(df, x >= critValue),
              aes(y=y),
              fill = "#023B70",
              color = NA,
              alpha = 0.4)
  }
}

# TTest Plot FUNCTION for SLR outputs

hypTTestPlot <- function(testStatistic, degfree, critValue, altHypothesis){
  tTail = qt(0.999, df = degfree, lower.tail = FALSE)
  tHead = qt(0.999, df = degfree, lower.tail = TRUE)
  x <- round(seq(from = tTail, to = tHead, by = 0.1), 2)
  
  if(altHypothesis == "two.sided") {
    CVs <- c(-critValue, critValue)
    RRLabels <- c((-critValue + tTail)/2, (critValue + tHead)/2)
  } else{
    CVs <- c(critValue)
    if(altHypothesis == 'less') {
      RRLabels <- c((critValue + tTail)/2)
    } else {
      RRLabels <- c((critValue + tHead)/2)
    }
  }
  
  xSeq <- unique(sort(c(x, testStatistic, CVs, RRLabels, 0)))
  
  df <- data.frame(x = xSeq, y = dt(xSeq, degfree))
  cvDF <- filter(df, x %in% CVs)
  RRLabelsDF <- filter(df, x %in% RRLabels)
  tsDF <- filter(df, x %in% testStatistic)
  centerDF <- filter(df, x %in% c(0))
  
  htPlot <- ggplot(df, aes(x = x, y = y))
  
  if(altHypothesis == 'two.sided') {
    htPlot <- htPlot + shadeHtArea(df, -critValue, "less") +
      shadeHtArea(df, critValue, "greater")
  } else {
    htPlot <- htPlot + shadeHtArea(df, critValue, altHypothesis)
  }
  
  htPlot <- htPlot + stat_function(fun = dt,
                                   args = list(df = degfree),
                                   geom = "density",
                                   fill = NA) +
    theme_void()  +
    scale_y_continuous(breaks = NULL) +
    ylab("") +
    xlab("t") +
    geom_segment(data = filter(df, x %in% c(0)),
                 aes(x = x, xend = x, y = 0, yend = y),
                 linetype = "dotted",
                 linewidth = 0.75,
                 color='black') +
    geom_text(data = filter(df, x %in% c(0)),
              aes(x = x, y = y/2, label = "A R"),
              size = 16 / .pt,
              fontface = "bold") +
    geom_text(data = filter(df, x %in% c(0)),
              aes(x = x, y = 0, label = "0"),
              size = 14 / .pt,
              fontface = "bold",
              nudge_y = -.03) +
    geom_segment(data = tsDF,
                 aes(x = x, xend = x, y = 0, yend = y + .03),
                 linetype = "solid",
                 linewidth = 1.25,
                 color='#BD130B') +
    geom_text(data = tsDF,
              aes(x = x, y = y, label = x),
              size = 16 / .pt,
              fontface = "bold",
              nudge_y = .075) +
    geom_segment(data = cvDF,
                 aes(x = x, xend = x, y = 0, yend = y),
                 linetype = "solid",
                 lineend = 'butt',
                 linewidth = 1.5,
                 color='#023B70') +
    geom_text(data = cvDF,
              aes(x = x, y = 0, label = x),
              size = 14 / .pt,
              fontface = "bold",
              nudge_y = -.03) +
    geom_text(data = RRLabelsDF,
              aes(x = x, y = y, label = "RR"),
              size = 16 / .pt,
              fontface = "bold",
              nudge_y = .03) +
    theme(axis.title.x = element_text(size = 16,
                                      face = "bold.italic"))
  
  return(htPlot)
}

hypZTestPlot <- function(testStatistic, critValue, altHypothesis) {
  zTail <- qnorm(0.999, lower.tail = FALSE)
  zHead <- qnorm(0.999, lower.tail = TRUE)
  x     <- round(seq(from = zTail, to = zHead, by = 0.1), 2)

  if (altHypothesis == "two.sided") {
    CVs      <- c(-critValue, critValue)
    RRLabels <- c((-critValue + zTail) / 2, (critValue + zHead) / 2)
  } else {
    CVs      <- c(critValue)
    RRLabels <- if (altHypothesis == "less") c((critValue + zTail) / 2) else c((critValue + zHead) / 2)
  }

  xSeq       <- unique(sort(c(x, testStatistic, CVs, RRLabels, 0)))
  df         <- data.frame(x = xSeq, y = dnorm(xSeq))
  cvDF       <- filter(df, x %in% CVs)
  RRLabelsDF <- filter(df, x %in% RRLabels)
  tsDF       <- filter(df, x %in% testStatistic)

  htPlot <- ggplot(df, aes(x = x, y = y))

  if (altHypothesis == "two.sided") {
    htPlot <- htPlot + shadeHtArea(df, -critValue, "less") +
      shadeHtArea(df, critValue, "greater")
  } else {
    htPlot <- htPlot + shadeHtArea(df, critValue, altHypothesis)
  }

  htPlot +
    stat_function(fun = dnorm, geom = "density", fill = NA) +
    theme_void() +
    scale_y_continuous(breaks = NULL) +
    ylab("") +
    xlab("z") +
    geom_segment(data = filter(df, x %in% 0),
                 aes(x = x, xend = x, y = 0, yend = y),
                 linetype = "dotted", linewidth = 0.75, color = "black") +
    geom_text(data = filter(df, x %in% 0),
              aes(x = x, y = y / 2, label = "A R"),
              size = 16 / .pt, fontface = "bold") +
    geom_text(data = filter(df, x %in% 0),
              aes(x = x, y = 0, label = "0"),
              size = 14 / .pt, fontface = "bold", nudge_y = -.03) +
    geom_segment(data = tsDF,
                 aes(x = x, xend = x, y = 0, yend = y + .03),
                 linetype = "solid", linewidth = 1.25, color = "#BD130B") +
    geom_text(data = tsDF,
              aes(x = x, y = y, label = x),
              size = 16 / .pt, fontface = "bold", nudge_y = .075) +
    geom_segment(data = cvDF,
                 aes(x = x, xend = x, y = 0, yend = y),
                 linetype = "solid", lineend = "butt", linewidth = 1.5, color = "#023B70") +
    geom_text(data = cvDF,
              aes(x = x, y = 0, label = x),
              size = 14 / .pt, fontface = "bold", nudge_y = -.03) +
    geom_text(data = RRLabelsDF,
              aes(x = x, y = y, label = "RR"),
              size = 16 / .pt, fontface = "bold", nudge_y = .03) +
    theme(axis.title.x = element_text(size = 16, face = "bold.italic"))
}
