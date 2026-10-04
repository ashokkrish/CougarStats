## ------------------------------------------------------------------------ #
## App-wide overrides of shiny UI helpers: conditionalPanel(), withMathJax(),
## tabsetPanel() and navbarPage().
##
## The "0_" prefix makes Shiny's R/ autoloader (which sorts file names in the
## C locale) source this file before every other file in R/, so every module
## that calls these helpers picks these versions up through normal lexical
## scoping (ui.R too). Only functions are defined here; nothing runs at load
## time apart from creating small counter environments.
## ------------------------------------------------------------------------ #

## conditionalPanel() --------------------------------------------------------
## Drop-in replacement for shiny::conditionalPanel() with the same signature.
##
## shiny::conditionalPanel(ns = ns) puts data-ns-prefix on the panel. On every
## incoming websocket message and every input change, Shiny's client then
## rebuilds a prefix-filtered copy of the whole input and output dictionaries
## for each such panel before evaluating its condition (_narrowScope in
## shiny.js), which is the main client-side startup cost of this app.
##
## In the narrowed scope 'input.foo' means input['<prefix>foo'] of the full
## scope (same for output). So when a namespace is given, the JS condition is
## rewritten to use fully namespaced ids, e.g.
##   "input.siMethod == '1'"  ->  "input['si-siMethod'] == '1'"
## and the panel is emitted without data-ns-prefix, so the client evaluates it
## against the flat dictionaries. The result is exactly equivalent.
##
## Handled references: input.id, input['id'], input["id"] and the same three
## forms for output. Everything else in the condition (string literals,
## operators, method calls on the values such as .indexOf()) is left untouched.
## If a condition contains anything the rewrite cannot prove equivalent (a
## computed key such as input[x], 'this', a method called on the input object
## itself, comments, template literals, non-ASCII text, ...), the panel falls
## back to shiny's own namespaced form, unchanged.
conditionalPanel <- function(condition, ..., ns = NS(NULL)) {
  prefix <- ns("")
  if (!is.character(prefix) || length(prefix) != 1 || is.na(prefix) || !nzchar(prefix)) {
    return(shiny::conditionalPanel(condition, ..., ns = ns))
  }

  rewritten <- .csNamespaceCondition(condition, prefix)
  if (is.null(rewritten)) {
    return(shiny::conditionalPanel(condition, ..., ns = ns))
  }

  panel <- shiny::conditionalPanel(rewritten, ...)
  panel$attribs[names(panel$attribs) == "data-ns-prefix"] <- NULL
  panel
}

## Rewrites every input/output reference in a JS condition to its fully
## namespaced form. Returns NULL when the rewrite cannot be proven equivalent.
.csNamespaceCondition <- function(condition, prefix) {
  if (!is.character(condition) || length(condition) != 1 || is.na(condition)) return(NULL)
  if (grepl("['\"\\\\[:cntrl:]]", prefix) || grepl("[^ -~\t\r\n]", condition, perl = TRUE)) return(NULL)
  if (grepl("`", condition, fixed = TRUE)) return(NULL)

  ## Tokens: string literals, identifiers, numbers, whitespace, any other char.
  tokRe <- paste0("'(?:[^'\\\\\\n]|\\\\.)*'", "|", "\"(?:[^\"\\\\\\n]|\\\\.)*\"", "|",
                  "[A-Za-z_$][A-Za-z0-9_$]*", "|", "[0-9]+(?:\\.[0-9]+)?", "|",
                  "\\s+", "|", "(?s:.)")
  toks <- regmatches(condition, gregexpr(tokRe, condition, perl = TRUE))[[1]]
  if (!identical(paste(toks, collapse = ""), condition)) return(NULL)

  isWs  <- grepl("^\\s+$", toks, perl = TRUE)
  isStr <- grepl("^['\"]", toks)
  ## Constructs that could reach the scope object some other way, or that the
  ## tokenizer cannot follow (regex literals, comments).
  if (any(toks[!isStr] %in% c("this", "function", "with", "eval", "var", "let", "const", "/"))) return(NULL)
  ## Names that resolve through Object.prototype in the narrowed scope.
  protoNames <- c("constructor", "hasOwnProperty", "isPrototypeOf", "propertyIsEnumerable",
                  "toLocaleString", "toString", "valueOf", "__proto__", "__defineGetter__",
                  "__defineSetter__", "__lookupGetter__", "__lookupSetter__")

  n <- length(toks)
  nextTok <- function(i) { j <- i + 1; while (j <= n && isWs[j]) j <- j + 1; j }
  prevTok <- function(i) { j <- i - 1; while (j >= 1 && isWs[j]) j <- j - 1; j }

  out <- character(0)
  i <- 1
  while (i <= n) {
    tk <- toks[i]
    isScopeRef <- tk %in% c("input", "output") && {
      p <- prevTok(i)
      p < 1 || toks[p] != "."
    }
    if (!isScopeRef) {
      out <- c(out, tk)
      i <- i + 1
      next
    }

    j <- nextTok(i)
    if (j <= n && toks[j] == ".") {
      k <- nextTok(j)
      if (k > n || !grepl("^[A-Za-z_$][A-Za-z0-9_$]*$", toks[k])) return(NULL)
      key <- toks[k]
      after <- k
      replacement <- paste0(tk, "['", prefix, key, "']")
    } else if (j <= n && toks[j] == "[") {
      k <- nextTok(j)
      m <- nextTok(k)
      if (k > n || !isStr[k] || m > n || toks[m] != "]") return(NULL)
      lit <- toks[k]
      q <- substr(lit, 1, 1)
      key <- substr(lit, 2, nchar(lit) - 1)
      after <- m
      replacement <- paste0(tk, "[", q, prefix, key, q, "]")
    } else {
      return(NULL)
    }
    if (key %in% protoNames) return(NULL)
    ## A method called on the input/output object itself (input.foo(...)).
    a <- nextTok(after)
    if (a <= n && toks[a] == "(") return(NULL)

    out <- c(out, replacement)
    i <- after + 1
  }
  paste(out, collapse = "")
}

## withMathJax() -------------------------------------------------------------
## Drop-in replacement for shiny::withMathJax(). It loads MathJax exactly as
## shiny does (same URL, same shiny.mathjax.url / shiny.mathjax.config options,
## same head singleton) and returns the content unchanged.
##
## shiny's version ends with a script that queues a typeset of the WHOLE
## document. Here the script typesets only the part of the page this content
## was rendered into: the closest enclosing uiOutput/htmlOutput container (the
## whole renderUI result), or, outside outputs, the element that contains the
## content. The script finds its own position through a unique id on the
## script element, so no extra element is added to the page.
##
## The MathJax script is loaded with `async`, so neither parsing of the page nor
## the start of Shiny waits for the download from the MathJax CDN (with a slow
## CDN the app used to stay blank until MathJax.js had arrived). Content that is
## on the page before MathJax.js has run -- static labels and any output that
## arrives early -- is typeset by MathJax's own startup typeset, which processes
## the whole page once MathJax has loaded; the inline scripts of such content
## do nothing (they check window.MathJax.Hub first). Content inserted after
## MathJax.js has run queues its own scoped typeset, which waits for MathJax's
## startup to finish. Checked with MathJax served normally and with every
## MathJax file held back 5 s: no TeX is left untypeset in any module.
.csMathJax <- new.env(parent = emptyenv())
.csMathJax$n <- 0L
.csMathJax$token <- format(as.hexmode(floor(as.numeric(Sys.time()) * 1000) %% 16^7))

withMathJax <- function(...) {
  path <- paste0(getOption("shiny.mathjax.url", "https://mathjax.rstudio.com/latest/MathJax.js"),
                 "?", getOption("shiny.mathjax.config", "config=TeX-AMS-MML_HTMLorMML"))

  .csMathJax$n <- .csMathJax$n + 1L
  id <- paste0("cs-mathjax-", .csMathJax$token, "-", .csMathJax$n)

  tagList(
    tags$head(singleton(tags$script(src = path, type = "text/javascript", async = NA))),
    ...,
    tags$script(id = id, HTML(sprintf(
      paste0("(function(){var s=document.getElementById('%s');",
             "var r=s&&((s.closest&&s.closest('.shiny-html-output'))||s.parentElement);",
             "if(window.MathJax&&window.MathJax.Hub)MathJax.Hub.Queue([\"Typeset\",MathJax.Hub,r||document.body]);})();"),
      id
    )))
  )
}

## tabsetPanel() and navbarPage() --------------------------------------------
## Drop-in replacements for shiny::tabsetPanel() and shiny::navbarPage() with
## the same signatures and output, except for the generated tab ids.
##
## shiny (through bslib) numbers each tabset with a random 4-digit tabset id
## (1000-9999), used in data-tabsetid, in the pane ids "tab-<id>-<n>" and in
## the links' href="#tab-<id>-<n>". With some 30 tabsets on the page, two of
## them get the same id now and then; a tab link then opens the pane of the
## other tabset, and showTab()/hideTab() can act on the wrong one. Here each
## call's tabset ids are replaced by numbers from a process-wide counter
## (10001, 10002, ...), so they are unique across the static page and the
## tabsets that renderUI() builds later. Ids that were already renumbered
## (5 digits or more, e.g. a nested tabset) are left as they are.
.csTabsets <- new.env(parent = emptyenv())
.csTabsets$n <- 10000

.csRenumberTabsets <- function(x) {
  isTag  <- function(el) inherits(el, "shiny.tag")
  isList <- function(el) (is.list(el) && !is.object(el)) || inherits(el, "shiny.tag.list")

  ## The random tabset ids created by this call.
  ids <- character(0)
  collect <- function(el) {
    if (isTag(el)) {
      v <- el$attribs[["data-tabsetid"]]
      if (length(v) == 1 && grepl("^[0-9]{4}$", v)) ids <<- union(ids, as.character(v))
      for (ch in el$children) collect(ch)
    } else if (isList(el)) {
      for (ch in el) collect(ch)
    }
  }
  collect(x)
  if (length(ids) == 0) return(x)

  ## Applies f() to every tag of the tree, keeping everything else as it is.
  walk <- function(el, f) {
    if (isTag(el)) {
      el <- f(el)
      if (length(el$children)) el$children <- walk(el$children, f)
    } else if (isList(el)) {
      for (i in seq_along(el)) {
        if (!is.null(el[[i]])) el[[i]] <- walk(el[[i]], f)
      }
    }
    el
  }
  newIds <- vapply(ids, function(id) {
    .csTabsets$n <- .csTabsets$n + 1
    format(.csTabsets$n, scientific = FALSE)
  }, character(1))

  pattern <- paste0("^(#?tab-)(", paste(ids, collapse = "|"), ")(-[0-9]+)$")
  renumber <- function(v) {
    if (!is.character(v) || length(v) != 1 || !grepl(pattern, v)) return(v)
    old <- sub(pattern, "\\2", v)
    sub(pattern, paste0("\\1", newIds[[old]], "\\3"), v)
  }
  walk(x, function(tag) {
    a <- tag$attribs
    if (!length(a)) return(tag)
    for (k in which(names(a) %in% c("id", "href", "data-target", "data-bs-target"))) {
      a[[k]] <- renumber(a[[k]])
    }
    k <- which(names(a) == "data-tabsetid")
    for (j in k) {
      v <- as.character(a[[j]])
      if (length(v) == 1 && v %in% ids) a[[j]] <- newIds[[v]]
    }
    tag$attribs <- a
    tag
  })
}

tabsetPanel <- function(..., id = NULL, selected = NULL, type = c("tabs", "pills", "hidden"),
                        header = NULL, footer = NULL) {
  .csRenumberTabsets(shiny::tabsetPanel(..., id = id, selected = selected, type = type,
                                        header = header, footer = footer))
}

navbarPage <- function(title, ..., id = NULL, selected = NULL,
                       position = c("static-top", "fixed-top", "fixed-bottom"),
                       header = NULL, footer = NULL, inverse = FALSE, collapsible = FALSE,
                       fluid = TRUE, theme = NULL, windowTitle = NA, lang = NULL) {
  .csRenumberTabsets(shiny::navbarPage(title, ..., id = id, selected = selected,
                                       position = position, header = header, footer = footer,
                                       inverse = inverse, collapsible = collapsible,
                                       fluid = fluid, theme = theme,
                                       windowTitle = windowTitle, lang = lang))
}
