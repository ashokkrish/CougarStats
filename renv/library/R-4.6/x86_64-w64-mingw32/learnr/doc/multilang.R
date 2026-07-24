## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## Language codes and names borrowed from BootBox
# library(rvest)
# read_html("https://bootboxjs.com/v5.x/documentation.html") %>%
#   html_table() %>%
#   .[[6]] %>%
#   {dplyr::bind_rows(.[, 1:2], .[, 3:4])}

languages <- c("Arabic","Bulgarian","Czech",
    "German","English","Estonian","Farsi / Persian",
    "French / Français","Croatian","Indonesian","Japanese",
    "Korean","Latvian","Norwegian","Portuguese","Slovak",
    "Albanian","Swahili","Thai","Ukrainian",
    "Chinese (People's Republic of China)","Azerbaijani",
    "Portuguese - Brazil","Danish","Greek","Spanish / Español","Basque",
    "Finnish","Hebrew","Hungarian","Italian","Georgian",
    "Lithuanian","Dutch","Polish","Russian","Slovenian",
    "Swedish","Tamil","Turkish","Vietnamese",
    "Chinese (Taiwan / Republic of China)")

names(languages) <- c("ar","bg_BG","cs","de","en",
  "et","fa","fr","hr","id","ja","ko","lv","no",
  "pt","sk","sq","sw","th","uk","zh_CN","az","br",
  "da","el","es","eu","fi","he","hu","it","ka","lt",
  "nl","pl","ru","sl","sv","ta","tr","vi","zh_TW")


i18n_custom_language_defaults <- function(parent_key, docs = TRUE) {
  default <- learnr:::i18n_translations()$en$translation
  parent_key <- match.arg(parent_key, names(default))
  x <- default[[parent_key]]
  if (!isTRUE(docs)) return(x)
  paste0(
    parent_key, ":\n",
    paste0("  ", names(x), ": ", unname(unlist(x)), collapse = "\n"),
    sep = ""
  )
}

learnr_langs <- setdiff(names(learnr:::i18n_translations()), "emo")
learnr_supported_languages <- knitr::combine_words(paste0('`"', learnr_langs, '"`'))
bb_langs_code <- paste0("`", names(languages), "`")

## ----echo=FALSE, results="asis"-----------------------------------------------
for (lng in learnr_langs) {
  lng_name <- if (lng %in% names(languages)) paste(" for", languages[lng])
  if (lng == "en") 
    lng_name <- paste(lng_name, "(default)")
  if (lng == "zh")
    lng_name <- " for Chinese"
  cat("\n- `", lng, "`", lng_name, sep = "")
}

## ----eval=FALSE---------------------------------------------------------------
# jsonlite::write_json(
#   list(
#     button = list(runcode = "Ejecutar"),
#     text = list(startover = "Empezar de nuevo")
#   ),
#   path = "tutorial_es.json",
#   auto_unbox = TRUE
# )

## ----eval=FALSE---------------------------------------------------------------
# jsonlite::write_json(
#   list(
#     en = list(
#       button = list(runcode = "Run the code"),
#       text = list(startover = "Restart the tutorial")
#     ),
#     es = list(
#       button = list(runcode = "Ejecutar"),
#       text = list(startover = "Empezar de nuevo")
#     )
#   ),
#   path = "custom_language.json",
#   auto_unbox = TRUE
# )

