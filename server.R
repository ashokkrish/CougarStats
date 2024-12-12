server <- function(session, input, output) {

  darkmode_toggle(inputid = 'togglemode')

  descStatsServer(id = "ds")
  probDistServer(id = "pd")
  sampSizeEstServer(id = "sse")
  statInfrServer(id = "si")

  ## Regression and correlation servers
  uploadedTibble <- import_server("dataImport", return_class = "tbl_df")
  selectedVariables <- regressionCorrelationServer(id = "rc", uploadedTibble)
  singleRegressionCorrelationServer(id = "src", uploadedTibble, selectedVariables)
  multipleRegressionCorrelationServer(id = "mrc", uploadedTibble, selectedVariables)
}
