server <- function(session, input, output) {
  descStatsServer(id = "ds")
  probDistServer(id = "pd")
  sampSizeEstServer(id = "sse")
  statInfrServer(id = "si")
  regressionAndCorrelationServer(id = "rc")
  machineLearningServer(id = "ml")
}
