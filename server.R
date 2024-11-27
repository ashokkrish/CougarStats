server <- function(session, input, output) {

  darkmode_toggle(inputid = 'togglemode')
  
  descStatsServer(id = "ds")
  probDistServer(id = "pd")
  sampSizeEstServer(id = "sse")
  statInfrServer(id = "si")
  regCorrServer(id = "rc")
  
}