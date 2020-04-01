library(rsconnect)

# Load shinyapps.io config
source("shinyappsio.R", local=TRUE)
set_shinyappsio_config()

rsconnect::deployApp(
  appName = 'analysis',
  appTitle = "Covid-19 analysis",
  appPrimaryDoc = 'server.R',
  forceUpdate = TRUE,
  lint = TRUE,
  launch.browser = FALSE
)

