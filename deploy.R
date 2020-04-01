library(rsconnect)

# Load shinyapps.io config
source("shinyappsio.R", local=TRUE)
sio_config <- get_shinyappsio_config()

if (is.na(sio_config)) {
  
}