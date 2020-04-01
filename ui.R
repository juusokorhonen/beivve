library(shiny)

# Define UI for application that draws a histogram
fluidPage(
  # Application title
  titlePanel("Leaflet map test2"),
  
  leaflet::leafletOutput("test_map"),
  p()
)