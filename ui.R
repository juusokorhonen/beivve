library(shiny)

# Define UI for application that draws a histogram
fluidPage(
  # Application title
  titlePanel("COVID-19 dashboard"),
  plotOutput(
    "map", 
    hover = "map_hover"),
  verbatimTextOutput("info"),
  leaflet::leafletOutput("interactive_map"),
  p()
)