library(shiny)

# Define server logic required to draw a histogram
function(input, output, session) {
  output$test_map <- leaflet::renderLeaflet({
    leaflet::leaflet() %>%
      leaflet::addProviderTiles(leaflet::providers$Stamen.Toner,
                       options = leaflet::providerTileOptions(noWrap = FALSE)
      ) 
  })
}