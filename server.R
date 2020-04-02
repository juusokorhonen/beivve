library(shiny)
source("./global.R")

# Define server logic required to draw a histogram
function(input, output, session) {
  output$test_map <- leaflet::renderLeaflet({
    leaflet::leaflet() %>%
      leaflet::addProviderTiles(leaflet::providers$Stamen.Toner,
                       options = leaflet::providerTileOptions(noWrap = FALSE)
      ) 
  })
  
  countries_plot <-
  countries_shapes %>%
    ggplot2::ggplot() +
    ggplot2::geom_sf(ggplot2::aes(fill = CNTR_ID)) +
    ggplot2::guides(fill = FALSE) 
   
    output$test_map_2 <- renderPlot(countries_plot)
}