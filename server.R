library(shiny)
source("./global.R")

# Define server logic required to draw a histogram
function(input, output, session) {
  # Get latest values for each country
  covid_latest_by_country <-
  covid_daily_reports_by_country_region_daily_changes %>%
    dplyr::group_by(CNTR_ID) %>%
    dplyr::arrange(date) %>%
    dplyr::summarise(
      confirmed = dplyr::last(confirmed),
      deaths = dplyr::last(deaths),
      recovered = dplyr::last(recovered)
    )
  
  map_data <-
  countries_shapes %>%
    dplyr::left_join(covid_latest_by_country, by = 'CNTR_ID') %>% 
    dplyr::group_by(CNTR_ID) %>%
    dplyr::left_join(world_population, by = 'CNTR_ID') %>%
    dplyr::mutate(
      f_confirmed = 100000 * confirmed / population,
      f_deaths = 100000 * deaths / population,
      f_recovered = 100000 * recovered / population
    ) %>%
    dplyr::ungroup()
  
  countries_plot <-
  map_data %>% 
    ggplot2::ggplot() +
    ggplot2::geom_sf(ggplot2::aes(geometry = geometry, fill = f_confirmed), size = 0.25) +
    ggplot2::coord_sf(crs = sf::st_crs('+proj=robin')) +
    viridis::scale_fill_viridis(option = "magma", direction = -1) +
    ggplot2::guides(
      fill = ggplot2::guide_colorbar(
        direction = "horizontal",
        barheight = grid::unit(2, units = "mm"),
        barwidth = grid::unit(50, units = "mm"),
        draw.ulim = F,
        title.position = 'top',
        # some shifting around
        title.hjust = 0.5,
        label.hjust = 0.5
      )) +
    theme_map(
      legend.position = "bottom"
    )
   
    output$map <- renderPlot(
      {
        countries_plot
      })
    
    output$info <- renderText({
      paste0("x=", input$map_hover$x, "\ny=", input$map_hover$y)
    })
    
    #pal <- viridis::viridis_pal(direction = -1, option = 'magma')
    #pal_func <- leaflet::colorNumeric("viridis", reverse = FALSE, domain = NULL)
    pal_func <- leaflet::colorBin("magma", reverse = TRUE, domain = NULL, bins = 10)
    
    map_data_with_colors <-
    map_data %>%
      dplyr::mutate(
        confirmed_col = pal_func(f_confirmed)
      ) 
    
    output$interactive_map <- leaflet::renderLeaflet({
      leaflet::leaflet() %>%
        leaflet::addProviderTiles(leaflet::providers$Stamen.TonerBackground,
                                  options = leaflet::providerTileOptions(noWrap = TRUE)
        ) %>%
        leaflet::addPolygons(data = map_data_with_colors, 
                  fillColor = ~confirmed_col,
                  fillOpacity = 0.50,
                  stroke = TRUE,
                  color = "#ebebe5",
                  weight = 1,
                  layerId = ~CNTR_ID)
    })
    
    map_data_with_colors %>%
      dplyr::select(confirmed_col)
}