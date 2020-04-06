library(magrittr)
library(shiny)
source("./global.R")

# Define server logic required to draw a histogram
function(input, output, session) {
  map_data <-
    covid_daily_reports_by_country_region_daily_changes %>%
    dplyr::group_by(CNTR_ID) %>%
    dplyr::left_join(world_population, by = 'CNTR_ID') %>%
    dplyr::mutate(
      f_confirmed = 100000 * confirmed / population,
      f_deaths = 100000 * deaths / population,
      f_recovered = 100000 * recovered / population
    ) %>%
    dplyr::ungroup() %>%
    tidyr::pivot_longer(cols = c('confirmed', 'deaths', 'recovered', 
                                 'f_confirmed', 'f_deaths', 'f_recovered'),
                        names_to = 'data_type')
  
  output$map <- renderPlot(
    {
      map_data %>%
        dplyr::filter(
          data_type == input$data_type,
          date <= input$date
        ) %>%
        dplyr::arrange(date) %>%
        dplyr::group_by(CNTR_ID, date, data_type) %>%
        dplyr::summarise(
          value = dplyr::last(value)
        ) %>%
        dplyr::ungroup() %>%
        dplyr::right_join(countries_shapes, by = 'CNTR_ID') %>%
        ggplot2::ggplot() +
        ggplot2::geom_sf(ggplot2::aes(geometry = geometry, fill = value), size = 0.25) +
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
        ggplot2::ggtitle(paste(input$data_type)) +
        theme_map(
          legend.position = "bottom"
        ) 
    })
    
    output$chart <- renderPlot(
      {
        map_data %>%
          dplyr::filter(
            data_type == input$data_type,
            CNTR_ID == input$country,
            !is.na(value)
          ) %>%
          ggplot2::ggplot(ggplot2::aes(x = date, y = value)) +
          ggplot2::geom_line() + 
          ggplot2::geom_vline(xintercept = input$date, linetype = 2) +
          ggplot2::ggtitle(paste(input$country, input$data_type)) +
          ggplot2::labs(x = "Date", y = input$data_type)
      }
    )
    
    output$info <- renderText({
      paste0("x=", input$map_click$x, "\ny=", input$map_click$y)
    })
    
    # # Disabled for now
    # #pal <- viridis::viridis_pal(direction = -1, option = 'magma')
    # #pal_func <- leaflet::colorNumeric("viridis", reverse = FALSE, domain = NULL)
    # pal_func <- leaflet::colorBin("magma", reverse = TRUE, domain = NULL, bins = 10)
    # 
    # map_data_with_colors <-
    # map_data %>%
    #   dplyr::mutate(
    #     confirmed_col = pal_func(f_confirmed)
    #   ) 
    # 
    # output$interactive_map <- leaflet::renderLeaflet({
    #   leaflet::leaflet() %>%
    #     leaflet::addProviderTiles(leaflet::providers$Stamen.TonerBackground,
    #                               options = leaflet::providerTileOptions(noWrap = TRUE)
    #     ) %>%
    #     leaflet::addPolygons(data = map_data_with_colors, 
    #               fillColor = ~confirmed_col,
    #               fillOpacity = 0.50,
    #               stroke = TRUE,
    #               color = "#ebebe5",
    #               weight = 1,
    #               layerId = ~CNTR_ID)
    # })
    # 
    # map_data_with_colors %>%
    #   dplyr::select(confirmed_col)
}

# map_data %>%
#   dplyr::filter(
#     data_type == "confirmed",
#     CNTR_ID == "FI",
#     !is.na(value)
#   ) %>%
# head()
