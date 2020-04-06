library(magrittr)
library(shiny)
library(shinythemes)
source("./global.R")

# Define UI
ui <- fluidPage(theme = shinytheme("slate"),
                # Application title
                titlePanel("COVID-19 dashboard"),
                
                fluidRow(
                  column(3,
                         wellPanel(
                           sliderInput("date", "Date:",  
                                       min = earliestData(), max = latestData(), value = latestData(),
                                       timeFormat = "%F")
                         ),
                         selectInput("data_type", 
                                     label = "Choose a variable to display",
                                     choices = list("Confirmed" = "confirmed", 
                                                    "Deaths" = "deaths",
                                                    "Recovered" = "recovered"),
                                     selected = "confirmed")
                  ),
                  column(6,
                         plotOutput(
                           "map", 
                           click = "map_click"),
                         verbatimTextOutput("info"),
                         plotOutput(
                           "chart"
                         )
                         #leaflet::leafletOutput("interactive_map"),   # Enable on will
                  ),
                  column(3,
                         selectInput("country", 
                                     label = "Choose a country for analysis",
                                     choices = list(
                                       "Finland" = "FI",
                                       "China" = "CN",
                                       "Brazil" = "BR",
                                       "USA" = "US"
                                     ),
                                     selected = "confirmed"),
                  )
                )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$map <- renderPlot(
    {
      covid_daily_data_by_region %>%
        dplyr::filter(
          data_type == input$data_type,
          date == input$date
        ) %>%
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
        covid_daily_data %>%
          dplyr::filter(
            data_type == input$data_type,
            CNTR_ID == input$country,
            !is.na(value)
          ) %>%
          ggplot2::ggplot(ggplot2::aes(x = date, y = value)) +
          ggplot2::geom_line() + 
          #ggplot2::geom_vline(xintercept = input$date, linetype = 2) +
          ggplot2::ggtitle(paste(input$country, input$data_type)) +
          ggplot2::labs(x = "Date", y = input$data_type) +
          ggplot2::scale_y_continuous(trans = "log")
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

# Run
shinyApp(ui, server)
