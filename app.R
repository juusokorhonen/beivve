library(magrittr)
library(shiny)
library(shinythemes)
source("./global.R")

proj4_longlat <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
proj4_webmerc <- "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext  +no_defs"
proj4_robin <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

crs <- function(proj4string = NULL) {
  #' @return class crs for the given proj4string
  #' Examples:
  #' "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs" 
  if (!is.null(proj4string) || !exists("crs_")) {
    if (is.null(proj4string)) {
      proj4string <- proj4_robin   # Default projection
    }
    crs_ <<- tryCatch(
      sf::st_crs(proj4string),
      error = sf::st_crs(proj4string, use_gdal = FALSE)
    )
  }
  crs_
}

world_outline <- function(force_update = FALSE) {
  #' Generates a world outline
  #' NOTE: Thanks to 'clauswilke' at https://github.com/tidyverse/ggplot2/issues/3536
  if (!exists("world_outline_") || force_update) {
    lats <- c(90:-90, -90:90, 90)
    longs <- c(rep(c(180, -180), each = 181), 180)
    
    world_outline_ <<-
      list(cbind(longs, lats)) %>%
        sf::st_polygon() %>%
        sf::st_sfc(crs = proj4_longlat) %>% 
        sf::st_sf() %>%
        sf::st_transform(crs = crs())
  }
  world_outline_
}

proto_map <- function(force_update = FALSE) {
  if (!exists("proto_map_") || force_update) {
    cs <- country_shapes(crs = crs())
    proto_map_ <<-
      cs %>%
      ggplot2::ggplot() +
      ggplot2::geom_sf(
        data = world_outline(),
        fill = '#15190c',
        color = '#f0f8ff', size = 0.5, linetype = 0
      ) +
      ggplot2::geom_sf(
        data = sf::st_graticule(crs = crs()),
        colour = '#2f393f', size = 0.25, linetype = 1
      ) +
      ggplot2::geom_sf(
        ggplot2::aes(geometry = geometry),
        colour = '#f0f8ff', size = 0.25, fill = '#2f393f') +
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
      ggplot2::ggtitle("Unintialized") +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        text = ggplot2::element_text(family = "Fira Sans", color = "#f0f8ff"),
        axis.line = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        axis.title.x = ggplot2::element_blank(),
        axis.title.y = ggplot2::element_blank(),
        #panel.grid.minor = ggplot2::element_line(color = "#ebebe5", size = 0.25),
        #panel.grid.major = ggplot2::element_line(color = "#ebebe5", size = 0.5),
        panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        #plot.background = ggplot2::element_rect(fill = "#f5f5f2", color = NA), 
        plot.background = ggplot2::element_blank(),
        panel.background = ggplot2::element_blank(),
        #panel.background = ggplot2::element_rect(fill = "aliceblue", color = NA), 
        legend.background = ggplot2::element_rect(fill = "#f5f5f2", color = NA),
        legend.position = 'bottom',
        panel.border = ggplot2::element_blank(),
        panel.ontop = FALSE
      )
  }
  proto_map_
}

# Define UI
ui <- fluidPage(theme = shinytheme("slate"),
                # Application title
                titlePanel("COVID-19 dashboard"),
                fluidRow(
                  column(10,
                         plotOutput(
                           "map", 
                           click = "map_click",
                           width = "100%",
                           height = "800px"),
                         verbatimTextOutput("info"),
                         plotOutput(
                           "chart"
                         )
                         #leaflet::leafletOutput("interactive_map"),   # Enable on will
                  ),
                  column(2,
                         wellPanel(
                           sliderInput("date", "Date:",  
                                       min = earliestDate(), max = latestDate(), value = latestDate(),
                                       timeFormat = "%F"),
                           selectInput("data_type", 
                                     label = "Choose a variable to display",
                                     choices = list("Confirmed" = "confirmed", 
                                                    "Deaths" = "deaths",
                                                    "Recovered" = "recovered"),
                                     selected = "confirmed"),
                           selectInput("country", 
                              label = "Choose a country for analysis",
                              choices = list(
                                "Finland" = "FI",
                                "China" = "CN",
                                "Brazil" = "BR",
                                "USA" = "US"
                              ),
                              selected = "confirmed")
                      )
                  )
                )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
    output$map <- renderPlot({
      proto_map() +
        ggplot2::geom_sf(
          data = (
            country_shapes() %>%
              dplyr::inner_join(covid_daily_data_by_country() %>%
                                  dplyr::filter(
                                    date == input$date,
                                    data_type == input$data_type
                                  ), by = c("CNTR_ID", "ISO3_CODE", "NAME_ENGL")) %>%
              dplyr::mutate(
                lty = as.character(dplyr::if_else(CNTR_ID == input$country, "solid", "blank")),
                lcol = as.character(dplyr::if_else(CNTR_ID == input$country, "#a59329", "#f0f8ff")),
                lw = as.numeric(dplyr::if_else(CNTR_ID == input$country, 1,0, 0.25)))
            ), ggplot2::aes(geometry = geometry, color = lcol, size = lw, fill = value), alpha = 1.0, 
          ) +
        ggplot2::scale_linetype_identity(guide = FALSE) +
        ggplot2::scale_color_identity(guide = FALSE) +
        ggplot2::scale_size_identity(guide = FALSE) +
        viridis::scale_fill_viridis(option = "inferno", direction = 1) +
        ggplot2::ggtitle(paste(input$data_type, "on", input$date))
    }, bg = "transparent")
    
    output$chart <- renderPlot({
      covid_daily_data_for_country(input$country) %>%
        dplyr::filter(data_type == input$data_type) %>%
        ggplot2::ggplot(ggplot2::aes(x = date, y = value)) +
        ggplot2::geom_line() + 
        ggplot2::ggtitle(paste(input$country, input$data_type)) +
        ggplot2::labs(x = "Date", y = input$data_type) 
    }, bg = "transparent")
    
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
