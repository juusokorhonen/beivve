library(shiny)
library(shinythemes)

today <- lubridate::now("GMT") %>% lubridate::date()

# Define UI for application that draws a histogram
fluidPage(theme = shinytheme("slate"),
  # Application title
  titlePanel("COVID-19 dashboard"),
  
  fluidRow(
    column(3,
           wellPanel(
             sliderInput("date", "Date:",  
                         min = lubridate::date("2020-01-22"), max = today, value = today,
                         timeFormat = "%F")
           ),
           selectInput("data_type", 
                       label = "Choose a variable to display",
                       choices = list("confirmed", 
                                      "deaths",
                                      "recovered", 
                                      "f_confirmed",
                                      "f_deathrs",
                                      "f_recovered"),
                       selected = "confirmed"),
           
    ),
    column(6,
      plotOutput(
        "map", 
        hover = "map_hover"),
      verbatimTextOutput("info")
      #leaflet::leafletOutput("interactive_map"),   # Enable on will
    ),
    column(3,
          p("Hello")
    )
  )
)