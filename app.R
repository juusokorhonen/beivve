#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#library(dotenv)
dotenv::load_dot_env('.env')   

# Check for Shinyapps.IO account
get_shinyappsio_config <- function() {
    if (Sys.getenv('SHINYAPPSIO_NAME') == '') {
        return(NULL)
    }
    if (Sys.getenv('SHINYAPPSIO_TOKEN') == '') {
        return(NULL)
    }
    if (Sys.getenv('SHINYAPPSIO_SECRET') == '') {
        return(NULL)
    }
    invisible(c(
        name = Sys.getenv('SHINYAPPSIO_NAME'),
        token = Sys.getenv('SHINYAPPSIO_TOKEN'),
        secret = Sys.getenv('SHINYAPPSIO_SECRET')
    ))
}

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Leaflet map test"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
