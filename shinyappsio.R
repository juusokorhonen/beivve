library(dotenv)
#dotenv::load_dot_env('.env')   

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