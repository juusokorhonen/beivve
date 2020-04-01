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

set_shinyappsio_config <- function() {
  sio_config <- get_shinyappsio_config()
  if (is.null(sio_config)) {
    stop("No shinyapps.io config found. Cannot proceed.")
  }
  rsconnect::setAccountInfo(
    name = sio_config[['name']], 
    token = sio_config[['token']], 
    secret = sio_config[['secret']])
}