library(shiny)
library(leaflet)
dotenv::load_dot_env('.env')

#%% Function definitions

load_country_names <- function() {
  names_file <- Sys.getenv('COUNTRIES_NAMES_PATH')
  readr::read_csv(names_file,
                  col_names = c('CNTR_ID', 'CNTR_NAME', 'NAME_ENGL', 'ISO3_CODE'),
                  col_types = c(
                      CNTR_ID = 'c',
                      CNTR_NAME = 'c',
                      NAME_ENGL = 'c',
                      ISO3_CODE = 'c'
                    )
                  )
}

load_country_file_names <- function() {
  countries_path <- Sys.getenv('COUNTRIES_PATH')
  if (!dir.exists(countries_path)) {
    stop("COUNTRIES_PATH does not exist.")
  }
  
  themes <- c("CNTR")   # Theme: Countries
  spatial_types <- c("BN", "RG", "LN")   # boundaries (multilines); RG: regions (multipolygons); LB: labels (points)
  resolutions <- c("60M", "20M", "10M", "03M", "01M")
  release_years <- c("2016", "2013", "2010", "2006", "2001")
  projections <- c("4326", "3035", "3857")   # EPSG:4326 (WGS84, coordinates in decimal degrees);
                                             # EPSG:3035 (ETRS 1989 in Lambert Azimutal projection with centre in E52N10, coordinates in meters);
                                             # EPSG:3857 (WGS84 Web Mercator Auxiliary Sphere, coordinates in meters)
  subsets <- c("COASTL", "INLAND")   # coastal lines; inland lines
                                     # Coastal and inland lines are complementary - both together form full set of country boundaries. 
                                     # File with no subset code consists of both coastal and inland boundaries.
  data_formats <- c("gdb", "shp.zip", "geojson", "json", "pbf", "svg.zip")
  
  #filename_pattern <- "CNTR_BN_01M_2016_3035_COASTL.shp.zip"
  
  match_pattern <- paste0(
    "(CNTR)",
    "\\_(BN|RG|LN)",   # Spatial type
    "\\_?([0-9]{2}M)?",  # Scale
    "\\_([0-9]{4})",  # Year
    "\\_([0-9]{4})",  # Projection
    "\\_?(COASTL|INLAND)?",  
    "\\.(gdb|shp\\.zip|geojson|json|pbf|svg\\.zip)$"
  )
  files <- list.files(countries_path, pattern = match_pattern)
  col_names <- c("filename", "theme", "spatial_type", "scale", "year", "projection", "boundaries", "filetype")
  files_data <- purrr::map(files, function(x) stringr::str_match(x, match_pattern) %>% dplyr::tibble(., .name_repair = 'unique'))
  files_data <- matrix(unlist(files_data), nrow=length(files_data), byrow=T) %>% dplyr::as_tibble()
  names(files_data) <- col_names
  files_data %>%
    dplyr::mutate(
      path = countries_path,
      layer = substring(filename, 1, nchar(filename)-nchar(filetype)-1)
    )
}

load_countries <- function(file_name) {
  base_name = basename(file_name)
  dir_name = dirname(file_name)
  
  if (stringr::str_ends(base_name, ".zip")) {
    zip_file <- base_name
    shp_file <- stringr::str_remove(base_name, ".zip$")
    
    temp_dir <- tempdir()
    #print("Extrating from zip file.")
    
    unzip(paste0(dir_name, "/", zip_file), junkpaths = TRUE, exdir = temp_dir, overwrite = TRUE)
    dir_name <- temp_dir
  } else {
    shp_file <- base_name
  }
    
  if (!file.exists(paste0(dir_name, "/", shp_file))) {
      stop("Cannot find data source file.")
  }
  
  data <- sf::read_sf(paste0(dir_name, "/", shp_file))
  data
}

read_covid_daily_reports <- function() {
  path <- Sys.getenv('COVID_DAILY_REPORTS_PATH')
  
  if (!dir.exists(path)) {
    exit("Path does not exist! Please set COVID_DAILY_REPORTS_PATH.")
  }
  
  
  
}

#%% Load values into variables

countries_names <- load_country_names()
countries_files <- load_country_file_names()
countries_shapes <- load_countries("/Users/jtkorho2/Developer/beivvi/countries/ref-countries-2016-60m.shp/CNTR_RG_60M_2016_4326.shp.zip")
