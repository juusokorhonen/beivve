library(shiny)
library(leaflet)
dotenv::load_dot_env('.env')

#%% Function definitions

load_country_names <- function() {
  names_file <- Sys.getenv('COUNTRIES_NAMES_PATH')
  readr::read_csv(names_file, skip = 1,
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

load_covid_daily_reports <- function() {
  path <- Sys.getenv('COVID_DAILY_REPORTS_PATH')
  
  if (!dir.exists(path)) {
    exit("Path does not exist! Please set COVID_DAILY_REPORTS_PATH.")
  }
  
  process_file <- function(file_name) {
    file_path <- paste0(path, "/", file_name)
    header <- readr::read_lines(file_path, n_max = 1)
    if (header == "FIPS,Admin2,Province_State,Country_Region,Last_Update,Lat,Long_,Confirmed,Deaths,Recovered,Active,Combined_Key") {
      # New type  header
      col_names <- c(
        "fips",
        "admin_2",
        "province_state",
        "country_region",
        "last_update",
        "latitude",
        "longitude",
        "confirmed",
        "deaths",
        "recovered",
        "active",
        "combined_key"
      )
      col_types <- readr::cols(
        fips = readr::col_character(),
        admin_2 = readr::col_character(),
        province_state = readr::col_character(),
        country_region = readr::col_character(),
        #Last_Update = readr::col_datetime(format = "%Y-%m-%d %H:%M:%S"),
        last_update = readr::col_character(),
        latitude = readr::col_double(),
        longitude = readr::col_double(),
        confirmed = readr::col_double(),
        deaths = readr::col_double(),
        recovered = readr::col_double(),
        active = readr::col_double(),
        combined_key = readr::col_character()
      )
    } else if (header == "Province/State,Country/Region,Last Update,Confirmed,Deaths,Recovered") {
       # Old type header
      col_names <- c(
        "province_state",
        "country_region",
        "last_update",
        "confirmed",
        "deaths",
        "recovered"
      )
      col_types <- readr::cols(
        `province_state` = readr::col_character(),
        `country_region` = readr::col_character(),
        #`Last Update` = readr::col_datetime(format = "%m/%d/%Y %H:%M"),
        `last_update` = readr::col_character(),
        confirmed = readr::col_double(),
        deaths = readr::col_double(),
        recovered = readr::col_double()
      )
    } else if (header == "Province/State,Country/Region,Last Update,Confirmed,Deaths,Recovered,Latitude,Longitude") {
      col_names <- c(
        "province_state",
        "country_region",
        "last_update",
        "confirmed",
        "deaths",
        "recovered",
        "latitude",
        "longitude"
      )
      col_types <- readr::cols(
        `province_state` = readr::col_character(),
        `country_region` = readr::col_character(),
        #`Last Update` = readr::col_datetime(format = "%m/%d/%Y %H:%M"),
        `last_update` = readr::col_character(),
        confirmed = readr::col_double(),
        deaths = readr::col_double(),
        recovered = readr::col_double(),
        latitude = readr::col_double(),
        longitude = readr::col_double()
      )
    } else {
      print(header)
      stop("Could not recognize header")
    }
    readr::read_csv(file_path, col_names = col_names, col_types = col_types, skip = 1)
  }
  
  daily_reports_files <- list.files(path, pattern = ".csv$")
  daily_reports_files  %>%
    purrr::map_df(process_file) %>%
    dplyr::mutate(
      last_update = lubridate::parse_date_time(last_update, c("mdY HM", "mdy HM", "Ymd HMS")),
      date = lubridate::as_date(last_update)
    )
}

#%% Load values into variables

countries_names <- load_country_names()
countries_files <- load_country_file_names()
countries_shapes <- load_countries("/Users/jtkorho2/Developer/beivvi/countries/ref-countries-2016-60m.shp/CNTR_RG_60M_2016_4326.shp.zip")

covid_daily_reports <- load_covid_daily_reports()

covid_daily_reports_country_regions <-
covid_daily_reports %>%
  dplyr::distinct(country_region)

covid_daily_reports_by_country_region <-
covid_daily_reports %>%
  dplyr::group_by(date, country_region) %>%
  dplyr::summarise(
    confirmed = sum(confirmed, na.rm = TRUE),
    deaths = sum(deaths, na.rm = TRUE),
    recovered = sum(recovered, na.rm = TRUE)
  )
  
covid_daily_reports_country_regions %>%
  left_join(by = )
