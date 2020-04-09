library(shiny)
library(leaflet)
library(sf)
dotenv::load_dot_env('.env')

#%% Function definitions

setRefClass("Database",
            fields=list(
              created = "POSIXct",
              country_names = "data.frame",
              country_filenames = "data.frame",
              country_data_filename = "character",
              country_shapes = "sf",
              country_shapes_crs = "crs",
              covid_daily_reports = "data.frame",
              world_population = "data.frame",
              region_to_country = "data.frame",
              covid_daily_data = "data.frame",
              covid_daily_data_by_country = "data.frame"
            )
)

is_initialized <- function(db, field_name) {
  is.list(db[[field_name]]) && length(db[[field_name]]) > 0
}

database <- function(force_reset = FALSE) {
  #' Lazy loads a database
  #' @param force_reset -- Force a full reset of the database object
  #' @return -- List
  if (force_reset && exists("database_")) {
    remove("database_", envir = .GlobalEnv)
  }
  if (!exists("database_")) {
    database_ <<- new("Database", created = lubridate::now())
  }
  database_
}

country_names <- function() {
  #' Lazy loads country names
  #' @return -- Dataframe of country names
  db <- database()
  if (!is_initialized(db, "country_names")) {
    names_file <- Sys.getenv('COUNTRIES_NAMES_PATH', "data/countries/CNTR_AT_2016.csv")
    
    db$country_names <- 
      readr::read_csv(names_file, skip = 1, na = c("", "-"),
                    col_names = c('CNTR_ID', 'CNTR_NAME', 'NAME_ENGL', 'ISO3_CODE'),
                    col_types = c(
                        CNTR_ID = 'c',
                        CNTR_NAME = 'c',
                        NAME_ENGL = 'c',
                        ISO3_CODE = 'c'
                      )
                    )
  }
  db$country_names
}

country_filenames <- function() {
  #' Lazy loads filenames for country shapefiles that are available
  #' -- Dataframe of country shapefiles with metadata
  db <- database()
  if (!is_initialized(db, "country_filenames")) {
    
    countries_path <- Sys.getenv('COUNTRIES_PATH', "data/countries")
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
    
    db$country_filenames <-
      files_data %>%
        dplyr::mutate(
          path = countries_path,
          layer = substring(filename, 1, nchar(filename)-nchar(filetype)-1)
        )
  }
  db$country_filenames
}

country_shapes <- function(filename = "CNTR_RG_60M_2016_4326.shp.zip", crs = NULL) {
  #' Lazy loads a shapefile with country outlines
  #' File is searched in the path set by COUNTRIES_PATH (default: 'data/countries')
  #' @param filename -- Name of file to load in
  #' @return Class sf -- Country shapefile data
  db <- database()
  if (!is_initialized(db, "country_shapes") || db$country_data_filename != filename) {
    db$country_data_filename <- filename
    dir_name <- Sys.getenv('COUNTRIES_PATH', "data/countries")
    base_name <- filename
  
    #base_name = basename(filename)
    #dir_name = dirname(filename)
    
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
    
    db$country_shapes <- 
      sf::read_sf(paste0(dir_name, "/", shp_file)) 
    
    db$country_shapes_crs <-
      db$country_shapes %>%
        sf::st_crs()
  }
  if (!is.null(crs) && crs != db$country_shapes_crs) {
    crs_from <- db$country_shapes_crs$proj4string
    crs_to <- ifelse(is(crs, "crs"), crs$proj4string, crs)
    crs_transform <- paste(crs_from, "+to", crs_to)
    
    tryCatch(
      db$country_shapes <- 
        db$country_shapes %>%
        sf::st_transform(crs = crs_transform),
      error = db$country_shapes <- 
        db$country_shapes %>%
        sf::st_transform(crs = crs_transform, use_gdal = FALSE)
    )
    db$country_shapes_crs <-
      db$country_shapes %>%
        sf::st_crs()
  }
  db$country_shapes
}

country_shapes_crs <- function(use_proj4string = TRUE) {
  #' Returns the current CRS for the country shapes.
  #' @return Either string or class crs
  db <- database()
  if (!is_initialized(db, "country_shapes_crs")) {
    return(NULL)
  }
  
  if (use_proj4string) {
    db$country_shapes_crs$proj4string
  } else { 
    db$country_shapes_crs
  }
}

covid_daily_reports <- function(force_update = FALSE) {
  #' Reads in and returns the daily COVID-19 reports
  #' Note: reads data from COVID_DAILY_REPORTS_PATH (default: data/csse_covid_19_daily_reports)
  #' @return -- Dataframe
  db <- database()
  if (!is_initialized(db, "covid_daily_reports") || force_update) {
    
    path <- Sys.getenv('COVID_DAILY_REPORTS_PATH', "data/csse_covid_19_daily_reports")
    
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
    
    db$covid_daily_reports <-
      daily_reports_files  %>%
        purrr::map_df(process_file) %>%
        dplyr::mutate(
          last_update = lubridate::parse_date_time(last_update, c("mdY HM", "mdy HM", "Ymd HMS")),
          date = lubridate::as_date(last_update)
        )
  }
  
  db$covid_daily_reports
}

# World population
world_population <- function() {
  db <- database()
  if (!is_initialized(db, "world_population")) {
              
    path <- Sys.getenv('WORLD_POPULATION_PATH', "data/world_population/API_SP.POP.TOTL_DS2_en_csv_v2_887275.csv")
    if (!file.exists(path)) {
      stop("WORLD_POPULATION_PATH does not exist.")
    }  
    
    col_names <- c("country_name", "country_code", "indicator_name", "indicator_code",
                   1960:2019, "2020")
    
    col_types <- readr::cols(
        .default = readr::col_double(),
        country_name = readr::col_character(),
        country_code = readr::col_character(),
        indicator_name = readr::col_character(),
        indicator_code = readr::col_character(),
        `2019` = readr::col_skip(),
        `2020` = readr::col_skip()
    )
    
    db$world_population <-
      readr::read_csv(path, skip = 5, 
                          col_names = col_names,
                          col_types = col_types,
      ) %>%
      tidyr::pivot_longer(c(-country_name, -country_code, -indicator_name, -indicator_code), names_to = 'year') %>%
      dplyr::group_by(country_code) %>%
      dplyr::summarize(population = dplyr::last(value)) %>%
      dplyr::left_join(country_names(), by = c("country_code" = "ISO3_CODE")) %>%
      dplyr::ungroup()
  }
  
  db$world_population
}

# Store all possibilities in regions_mapping
region_to_country <- function() {
  #' Lazy load mapping regions
  #' @return -- Dataframe of region names
  db <- database()
  if (!is_initialized(db, "region_to_country")) {
  
    manual_regions <-
      readr::read_csv('data/manual_regions_mapping.csv', skip = 1,
                      col_names = c("country_region", "CNTR_ID"), 
                      col_types = readr::cols(
                        country_region = readr::col_character(),
                        CNTR_ID = readr::col_character()
                      )) %>%
        dplyr::left_join(country_names(), by = "CNTR_ID")
    
    db$region_to_country <-
      country_names() %>%
        dplyr::bind_rows(manual_regions) %>%
        dplyr::mutate(
          country_region = dplyr::if_else(is.na(country_region), NAME_ENGL, country_region)
      )
  }
  
  db$region_to_country
}

date_range <- function() {
  #' Date range for data
  #' @return -- Dataframe of dates
  data.frame(date = seq(
    covid_daily_reports()$date %>% min(), 
    covid_daily_reports()$date %>% max(), 
    by='days')
  )
}

# !!! Daily data
covid_daily_data <- function(force_update = FALSE, difference_days = 1) {
  db <- database()
  if (!is_initialized(db, "covid_daily_data") || force_update) {
    db$covid_daily_data_by_country <- data.frame()   # Reset also this dataframe
    db$covid_daily_data <-
      covid_daily_reports() %>%
        dplyr::arrange(date) %>% 
        dplyr::mutate(province_state = dplyr::na_if(province_state, "None")) %>% 
        dplyr::left_join(region_to_country(), by = "country_region") %>% 
        dplyr::select(date, CNTR_ID, ISO3_CODE, NAME_ENGL, country_region, province_state, confirmed, deaths, recovered) %>% 
        #dplyr::mutate(CNTR_ID = as.factor(CNTR_ID)) %>%
        dplyr::right_join(date_range(), by = 'date') %>%   ## Fill in missing dates
        dplyr::filter(!is.na(CNTR_ID)) %>%   # NOTE: Fixes situation where there are no data for given date
        dplyr::select(date, CNTR_ID, ISO3_CODE, NAME_ENGL, country_region, province_state, confirmed, deaths, recovered) %>% 
        tidyr::complete(date, tidyr::nesting(CNTR_ID, ISO3_CODE, NAME_ENGL, country_region, province_state)) %>% 
        dplyr::group_by(CNTR_ID) %>%
        dplyr::arrange(date) %>% 
        tidyr::fill(confirmed, deaths, recovered, .direction = 'down') %>% 
        dplyr::ungroup() %>%
        tidyr::replace_na(list(confirmed = 0, deaths = 0, recovered = 0)) %>% 
        dplyr::mutate(
          prev_confirmed = dplyr::lag(confirmed, n = difference_days, default = 0),
          prev_deaths = dplyr::lag(deaths, n = difference_days, default = 0),
          prev_recovered = dplyr::lag(recovered, n = difference_days, default = 0),
          d_confirmed = confirmed - prev_confirmed,
          d_deaths = deaths - prev_deaths,
          d_recovered = recovered - prev_recovered
        ) %>% 
        dplyr::select(-prev_confirmed, -prev_deaths, -prev_recovered) %>%
        tidyr::replace_na(list(d_confirmed = 0, d_deaths = 0, d_recovered = 0)) %>% 
        tidyr::pivot_longer(c(confirmed, deaths, recovered,
                              d_confirmed, d_deaths, d_recovered), names_to = "data_type") %>%
        dplyr::arrange(date, CNTR_ID, data_type)
  }
  db$covid_daily_data
}
  
covid_daily_data_by_country <- function(aggregate = TRUE, force_update = FALSE) {
  #' COVID-19 daily data aggregated by country
  #' @return -- Dataframe
  db <- database()
  if (!is_initialized(db, "covid_daily_data_by_country") || force_update) {
    db$covid_daily_data_by_country <-
      covid_daily_data(force_update) %>%
        dplyr::group_by(date, CNTR_ID, ISO3_CODE, NAME_ENGL, country_region, province_state, data_type) %>%
        dplyr::summarise(value = sum(value)) %>%
        dplyr::ungroup() 
  }
  if (aggregate) {
    db$covid_daily_data_by_country %>%
      dplyr::group_by(date, CNTR_ID, ISO3_CODE, NAME_ENGL, data_type) %>%
      dplyr::summarise_if(is.numeric, sum) %>%
      dplyr::ungroup()
  } else {
    db$covid_daily_data_by_country 
  }
}

covid_daily_data_for_country <- function(country, aggregate = TRUE, force_update = FALSE) {
  #' COVID-19 daily data for given country
  #' @param country -- Two letter ID for country
  #' @param aggregate -- If TRUE regions within the country are aggregated together
  #' @return -- Dataframe
  covid_daily_data_by_country(aggregate = aggregate, force_update = force_update) %>%
    dplyr::filter(CNTR_ID == country)
}

earliestDate <- function(country = NULL) {
  #' Returns the earlies date for given country or for all data
  #' @return -- Date
  if (!is.null(country)) {
    covid_daily_data_for_country(country)$date %>% min()
  } else {
    covid_daily_data_by_country()$date %>% min()
  }
}

latestDate <- function(country = NULL) {
  #' Returns the latest date for given country or for all data
  #' @return -- Date
  if (!is.null(country)) {
    covid_daily_data_for_country(country)$date %>% max()
  } else {
    covid_daily_data_by_country()$date %>% max()
  }
}

today <- function() {
  lubridate::now("GMT") %>% lubridate::date()
}

pt_to_mm <- function(x) {
  pt <- grid::unit(x, "pt")
  grid::convertUnit(pt, "mm")
}


