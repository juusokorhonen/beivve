library(shiny)
library(leaflet)
dotenv::load_dot_env('.env')

r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

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
  
  themes <- c("CNTR")   # Countries
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
  
  match_pattern <- "^([A-Z]{4})_([A-Z]{2})_([0-9]{2}M)_([0-9]{4})_([0-9]{4})_([A-Z]{5})_([a-z\\.]+)$"
  candidates <- list.files(countries_path)
  
}

load_countries <- function() {
  
}