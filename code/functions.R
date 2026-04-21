
# Purpose ----------------------------------------------------------------------

# Explore grenadier larvae captures using depth discrete tows and oblique 
# tows at stations with positive tows for macrourids. 
# M Paquin and E Markowitz

# Install Libraries ------------------------------------------------------------

# Here we list all the packages we will need for this whole process
# We'll also use this in our works cited page!
PKG <- c(
  
  "devtools",
  
  "ggplot2", # Create Elegant Data Visualizations Using the Grammar of Graphics
  "scales", # nicer labels in ggplot2
  "ggthemes",
  "sf",
  "ggspatial",
  "maps",
  "tidyr",
  "plyr",
  "dplyr",
  "magrittr",
  "janitor",
  "readxl",
  "stringr",
  "stringi",
  "akgfmaps", # RACE-GAP Specific # devtools::install_github("afsc-gap-products/akgfmaps", build_vignettes = TRUE)
  "pingr", # check website links
  "httr", # check website links
  "flextable" # making pretty tables
)

PKG <- unique(PKG)
for (p in PKG) {
  if(!require(p,character.only = TRUE)) {
    if (p == "akgfmaps") {
      pak::pak("afsc-gap-products/akgfmaps")
    } else {
      install.packages(p)
    }
    require(p,character.only = TRUE)}
}

## Define CRS ------------------------------------------------------------------

crs_out <- "EPSG:3338"
crs_in <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"


## Get world map ---------------------------------------------------------------

world_coordinates <- maps::map("world", plot = FALSE, fill = TRUE) |> 
  sf::st_as_sf() |>
  # sf::st_union() |> 
  sf::st_transform(crs = crs_out) |> 
  dplyr::filter(ID %in% c("USA", "Russia", "Canada")) |> 
  dplyr::mutate(ID = ifelse(ID == "USA", "Alaska", ID))

## Get place labels for map ----------------------------------------------------

place_labels <- data.frame(
  type = c("islands", "islands", "islands", "islands", 
           "mainland", "mainland", "mainland", 
           "convention line", "peninsula", 
           "survey", "survey", "survey", "survey", "survey"), 
  lab = c("Pribilof Isl.", "Nunivak", "St. Matthew", "St. Lawrence", 
          "Alaska", "Russia", "Canada", 
          "U.S.-Russia Maritime Boundary", "Alaska Peninsula", 
          "Aleutian Islands", "Gulf of Alaska", 
          "Bering\nSea\nSlope", "Eastern\nBering Sea", "Northern\nBering Sea"), 
  angle = c(0, 0, 0, 0, 0, 0, 0, 30, 45, 0, 0, 0, 0, 0), 
  lat = c(57.033348, 60.7, 61, 64.2, 
          62.296686, 62.798276, 63.722890, 
          62.319419, 56.352495, 
          53.25, 54.720787, 
          57, 57.456912, 63.905936), 
  lon = c(-167.767168, -168, -174, -170.123016, 
          -157.377210, 173.205231, -136.664024, 
          -177.049063, -159.029430, 
          -173, -154.794131, 
          -176, -162, -165)) |>
  dplyr::filter(type != "peninsula") |> 
  dplyr::filter(type != "survey") |> 
  # dplyr::mutate(
  #   color = dplyr::case_when(
  #     type == "mainland" ~ "grey80", 
  #     TRUE ~ "grey30"), 
  #   fontface = dplyr::case_when(
  #     type == "mainland" ~ "bold", 
  #     TRUE ~ "regular"),
  #   size = dplyr::case_when(
  #     type == "mainland" ~ 3, 
  #     TRUE ~ 2) ) |> 
  sf::st_as_sf(coords = c("lon", "lat"),
               crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0") |>
  sf::st_transform(crs = crs_out) 

## Determine map boundaries ----------------------------------------------------

boundaries <- data.frame(lon = c(-180, -160), # c(-180, -140)
                         lat = c(50, 64) )  |> # c(46, 66)
  sf::st_as_sf(coords = c("lon", "lat"),
               crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0") |>
  sf::st_transform(crs = crs_out) |> 
  sf::st_coordinates() |> 
  data.frame()


