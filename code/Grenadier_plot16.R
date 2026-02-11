
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
      devtools::install_github("afsc-gap-products/akgfmaps", build_vignettes = TRUE)
    } else {
      install.packages(p)
    }
    require(p,character.only = TRUE)}
}

## Define CRS ------------------------------------------------------------------

crs_out <- "EPSG:3338"
crs_in <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

# Wrangle data -----------------------------------------------------------------

# larval_dat<-read_excel("~/My Desktop/r-tidyverse/GrenadierLarvlxy_time_step_target_yrsCanyons.xlsx",
larval_dat <- read_excel("./data/GrenadierLarvlxy_time_step_target_yrsCanyons.xlsx",
                         sheet = "GrenadierLarvlxy_time_step_targ") |>
  dplyr::filter(!is.na(Canyon)) |>
  dplyr::filter(!is.na(Corrected_Length)) |>
  dplyr::arrange((Corrected_Length)) |>
  dplyr::mutate(
    
    # same difference between bin values
    # size_bin1 = cut(x = Corrected_Length, breaks = c(8.5:30.5)), 
    
    # unique bins
    #size_bin1 = cut(x = Corrected_Length, breaks = c(.1, 7.9, 10.5, 12.5, 14.5, 16.5, 18.5, 20.5, 100)), # note .1 and 100 for values beyond normal bins
    size_bin1 = cut(x = Corrected_Length, breaks = c(.1, 7.9, 10.5, 12.5, 14.5, 16.5, 18.5, 20.5, 22.5, 24.5, 100)), # note .1 and 100 for values beyond normal bins
    size_bin_label = as.character(size_bin1), 
    size_bin_label = paste0(
      as.numeric(gsub(pattern = "(", replacement = "", fixed = TRUE, 
                      x = sapply(strsplit(split = ",", x = size_bin_label, fixed = TRUE),"[[",1)))+.1,
      "-", 
      gsub(pattern = "]", replacement = "", fixed = TRUE, 
           x = sapply(strsplit(split = ",", x = size_bin_label, fixed = TRUE),"[[",2)), 
      " mm"), 
    size_bin_label = factor(x = size_bin_label, 
                            levels = unique(size_bin_label), 
                            labels = unique(size_bin_label), 
                            ordered = TRUE), 
    canyon_title = dplyr::case_when(
      Canyon == "Bristol Cyn" ~ "Bristol\nCyn", # "\n" is a special character that makes a new line :) 
      Canyon == "Adjacent slope to Cyns" ~ "Adjacent\nSlope to Cyns", 
      Canyon == "Bering Cyn" ~ "Bering\nCyn", 
      Canyon == "Bristol Cyn close to shelf break" ~ "Close to Shelf\nBreak Bristol Cyn", 
      Canyon == "Bering Cyn W thalweg" ~ "West thalweg\nBering Cyn", 
      Canyon == "Aleutian Chain" ~ "Aleut.\nChain", 
      Canyon == "Bogoslof Complex" ~ "Bogoslof\nComplex", 
      Canyon == "Bering Cyn Bering Trunk" ~ "Bering Trunk\nBering Cyn",
      Canyon == "S of Zhemchug Cyn in unnamed 6 cyn" ~ "Unnamed 6 cyn\nSo. of Zhemchug Cyn", # adding many more canyon titles
      Canyon == "N of Pribilof Cyn adjacent to unnamed 4 cyn" ~ "Adjacent to\nUnnamed 4 cyn\nNo. of Pribilof Cyn",
      Canyon == "N of unnamed 5 cyn" ~ "No. of Unnamed 5 cyn",
      Canyon == "Adjacent to St Paul Cyn in unnamed 5 cyn" ~ "Unnamed 5 cyn\nAdjacent to\nSt Paul Cyn",
      Canyon == "S of Pribilof Cyn" ~ "So. of Pribilof \nCyn",
      Canyon == "Pribilof Cyn" ~ "Pribilof \nCyn"
      # example for grouping variables 
      # Canyon == "Bering Cyn W thalweg" ~ "Bering\nCyn", # "Bering\nCyn\nw/thalweg", 
      # Canyon %in% c("Bering Cyn W thalweg", "Bering Cyn") ~ "A",      
    ) # note that NA was not defined here. if something is not defined, it will be NA in the new column unless you species TRUE ~ "blah"
  ) |> 
  # for troubleshooting ease, I am going to cut this table down to the necessary columns
  dplyr::select(Year, Canyon, canyon_title, MAX_GEAR_DEPTH, MIN_GEAR_DEPTH, GearAbrv,
                Corrected_Length, size_bin_label, Latitude, Longitude) |> 
  sf::st_as_sf(coords = c("Longitude", "Latitude"), 
               remove = FALSE,
               crs = crs_in) |>
  sf::st_transform(crs = crs_out)

write.csv(x = larval_dat,file = "./data/larval_dat_processed.csv")

# labels in this vector need to match values in: 
# unique(larval_dat$size_bin_label)

# Identify size bin color pallete ----------------------------------------------

color_palette <- c("0.1-8 mm" = "grey",
                   "8-10.5 mm" = "pink",
                   "10.6-12.5 mm" = "blue",
                   "12.6-14.5 mm" = "green",
                   "14.6-16.5 mm" = "purple",
                   "16.6-18.5 mm" = "gold",
                   "18.6-20.5 mm" = "forestgreen", 
                   "20.6-22.5 mm" = "cyan",# Addition of size bin titles
                   "22.6-24.5 mm" = "gold",
                   "24.6-100 mm" = "black")

# Plot size bins by canyon and depth -------------------------------------------

# since we are plotting this a few ways, we'll prepare plotting into a function

plot_p16 <- function(year0) {
  p16 <- larval_dat |>
    dplyr::filter(Year %in% year0) |> 
    ggplot(mapping = aes(x = canyon_title, y = MAX_GEAR_DEPTH, color = size_bin_label))+
    #Add line at 200 m depth
    geom_hline(yintercept = 200, linewidth = 0.2, color="gray") +
    # geom_jitter(mapping = aes(color = size_bin_label), # using geom_jitter instead of geom_point because points are being hidden
    #            alpha = .5, 
    #            width = .2,
    #            height = 0,
    #            size = 3) +
    # Plot 1993 and 2008 with following conditions for jitter so plot looks correct
    #geom_point(position=position_jitter(h=.05, w=0.3),
    #mapping = aes(color = size_bin_label), alpha = 0.5, size = 3) +
    # Plot 2008 with following conditions for jitter so plot looks correct
    #geom_point(position=position_jitter(h=.0, w=0.1),
    #mapping = aes(color = size_bin_label), alpha = 0.5, size = 3) +
    geom_point(position=position_jitter(h=0, w=0.2), # geom_jitter values for 2007 but run multiple X to get horizontal spread
               mapping = aes(color = size_bin_label), alpha = 0.5, size = 3) +
    
    #reverse y-axis for depth
    scale_y_reverse(limits = c(700, 0, by=25),
                    expand = c(0.03, 0.03))+
    facet_wrap(~Year, nrow = 5)+
    labs(x = "Capture location",
         y = "Maximum gear depth (m)",
         title = "Grenadier larvae captures in the southeast Bering Sea showing maximum gear depth") + 
    # manually define color for points
    ggplot2::scale_color_manual(
      values = color_palette,
      name = "",
      drop = TRUE) +
    #Add theme backgrnd white###
    theme_bw() +
    theme(
      # text = element_text(family = "monserrat"), # 
      axis.line = element_line(),
      panel.background = element_rect(fill = "#FFFFFF"),
    )
  
  return(p16)
}

## Plot and save data for one year iteratively ---------------------------------
for (i in c(1993, 2007, 2008, 2009)) {  
  aaa <- plot_p16(year0 = i) 
  ggsave(filename = paste0("./output/",i,"_Grenadier_larv_capture_in_Canyons_plot16_2009.png"),
         plot=aaa, width=8, height=4)
  ggsave(filename = paste0("./output/",i,"_Grenadier_larv_capture_in_Canyons_plot16_2009.tiff"),
         plot=aaa, width=8, height=4)
  aaa  #Added this code in order to see plot in Plot window
}

## plot and save specific year(s) of data in one plot --------------------------
yrs <- c(1993, 2007, 2008, 2009)
aaa <- plot_p16(year0 = yrs)
ggsave(filename = paste0("./output/", paste0(yrs, collapse = "_"),"_Grenadier_larv_capture_in_Canyons_plot16Test.png"),
       plot=aaa, width=8, height=4)
ggsave(filename = paste0("./output/",paste0(yrs, collapse = "_"),"_Grenadier_larv_capture_in_Canyons_plot16Test.tiff"),
       plot=aaa, width=8, height=4)
aaa

# Plot maps of where grenadier were found by year ------------------------------

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

## Groundfish Bottom Trawl Survey catch and haul data (FOSS) -------------------

### Download haul data ---------------------------------------------------------

dat <- data.frame()
for (i in seq(0, 500000, 10000)){
  ## find how many iterations it takes to cycle through the data
  print(i)
  ## query the API link
  res <- httr::GET(url = paste0(
    'https://apps-st.fisheries.noaa.gov/ods/foss/afsc_groundfish_survey_haul/', 
    "?offset=",i,"&limit=10000"))
  ## convert from JSON format
  data <- jsonlite::fromJSON(base::rawToChar(res$content)) 
  
  ## if there are no data, stop the loop
  if (is.null(nrow(data$items))) {
    break
  }
  
  ## bind sub-pull to dat data.frame
  dat <- dplyr::bind_rows(dat, 
                          data$items |>
                            dplyr::select(-links)) # necessary for API accounting, but not part of the dataset)
}
dat_haul <- dat

### Download catch data for Grenadier ------------------------------------------

## query the API link
# data for all walleye pollock caught in all 2023 eastern Bering Sea survey hauls
dat <- data.frame()
# there must be a better way to select multiple values for one parameter, 
# but saving that, we will loop through each hauljoin and collect the data of interest
grenadier_codes <- c(21204, # Caelorinchus scaphopsis shoulderspot grenadier
                     21210, # Coryphaenoides sp.
                     21220, # Coryphaenoides acrolepis Pacific grenadier
                     21230, # Albatrossia pectoralis giant grenadier
                     21232, # Coryphaenoides cinereus popeye grenadier
                     21238, # Coryphaenoides filifer filamented grenadier
                     21239 # Coryphaenoides longifilis longfin grenadier
)

for (i in grenadier_codes) {
  res <- httr::GET(url = paste0(
    'https://apps-st.fisheries.noaa.gov/ods/foss/afsc_groundfish_survey_catch/', 
    '?q={"species_code":',i,'}'))
  ## convert from JSON format
  data <- jsonlite::fromJSON(base::rawToChar(res$content)) 
  if (length(data$items) != 0) {
    dat <- dplyr::bind_rows(
      dat,
      data$items |> 
        dplyr::select(-links)) # necessary for API accounting, but not part of the dataset
  }
}
dat_catch <- dat

### Join groundfish haul and catch data ----------------------------------------

gfdat <- dat_catch |>
  dplyr::left_join(dat_haul) |> 
  dplyr::group_by(srvy, species_code, hauljoin, longitude_dd_start, latitude_dd_start, year) |> 
  dplyr::summarise(cpue_nokm2 = sum(cpue_nokm2, na.rm = TRUE), 
                   cpue_kgkm2 = sum(cpue_kgkm2, na.rm = TRUE),
                   count = sum(count, na.rm = TRUE),
                   weight_kg = sum(weight_kg, na.rm = TRUE))
write.csv(x = gfdat,file = "./data/gfdat_processed.csv")

gfdat<-gfdat |>
  sf::st_as_sf(coords = c("longitude_dd_start", "latitude_dd_start"), 
               remove = FALSE,
               crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0") |>
  sf::st_transform(crs = crs_out) |> 
  dplyr::mutate(Year = year)

## Pick years in both larval and groundfish data for plotting ------------------

surveyyrs <- dat_haul |> 
  dplyr::select(year, srvy) |> 
  unique() |> 
  dplyr::arrange(desc(year)) |> 
  dplyr::filter(year %in% unique(c(gfdat$Year, larval_dat$Year)))

## Inport Groundfish Bottom Trawl Survey shapefiles (akgfmaps) -----------------

shp_ebs <- akgfmaps::get_base_layers(select.region = "bs.south", set.crs = "auto")
shp_nbs <- akgfmaps::get_base_layers(select.region = "bs.north", set.crs = "auto")
shp_ai <- akgfmaps::get_base_layers(select.region = "ai", set.crs = "auto")
shp_ai$survey.strata$Stratum <- shp_ai$survey.strata$STRATUM
shp_goa <- akgfmaps::get_base_layers(select.region = "goa", set.crs = "auto")
shp_goa$survey.strata$Stratum <- shp_goa$survey.strata$STRATUM
shp_bss <- akgfmaps::get_base_layers(select.region = "ebs.slope", set.crs = "auto")

shp_all <- shp <- dplyr::bind_rows(list(
  tidyr::crossing(shp_ebs$survey.area |>
                    sf::st_transform(crs = crs_out), 
                  Year = surveyyrs$year[surveyyrs$srvy == "EBS"]) |>
    dplyr::mutate(SURVEY = "EBS"),
  tidyr::crossing( shp_nbs$survey.area  |>
                     sf::st_transform(crs = crs_out), 
                   Year = surveyyrs$year[surveyyrs$srvy == "NBS"]) |>
    dplyr::mutate(SURVEY = "NBS"),
  tidyr::crossing(shp_ai$survey.area |>
                    sf::st_transform(crs = crs_out), 
                  Year = surveyyrs$year[surveyyrs$srvy == "AI"]) |>
    dplyr::mutate(SURVEY = "AI"),
  tidyr::crossing(shp_goa$survey.area |>
                    sf::st_transform(crs = crs_out), 
                  Year = surveyyrs$year[surveyyrs$srvy == "GOA"]) |>
    dplyr::mutate(SURVEY = "GOA"),
  tidyr::crossing(shp_bss$survey.area |>
                    sf::st_transform(crs = crs_out), 
                  Year = surveyyrs$year[surveyyrs$srvy == "BSS"]) |>
    dplyr::mutate(SURVEY = "BSS"))) |>
  dplyr::select(Survey = SURVEY, geometry, Year)

## Plot map --------------------------------------------------------------------

p17 <- ggplot2::ggplot() +
  # Survey area shapefile
  ggplot2::geom_sf(data = shp_all,
                   mapping = aes(geometry = geometry,
                                 color = Year),
                   fill = "transparent",
                   color = "grey50",
                   show.legend = FALSE) +
  ggplot2::geom_sf(data = world_coordinates,
                   fill = "grey10",
                   color = "grey20")  + 
  # Manage Axis extents (limits) and breaks
  ggplot2::scale_x_continuous(name = "Longitude °W",
                              breaks = seq(-180, -150, 5)) +
  ggplot2::scale_y_continuous(name = "Latitude °N",
                              breaks = seq(50, 65, 5)) + # seq(52, 62, 2)
  ggplot2::facet_wrap(~Year) +
  
  ggplot2::geom_sf_text(
    data = place_labels |> dplyr::filter(type == "mainland"),
    mapping = aes(label = lab, angle = angle), 
    color = "grey60", 
    size = 3, 
    show.legend = FALSE) + 
  ggplot2::geom_sf_text(
    data = place_labels |> dplyr::filter(type == "survey"),
    mapping = aes(label = lab, angle = angle), 
    color = "black",
    fontface = "bold",
    size = 2, 
    show.legend = FALSE) + 
  ggplot2::geom_sf_text(
    data = place_labels |> dplyr::filter(!(type %in% c("mainland", "survey"))),
    mapping = aes(label = lab, angle = angle), 
    color = "grey10", 
    fontface = "italic", 
    size = 2, 
    show.legend = FALSE) +
  ggplot2::geom_sf(
    data = gfdat, 
    mapping = aes(
      size = cpue_kgkm2,
      geometry = geometry),
    color = "red", 
    alpha = 0.7) + 
  ggplot2::scale_size(name = "Adult\nCPUE (kg/km²)\nGAP Surveys", labels = scales::label_comma()) + # , limits = c(2,5)
  ggplot2::geom_sf(
    data = larval_dat, 
    mapping = aes(
      color = size_bin_label,
      geometry = geometry),
    size = 3,
    alpha = 0.5) + 
  
  # manually define color for points
  ggplot2::scale_color_manual(
    values = color_palette,
    name = "Larvae\nsize bins",
    drop = TRUE) +
  labs(title = "Grenadier captures in the southeast Bering Sea ") +
  ggplot2::coord_sf(xlim = boundaries$X,
                    ylim = boundaries$Y) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    plot.margin=unit(c(0,0,0,0), "cm"), 
    strip.background = element_rect(fill = "transparent", colour = "white"), 
    strip.text = element_text(face = "bold"), # , family = font0
    panel.border = element_rect(colour = "grey20", linewidth = .25, fill = NA),
    panel.background = element_rect(fill = "white"), 
    panel.grid = element_line(colour="grey80", linewidth = 0.5), 
    plot.title = element_text(face = "bold"), # , size = 12, family = font0
    axis.text = element_text(face = "bold"), # , size = 12 , family = font0
    legend.key = element_blank(), 
    legend.key.width = unit(0.6, "cm"),
    legend.key.size = unit(0.6, "cm"),
    legend.title = element_text(face = "bold"), # size = 10, , family = font0
    legend.title.position = "top", 
    legend.background = element_blank(),
    # legend.text = element_text(size = 10, angle = 90),
    legend.key.spacing = unit(0.0010, 'cm'), 
    legend.position = "right", # "bottom",
    legend.text.position = "right"# "bottom"
  )

str00 <- "example variable"
str0 <- paste0(str00, ": blah blah notes about figure and process. ")
p17

## Plot and save data ----------------------------------------------------------
filename_end <- paste0(min(larval_dat$Year), "_", max(larval_dat$Year)) # "1993-2009" # 
# writeLines(text = str0, con = paste0("./output/Grenadier_larv_capture_in_Canyons_plotLabels_mapTEST.txt"))
writeLines(text = str0, con = paste0("./output/Grenadier_larv_capture_in_Canyons_plotLabels_mapTEST_",filename_end,".txt"))
ggsave(filename = paste0("./output/Grenadier_larv_capture_in_Canyons_plotLabels_mapTEST.png"),
       plot=p17, width=8, height=4)
ggsave(filename = paste0("./output/Grenadier_larv_capture_in_Canyons_plotLabels_mapTEST.tiff"),
       plot=p17, width=8, height=4)

# Gear by year depths histograms -----------------------------------------------

## Max depth -------------------------------------------------------------------

p18 <- ggplot2::ggplot(data = larval_dat,# |>
                       # dplyr::mutate(canyon_title = ifelse(is.na(canyon_title), "Other\ncanyon", canyon_title)), 
                       # dplyr::filter(!is.na(canyon_title)),
                       mapping = aes(
                         # color = canyon_title, 
                         fill = canyon_title, 
                         x = MAX_GEAR_DEPTH))  +
  ggplot2::geom_histogram(bins = 10) +
  ggplot2::scale_fill_viridis_d(name = "Canyon") + 
  ggplot2::theme_bw()+
  #  ggplot2::facet_wrap(vars(GearAbrv), ncol = 1)
  #  ggplot2::facet_grid(vars(GearAbrv, Year))
  ggplot2::facet_grid(Year ~ GearAbrv) + 
  ggplot2::ggtitle("Max Gear Depth")

p18

## Min depth -------------------------------------------------------------------

p19 <- ggplot2::ggplot(data = larval_dat,# |>
                       # dplyr::mutate(canyon_title = ifelse(is.na(canyon_title), "Other\ncanyon", canyon_title)), 
                       # dplyr::filter(!is.na(canyon_title)),
                       mapping = aes(
                         # color = canyon_title, 
                         fill = canyon_title, 
                         x = MIN_GEAR_DEPTH))  +
  ggplot2::geom_histogram(bins = 10) +
  ggplot2::scale_fill_viridis_d(name = "Canyon") + 
  ggplot2::theme_bw()+
  #  ggplot2::facet_wrap(vars(GearAbrv), ncol = 1)
  #  ggplot2::facet_grid(vars(GearAbrv, Year))
  ggplot2::facet_grid(Year ~ GearAbrv) + 
  ggplot2::ggtitle("Min Gear Depth")

p19

## Check missing data in histogram ----------------------------------------------

# Observation counts
table(larval_dat$GearAbrv, larval_dat$Canyon, larval_dat$Year)
# Look at data subset
larval_dat |>
  dplyr::select(Year, GearAbrv, Canyon, MAX_GEAR_DEPTH, MIN_GEAR_DEPTH) |>
  dplyr::filter(Year == 1993, GearAbrv == "MOC1")

# Gear by year depths line range plots -----------------------------------------------

p20 <- ggplot2::ggplot(
  data = larval_dat |>
    # dplyr::filter(Year == 1993)  |> # Remove line if want all gear type all years
    # dplyr::filter(GearAbrv != "60BON")  |>
    dplyr::select(Latitude, canyon_title, GearAbrv, Year, MAX = MAX_GEAR_DEPTH, MIN = MIN_GEAR_DEPTH) |>
    dplyr::mutate(MIN = dplyr::case_when(
      GearAbrv == "60BON" ~ MAX, 
      .default = MIN
    )) |>
    dplyr::mutate(id = 1:n()), # |> 
  # tidyr::pivot_longer(cols = c("MAX", "MIN"), 
  #                     names_to = "Location", 
  #                     values_to = "Depth"), 
  # dplyr::mutate(canyon_title = ifelse(is.na(canyon_title), "Other\ncanyon", canyon_title)), 
  # dplyr::filter(!is.na(canyon_title)),
  mapping = aes(
    color = canyon_title,
    # fill = canyon_title, 
    y = Latitude # id
  ))  +
  ggplot2::geom_linerange(aes(xmin = MIN, xmax = MAX)) +
  ggplot2::geom_point(mapping = aes(x = MIN)) + 
  ggplot2::geom_point(mapping = aes(x = MAX)) + 
  ggplot2::scale_color_viridis_d(name = "Canyon") + 
  ggplot2::theme_bw()+
  ggplot2::facet_grid(Year ~ GearAbrv) + 
  ggplot2::geom_vline(xintercept = 200, color = "grey") + 
  ggplot2::xlab("Depth Range (m)") +
  ggplot2::ylab("Latitude °N") +
  ggplot2::ggtitle("Max and Min Gear Depth (m) for Discrete Tows")

p20

# Calculate distances from ROMS outputs ----------------------------------------

a <- list.files(path = "data", pattern = "drftB", full.names = FALSE)
cols <- c("gmt", "lon_start", "lat_start", "lon_end", "lat_end")
roms_dat <- c()

for (i in 1:length(a)) {
  
  temp <- read.csv(file = paste0("./data/", a[i]), header = FALSE) |> 
    data.frame() |> 
    dplyr::mutate(
      V1 = gsub(x = V1, pattern = "    ", replacement = ",", fixed = TRUE), 
      V1 = gsub(x = V1, pattern = " ", replacement = "", fixed = TRUE), 
      V1 = paste0(V1, ",")) |> 
    tidyr::separate(col = "V1", into = c("gmt", "lon_start", "lat_start", "lon_end", "lat_end"), sep = ",") |> 
    data.frame() |>
    dplyr::mutate(dplyr::across(all_of(cols), as.numeric)) 
  
  
  # reformat data for next part of analysis Use the start columns and the last row of the end columns to create data format
  temp <- 
    rbind.data.frame(
      temp |> 
        dplyr::select(gmt, lon = lon_start, lat = lat_start), 
      temp |> 
        dplyr::select(gmt, lon = lon_end, lat = lat_end) |> 
        dplyr::slice_tail(n = 1) |> 
        dplyr::mutate(gmt = gmt - .25)) |> # add 6 hours|> 
    dplyr::mutate(
      filename = a[i], 
      # year = substr(start = 12, stop = 15, x= filename), 
      depth_m = as.numeric(substr(start = 7, stop = 9, x= filename))) |> 
    # add date details for data plotting
    dplyr::arrange(gmt) |>
    dplyr::mutate(
      date0 = as.Date(gmt, origin = "1900-01-01 00:00"), 
      time = 24*(gmt%%1), 
      year = format(date0, format = "%Y"))
  
  
  file_cutoff_dates <- data.frame(matrix(data = c(
    # # 2007
    # # change file names and gmts
    # "drftB_400m_1993_0423_6hr.pos", 34063, #  NOTES
    # "drftB_500m_1993_0423_6hr.pos", 34063, 
    # "drftB_100m_1993_0429_6hr.pos", 34015, # NOTES
    # "drftB_200m_1993_0429_6hr.pos", 34015, 
    # "drftB_300m_1993_0429_6hr.pos", 34015, 
    # "drftB_375m_1993_0429_6hr.pos", 34035, # NOTES
    
    # 1993
    "drftB_400m_1993_0423_6hr.pos", 34063, #  April 6 (GMT = 34063) - April 23 (GMT = 34080.75; last day of roms run) #  & gmt <= 34080.75
    "drftB_500m_1993_0423_6hr.pos", 34063, 
    "drftB_100m_1993_0429_6hr.pos", 34015, # Feb 17 (GMT =  34015) - April 29 (GMT = 34086; last day of roms run) 
    "drftB_200m_1993_0429_6hr.pos", 34015, 
    "drftB_300m_1993_0429_6hr.pos", 34015, 
    "drftB_375m_1993_0429_6hr.pos", 34035  #  Skipping zeroes March 9 (GMT =  34035) - April 29 (GMT = 34086; last day of roms run)
  ), ncol = 2, byrow = TRUE)) |> 
    dplyr::rename(file = X1, gmt = X2) |> 
    dplyr::mutate(gmt = as.numeric(gmt))
  
  temp <- temp |>
    dplyr::filter(gmt >= file_cutoff_dates$gmt[file_cutoff_dates$file == a[i]])
  
  # add human-readable data attributes
  temp <- temp |> 
    dplyr::mutate(
      date_md = format(date0, format = "%B %d"), 
      date_mdy = format(date0, format = "%B %d, %Y"), 
      date = paste0(format(min(date0), format = "%B %d"), " - ", # " -\n", 
                    format(max(date0), format = "%B %d, %Y")),
      # event = c("Start", 
      #           rep_len(length.out = floor((n()-2)*0.6), NA), # 60%
      #           "End Fast Growth", 
      #           rep_len(length.out = floor((n()-3)*0.4), NA), # 40%
      #           "End Slow Growth"), 
      event = c("Start", rep_len(length.out = (n()-2), NA), "End"),
      event = factor(event, ordered = TRUE)) |> 
    # add geospatial to data
    sf::st_as_sf(coords = c("lon", "lat"), 
                 agr = "constant", 
                 remove = FALSE, 
                 crs = crs_in) |>
    sf::st_transform(crs = crs_out) |> 
    dplyr::mutate( 
      lat_rad = lat*pi/180, 
      lon_rad = lon*pi/180, 
      lat_rad_diff = NA, 
      lon_rad_diff = NA, 
      dist_km_projected = NA, 
      dist_nmi_radians = NA)
  
  
  # add projected data points
  temp <- temp |>
    dplyr::bind_cols(temp |>
                       sf::st_coordinates())
  
  
  # calculate distance between each point and its consecutive point
  for (ii in 2:(nrow(temp))){
    # CALCULATE DISTANCE WITH SF PROJECTION ESPG:3338, METHOD 1
    temp1 <- temp[(ii-1):(ii),] |>
      sf::st_distance()
    units(temp1)$numerator <- "km" # convert matrix unit from meters (m) to kilometers (km)
    temp$dist_km_projected[ii] <- temp1[2,1] # pull distance from in the matrix and add to vector
    
    ## CALCULATE DISTANCE IN RADIAN # METHOD 2
    temp$lat_rad_diff[ii] <- (temp$lat_rad[ii] - temp$lat_rad[ii+1])
    temp$lon_rad_diff[ii] <- (temp$lon_rad[ii] - temp$lon_rad[ii+1])
    temp$dist_nmi_radians[ii] <- # in excel: # ACOS((SIN(R9)*SIN(R10))+(COS(R9)*COS(R10)*COS(T10)))/(PI()/180)*60
      acos((sin(temp$lat_rad[ii+1])*sin(temp$lat_rad[ii])) +
             (cos(temp$lat_rad[ii+1])*cos(temp$lat_rad[ii])*
                cos(temp$lon_rad_diff[ii])))/(pi/180)*60 
  }
  
  temp <- temp |> 
    dplyr::mutate(
      dist_km_radians = dist_nmi_radians*1.852, 
      dist_nmi_projected = dist_km_projected/1.852
    ) 
  
  roms_dat <- roms_dat |>
    dplyr::bind_rows(temp)
}

roms_dat <- roms_dat |> 
  dplyr::ungroup() |>
  dplyr::arrange(depth_m)  |> 
  dplyr::mutate(
    year = factor(year, ordered = TRUE), 
    depth_m = factor(depth_m, ordered = TRUE), 
    dist_km = dist_km_radians, # DECISION POINT - using radians
    dist_nmi = dist_nmi_radians, # DECISION POINT
    # velocity_kmhr = dist_km/6, # distance (km) over time (6 hours)
    currentspeed_cms = (dist_km*100000)/(60*60*6) # distance (cm) over time (in seconds, 6 hours between)
  ) 

### Create lines from points --------------------------------------------------

roms_dat_lines <- roms_dat |>
  dplyr::group_by(date, year, depth_m, filename) |>
  dplyr::summarise(
    do_union = FALSE,
    dist_km_sum = sum(dist_km, na.rm = TRUE), 
    dist_nmi_sum = sum(dist_nmi, na.rm = TRUE), 
    obs = n(), 
    # average of all speed observations (all values/count aka number of rows)
    currentspeed_mean_by_averagecms = mean(currentspeed_cms, na.rm = TRUE), 
    currentspeed_mean_by_sdcms = sd(currentspeed_cms, na.rm = TRUE), 
    currentspeed_min = min(currentspeed_cms, na.rm = TRUE), 
    currentspeed_max = max(currentspeed_cms, na.rm = TRUE)) |> 
  sf::st_cast("LINESTRING") |> 
  dplyr::ungroup() |> 
  dplyr::mutate(
    # total distance divided by total time - em thinks is less precise  
    currentspeed_mean_bytotaldisttime = (dist_km_sum*100000)/(24*(obs/4)*60*60), 
    currentspeed_mean = currentspeed_mean_by_averagecms) |> # DECISION POINT
  dplyr::arrange(filename)

## map plot -------------------------------------------------------------------

p21 <- ggplot2::ggplot() +
  
  ### Map shapefile aesthetics ----------------------------------
# Manage Axis extents (limits) and breaks
ggplot2::geom_sf(data = world_coordinates,
                 fill = "grey10",
                 color = "grey20")  + 
  ggplot2::scale_x_continuous(name = "Longitude °W",
                              breaks = seq(-180, -150, 5)) +
  ggplot2::scale_y_continuous(name = "Latitude °N",
                              breaks = seq(40, 65, 2)) + # seq(52, 62, 2)
  
  # ggplot2::geom_sf_text(
  #   data = place_labels |> dplyr::filter(type == "mainland"),
  #   mapping = aes(label = lab, angle = angle), 
  #   color = "grey60", 
  #   size = 3, 
  #   show.legend = FALSE) + 
  # ggplot2::geom_sf_text(
  #   data = place_labels |> dplyr::filter(type == "survey"),
  #   mapping = aes(label = lab, angle = angle), 
  #   color = "black",
  #   fontface = "bold",
  #   size = 2, 
  #   show.legend = FALSE) + 
  # ggplot2::geom_sf_text(
  #   data = place_labels |> dplyr::filter(!(type %in% c("mainland", "survey"))),
  #   mapping = aes(label = lab, angle = angle), 
  #   color = "grey10", 
  #   fontface = "italic", 
  #   size = 2, 
  #   show.legend = FALSE) +
  
  ### Plot data ----------------------------------------------------------------

ggplot2::geom_sf(
  data = roms_dat_lines, 
  mapping = aes(
    color = depth_m,
    # linetype = depth_m, 
    geometry = geometry), 
  alpha = 0.7,
  linewidth = 2) + 
  ggplot2::geom_sf(
    data = roms_dat, 
    mapping = aes(
      shape = event, 
      color = depth_m,
      geometry = geometry), 
    # alpha = 0.7,
    size = 3) + 
  ggplot2::facet_wrap(~date) + # filename
  ggplot2::scale_color_viridis_d(name = "Depth (m)", option = "D", begin = .2, end = .8) + 
  ggplot2::ggtitle(label = "Larval Grenadier Modeled ROMS Dispersal", 
                   subtitle = "At different depths, years, and environmental conditions") + 
  ggplot2::scale_shape_discrete(name = "Event", 
                                na.value = NA,
                                na.translate = FALSE) +
  
  ### Plot aesthetics ----------------------------------

ggplot2::coord_sf(xlim = c(max(roms_dat$X), min(roms_dat$X)),
                  ylim = c(max(roms_dat$Y), min(roms_dat$Y))) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    plot.margin=unit(c(0,0,0,0), "cm"), 
    strip.background = element_rect(fill = "transparent", colour = "black"), 
    strip.text = element_text(face = "bold"), # , family = font0
    panel.border = element_rect(colour = "grey20", linewidth = .25, fill = NA),
    panel.background = element_rect(fill = "white"), 
    panel.grid = element_line(colour="grey80", linewidth = 0.5), 
    plot.title = element_text(face = "bold"), # , size = 12, family = font0
    axis.text = element_text(face = "bold"), # , size = 12 , family = font0
    legend.key = element_blank(), 
    legend.key.width = unit(0.6, "cm"),
    legend.key.size = unit(0.6, "cm"),
    legend.title = element_text(face = "bold"), # size = 10, , family = font0
    legend.title.position = "top", 
    legend.background = element_blank(),
    # legend.text = element_text(size = 10, angle = 90),
    legend.key.spacing = unit(0.0010, 'cm'), 
    legend.position = "right", # "bottom",
    legend.text.position = "right"# "bottom"
  )


p21

ggsave(filename = paste0("./output/Grenadier_larv_ROMS_plot21_Test.png"),
       plot=p21, width=6, height=6)
ggsave(filename = paste0("./output/Grenadier_larv_ROMS_plot21_Test.tiff"),
       plot=p21, width=6, height=6)

### summary table -------------------------------------------------------------

t21 <- roms_dat_lines |> 
  dplyr::mutate(depth_m = as.numeric(paste0(depth_m))) |>
  sf::st_drop_geometry() |> 
  dplyr::select(-year) |> # -obs,  , -currentspeed_mean_bytotaldisttime, -currentspeed_mean_by_averagecms) |> 
  flextable::flextable()  |> 
  flextable::set_header_labels(
    date = "Date range",
    depth_m = "Current Depth (m)",
    dist_km_sum ="Distance traveled (km)",
    dist_nmi_sum ="Distance traveled (nmi)",
    currentspeed_mean = "Mean current speed (cm/s)",
    currentspeed_min = "Low current speed (cm/s)",
    currentspeed_max = "High current speed (cm/s)"
  ) |>
  flextable::merge_v(j = "date") |>
  flextable::colformat_double(
    big.mark = ",", 
    digits = 2, 
    na_str = "-") |> 
  flextable::colformat_double(
    j = c("depth_m"), 
    big.mark = "", 
    digits = 0, 
    na_str = "-") |> 
  flextable::theme_vanilla() |>
  flextable::width(width = 6.5/(7)) |> 
  flextable::theme_zebra() |> 
  flextable::width(width = .75)|> 
  flextable::width(width = 1.5, j = "date")

t21

### compare -------------

# temp <- roms_dat |> 
#   sf::st_drop_geometry() |> 
#   dplyr::mutate(
#     depth_m = as.numeric(paste0(depth_m)),
#     year = as.numeric(paste0(year)), 
#     lat_rad_radians = lat*pi/180, 
#     lon_rad_radians = lon*pi/180, 
#     lat_rad_diff_radians = NA, 
#     lon_rad_diff_radians = NA) 
# 
# for (i in 2:nrow(temp)) {
#   temp$lat_rad_diff_radians[i] <- (temp$lat_rad_radians[i] - temp$lat_rad_radians[i-1])
#   temp$lon_rad_diff_radians[i] <- (temp$lon_rad_radians[i] - temp$lon_rad_radians[i-1])
#   temp$dist_nmi_radians[i] <- # in excel: # ACOS((SIN(R9)*SIN(R10))+(COS(R9)*COS(R10)*COS(T10)))/(PI()/180)*60
#     acos((sin(temp$lat_rad_radians[i-1])*sin(temp$lat_rad_radians[i]))+
#            (cos(temp$lat_rad_radians[i-1])*cos(temp$lat_rad_radians[i])*
#               cos(temp$lon_rad_diff_radians[i])))/(pi/180)*60 
# }
# 
# temp <- temp |> 
#   dplyr::mutate(
#     dist_km_radians = dist_nmi_radians*1.852
#   ) |> 
#   dplyr::select(gmt, lat, lon, year, depth_m, 
#                 dist_nmi_r = dist_nmi, 
#                 dist_km_r = dist_km, 
#                 dist_nmi_radians, 
#                 dist_km_radians)

temp <- roms_dat |> # compare all available data
  sf::st_drop_geometry() |> 
  dplyr::filter(
    year == 1993 &
      depth_m == 300) |> 
  dplyr::mutate(
    # gmt = gmt + 0.25,
    year = as.numeric(paste0(year)), 
    depth_m = as.numeric(paste0(depth_m))) |> 
  dplyr::select(gmt, lat, lon, year, depth_m, 
                dist_nmi_projected, dist_km_projected, 
                dist_nmi_radians, dist_km_radians)

compare_roms_outputs_dat <- read_excel(
  path = "./data/StationDistanceCalc_6hr_AnnotatedMACE_Paquin.xlsx", 
  skip = 5) |> 
  janitor::clean_names() |> 
  dplyr::select(lat = dec_lat, lon = dec_long, dist_nmi_radiansexcel = distance_nm) |> 
  dplyr::mutate(lon = lon*-1, 
                depth_m = 300, 
                year = 1993, 
                dist_km_radiansexcel = dist_nmi_radiansexcel*1.852) |> 
  dplyr::filter(!is.na(lat)) |>
  # dplyr::full_join(
  #   temp |> 
  #     dplyr::select(gmt, lat, lon, year, depth_m)
  # ) |> 
  # add roms data to this table for comparison
  dplyr::full_join(temp) |>
  # dplyr::left_join(temp) |> # compare only data in excel
  dplyr::mutate(
    dist_km_diff_radians_radiansexcel = dist_km_radians - dist_km_radiansexcel, 
    dist_km_diff_projected_radians = dist_km_projected - dist_km_radians, 
    dist_km_diff_projected_radiansexcel = dist_km_projected - dist_km_radiansexcel, 
    dist_nmi_diff_radians_radiansexcel = dist_nmi_radians - dist_nmi_radiansexcel, 
    dist_nmi_diff_projected_radians = dist_nmi_projected - dist_nmi_radians, 
    dist_nmi_diff_projected_radiansexcel = dist_nmi_projected - dist_nmi_radiansexcel
  ) |> 
  dplyr::relocate(gmt, year, lat, lon, depth_m, 
                  dist_km_projected, 
                  dist_km_radians, 
                  dist_km_radiansexcel, 
                  dist_km_diff_radians_radiansexcel, 
                  dist_km_diff_projected_radians, 
                  dist_km_diff_projected_radiansexcel, 
                  dist_nmi_projected, 
                  dist_nmi_radians, 
                  dist_nmi_radiansexcel, 
                  dist_nmi_diff_radians_radiansexcel, 
                  dist_nmi_diff_projected_radians, 
                  dist_nmi_diff_projected_radiansexcel)

write.csv(x = compare_roms_outputs_dat, file = here::here("output/compare_roms_outputs_dat.csv"))

compare_roms_outputs_dat_sums <- compare_roms_outputs_dat |> 
  dplyr::group_by(depth_m, year) |>
  dplyr::summarise(
    #km
    dist_km_projected = sum(dist_km_projected, na.rm = TRUE), 
    dist_km_radiansexcel = sum(dist_km_radiansexcel, na.rm = TRUE), 
    dist_km_radians = sum(dist_km_radians, na.rm = TRUE), 
    dist_km_diff_radians_radiansexcel = sum(dist_km_diff_radians_radiansexcel, na.rm = TRUE), 
    dist_km_diff_projected_radians = sum(dist_km_diff_projected_radians, na.rm = TRUE), 
    dist_km_diff_projected_radiansexcel = sum(dist_km_diff_projected_radiansexcel, na.rm = TRUE), 
    # nmi
    dist_nmi_projected = sum(dist_nmi_projected, na.rm = TRUE), 
    dist_nmi_radiansexcel = sum(dist_nmi_radiansexcel, na.rm = TRUE), 
    dist_nmi_radians = sum(dist_nmi_radians, na.rm = TRUE), 
    dist_nmi_diff_radians_radiansexcel = sum(dist_nmi_diff_radians_radiansexcel, na.rm = TRUE), 
    dist_nmi_diff_projected_radians = sum(dist_nmi_diff_projected_radians, na.rm = TRUE), 
    dist_nmi_diff_projected_radiansexcel = sum(dist_nmi_diff_projected_radiansexcel, na.rm = TRUE)) |> 
  dplyr::ungroup() |> 
  dplyr::mutate(
    #   dist_km_diff_tot_radiansexcel_r = dist_km_radiansexcel - dist_km_r,
    #   dist_km_diff_tot_radiansexcel_radians = dist_km_radiansexcel - dist_km_radians,
    #   dist_nmi_diff_tot_radiansexcel_r = dist_nmi_radians_radiansexcel - dist_nmi_r, 
    #   dist_nmi_diff_tot_radiansexcel_radians = dist_nmi_radians_radiansexcel - dist_nmi_radians, 
    obs_radiansexcel = read_excel(
      path = "./data/StationDistanceCalc_6hr_AnnotatedMACE_Paquin.xlsx", 
      skip = 5) |> 
      janitor::clean_names() |> 
      dplyr::select(lat) |> 
      dplyr::filter(!is.na(lat)) |> 
      nrow(), 
    obs_r = nrow(temp))

summary(compare_roms_outputs_dat)

t(compare_roms_outputs_dat_sums)



