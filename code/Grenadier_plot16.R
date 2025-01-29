
# Purpose ----------------------------------------------------------------------

# Explore grenadier larvae captures using depth discrete tows and oblique 
# tows at stations with positive tows for macrourids. 
# M Paquin and E Markowitz

# Install Libraries ------------------------------------------------------------

# Here we list all the packages we will need for this whole process
# We'll also use this in our works cited page!!!
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
  "httr" # check website links
  # "flextable", # making pretty tables
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

# Wrangle data -----------------------------------------------------------------

# larval_dat<-read_excel("~/My Desktop/r-tidyverse/GrenadierLarvlxy_time_step_target_yrsCanyons.xlsx",
larval_dat <- read_excel("./data/GrenadierLarvlxy_time_step_target_yrsCanyons.xlsx",
                         sheet = "GrenadierLarvlxy_time_step_targ") %>%
  dplyr::filter(!is.na(Corrected_Length)) %>%
  dplyr::arrange((Corrected_Length)) %>%
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
  ) %>% 
  # for troubleshooting ease, I am going to cut this table down to the necessary columns
  dplyr::select(Year, Canyon, canyon_title, MAX_GEAR_DEPTH, 
                Corrected_Length, size_bin_label, Latitude, Longitude) %>% 
  sf::st_as_sf(coords = c("Longitude", "Latitude"), 
               remove = FALSE,
               crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0") %>%
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
p16 <- larval_dat %>%
  dplyr::filter(Year %in% year0) %>% 
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
for (i in c(1993, 2007, 2008)) {  
  aaa <- plot_p16(year0 = i) 
  ggsave(filename = paste0("./output/",i,"_Grenadier_larv_capture_in_Canyons_plot16MaxGear.png"),
         plot=aaa, width=8, height=4)
  ggsave(filename = paste0("./output/",i,"_Grenadier_larv_capture_in_Canyons_plot16MaxGear.tiff"),
         plot=aaa, width=8, height=4)
  
}

## plot and save specific year(s) of data in one plot --------------------------
yrs <- c(1993, 2007, 2008)
aaa <- plot_p16(year0 = yrs)
ggsave(filename = paste0("./output/", paste0(yrs, collapse = "_"),"_Grenadier_larv_capture_in_Canyons_plot16MaxGear.png"),
       plot=aaa, width=8, height=4)
ggsave(filename = paste0("./output/",paste0(yrs, collapse = "_"),"_Grenadier_larv_capture_in_Canyons_plot16MaxGear.tiff"),
       plot=aaa, width=8, height=4)

# Plot maps of where grenadier were found by year ------------------------------

## Get world map ---------------------------------------------------------------

world_coordinates <- maps::map("world", plot = FALSE, fill = TRUE) %>% 
  sf::st_as_sf() %>%
  # sf::st_union() %>% 
  sf::st_transform(crs = crs_out) %>% 
  dplyr::filter(ID %in% c("USA", "Russia", "Canada")) %>% 
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
          -176, -162, -165)) %>%
  dplyr::filter(type != "peninsula") %>% 
  dplyr::filter(type != "survey") %>% 
  # dplyr::mutate(
  #   color = dplyr::case_when(
  #     type == "mainland" ~ "grey80", 
  #     TRUE ~ "grey30"), 
  #   fontface = dplyr::case_when(
  #     type == "mainland" ~ "bold", 
  #     TRUE ~ "regular"),
  #   size = dplyr::case_when(
  #     type == "mainland" ~ 3, 
  #     TRUE ~ 2) ) %>% 
  sf::st_as_sf(coords = c("lon", "lat"),
               crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0") %>%
  sf::st_transform(crs = crs_out) 

## Determine map boundaries ----------------------------------------------------

boundaries <- data.frame(lon = c(-180, -160), # c(-180, -140)
                        lat = c(50, 64) )  %>% # c(46, 66)
  sf::st_as_sf(coords = c("lon", "lat"),
               crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0") %>%
  sf::st_transform(crs = crs_out) %>% 
  sf::st_coordinates() %>% 
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
                          data$items %>%
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
      data$items %>% 
        dplyr::select(-links)) # necessary for API accounting, but not part of the dataset
  }
}
dat_catch <- dat

### Join groundfish haul and catch data ----------------------------------------

gfdat <- dat_catch %>%
  dplyr::left_join(dat_haul) %>% 
  dplyr::group_by(srvy, species_code, hauljoin, longitude_dd_start, latitude_dd_start, year) %>% 
  dplyr::summarise(cpue_nokm2 = sum(cpue_nokm2, na.rm = TRUE), 
                   cpue_kgkm2 = sum(cpue_kgkm2, na.rm = TRUE),
                   count = sum(count, na.rm = TRUE),
                   weight_kg = sum(weight_kg, na.rm = TRUE))
write.csv(x = gfdat,file = "./data/gfdat_processed.csv")

gfdat<-gfdat %>%
  sf::st_as_sf(coords = c("longitude_dd_start", "latitude_dd_start"), 
               remove = FALSE,
               crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0") %>%
  sf::st_transform(crs = crs_out) %>% 
  dplyr::mutate(Year = year)

## Pick years in both larval and groundfish data for plotting ------------------

surveyyrs <- dat_haul %>% 
  dplyr::select(year, srvy) %>% 
  unique() %>% 
  dplyr::arrange(desc(year)) %>% 
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
  tidyr::crossing(shp_ebs$survey.area %>%
    sf::st_transform(crs = crs_out), 
    Year = surveyyrs$year[surveyyrs$srvy == "EBS"]) %>%
    dplyr::mutate(SURVEY = "EBS"),
  tidyr::crossing( shp_nbs$survey.area  %>%
    sf::st_transform(crs = crs_out), 
    Year = surveyyrs$year[surveyyrs$srvy == "NBS"]) %>%
    dplyr::mutate(SURVEY = "NBS"),
  tidyr::crossing(shp_ai$survey.area %>%
    sf::st_transform(crs = crs_out), 
    Year = surveyyrs$year[surveyyrs$srvy == "AI"]) %>%
    dplyr::mutate(SURVEY = "AI"),
  tidyr::crossing(shp_goa$survey.area %>%
    sf::st_transform(crs = crs_out), 
    Year = surveyyrs$year[surveyyrs$srvy == "GOA"]) %>%
    dplyr::mutate(SURVEY = "GOA"),
tidyr::crossing(shp_bss$survey.area %>%
    sf::st_transform(crs = crs_out), 
    Year = surveyyrs$year[surveyyrs$srvy == "BSS"]) %>%
    dplyr::mutate(SURVEY = "BSS"))) %>%
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
    data = place_labels %>% dplyr::filter(type == "mainland"),
    mapping = aes(label = lab, angle = angle), 
    color = "grey60", 
    size = 3, 
    show.legend = FALSE) + 
  ggplot2::geom_sf_text(
    data = place_labels %>% dplyr::filter(type == "survey"),
    mapping = aes(label = lab, angle = angle), 
    color = "black",
    fontface = "bold",
    size = 2, 
    show.legend = FALSE) + 
  ggplot2::geom_sf_text(
    data = place_labels %>% dplyr::filter(!(type %in% c("mainland", "survey"))),
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


p17

## Plot and save data ----------------------------------------------------------

ggsave(filename = paste0("./output/Grenadier_larv_capture_in_Canyons_plotLabels_mapTEST.png"),
       plot=p17, width=8, height=4)
ggsave(filename = paste0("./output/Grenadier_larv_capture_in_Canyons_plotLabels_mapTEST.tiff"),
       plot=p17, width=8, height=4)


