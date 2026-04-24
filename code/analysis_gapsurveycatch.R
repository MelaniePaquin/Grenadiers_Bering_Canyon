
# Load packages and set plotting features --------------------------------------
source("./code/functions.R")

## load data from GAP_PRODUCTS -------------------------------------------------

library("RODBC")

# Documentation
# https://afsc-gap-products.github.io/gap_products/
# https://www.fisheries.noaa.gov/resource/document/groundfish-survey-species-code-manual-and-data-codes-manual  

# # Sign into Oracle
 oracle_user <- "paquinm"
 oracle_pw <- "Phrynosoma#1" # NEED!
 channel <- RODBC::odbcConnect(dsn = "AFSC", 
                               uid = oracle_user, 
                               pwd = oracle_pw, 
                               believeNRows = FALSE)
 
# # # Download data
#  specimen_gap <- RODBC::sqlQuery(channel, 
#                                       paste0("SELECT * 
#  FROM GAP_PRODUCTS.AKFIN_SPECIMEN
#  WHERE SPECIES_CODE IN (24001, 21220, 21232, 21230); "))
#  write.csv(x = specimen_gap, file = "data/specimen_gap.csv")
#  
#  
#  sizecomp_gap <- RODBC::sqlQuery(channel, 
#                                       paste0("SELECT * 
#  FROM GAP_PRODUCTS.AKFIN_SIZECOMP
#  WHERE SPECIES_CODE IN (24001, 21220, 21232, 21230); "))
#  write.csv(x = sizecomp_gap, file = "data/sizecomp_gap.csv")
# 
#  catch_gap <- RODBC::sqlQuery(channel, 
#                                       paste0("SELECT * 
#  FROM GAP_PRODUCTS.AKFIN_CATCH
#  WHERE SPECIES_CODE IN (24001, 21220, 21232, 21230); "))
#  write.csv(x = catch_gap, file = "data/catch_gap.csv")
#  
# 
# specimen_gap <- read.csv("data/specimen_gap.csv")
# sizecomp_gap <- read.csv("data/sizecomp_gap.csv")
# catch_gap <- read.csv("data/catch_gap.csv")

specimen_gap_all <- # even the bad hauls!
  RODBC::sqlQuery(channel, paste0(
    "SELECT DISTINCT
  s.SPECIMEN_ID, 
  s.SPECIES_CODE, 
  s.LENGTH_MM, 
  s.SEX, 
  s.WEIGHT_G, 
  s.AGE, 
  s.SPECIMEN_SAMPLE_TYPE, 
  c.CRUISE, 
  c.CRUISEJOIN,
  c.YEAR, 
  c.SURVEY_DEFINITION_ID, 
  hhh.STATIONID AS STATION, 
  hhh.STRATUM, 
  hhh.HAUL, 
  c.VESSEL_ID, 
  c.VESSEL_NAME, 
  c.SURVEY_NAME,
  hhh.START_LATITUDE, 
  hhh.START_LONGITUDE, 
  hhh.BOTTOM_DEPTH, 
  hhh.SURFACE_TEMPERATURE, 
  hhh.GEAR_TEMPERATURE
FROM RACE_DATA.HAULS h
-- Integrate RACE_DATA tables
JOIN RACE_DATA.CRUISES A 
  ON h.CRUISE_ID = A.CRUISE_ID
JOIN RACE_DATA.SURVEYS S 
  ON S.SURVEY_ID = A.SURVEY_ID
JOIN RACE_DATA.SURVEY_DEFINITIONS SD 
  ON SD.SURVEY_DEFINITION_ID = S.SURVEY_DEFINITION_ID
JOIN GAP_PRODUCTS.AKFIN_CRUISE c 
  ON A.RACEBASE_CRUISEJOIN = c.CRUISEJOIN  
JOIN RACEBASE.HAUL hhh 
  ON hhh.CRUISEJOIN = hhh.CRUISEJOIN AND hhh.VESSEL = A.VESSEL_ID AND hhh.STATIONID = h.STATION AND hhh.STRATUM = h.STRATUM 
JOIN GAP_PRODUCTS.AKFIN_SPECIMEN s
  ON hhh.HAULJOIN = s.HAULJOIN
WHERE s.SPECIES_CODE IN (24001, 21220, 21232, 21230);"))
write.csv(x = specimen_gap_all, file = "data/specimen_gap_all.csv")

specimen_gap_all <- read.csv("data/specimen_gap_all.csv")


# Plot maps of where grenadier were found by year ------------------------------

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
str0 <- paste0(str00, ": Note run Source gives all new plots. Just leave them and overwrite them continuously, unless I want to save a version for something. notes about figure and process: Hatch date was calculated using capture date and slow growth rate see file GrenadierLarv_xy_time_stepDEC24MMP_Nedv3Calc_date_hatchESTIMATE.xml. At hatch day larva is one day old. Also explained in .pos file compilation drftB_depth_1993_07_08_09_6hr_data_slow_fast.xml. Skipping zeroes March 9 (GMT =  34035) - April 29 (GMT = 34086; last day of roms run) for depth 375 m see code drftB_375m_1993_0429_6hr.pos, 34035. Next step 1) ground truth the GMT date to a real date. It is in output to file Gren_larv_ROMS_summary_speed.doc as the hatch and  capture dates.  Also, 2) why the output file  compare_roms_outputs_dat.csv is still outputting 1993 only (2007 is missing). Note that and that file Gren_larv_ROMS_summary_speed.doc is summarized as roms_dat_lines from roms_dat. So roms_dat has the math At each data point, and roms_dat_lines has the summary statistics by year and depth (maybe one more thing). One issue I've found is that in 2007 I get a date discrepancy (by a day) instead of # Hatch date February 28 (GMT =  39138) (expected input), the output file shows February 27 th  (file Gren_larv_ROMS_summary_speed.doc). Oddly  the 1993 data output look correct.FIX: it is likely b/c of the math. Note Subtraction of GMT values + 1 = number of days particle drift. This returns correct hatch date. Table output file name Gren_larv_ROMS_summary_speed is this specifying data are not radians calculated? If not then how to specify so know which method used? Issue w/ in the file Gren_larv_ROMS_summary_speed that this data row drftB_100m_2008_0224_6hr.pos listed out of order by depth.Note that these output data have updated current speed mean, low and high values for each depth. One thing that Wei and Ned will probably notice is that there was a jump in some high values reported.These simulations are done using archived weekly averaged velocity fields with 6-hour time steps for the particle trajectories.")
p17

## Plot and save data ----------------------------------------------------------
filename_end <- paste0(min(larval_dat$Year), "_", max(larval_dat$Year)) # "1993-2009" # 
# writeLines(text = str0, con = paste0("./output/gren_larv_capt_cyn_plotLabels_mapTEST.txt"))
writeLines(text = str0, con = paste0("./output/gren_larv_capt_cyn_plotLabels_mapTEST_notes_",filename_end,".txt"))
ggsave(filename = paste0("./output/gren_larv_capt_cyn_plotLabels_mapTEST.png"),
       plot=p17, width=8, height=4)
ggsave(filename = paste0("./output/gren_larv_capt_cyn_plotLabels_mapTEST.tiff"),
       plot=p17, width=8, height=4)