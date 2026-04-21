
# Load packages and set plotting features --------------------------------------
source("./code/functions.R")

# Load and calculate distances from ROMS outputs -------------------------------

a <- list.files(path = "data", pattern = "drftB", full.names = FALSE)
cols <- c("gmt", "lon_start", "lat_start", "lon_end", "lat_end")
roms_dat <- c()

## Interate through each .pos file in the folder -------------------------------
for (i in 1:length(a)) {
  
  ## Read each .pos file -------------------------------------------------------
  temp <- read.csv(file = paste0("./data/", a[i]), header = FALSE) |> 
    data.frame() |> 
    dplyr::mutate(
      V1 = gsub(x = V1, pattern = "    ", replacement = ",", fixed = TRUE), 
      V1 = gsub(x = V1, pattern = " ", replacement = "", fixed = TRUE), 
      V1 = paste0(V1, ",")) |> 
    tidyr::separate(col = "V1", into = c("gmt", "lon_start", "lat_start", "lon_end", "lat_end"), sep = ",") |> 
    data.frame() |>
    dplyr::mutate(dplyr::across(all_of(cols), as.numeric)) 
  
  
  # Reformat data for next part of analysis Use the start columns and the last row of the end columns to create data format
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
  
  ## Establish cut off dates for each .pos file --------------------------------
  # This just deals with establishing a cut off date, if there is one
  file_cutoff_dates <- data.frame(matrix(data = c(
    # #2009
    # "drftB_100m_2009_0227_6hr.pos", 0, # 39842, # 28 days for example
    "drftB_100m_2009_0227_6hr.pos", 39842, # Hatch date January 31 (GMT =  39842) - last day of roms run is the capture date February 27 (GMT = 39869)    
    "drftB_200m_2009_0227_6hr.pos", 39842,
    "drftB_300m_2009_0227_6hr.pos", 39842,
    "drftB_400m_2009_0227_6hr.pos", 39842,
    "drftB_500m_2009_0227_6hr.pos", 39842, 
    "drftB_600m_2009_0227_6hr.pos", 39842,
    "drftB_100m_2009_0301_6hr.pos", 39863, # Hatch date February 21 (GMT =  39863) - last day of roms run is the capture date March 1 (GMT = 39871)    
    "drftB_200m_2009_0301_6hr.pos", 39863,
    "drftB_300m_2009_0301_6hr.pos", 39863, 
    "drftB_400m_2009_0301_6hr.pos", 39863,
    "drftB_500m_2009_0301_6hr.pos", 39863, 
    "drftB_100m_2009_0302_6hr.pos", 39832, # Hatch date January 21 (GMT =  39832) - last day of roms run is the capture date March 2 (GMT = 39872)    
    "drftB_200m_2009_0302_6hr.pos", 39832,     
    "drftB_300m_2009_0302_6hr.pos", 39832, 
    "drftB_400m_2009_0302_6hr.pos", 39832, 
    "drftB_450m_2009_0302_6hr.pos", 39832,
    
    # # 2008
    # "drftB_100m_2008_0220_6hr.pos", 0, # 39473, # 24 days for example
    "drftB_100m_2008_0220_6hr.pos", 39473, # Hatch date January 28 (GMT =  39473) - last day of roms run is the capture date February 20 (GMT = 39496)    
    "drftB_200m_2008_0220_6hr.pos", 39473, 
    "drftB_300m_2008_0220_6hr.pos", 39473,
    "drftB_400m_2008_0220_6hr.pos", 39473, 
    "drftB_500m_2008_0220_6hr.pos", 39473,
    "drftB_100m_2008_0224_6hr.pos", 39477, # Hatch date February 1 (GMT =  39477) - last day of roms run is the capture date February 24 (GMT = 39500)    
    "drftB_200m_2008_0224_6hr.pos", 39477,     
    "drftB_300m_2008_0224_6hr.pos", 39477,
    "drftB_400m_2008_0224_6hr.pos", 39477, 
    "drftB_500m_2008_0224_6hr.pos", 39477,
    "drftB_600m_2008_0224_6hr.pos", 39477,
    "drftB_300m_2008_0226_6hr.pos", 39467,  # Hatch date January 22 (GMT =  39467) - last day of roms run is the capture date February 26 (GMT = 39502)    
    "drftB_400m_2008_0226_6hr.pos", 39467,     
    "drftB_450m_2008_0226_6hr.pos", 39467, 
    
    # # 2007
    # "drftB_100m_2007_0510_6hr.pos", 39197 # 14 days for example
    "drftB_100m_2007_0510_6hr.pos", 39197, # Hatch date April 27 (GMT =  39196) - last day of roms run is the capture date May 10 (GMT = 39210) 
    "drftB_200m_2007_0510_6hr.pos", 39197, 
    "drftB_300m_2007_0510_6hr.pos", 39197,
    "drftB_100m_2007_0515_6hr.pos", 39139, # Hatch date February 28 (GMT =  39138) - last day of roms run is the capture date May 15 (GMT = 39215) 
    "drftB_200m_2007_0515_6hr.pos", 39139,
    "drftB_300m_2007_0515_6hr.pos", 39139, 
    "drftB_100m_2007_0517_6hr.pos", 39205, # Hatch date May 5 (GMT =  39204) - last day of roms run is the capture date May 17 (GMT = 39217) 
    "drftB_200m_2007_0517_6hr.pos", 39205, 
    "drftB_300m_2007_0517_6hr.pos", 39205,    
    "drftB_400m_2007_0517_6hr.pos", 39205,
    "drftB_500m_2007_0517_6hr.pos", 39205, 
    "drftB_600m_2007_0517_6hr.pos", 39205,
    
    # 1993
    #"drftB_375m_1993_0429_6hr.pos", 34035  #  Skipping zeroes March 9 (GMT =  34035) - April 29 (GMT = 34086; last day of roms run). This must get used for the larval drift tracks not the calculation of current speed???
    "drftB_400m_1993_0416_6hr.pos", 34060, #  Hatch date April 3 (GMT =  34060) - last day of roms run is the capture date April 16 (GMT = 34073) 
    "drftB_500m_1993_0416_6hr.pos", 34060,
    "drftB_525m_1993_0416_6hr.pos", 34060,
    "drftB_400m_1993_0423_6hr.pos", 34063, #  Hatch date April 6 (GMT = 34063) - capture date April 23 (GMT = 34080 [incl. 34080.75]; last day of roms run) #  & gmt <= 34080
    "drftB_500m_1993_0423_6hr.pos", 34063, 
    "drftB_100m_1993_0429_6hr.pos", 34016, #  Hatch date Feb 18 (GMT =  34016) - last day of roms run is the capture date April 29 (GMT = 34086) 
    "drftB_200m_1993_0429_6hr.pos", 34016, 
    "drftB_300m_1993_0429_6hr.pos", 34016, 
    "drftB_375m_1993_0429_6hr.pos", 34016  #  Not skipping zeroes March 9 (GMT =  34035) - April 29 (GMT = 34086; last day of roms run). This must get used for the larval drift tracks not the calculation of current speed???
    
  ), ncol = 2, byrow = TRUE)) |> 
    dplyr::rename(file = X1, gmt = X2) |> 
    dplyr::mutate(gmt = as.numeric(gmt))
  
  # if we have a specified start date (GMT) in file_cutoff_dates (see above for 1993), TRUE and crop data accordingly
  if (a[i] %in% file_cutoff_dates$file) { 
    temp <- temp |>
      # include all date-times this gmt and later. the whole number represents midnight, which is when we want to start everything and includes everything within that whole number (GMT = 1234 through 1234.75, and later)
      dplyr::filter(gmt >= file_cutoff_dates$gmt[file_cutoff_dates$file == a[i]])
  }
  
  ## Add human-readable data attributes (date, events, lat/lon) ----------------
  # the gmt date is converted to a human readable date in the script. 
  # roms_dat should have your human readable dates in it. We convert gmt to a month day year here:
  temp <- temp |> 
    dplyr::mutate(
      date_md = format(date0, format = "%B %d"), # For example Feb 17 1993 (GMT =  34015) 
      date_mdy = format(date0, format = "%B %d, %Y"), 
      date = paste0(format(min(date0), format = "%B %d"), " - ", # " -\n", 
                    format(max(date0), format = "%B %d, %Y")),
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
      dist_km_proj = NA, 
      dist_nmi_rad = NA)
  
  
  # add projected data points to the .pos file data
  temp <- temp |>
    dplyr::bind_cols(temp |>
                       sf::st_coordinates())
  
  ## Calculate distances -------------------------------------------------------
  # Calculate distance between each point and its consecutive point
  for (ii in 2:(nrow(temp))){
    # METHOD 1: CALCULATE DISTANCE WITH SF PROJECTION ESPG:3338
    temp1 <- temp[(ii-1):(ii),] |>
      sf::st_distance()
    units(temp1)$numerator <- "km" # convert matrix unit from meters (m) to kilometers (km)
    temp$dist_km_proj[ii] <- temp1[2,1] # pull distance from in the matrix and add to vector
    
    ## METHOD 2: CALCULATE DISTANCE IN RADIANS, using excel from MACE equations
    temp$lat_rad_diff[ii] <- (temp$lat_rad[ii] - temp$lat_rad[ii+1])
    temp$lon_rad_diff[ii] <- (temp$lon_rad[ii] - temp$lon_rad[ii+1])
    temp$dist_nmi_rad[ii] <- # in excel: # ACOS((SIN(R9)*SIN(R10))+(COS(R9)*COS(R10)*COS(T10)))/(PI()/180)*60
      acos((sin(temp$lat_rad[ii+1])*sin(temp$lat_rad[ii])) +
             (cos(temp$lat_rad[ii+1])*cos(temp$lat_rad[ii])*
                cos(temp$lon_rad_diff[ii])))/(pi/180)*60 
  }
  
  temp <- temp |> 
    dplyr::mutate(
      dist_km_rad = dist_nmi_rad*1.852, 
      dist_nmi_proj = dist_km_proj/1.852
    ) 
  
  # Add data from this .pos file to master table of all .pos's data ------------
  roms_dat <- roms_dat |>
    dplyr::bind_rows(temp)
}

### Add filter for year if I want to run 1 year/specific years at a time. 
#roms_dat <- roms_dat |> 
#dplyr::filter(year ==  c(2009))

#roms_dat <- roms_dat |> 
#dplyr::filter(year ==  c(2009, 1993))

## Calculate distances and current speed ---------------------------------------

roms_dat <- roms_dat |> 
  dplyr::ungroup() |>
  dplyr::arrange(depth_m)  |> 
  dplyr::mutate(
    year = factor(year, ordered = TRUE), 
    depth_m = factor(depth_m, ordered = TRUE), 
    dist_km = dist_km_rad, # DECISION POINT - using radians
    dist_nmi = dist_nmi_rad, # DECISION POINT #1: Use method RADIANS/METHOD 2
    # dist_km = dist_km_proj, # DECISION POINT - using projected
    # dist_nmi = dist_nmi_proj, # DECISION POINT #2: Use method PROJECTED/METHOD 1
    # velocity_kmhr = dist_km/6, # distance (km) over time (6 hours)
    currentspeed_cms = (dist_km*100000)/(60*60*6) # distance (cm) over time (in seconds, 6 hours between)
  )

# Create lines from points and travel summaries --------------------------------

### Calculation of average speed. Therefore SD method preferred.
roms_dat_lines <- roms_dat |>
  dplyr::filter(currentspeed_cms != 0) |> # issue for 375 m 1993 from feb 18-march 8
  dplyr::group_by(date, year, depth_m, filename) |>
  dplyr::summarise(
    do_union = FALSE,
    gmt_min  = min(gmt, na.rm = TRUE),
    gmt_max  = max(gmt, na.rm = TRUE),
    dist_km_sum = sum(dist_km, na.rm = TRUE), 
    dist_nmi_sum = sum(dist_nmi, na.rm = TRUE), 
    obs = n(), 
    # METHOD 1: average of all speed observations (all values/count aka number of rows)
    currentspeed_cms_mean_byobs = mean(currentspeed_cms, na.rm = TRUE), 
    currentspeed_cms_sd_byobs = sd(currentspeed_cms, na.rm = TRUE), 
    currentspeed_cms_min = min(currentspeed_cms, na.rm = TRUE), 
    currentspeed_cms_max = max(currentspeed_cms, na.rm = TRUE)) |> 
  sf::st_cast("LINESTRING") |> 
  dplyr::ungroup() |> 
  dplyr::arrange(gmt_min) |> 
  dplyr::mutate(
    date = factor(x = date, levels = date, labels = date, ordered = TRUE),
    # year = factor(x = year, levels = year, labels = year, ordered = TRUE),
    # total distance divided by total time - em thinks is less precise  
    currentspeed_cms_mean_bytotaldisttime = (dist_km_sum*100000)/(24*(obs/4)*60*60), # METHOD 2
    currentspeed_cms_mean = currentspeed_cms_mean_byobs) # DECISION POINT #2: Use OBS mean and SD/METHOD 1 


# Plots ------------------------------------------------------------------------

## Plot map off all fish by depth ----------------------------------------------

p21 <- ggplot2::ggplot() +
  
  ### Map shapefile aesthetics
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
  
  ### Plot data
  
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
  ggplot2::scale_color_viridis_d(name = "Depth (m)", option = "D", begin = .2, end = .8) + 
  ggplot2::ggtitle(label = "Larval Grenadier Modeled ROMS Dispersal", 
                   subtitle = "At different depths, years, and environmental conditions") + 
  ggplot2::scale_shape_discrete(name = "Event", 
                                na.value = NA,
                                na.translate = FALSE) +
  
  ### Plot aesthetics
  
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

#### no facet, plot everything together ----------------------------------------

p21 # everything altogether

#### facet by date -------------------------------------------------------------
pp <- p21 + ggplot2::facet_wrap(~factor(date), 
                                labeller = label_wrap_gen(width = 25), 
                                ncol = 3)
pp

# change file names by year
ggsave(filename = paste0("./output/Gren_larv_ROMS_plot21_facetdate.png"),
       plot=pp, 
       width=6, height=6)
ggsave(filename = paste0("./output/Gren_larv_ROMS_plot21_facetdate.tiff"),
       plot=pp,
       width=6, height=6)

#### facet by year -------------------------------------------------------------------
pp <- p21 + ggplot2::facet_wrap(~year)
pp

# change file names by year
ggsave(filename = paste0("./output/Gren_larv_ROMS_plot21_facetyear.png"),
       plot=pp, 
       width=6, height=6)
ggsave(filename = paste0("./output/Gren_larv_ROMS_plot21_facetyear.tiff"),
       plot=pp, 
       width=6, height=6)

# Summary and Comparison Tables ------------------------------------------------

## ROMS Path Summary Table (pretty docx) --------------------------------------------

t21 <- roms_dat_lines |> 
  dplyr::mutate(depth_m = as.numeric(paste0(depth_m))) |>
  sf::st_drop_geometry() |> 
  dplyr::select(-year, -currentspeed_cms_mean) |> # -obs,  , -currentspeed_cms_mean_bytotaldisttime, -currentspeed_cms_mean_by_averagecms) |> 
  flextable::flextable()  |> 
  flextable::set_header_labels(
    date = "Date range",
    gmt_min = "Min GMT date",
    gmt_max = "Max GMT date",
    obs = "Number of 6 hour intervals",
    depth_m = "Current Depth (m)",
    dist_km_sum ="Distance traveled (km)",
    dist_nmi_sum ="Distance traveled (nmi)",
    currentspeed_cms_mean_byobs = "Mean current speed (cm/s)",
    currentspeed_cms_sd_byobs = "SD current speed (cm/s)",
    currentspeed_cms_min = "Low current speed (cm/s)",
    currentspeed_cms_max = "High current speed (cm/s)", 
    currentspeed_cms_mean_bytotaldisttime = "Mean current speed by TOTAL (cm/s)"
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
save_as_docx(t21, path = paste0("./output/Gren_larv_ROMS_summary_speed.docx"))

### Tables to compare current speed and distance methods -----------------------

compare_roms_outputs_dat <- 
  read_excel( # METHOD 3: calculate radians by hand in Excel and import
    path = "./data/StationDistanceCalc_6hr_AnnotatedMACE_Paquin.xlsx", 
    skip = 5) |> 
  janitor::clean_names() |> 
  dplyr::select(lat = dec_lat, lon = dec_long, dist_nmi_radexcel = distance_nm) |> 
  dplyr::mutate(lon = lon*-1, 
                depth_m = 300,
                year = 1993,
                dist_km_radexcel = dist_nmi_radexcel*1.852) |> 
  dplyr::filter(!is.na(lat)) |>
  dplyr::full_join( # add roms data to this table for comparison
    roms_dat |> # compare all available data
      sf::st_drop_geometry() |> 
      dplyr::mutate(
        year = as.numeric(paste0(year)), 
        depth_m = as.numeric(paste0(depth_m))) |> 
      dplyr::select(gmt, lat, lon, year, depth_m, 
                    dist_nmi_proj, dist_km_proj, 
                    dist_nmi_rad, dist_km_rad)
    ) |> 
  dplyr::mutate(
    dist_km_diff_rad_radexcel = dist_km_rad - dist_km_radexcel, 
    dist_km_diff_proj_rad = dist_km_proj - dist_km_rad, 
    dist_km_diff_proj_radexcel = dist_km_proj - dist_km_radexcel, 
    dist_nmi_diff_rad_radexcel = dist_nmi_rad - dist_nmi_radexcel, 
    dist_nmi_diff_proj_rad = dist_nmi_proj - dist_nmi_rad, 
    dist_nmi_diff_proj_radexcel = dist_nmi_proj - dist_nmi_radexcel
  ) |> 
  dplyr::relocate(gmt, year, lat, lon, depth_m, 
                  dist_km_proj, 
                  dist_km_rad, 
                  dist_km_radexcel, 
                  dist_km_diff_rad_radexcel, 
                  dist_km_diff_proj_rad, 
                  dist_km_diff_proj_radexcel, 
                  dist_nmi_proj, 
                  dist_nmi_rad, 
                  dist_nmi_radexcel, 
                  dist_nmi_diff_rad_radexcel, 
                  dist_nmi_diff_proj_rad, 
                  dist_nmi_diff_proj_radexcel)

write.csv(x = compare_roms_outputs_dat, file = here::here("output/compare_roms_outputs_dat.csv"))

compare_roms_outputs_dat_sums <- compare_roms_outputs_dat |> 
  dplyr::group_by(depth_m, year) |>
  dplyr::summarise(
    #km
    dist_km_proj = sum(dist_km_proj, na.rm = TRUE), 
    dist_km_radexcel = sum(dist_km_radexcel, na.rm = TRUE), 
    dist_km_rad = sum(dist_km_rad, na.rm = TRUE), 
    dist_km_diff_rad_radexcel = sum(dist_km_diff_rad_radexcel, na.rm = TRUE), 
    dist_km_diff_proj_rad = sum(dist_km_diff_proj_rad, na.rm = TRUE), 
    dist_km_diff_proj_radexcel = sum(dist_km_diff_proj_radexcel, na.rm = TRUE), 
    # nmi
    dist_nmi_proj = sum(dist_nmi_proj, na.rm = TRUE), 
    dist_nmi_radexcel = sum(dist_nmi_radexcel, na.rm = TRUE), 
    dist_nmi_rad = sum(dist_nmi_rad, na.rm = TRUE), 
    dist_nmi_diff_rad_radexcel = sum(dist_nmi_diff_rad_radexcel, na.rm = TRUE), 
    dist_nmi_diff_proj_rad = sum(dist_nmi_diff_proj_rad, na.rm = TRUE), 
    dist_nmi_diff_proj_radexcel = sum(dist_nmi_diff_proj_radexcel, na.rm = TRUE)) |> 
  dplyr::ungroup() |> 
  dplyr::mutate(
    #   dist_km_diff_tot_radexcel_r = dist_km_radexcel - dist_km_r,
    #   dist_km_diff_tot_radexcel_rad = dist_km_radexcel - dist_km_rad,
    #   dist_nmi_diff_tot_radexcel_r = dist_nmi_rad_radexcel - dist_nmi_r, 
    #   dist_nmi_diff_tot_radexcel_rad = dist_nmi_rad_radexcel - dist_nmi_rad, 
    obs_radexcel = read_excel(
      path = "./data/StationDistanceCalc_6hr_AnnotatedMACE_Paquin.xlsx", 
      skip = 5) |> 
      janitor::clean_names() |> 
      dplyr::select(lat) |> 
      dplyr::filter(!is.na(lat)) |> 
      nrow(), 
    obs_r = nrow(temp))

# summary(compare_roms_outputs_dat)
compare_roms_outputs_dat_sums
write.csv(x = compare_roms_outputs_dat_sums, file = here::here("output/compare_roms_outputs_dat_sums.csv"))

