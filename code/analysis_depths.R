
# Load packages and set plotting features --------------------------------------
source("./code/functions.R")

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
    ggplot2::ggplot(mapping = aes(x = canyon_title, y = MAX_GEAR_DEPTH, color = size_bin_label))+
    #Add line at 200 m depth
    ggplot2::geom_hline(yintercept = 200, linewidth = 0.2, color="gray") +
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
    ggplot2::geom_point(position=position_jitter(h=0, w=0.2), # geom_jitter values for 2007 but run multiple X to get horizontal spread
                        mapping = aes(color = size_bin_label), alpha = 0.5, size = 3) +
    
    #reverse y-axis for depth
    ggplot2::scale_y_reverse(limits = c(700, 0), 
                             breaks = seq(0, 700, 100),
                             # minor_breaks = 25, 
                             expand = c(0.03, 0.03)) + 
    ggplot2::facet_wrap(~Year, nrow = 5)+
    ggplot2::labs(x = "Capture location",
                  y = "Maximum gear depth (m)",
                  title = "Grenadier larvae captures in the southeast Bering Sea showing maximum gear depth") + 
    # manually define color for points
    ggplot2::scale_color_manual(
      values = color_palette,
      name = "",
      drop = TRUE) +
    #Add theme backgrnd white###
    ggplot2::theme_bw() +
    ggplot2::theme(
      # text = element_text(family = "monserrat"), # 
      axis.line = element_line(),
      panel.background = element_rect(fill = "#FFFFFF"),
    )
  
  return(p16)
}

## Plot and save data for one year interactively ---------------------------------
for (i in c(1993, 2007, 2008, 2009)) {  
  aaa <- plot_p16(year0 = i) 
  ggsave(filename = paste0("./output/",i,"_gren_larv_capt_cyn_plot16_2009.png"),
         plot=aaa, width=8, height=4)
  ggsave(filename = paste0("./output/",i,"_gren_larv_capt_cyn_plot16_2009.tiff"),
         plot=aaa, width=8, height=4)
  aaa  #Added this code in order to see plot in Plot window
}

## plot and save specific year(s) of data in one plot --------------------------
yrs <- c(1993, 2007, 2008, 2009)
aaa <- plot_p16(year0 = yrs)
ggsave(filename = paste0("./output/", paste0(yrs, collapse = "_"),"_gren_larv_capt_cyn_plot16Test.png"),
       plot=aaa, width=8, height=4)
ggsave(filename = paste0("./output/",paste0(yrs, collapse = "_"),"_gren_larv_capt_cyn_plot16Test.tiff"),
       plot=aaa, width=8, height=4)
aaa

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

