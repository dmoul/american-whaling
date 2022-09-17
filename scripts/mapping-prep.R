# mapping-prep.R

######
# World map example from https://www.simoncoulombe.com/2020/11/animated-ships/
world <- rnaturalearth::ne_countries(scale='medium', returnclass = 'sf') %>%
  st_transform(crs = my_proj)

world_graticule <- st_graticule(lat = c(-90, seq(-80, 80, 20), 90)) %>%
  st_transform(my_proj)

# create water polygon for background 
water_outline <- 
  list(cbind(c(rep(c(180, -180), each = 181), 180), # longs
             c(90:-90, -90:90, 90) # lats
  )
  ) %>%
  st_polygon() %>%
  st_sfc(crs = "WGS84") %>%
  st_sf() %>%
  st_transform(my_proj)


# test
# ggplot() +
#   geom_sf(data = water_outline,
#           fill = "black") +
#   geom_sf(data = world_graticule, 
#           color = "gray80", size = 0.25/.pt
#   ) +
#   geom_sf(data = world, 
#           fill = "#E69F00B0", # "tan",   #"sandy brown"  # "#E69F00B0"
#           color = NA, #"white smoke", #"snow"
#           size = 0.2)


###### define-plot-voyages-function}
# voyage_subset_for_plot <- tibble(voyage_id = sample(good_tracks, 5)) # use this to get 5 known good voyage tracks
plot_voyages <- function(df_voyages = df_voyages_for_plot,
                         df_voyage_ids = NULL,
                         title = "Whaling voyage tracks",
                         file_name = NULL,
                         plot_height = 8.5,
                         plot_width = 15) {

  # OUTPUT: plot world map (if no file_name is provided, otherwise save plot as file)
  # Assumes water_outline, world_graticule, and wrld_wrap exist in the parent environment
  # Assumes df_voyages, df_voyage_ids are passed in or df_voyages_for_plot exists in the parent environment
  # Assumes df_voyages_for_plot_strikes is available in the parent env
  
  # test
  # df_voyages = voyages_for_plot
  # df_voyage_ids <- tibble(voyage_id = c("AV15166"))
  # title = "Whaling voyage tracks"
  
  if(is.null(df_voyage_ids)) abort("plot_voyages: must provide 'df_voyage_ids'")
  
  p <- ggplot() +
    geom_sf(data = water_outline, 
            fill = "black") +
    geom_sf(data = world_graticule, 
            color = "gray80", size = 0.25/.pt
    ) +
    geom_sf(data = world,
            fill = "#E69F00B0",
            color = NA,
            size = 0.2) +
    # add track lines
    geom_sf(data = df_voyages %>% 
              filter(voyage_id %in% df_voyage_ids$voyage_id) %>%
              mutate(voyage_id_name = glue("{voyage_id}: {voyage_name}"),
                     voyage_id_name = fct_reorder(voyage_id_name, log_date_min)) %>%
              st_transform(my_proj),
            aes(color = voyage_id_name),
            size = 0.3, alpha = 1) +
    # add points for strikes
    geom_sf(data = df_voyages_for_plot_strikes %>%
              filter(voyage_id %in% df_voyage_ids$voyage_id) %>%
              st_transform(my_proj),
            size = 0.3, alpha = 1, color = "white") +
    dark_mode() +
    theme(legend.position = "right") +
    guides(color = guide_legend(override.aes = list(size = 2))) +
    labs(title = title,
         caption = my_caption)
  
  if (is.null(file_name)) {
    print(p)
  } else {
    ggsave(filename = file_name,
           p,
           height = plot_height,
           width = plot_width)
  }
}


###### define-plot-voyages-all-function}
# voyage_subset_for_plot <- tibble(voyage_id = sample(good_tracks, 5)) # use this to get 5 known good voyage tracks
plot_voyages_all <- function(df_voyages = df_voyages_for_plot,
                         df_voyage_ids, # = voyage_subset_for_plot,
                         title = "Whaling voyage tracks",
                         file_name = NULL,
                         plot_height = 8.5,
                         plot_width = 15) {
  # OUTPUT: plot world map (if no file_name is provided, otherwise save plot as file)
  # Assumes water_outline, world_graticule, and world exist in the parent environment
  # Assumes df_voyages_for_plot_strikes exists in the parent environment
  # defined in constants.R: my_color_blue, my_color_yellow
  
  # test
  # df_voyages = voyages_for_plot
  # df_voyage_ids <- tibble(voyage_id = c("AV15166"))
  # title = "Whaling voyage tracks"
  
  if(is.null(df_voyage_ids)) abort("plot_voyages_all: must provide 'df_voyage_ids'")
  
  p <- ggplot() +
    geom_sf(data = water_outline, 
            fill = "black") +
    geom_sf(data = world_graticule, 
            color = "gray80", size = 0.25/.pt
    ) +
    geom_sf(data = world,
            fill = "#E69F00B0",
            color = NA,
            size = 0.2) +
    # add track lines
    geom_sf(data = df_voyages %>% 
              filter(voyage_id %in% df_voyage_ids$voyage_id) %>%
              mutate(voyage_id_name = glue("{voyage_id}: {voyage_name}"),
                     voyage_id_name = fct_reorder(voyage_id_name, log_date_min)) %>%
              st_transform(my_proj),
            aes(color = year(log_date_min), fill = year(log_date_min)),
            size = 0.3, alpha = 1) +
    # add points for strikes
    geom_sf(data = df_voyages_for_plot_strikes %>%
              filter(voyage_id %in% df_voyage_ids$voyage_id) %>%
              st_transform(my_proj),
            size = 0.3, alpha = 1, color = "white") +
    scale_color_gradient(low = my_color_blue, high = my_color_yellow,
                         limits = c(1784,1920)) +
    scale_fill_gradient(low = my_color_blue, high = my_color_yellow,
                         limits = c(1784,1920)) +
    dark_mode() +
    theme(legend.position = "right") +
    guides(color = "none") +
    labs(title = title,
         fill = "Year of\ndeparture",
         caption = my_caption)
  
  if (is.null(file_name)) {
    print(p)
  } else {
    ggsave(filename = file_name,
           p,
           height = plot_height,
           width = plot_width)
  }
  
}


###### define-plot-whale-sightings-strikes-function
plot_whale_sightings_strikes <- function(df_voyages = df_voyages_for_plot,
                                         df_voyage_ids, # = voyage_subset_for_plot,
                                         title = "Whale sightings and strikes noted in logs",
                                         file_name = NULL,
                                         plot_height = 8.5,
                                         plot_width = 15) {
  # OUTPUT: world map with points where strikes are recorded in logbook table
  #           plot world map (if no file_name is provided, otherwise save plot as file)
  # Assumes water_outline, world_graticule, and world exist in the parent environment
  # Assumes df_voyages_for_plot_strikes and df_voyages_for_plot_sightings exist in the parent environment
  # defined in constants.R: my_color_blue, my_color_yellow
  
  # test
  # df_voyages = voyages_for_plot_sightings
  # df_voyage_ids = cwm_track #cwm_voyages$voyage_id
  # df_voyage_ids <- tibble(voyage_id = c("AV15166"))
  
  if(is.null(df_voyage_ids)) abort("plot_whale_sightings_strikes: must provide 'df_voyage_ids'")
  
  p <- ggplot() +
    geom_sf(data = water_outline,
            fill = "black") +
    geom_sf(data = world_graticule, 
            color = "gray80", size = 0.25/.pt
    ) +
    geom_sf(data = world,
            fill = "#E69F00B0",
            color = NA,
            size = 0.2) +
    # add points for strikes
    geom_sf(data = df_voyages_for_plot_strikes %>%
              filter(voyage_id %in% df_voyage_ids$voyage_id) %>%
              mutate(species = factor(species, levels = species_levels)) %>% #TODO: why isn't this having an effect?
              st_transform(my_proj),
            aes(color = year(log_date)), 
            size = 1, alpha = 1) +
    # add points for sightings
    geom_sf(data = df_voyages_for_plot_sightings %>%
              filter(voyage_id %in% df_voyage_ids$voyage_id) %>%
              st_transform(my_proj),
            aes(color = year(log_date)),
            size = 1, alpha = 1) +
    scale_color_gradient(low = my_color_blue, high = my_color_yellow,
                         limits = c(1784,1920)) +
    dark_mode() +
    theme(legend.position = "right") +
    labs(title = title,
         subtitle = "Each encounter involved one or more (sometimes many) whales",
         color = "Year of\nencounter",
         caption = my_caption)
  
  if (is.null(file_name)) {
    print(p)
  } else {
    ggsave(filename = file_name,
           p,
           height = plot_height,
           width = plot_width)
  }
  
}


###### define-plot-whale-sightings-species
# voyage_subset_for_plot <- tibble(voyage_id = sample(good_tracks, 5)) # use this to get 5 known good voyage tracks
plot_whale_sightings_species <- function(spec,
                                         title = glue("Whale sightings and strikes"),
                                         file_name = NULL,
                                         plot_height = 8.5,
                                         plot_width = 15) {

  # OUTPUT: world map with points where spec species were recorded in the logbook table
  #           plot world map (if no file_name is provided, otherwise save plot as file)
  # Assumes water_outline, world_graticule, and world exist in the parent environment
  # Assumes df_log exists in the parent environment
  # defined in constants.R: my_color_blue, my_color_yellow
  
  # test
  # df_voyages = voyages_for_plot_sightings
  # df_voyage_ids = cwm_track #cwm_voyages$voyage_id
  # df_voyage_ids <- tibble(voyage_id = c("AV15166"))
  # spec <- "Sperm"
  # spec <- "Pilot|Bowhead|Humback|Orca|Gray|Blue|Dolphin"
  
  p <- ggplot() +
    geom_sf(data = water_outline,
            fill = "black") +
    geom_sf(data = world_graticule,
            color = "gray80", size = 0.25/.pt
    ) +
    geom_sf(data = world,
            fill = "#E69F00B0",
            color = NA,
            size = 0.2) +
    geom_sf(data = df_log %>%
              filter(str_detect(species, spec)) %>%
              st_as_sf(coords = c("lon", "lat"), crs = "WGS84") %>%
              st_transform(my_proj),
            aes(color = year(log_date)),
            size = 0.2, alpha = 0.6) +
    scale_color_gradient(low = my_color_blue, high = my_color_yellow,
                         limits = c(1784,1920)) +
    dark_mode() +
    theme(legend.position = "right",
          plot.subtitle = element_markdown(lineheight = 1.2)) + # TODO: markdown not working
    labs(title = glue("{title} - Species: {spec}"),
         subtitle = glue("Each encounter involved one or more (sometimes many) whales"),
         color = "Year of\nencounter",
         caption = my_caption)
  
  if (is.null(file_name)) {
    print(p)
  } else {
    ggsave(filename = file_name,
           p,
           height = plot_height,
           width = plot_width)
  }
  
}

###### define-plot-whale-sightings-species-year-range
# voyage_subset_for_plot <- tibble(voyage_id = sample(good_tracks, 5)) # use this to get 5 known good voyage tracks
plot_whale_sightings_species_year_range <- function(spec, year_start, year_end) {
  # For use in creating gif or mp4 animations
  # INPUT: 
  # OUTPUT: 
  # Assumes water_outline, world_graticule, and world exist in the parent environment
  # Assumes df_log exists in the parent environment
  # defined in constants.R: my_color_blue, my_color_yellow
  
  # test
  # df_voyages = voyages_for_plot_sightings
  # df_voyage_ids = cwm_track #cwm_voyages$voyage_id
  # df_voyage_ids <- tibble(voyage_id = c("AV15166"))
  # spec <- "Sperm"
  # spec <- "Pilot|Bowhead|Humback|Orca|Gray|Blue|Dolphin"
  
  p <- ggplot() +
    geom_sf(data = water_outline,
            fill = "black") +
    geom_sf(data = world_graticule,
            color = "gray80", size = 0.25/.pt
    ) +
    geom_sf(data = world,
            fill = "#E69F00B0",
            color = NA,
            size = 0.2) +
    geom_sf(data = df_log %>%
              filter(str_detect(species, spec)) %>%
              filter(between(year, year_start, year_end)) %>%
              st_as_sf(coords = c("lon", "lat"), crs = "WGS84") %>%
              st_transform(my_proj),
            aes(color = year(log_date)),
            size = 0.1, alpha = 0.7) +
    scale_color_gradient(low = my_color_blue, high = my_color_yellow,
                         limits = c(1784,1920)) +
    dark_mode() +
    theme(legend.position = "right",
          plot.subtitle = element_markdown(lineheight = 1.2)) + # TODO: markdown not working
    labs(title = glue("{spec} whale encounters (sightings and strikes) {year_start}-{year_end}"),
         subtitle = "Each encounter involved one or more (sometimes many) whales",
         color = "Year of\nencounter",
         caption = my_caption)
  
  return(p)
  
}


###### define-make-whale-yearly-video
make_whale_yearly_video <- function(
    my_species = "Right",
    yr_start = NA_real_,
    yr_end = NA_real_) {
  
  # INPUT: 
  # OUTPUT: path+filename of mp4 video
  # assumes df_log is available in parent environment
  #         we don't use more than 999 png files to make the video
  # defined in constants.R: dir_out_temp, dir_out
  
  # test
  # my_species <- "Right"
  # yr_start <- 1830
  # yr_end <- 1840
  
  df_log_subset<- df_log %>%
    filter(species %in% my_species)
  yr_start <- if_else(is.na(yr_start), min(df_log_subset$year), yr_start)
  yr_end <- if_else(is.na(yr_end), max(df_log_subset$year), yr_end)
  
  video_fname_out <- paste0(dir_out, glue_collapse(my_species), "-whale-encounters-", yr_start, "-", yr_end, ".mp4")
  
  if(!file_exists(video_fname_out)) {
    if(!dir_exists(dir_out_temp)) dir_create(dir_out_temp)
    file_delete(dir_ls(dir_out_temp, type = "file", grob = "\\d{3}.png"))
    if(!dir_exists(dir_out)) dir_create(dir_out)
    for(yr in yr_start:yr_end) {
      plot_temp <- plot_whale_sightings_species_year_range(spec = my_species,
                                                           year_start = yr_start,
                                                           year_end = yr)
      ggsave(paste0(dir_out_temp, 
                    str_pad(yr - yr_start + 1, width = 3, side = "left", pad = "0"), 
                    ".png"),
             plot = plot_temp,
             width = 1280, height = 720, units = "px",  dpi = 108
      )
    }
    
    imgs <- dir_ls(dir_out_temp, type = "file", grob = "*.png")
    names(imgs) <- NULL # this may not be needed
    
    # duplicate last image as first one, so it shows before someone clicks "play"
    last_img <- last(imgs)
    file_copy(last_img, paste0(dir_out_temp, "000.png"))
    imgs <- dir_ls(dir_out_temp, type = "file", grob = "*.png")
    
    av_encode_video(imgs,
                    output = video_fname_out,
                    framerate = 4)
    
    file_delete(dir_ls(dir_out_temp, type = "file", grob = "\\d{3}.png"))
    
  }
  video_fname_out
}


###### define-plot-whale-seasonal-encounter-species
# voyage_subset_for_plot <- tibble(voyage_id = sample(good_tracks, 5)) # use this to get 5 known good voyage tracks
plot_whale_seasonal_encounter_species <- function(spec,
                                                  title = "Seasonality of whale sightings and strikes",
                                                  file_name = NULL,
                                                  plot_height = 8.5,
                                                  plot_width = 15) {
  # INPUT: species string
  # OUTPUT: global map of encounters with species matching the species string (can be string with regular expression "|" providing "or")
  #           plot world map (if no file_name is provided, otherwise save plot as file)
  # Assumes water_outline, world_graticule, world, and df_log exist in the parent environment
  
  # test
  # df_voyages = voyages_for_plot_sightings
  # df_voyage_ids = cwm_track #cwm_voyages$voyage_id
  # df_voyage_ids <- tibble(voyage_id = c("AV15166"))
  # spec <- "Sperm"
  # spec <- "Pilot|Bowhead|Humback|Orca|Gray|Blue|Dolphin"
  
  df_log_for_plot_encounter <- df_log %>%
    filter(str_detect(species, spec),
           encounter %in% c("Sight", "Strike")) %>%
    mutate(enc_season = case_when(
      month %in% c(12, 1, 2)    ~ "Northern winter",
      month %in% c(3, 4, 5)     ~ "Northern spring",
      month %in% c(6, 7, 8)     ~ "Northern summer",
      month %in% c(9, 10, 11)   ~ "Northern autumn"
    ),
    enc_season = factor(enc_season, 
                        levels = c("Northern winter", "Northern spring", "Northern summer", "Northern autumn"))
    )
  
  n_encounters <- nrow(df_log_for_plot_encounter)
  
  p <- ggplot() +
    geom_sf(data = water_outline,
            fill = "black") +
    geom_sf(data = world_graticule,
            color = "gray80", size = 0.25/.pt
    ) +
    geom_sf(data = world,
            fill = "#E69F00B0",
            color = NA,
            size = 0.2) +
    geom_sf(data = df_log_for_plot_encounter %>%
              st_as_sf(coords = c("lon", "lat"), crs = "WGS84") %>%
              st_transform(my_proj),
            aes(color = enc_season), 
            size = 0.2, alpha = 0.6) + 
    dark_mode() +
    theme(legend.position = "right",
          plot.subtitle = element_markdown(lineheight = 1.2)) + # TODO: markdown not working
    guides(color = guide_legend(override.aes = list(size = 3))) +
    labs(title = glue("{title} - Species: {spec}"),
         subtitle = glue("Each of the {comma(n_encounters)} encounters involved one or more (sometimes many) whales"),
         color = "Season",
         caption = my_caption)
  
  if (is.null(file_name)) {
    print(p)
  } else {
    ggsave(filename = file_name,
           p,
           height = plot_height,
           width = plot_width)
  }
  
}

## print_voyage_table() is not used
###### define-subset-details-table-function
# print_voyage_table <- function(df_voyage_ids = voyage_subset_for_plot) {
#   # Assumes df_voyages exists in the parent environment
#   # Assumes voyage_subset_for_plot exists in the parent environment if df_voyage_ids not set 
#   
#   # *** SHOULD BE FIXED NOW ***
#   # TODO: address this warning: 
#   #   Warning: Problem with `mutate()` column `tonnage_max`.
#   # ℹ `tonnage_max = max(c_across(x1:x5), na.rm = TRUE)`.
#   # ℹ no non-missing arguments to max; returning -Inf
#   # ℹ The warning occurred in row 1.
#   
#   voyage_subset_details <- df_voyage_ids %>%
#     inner_join(.,
#                df_voyage,
#                by = "voyage_id") %>%
#     inner_join(.,
#                voyages_for_plot %>% select(voyage_id, voyage_duration_days, n_obs, 
#                                            voyage_distance_km, voyage_ave_distance_km_day, 
#                                            log_date_min, log_date_max),
#                by = "voyage_id") %>%
#     separate(tonnage, sep = "/", into = c("x1", "x2", "x3", "x4", "x5"), 
#              extra = "merge", fill = "right", convert = TRUE) %>%
#     mutate(across(x1:x5, ~ ifelse(is.na(.x), "0", .x))) %>%
#     mutate(across(x1:x5, ~ readr::parse_number(.x))) %>%
#     rowwise() %>%
#     mutate(tonnage_max = max(c_across(x1:x5), na.rm = TRUE)#,
#            #tonnage_max = ifelse(is.infinite(tonnage_max), NA_real_, tonnage_max)
#     ) %>%
#     select(-(x1:x5)) %>%
#     distinct(voyage_id, .keep_all = TRUE) # shouldn't be needed
#   
#   my_gt <- voyage_subset_details %>%
#     arrange(log_date_min) %>%
#     select(voyage_id, voyage_name, ground, 
#            bone, sperm, oil, 
#            distance_km = voyage_distance_km, avg_distance_day = voyage_ave_distance_km_day, 
#            n_obs, duration_days = voyage_duration_days, log_date_min, log_date_max,
#            vessel_id, tonnage_max) %>%
#     gt() %>%
#     tab_options(table.font.size = 11) %>%
#     tab_spanner(
#       label = "Voyage",
#       columns = c(voyage_id, voyage_name, ground)
#     ) %>%
#     tab_spanner(
#       label = "Product",
#       columns = c(bone, sperm, oil)
#     ) %>%
#     tab_spanner(
#       label = "Voyage stats",
#       columns = c(distance_km, avg_distance_day, n_obs, n_obs, duration_days, log_date_min, log_date_max)
#     ) %>%
#     tab_spanner(
#       label = "Vessel",
#       columns = c(vessel_id, tonnage_max)
#     ) %>%
#     fmt_missing(columns = everything()) %>%
#     fmt_number(columns = c(bone, sperm, oil),
#                decimals = 0) %>%
#     fmt_number(columns = c(distance_km, avg_distance_day),
#                decimals = 0)
#   
#   my_gt %>%
#     as_raw_html()
#   
# }

