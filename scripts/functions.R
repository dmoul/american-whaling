# functions.R
# mostly called in _targets.R

###### save package list so each chapter can load it before rendering
save_package_list <- function(my_list) {
  my_list
}
  
###### prepare df_voyage from voyages table
prepare_voyage_data <- function() {
  
  # voyage_years_max defined in constants.R
  
  if(file.exists(here("data/processed", "df-voyage.rds"))) { 
    df_voyage <- read_rds(here("data/processed", "df-voyage.rds")) 
    df_voyage_distinct <- read_rds(here("data/processed", "df-voyage-distinct.rds")) 
    
  } else {
    df_voyage_file <- here("data/AmericanOffshoreWhalingVoyages", "voyages_20211020.txt") 
    df_voyage_tmp <- read_tsv(df_voyage_file, show_col_types = FALSE) %>%
      clean_names() %>%
      # fix some data errors
      mutate(day_out = str_trim(str_remove(day_out, " #")),
             day_out = if_else(day_out == "1811 Julky 11", "1811 July 11", day_out),
             vessel_id = str_remove(vessel_id, " \\?"), # let's assume vessel_ids are correct (what else could I do?)
             oil = if_else(voyage_id == "AV00305", 981, oil),
             tonnage = case_when(
               vessel_id == "AS1085"      ~ "362",
               vessel_id == "AS0096"      ~ "252",
               TRUE                       ~ tonnage
             ),
             port = str_replace(port, "(?i)unknown", "Unknown"),
             port = case_when(
               port == "New Bedford, MA  Milford Haven"             ~ "New Bedford, MA",
               port == "New Bedford, MA  Provincetown"              ~ "New Bedford, MA",
               port == "Nantucket, MA or New Bedford"               ~ "Nantucket, MA",
               port == "Gloucester, MA or Cape Ann, MA"             ~ "Gloucester, MA",
               port == "Cold Spring Harbor, NY  Sag Harbor, NY"     ~ "Cold Spring Harbor, NY",
               port == "New London, CT  Middletown"                 ~ "New London, CT",
               port == "Newport, RI  Dartmouth, MA"                 ~ "Newport, RI",
               port == "Norwalk, CT or Sag Harbor, NY"              ~ "Norwalk, CT",
               port == "Rockland, MEast"                            ~ "Rockland, ME",
               port == "Stonington, CT  Mystic, CT"                 ~ "Stonington, CT",
               port == "Westport  Boston, MA"                       ~ "Westport, MA",
               port == "Nantucket, MA  l"                           ~ "Nantucket, MA",
               port == "Nantucket, MA or New Bedford"               ~ "Nantucket, MA",
               port == "New Bedford, MA  Nantucket, MA or Dunkirk"  ~ "New Bedford, MA",
               port == "Nantucket, MA  Dunkirk"                     ~ "Nantucket, MA",
               str_detect(port, "Hudson River")                     ~ "Hudson, NY",
               str_detect(port, "Holmes’ Hole, MA")                 ~ "Holmes Hole, MA",
               TRUE                                                 ~ port
             )
      )
    
    is_last_voyage <- function(my_vessel_id, my_voyage_id) {
      df_voyage %>% 
        filter(vessel_id == my_vessel_id) %>%
        arrange(voyage_id, voyage_rank) %>%
        distinct(voyage_id, .keep_all = TRUE) %>% # keeps only row with first master
        arrange(-year_out, -voyage_rank) %>%
        distinct(vessel_id, .keep_all = TRUE) %>%
        pull(voyage_id) == my_voyage_id
    }
    
    # test
    # is_last_voyage(my_vessel_id = "AS0193",
    #                my_voyage_id =  "AV04153")
    # is_last_voyage(my_vessel_id = "AS0193",
    #                my_voyage_id =  "AV04164")
    
    df_voyage_distinct <- df_voyage_tmp %>%
      arrange(voyage_id, voyage_rank) %>%
      distinct(voyage_id, .keep_all = TRUE) %>% # keeps only row with first master
      # use heuristics to supply more "end" a.k.a. vessel end codes
      mutate(vessel_end_year  = parse_number(str_extract(end, "\\d{4}")),
             new_code = case_when(
               !is.na(return_code)                                                                           ~ return_code,
               !is.na(year_in)                                                                               ~ "OK",
               str_detect(end, "(?i)renamed") & vessel_end_year - year_out < voyage_years_max                ~ "OK",
               str_detect(end, "(?i)lost") & vessel_end_year - year_out < voyage_years_max                   ~ "L",
               str_detect(end, "(?i)wrecked") & vessel_end_year - year_out < voyage_years_max                ~ "L",
               str_detect(end, "(?i)crushed") & vessel_end_year - year_out < voyage_years_max                ~ "L",
               str_detect(end, "(?i)capsized") & vessel_end_year - year_out < voyage_years_max               ~ "L",
               str_detect(end, "(?i)foundered") & vessel_end_year - year_out < voyage_years_max              ~ "L",
               str_detect(end, "(?i)went to pieces") & vessel_end_year - year_out < voyage_years_max         ~ "L",
               str_detect(end, "(?i)exploded") & vessel_end_year - year_out < voyage_years_max               ~ "L",
               str_detect(end, "(?i)capt") & vessel_end_year - year_out < voyage_years_max                   ~ "S",
               str_detect(end, "(?i)taken by") & vessel_end_year - year_out < voyage_years_max               ~ "S",
               str_detect(end, "(?i)seized") & vessel_end_year - year_out < voyage_years_max                 ~ "S",
               str_detect(end, "(?i)aband") & vessel_end_year - year_out < voyage_years_max                  ~ "A",
               str_detect(end, "(?i)cond") & vessel_end_year - year_out < voyage_years_max                   ~ "C",
               str_detect(end, "(?i)broken up") & vessel_end_year - year_out < voyage_years_max              ~ "C",
               str_detect(end, "(?i)sold") & vessel_end_year - year_out < voyage_years_max                   ~ "So",
               str_detect(end, "(?i)withdr") & vessel_end_year - year_out < voyage_years_max                 ~ "W",
               str_detect(end, "(?i)register surrendered") & vessel_end_year - year_out < voyage_years_max   ~ "W",
               str_detect(end, "(?i)missing") & vessel_end_year - year_out < voyage_years_max                ~ "M",
               TRUE                                                                                          ~ "U"
             ),
      ) %>%
      mutate(return_code = coalesce(return_code, new_code)) %>%
      select(-new_code)
    
    write_rds(df_voyage_distinct, file = here("data/processed", "df-voyage-distinct.rds")) 
    
    df_voyage <- df_voyage_tmp %>%
      select(-return_code) %>%
      left_join(.,
                df_voyage_distinct %>%
                  select(voyage_id, return_code),
                by = "voyage_id"
      ) %>%
      relocate(return_code, .after = day_in)
    
    write_rds(df_voyage, file = here("data/processed", "df-voyage.rds")) 
    
  }
}

###### prepare logbook table data
prepare_logbook_data <- function() {
  # days_between_obs_max defined in constants.R
  
  if(file.exists(here("data/processed", "df-log-all.rds"))) { 
    df_log_all <- read_rds(here("data/processed", "df-log-all.rds")) 
    df_log <- read_rds(here("data/processed", "df-log.rds")) 
    
  } else {
    df_log_file <- here("data/AmericanOffshoreWhalingLogbookData", "aowl_download_20180104.txt") 
    
    df_log_all_temp_with_dups <- read_tsv(df_log_file, show_col_types = FALSE) %>%
      clean_names() %>%
      filter(day != 0) %>% # drop 5 obs
      mutate(log_date = ymd(paste0(year, "-", month, "-", day), quiet = TRUE)) %>%
      arrange(log_date) %>%
      group_by(voyage_id, source) %>%
      mutate(days_between_obs = as.double(difftime(log_date, lag(log_date),
                                                   units = "days")),
             first_log_entry = (row_number() == 1),
             n_obs = n()
      ) %>%
      ungroup() %>%
      # deal with multiple entries on the same day
      # there are 9K obs with days_between_obs == 0
      mutate(days_between_obs = if_else(days_between_obs == 0, 0.1, days_between_obs)) %>% 
      filter(!(is.na(days_between_obs) & !first_log_entry)) # remove another row
    
    # where there are duplicate voyage_id + source values, keep CoML or Maury in that order (and Townsend only if it's the only source)
    df_log_all <- df_log_all_temp_with_dups %>%
      filter(first_log_entry) %>%
      arrange(voyage_id, source) %>%
      distinct(voyage_id, .keep_all = TRUE) %>%
      select(voyage_id, source) %>%
      inner_join(df_log_all_temp_with_dups,
                 .,
                 by = c("voyage_id", "source")
      ) %>%
      replace_na(list(n_tried = 0)) %>%
      mutate(species = if_else(str_detect(species, "Grampus|Killer"), "Orca", species))
    
    write_rds(df_log_all, file = here("data/processed", "df-log-all.rds")) 
    
    # data quality checks: these voyages are suspect / of limited use
    df_log_broken_journey <- df_log_all %>%
      filter(days_between_obs > days_between_obs_max) %>%
      distinct(voyage_id, .keep_all = TRUE)
    
    n_voyages_with_logs_incl_broken_voyages <- df_log_all %>%
      filter(first_log_entry) %>%
      distinct(voyage_id)  %>% # this step is likely unnecessary
      nrow()
    
    df_log <- df_log_all %>%
      anti_join(., 
                df_log_broken_journey %>% select(voyage_id),
                by = "voyage_id"
      )
    
    write_rds(df_log, file = here("data/processed", "df-log.rds")) 
    
  }
}

###### prepare inflation (cpi) data 
prepare_cpi <- function(file_path) {
  # INPUT: fully qualified path+file name of excel file
  # OUTPUT: data frame from excel spreadsheet with CPI index 1.0 set to 1845
  #            which was the first year the Charles W. Morgan completed a voyage and sold product
  
  # test
  # library(readxl)
  # file_path <- './data/cpi/inflation-cpi-1800plus.xlsx'

  read_excel(file_path,
             skip = 3,
             col_types = "numeric") |>
    clean_names() |>
    mutate(index_1845 = annual_average_index / annual_average_index[[46]]) # set 1845 to 1.0000
}


###### prepare voyages_for_plot
prepare_voyages_for_plot_data <- function() {
  # the following are defined in constants.R
  #   cutoff_voyage_distance_km
  #   cutoff_voyage_duration_days
  
  if(file.exists(here("data/processed", "voyages-for-plot.rds"))) { 
    voyages_for_plot_sightings <- read_rds(here("data/processed", "voyages-for-plot-sightings.rds")) 
    voyages_for_plot_strikes <- read_rds(here("data/processed", "voyages-for-plot-strikes.rds")) 
    voyages_for_plot_points <- read_rds(here("data/processed", "voyages-for-plot.rds")) 
    voyages_for_plot <- read_rds(here("data/processed", "voyages-for-plot.rds")) 
    
  } else {
    
    if(!file.exists(here("data/processed", "df-log.rds"))) { 
      stop("File does not exist: ", here("data/processed", "df-log.rds"), ". Run `prepare-data.Rmd`\n") 
    }
    
    df_log <- read_rds(here("data/processed", "df-log.rds")) %>% 
      inner_join(.,
                 df_voyage %>% select(voyage_id, voyage_name),
                 by = "voyage_id") %>%
      mutate(vessel = str_extract(voyage_name, ".*(?= \\:)")) %>%
      select(voyage_id, voyage_name, everything())
    
    # following the approach in https://github.com/r-spatial/sf/issues/799
    empty <- st_as_sfc("POINT(EMPTY)", crs = "WGS84") #4326)
    
    voyages_for_plot_sightings <- df_log %>%
      filter(encounter == "Sight") %>%
      st_as_sf(coords = c("lon", "lat"),
               crs = "WGS84")
    
    voyages_for_plot_strikes <- df_log %>%
      filter(encounter == "Strike") %>%
      st_as_sf(coords = c("lon", "lat"),
               crs = "WGS84")
    
    voyages_for_plot_points <- df_log %>%
      st_as_sf(coords = c("lon", "lat"),
               crs = "WGS84") %>% # results in distances in meters
      #st_set_crs(4326) %>%  # will use great circle distance
      group_by(voyage_id, voyage_name, vessel) %>%
      mutate(voyage_duration_days = as.double(difftime(max(log_date), min(log_date),
                                                       units = "days")),
             n_obs = n(),
             distance_m = sf::st_distance(
               geometry, 
               lag(geometry, default = empty), 
               by_element = TRUE),
             distance_km = drop_units(distance_m) / 1000,
             distance_km_per_day = distance_km / days_between_obs
      ) %>%
      select(-distance_m) %>%
      ungroup()
    
    voyages_for_plot <- voyages_for_plot_points %>%
      group_by(voyage_id, voyage_name, vessel)%>%
      mutate(voyage_distance_km = sum(distance_km, na.rm = TRUE),
             voyage_ave_distance_km_day = weighted.mean(distance_km_per_day, w = days_between_obs, na.rm = TRUE),
             log_date_min = min(log_date),
             log_date_max = max(log_date)) %>%
      st_as_sf(coords = c("lon", "lat"),
               crs = "WGS84") %>%
      # following the st_cast() example at
      # https://stackoverflow.com/questions/50908771/create-multilines-from-points-grouped-by-id-with-sf-package
      group_by(voyage_id, voyage_name, vessel,
               voyage_duration_days, n_obs, voyage_distance_km, voyage_ave_distance_km_day, 
               log_date_min, log_date_max) %>%
      arrange(sequence) %>%
      summarise(do_union = FALSE) %>%
      st_cast("LINESTRING") %>%
      ungroup() %>%
      filter(
        # let's assume that any voyage_distance_km shorter than cutoff_voyage_distance_km is an error (is this too pessimistic?)
        voyage_distance_km > cutoff_voyage_distance_km,
        # let's assume that any voyage_duration_days shorter than cutoff_voyage_duration is an error (is this too pessimistic?)
        voyage_duration_days > cutoff_voyage_duration_days) %>% 
      st_wrap_dateline(options = c("WRAPDATELINE=YES", "DATELINEOFFSET=180"),
                       quiet = TRUE) %>%
      st_transform("+proj=moll")
    
    write_rds(voyages_for_plot_sightings, file = here("data/processed", "voyages-for-plot-sightings.rds")) 
    write_rds(voyages_for_plot_strikes, file = here("data/processed", "voyages-for-plot-strikes.rds")) 
    write_rds(voyages_for_plot_points, file = here("data/processed", "voyages-for-plot-points.rds")) 
    write_rds(voyages_for_plot, file = here("data/processed", "voyages-for-plot.rds")) 
    
  }
}

###### prepare df_crew and df_crewlist
prepare_crew_data <- function(voyage_distinct) {
  if(file.exists(here("data/processed", "df_crewlist.rds")) &
     file.exists(here("data/processed", "df_crew.rds"))) { 
    
    # test
    #df_voyage_distinct = tar_read(voyage_distinct)
    
    df_crewlist <- read_rds(here("data/processed", "df_crewlist.rds")) 
    df_crew <- read_rds(here("data/processed", "df_crew.rds")) 
    
  } else {
    df_crewlist_file <- here("data/AmericanOffshoreWhalingCrewlists", "crewlists_20200302.csv") 
    df_crewlist <- read_csv(df_crewlist_file, show_col_types = TRUE) %>%
      clean_names()
    
    df_crew_file  <- here("data/AmericanOffshoreWhalingCrewlists", "crewentries_20200302.csv") 
    df_crew <- read_csv(df_crew_file, show_col_types = TRUE,
                        col_types = cols(
                          crewentryID = col_character(),
                          crewlistID = col_character(),
                          voyageID = col_character(),
                          voyage = col_character(),
                          list_date = col_character(),
                          sequence = col_double(),
                          name_first = col_character(),
                          name_last = col_character(),
                          birthplace = col_character(),
                          res_city = col_character(),
                          res_state = col_character(),
                          res_country = col_character(),
                          citizenship = col_character(),
                          age = col_double(),
                          height_feet = col_double(),
                          height_inches = col_character(), # not col_double() since need to run parse_number() later
                          skin = col_character(),
                          hair = col_character(),
                          eye = col_character(),
                          rank = col_character(),
                          lay = col_character(),
                          remarks = col_character(),
                          transcr_note = col_character(),
                          transcriber = col_character()
                        )
    ) %>%
      clean_names() %>%
      rename(birth_place = birthplace) |>
      mutate(height_inches = parse_number(height_inches),
             height = height_feet * 12 + height_inches,
             skin = str_to_lower(skin),
             # use cleaned names (plus age) to uniquely identify crew when there are multiple crew lists for one voyage
             name_first_clean = str_remove_all(name_first, "[\\[\\(\\]\\)\\?'`-]"),
             name_first_clean = str_squish(str_replace(name_first_clean, "\\.", ' ')),
             # name_first_clean = str_extract(name_first_clean, "([[:alpha:]]( )?)+"), # allow second words/initials
             name_first_clean = str_extract(name_first_clean, "[[:alpha:]]+"), # first word only
             name_last_clean = str_remove_all(name_last, "[\\[\\(\\]\\)\\?'`-]"),
             name_last_clean = str_squish(str_replace(name_last_clean, "\\.", ' ')),
             name_last_clean = str_squish(str_extract(name_last_clean, "([[:alpha:]]( )?)+")),
             name_last_clean = case_when(
               name_last_clean == "Clarke"         ~ "Clark",
               TRUE                                ~ name_last_clean
             )
      ) %>%
      mutate(#rank_original = rank,
             rank_clean = str_to_lower(rank),
             rank_clean = str_remove_all(rank_clean, "\\(|\\)"),
             rank_clean = str_replace_all(rank_clean, c("first" = "1st",
                                            "second" = "2nd",
                                            "third" = "3rd",
                                            "fourth" = "4th",
                                            "fifth" = "5th",
                                            "1 " = "1st ",
                                            "2 |2d(\\.)? " = "2nd ",
                                            "3 " = "3rd ",
                                            "4tn" = "4th",
                                            "\\?" = "",
                                            "\\[|\\]" = "",
                                            "[[[:punct:]]']$" = "",
                                            "ass't" = "assistant")
             ),
             rank_clean = str_trim(rank_clean),
             rank_clean = case_when(
               str_detect(rank_clean, "assistant")                          ~ "assistant",
               str_detect(rank_clean, "apprentice")                         ~ "apprentice",
               str_detect(rank_clean, "^g") & str_detect(rank_clean, "hand")      ~ "greenhand/landsman",
               str_detect(rank_clean, "^o") & str_detect(rank_clean, "seaman")    ~ "ordinary seaman",
               rank_clean == "ordinary"                                     ~ "ordinary seaman",
               str_detect(rank_clean, "boy")                                ~ "boy (various)",
               rank_clean == "3rd mate & boat steerer"                      ~ "3rd mate",
               rank_clean == "2nd/3rd mate"                                 ~ "2nd mate",
               !str_detect(rank_clean, "^(1st|2nd|3rd|4th)") & str_detect(rank_clean, "mate")    ~ "mate (other)",
               str_detect(rank_clean, "offiver")                            ~ "officer",
               str_detect(rank_clean, "cook")                               ~ "cook",
               str_detect(rank_clean, "engineer")                           ~ "engineer",
               str_detect(rank_clean, "carpenter")                          ~ "carpenter",
               str_detect(rank_clean, "cooper")                             ~ "cooper",
               str_detect(rank_clean, "blacksmith")                         ~ "blacksmith",
               str_detect(rank_clean, "sail( )?maker")                      ~ "sailmaker",
               str_detect(rank_clean, "ship(s)?( )?(-)?keeper")             ~ "shipkeeper",
               rank_clean == "ship"                                         ~ "shipkeeper",
               str_detect(rank_clean, "steward")                            ~ "steward",
               str_detect(rank_clean, "boat( )?ste")                        ~ "boatsteerer",
               str_detect(rank_clean, "boathead")                           ~ "boatheader",
               str_detect(rank_clean, "ste.*man")                           ~ "steersman",
               str_detect(rank_clean, "able")                               ~ "able seaman/seaman",
               str_detect(rank_clean, "seaman|[-]man")                      ~ "able seaman/seaman",
               str_detect(rank_clean, "physician|surgeon")                  ~ "physician/surgeon",
               str_detect(rank_clean, "captain")                            ~ "master",
               str_detect(rank_clean, "landsm")                             ~ "greenhand/landsman",
               str_detect(rank_clean, "light hand")                         ~ "light hand",
               str_detect(rank_clean, "sailor")                             ~ "seaman",
               str_detect(rank_clean, "kanaka")                             ~ "other", # kanaka = Pacific islander
               TRUE                                                   ~ rank_clean
             )
             ) |>
      select(ends_with("_id"), voyage, day_out = list_date,
             starts_with("name_"), citizenship, age, height, skin, hair,
             rank, rank_clean, lay, remarks, birth_place, res_city) %>%
      mutate(name_first_clean = str_replace_all(name_first_clean, 
                                                c("\\bGeo\\b"= "George",
                                                  "\\bDanl\\b" = "Daniel",
                                                  "\\bJas\\b" = "James",
                                                  "\\b(Benj|Benjn)\\b" = "Benjamin",
                                                  "\\b(Wm|Wlliam)\\b" = "William",
                                                  "\\b(Abm|Abmn|Abram)\\b" = "Abraham",
                                                  "\\b(Thos|Thm|Tom|Tome)\\b" = "Thomas",
                                                  "\\b(Chas)\\b" = "Charles",
                                                  "\\b(Jjohn|Jhn)\\b" = "John"
                                                ),
      )
      ) |>
      # fix birth_place
      mutate(birth_place_clean = str_squish(str_to_title(str_remove_all(birth_place, "[\\[\\(\\]\\)\\?'`-]"))),
             birth_place_clean = str_replace(birth_place_clean, "New ", "New"),
             birth_place_clean = str_extract(birth_place_clean, "[A-z]+")) |>
      mutate(birth_place_clean = case_when(
        birth_place_clean == "Abington"                  ~ "Abbington",
        birth_place_clean == "Abrecon"                   ~ "Absacon",
        birth_place_clean == "Acbron"                    ~ "Absacon",
        birth_place_clean == "Acamac"                    ~ "Accomack",
        birth_place_clean == "Accoucack"                 ~ "Accomack",
        birth_place_clean == "Acomac"                    ~ "Accomack",
        birth_place_clean == "African"                   ~ "Africa",
        birth_place_clean == "Albin"                     ~ "Albion",
        birth_place_clean == "Alexander"                 ~ "Alexandria",
        birth_place_clean == "Alexandra"                 ~ "Alexandria",
        birth_place_clean == "Alleghany"                 ~ "Allegheny",
        birth_place_clean == "Almira"                    ~ "Almyra",
        birth_place_clean == "Anaplois"                  ~ "Anapolis",
        birth_place_clean == "Antwarp"                   ~ "Antwerp",
        birth_place_clean == "Ashly"                     ~ "Ashley",
        birth_place_clean == "at"                        ~ "At",
        birth_place_clean == "Attleboro"                 ~ "Attleborough",
        birth_place_clean == "Ballstown"                 ~ "Ballston",
        birth_place_clean == "Barbadoes"                 ~ "Barbados",
        birth_place_clean == "Barnett"                   ~ "Barnet",
        birth_place_clean == "Barey"                     ~ "Barre",
        birth_place_clean == "Barry"                     ~ "Barre",
        birth_place_clean == "Beckot"                    ~ "Becket",
        birth_place_clean == "Bedford"                   ~ "New Bedford",
        birth_place_clean == "Begford"                   ~ "New Bedford",
        birth_place_clean == "Bellville"                 ~ "Belleville",
        birth_place_clean == "Belville"                  ~ "Belleville",
        birth_place_clean == "Bergin"                    ~ "Bergen",
        birth_place_clean == "Berkly"                    ~ "Berkley",
        birth_place_clean == "Berling"                   ~ "Berlin",
        birth_place_clean == "Bethell"                   ~ "Bethel",
        birth_place_clean == "Bethleham"                 ~ "Bethlehem",
        birth_place_clean == "Bethlem"                   ~ "Bethlehem",
        birth_place_clean == "Bernard"                   ~ "Bernardstown",
        birth_place_clean == "Binghamton"                ~ "Binghampton",
        birth_place_clean == "Billirica"                 ~ "Billerica",
        birth_place_clean == "Belchentown"               ~ "Belchertown",
        birth_place_clean == "Blumfield"                 ~ "Bloomfield",
        birth_place_clean == "Bonivista"                 ~ "Bonavista",
        birth_place_clean == "Bona"                      ~ "Bonavista",
        birth_place_clean == "Bonevente"                 ~ "Bonavista",
        birth_place_clean == "Booth"                     ~ "Boothbay",
        birth_place_clean == "Bostin"                    ~ "Boston",
        birth_place_clean == "boston"                    ~ "Boston",
        birth_place_clean == "Boxbury"                   ~ "Boxborough",
        birth_place_clean == "Boylston"                  ~ "Boyleston",
        birth_place_clean == "Bozah"                     ~ "Bozrah",
        birth_place_clean == "Bozra"                     ~ "Bozrah",
        birth_place_clean == "Brattleboro"               ~ "Brattleborough",
        birth_place_clean == "Brava"                     ~ "Bravo",
        birth_place_clean == "Bravos"                    ~ "Bravo",
        birth_place_clean == "Breman"                    ~ "Bremen",
        birth_place_clean == "Bremmen"                   ~ "Bremen",
        birth_place_clean == "Bridgwater"                ~ "Bridgewater",
        birth_place_clean == "British"                   ~ "England",
        birth_place_clean == "Brunfield"                 ~ "Brownfield",
        birth_place_clean == "Buck"                      ~ "Bucks",
        birth_place_clean == "Abbingtfon"                ~ "Abbington",
        birth_place_clean == "AccomKCJ"                  ~ "Accomack",
        birth_place_clean == "Acomack"                   ~ "Accomack",
        birth_place_clean == "Brookhavens"               ~ "Brookhaven",
        birth_place_clean == "Brooklin"                  ~ "Brooklyn",
        birth_place_clean == "Brookling"                 ~ "Brooklyn",
        birth_place_clean == "Brookline"                 ~ "Brooklyn",
        birth_place_clean == "Brooklynn"                 ~ "Brooklyn",
        birth_place_clean == "Brownsville"               ~ "Brownville",
        birth_place_clean == "Brunswich"                 ~ "Brunswick",
        birth_place_clean == "BurkshireMass"             ~ "Berkshire",
        birth_place_clean == "Burnington"                ~ "Burlington",
        birth_place_clean == "Burrilville"               ~ "Burrilsville",
        birth_place_clean == "Bumingham"                 ~ "Birmingham",
        birth_place_clean == "Cabotville"                ~ "Cabot",
        birth_place_clean == "Cadoz"                     ~ "Cadiz",
        birth_place_clean == "Caladonia"                 ~ "Caledonia",
        birth_place_clean == "Caldonia"                  ~ "Caledonia",
        birth_place_clean == "Cambridgeport"             ~ "Cambridge",
        birth_place_clean == "Camdem"                    ~ "Camden",
        birth_place_clean == "Campechy"                  ~ "Campeachy",
        birth_place_clean == "Camptown"                  ~ "Campton",
        birth_place_clean == "Canadian"                  ~ "Canada",
        birth_place_clean == "Canandaga"                 ~ "Canandaigua",
        birth_place_clean == "Candia"                    ~ "Canandaigua",
        birth_place_clean == "Cannady"                   ~ "Canandaigua",
        birth_place_clean == "Cantabury"                 ~ "Canterbury",
        birth_place_clean == "Canterby"                  ~ "Canterbury",
        birth_place_clean == "Cap"                       ~ "Cape",
        birth_place_clean == "Capedesert"                ~ "Cape",
        birth_place_clean == "Carisle"                   ~ "Carlisle",
        birth_place_clean == "Carthigame"                ~ "Carthagena",
        birth_place_clean == "Carvo"                     ~ "Carver",
        birth_place_clean == "Cavandish"                 ~ "Cavendish",
        birth_place_clean == "Cayuta"                    ~ "Cayuga",
        birth_place_clean == "Chambersburg"              ~ "Chambersburgh",
        birth_place_clean == "Chaplain"                  ~ "Champlain",
        birth_place_clean == "Chaplin"                   ~ "Champlain",
        birth_place_clean == "Chappaquiddit"             ~ "Chappaquidik",
        birth_place_clean == "Charleston"                ~ "Charlestown",
        birth_place_clean == "Charlston"                 ~ "Charlestown",
        birth_place_clean == "Charlton"                  ~ "Charlestown",
        birth_place_clean == "Chatam"                    ~ "Chatham",
        birth_place_clean == "ChelseaState"              ~ "Chelsea",
        birth_place_clean == "Chemung"                   ~ "Chenango",
        birth_place_clean == "Chesterfirel"              ~ "Chesterfield",
        birth_place_clean == "Chestesr"                  ~ "Chester",
        birth_place_clean == "Chili"                     ~ "Chile",
        birth_place_clean == "Chilmarck"                 ~ "Chilmark",
        birth_place_clean == "Chiloe"                    ~ "Chile",
        birth_place_clean == "Cincinati"                 ~ "Cincinnati",
        birth_place_clean == "Cincinatti"                ~ "Cincinnati",
        birth_place_clean == "Clairmont"                 ~ "Claremont",
        birth_place_clean == "Colcester"                 ~ "Colchester",
        birth_place_clean == "Colechester"               ~ "Colchester",
        birth_place_clean == "Conneticutt"               ~ "Connecticut",
        birth_place_clean == "Copenhagan"                ~ "Copenhagen",
        birth_place_clean == "Copenhaugen"               ~ "Copenhagen",
        birth_place_clean == "Cormish"                   ~ "Cornish",
        birth_place_clean == "Corro"                     ~ "Corvo",
        birth_place_clean == "Cowell"                    ~ "Cow",
        birth_place_clean == "Coxachie"                  ~ "Coxsackie",
        birth_place_clean == "Cranton"                   ~ "Cranston",
        birth_place_clean == "Croyden"                   ~ "Croydon",
        birth_place_clean == "Danby"                     ~ "Danbury",
        birth_place_clean == "Danvil"                    ~ "Danville",
        birth_place_clean == "Danvill"                   ~ "Danville",
        birth_place_clean == "Darthmouth"                ~ "Dartmouth",
        birth_place_clean == "Dartmouith"                ~ "Dartmouth",
        birth_place_clean == "Dartmpouth"                ~ "Dartmouth",
        birth_place_clean == "Dartmt"                    ~ "Dartmouth",
        birth_place_clean == "Deleware"                  ~ "Delaware",
        birth_place_clean == "Deptford"                  ~ "Depford",
        birth_place_clean == "Dorcester"                 ~ "Dorchester",
        birth_place_clean == "Douglass"                  ~ "Douglas",
        birth_place_clean == "Dracutt"                   ~ "Dracut",
        birth_place_clean == "Dumfire"                   ~ "Dumfries",
        birth_place_clean == "Dumfris"                   ~ "Dumfries",
        birth_place_clean == "Duchess"                   ~ "Dutchess",
        birth_place_clean == "Duxborough"                ~ "Duxbury",
        birth_place_clean == "E"                         ~ "East",
        birth_place_clean == "Eastham"                   ~ "Easton",
        birth_place_clean == "Eastown"                   ~ "Easton",
        birth_place_clean == "Edagrtown"                 ~ "Edagartown",
        birth_place_clean == "Edgarton"                  ~ "Edgartown",
        birth_place_clean == "Edgartwon"                 ~ "Edgartown",
        birth_place_clean == "Edgatrown"                 ~ "Edgartown",
        birth_place_clean == "Edgratown"                 ~ "Edgartown",
        birth_place_clean == "Edgcomb"                   ~ "Edgecomb",
        birth_place_clean == "Edinborough"               ~ "Edinburgh",
        birth_place_clean == "Edinburg"                  ~ "Edinburgh",
        birth_place_clean == "Epson"                     ~ "Epsom",
        birth_place_clean == "Esopas"                    ~ "Esopus",
        birth_place_clean == "Fairhave"                  ~ "Fairhaven",
        birth_place_clean == "FairHaven"                 ~ "Fairhaven",
        birth_place_clean == "Esset"                     ~ "Essex",
        birth_place_clean == "Fayall"                    ~ "Fayal",
        birth_place_clean == "Fial"                      ~ "Fayal",
        birth_place_clean == "Fishkills"                 ~ "Fishkill",
        birth_place_clean == "Fitchwilliam"              ~ "Fitzwilliam",
        birth_place_clean == "Fitzwilliams"              ~ "Fitzwilliam",
        birth_place_clean == "FlatbushNY"                ~ "Flatbush",
        birth_place_clean == "Flora"                     ~ "Flores",
        birth_place_clean == "Floras"                    ~ "Flores",
        birth_place_clean == "Flories"                   ~ "Flores",
        birth_place_clean == "Florisbay"                 ~ "Flores",
        birth_place_clean == "Flors"                     ~ "Flores",
        birth_place_clean == "Florus"                    ~ "Flores",
        birth_place_clean == "Flouras"                   ~ "Flores",
        birth_place_clean == "Foreign"                   ~ "Foreigner",
        birth_place_clean == "foreign"                   ~ "Foreigner",
        birth_place_clean == "foreigner"                 ~ "Foreigner",
        birth_place_clean == "Foreigner"                 ~ "Foreigner",
        birth_place_clean == "Foreinger"                 ~ "Foreigner",
        birth_place_clean == "Foriegn"                   ~ "Foreigner",
        birth_place_clean == "Francestown"               ~ "Francistown",
        birth_place_clean == "Frankford"                 ~ "Frankfort",
        birth_place_clean == "Frankport"                 ~ "Frankfort",
        birth_place_clean == "Fredericksburgh"           ~ "Fredericksburg",
        birth_place_clean == "Fredricksburg"             ~ "Fredericksburg",
        birth_place_clean == "Fretown"                   ~ "Freetown",
        birth_place_clean == "Fyal"                      ~ "Fayal",
        birth_place_clean == "Fyall"                     ~ "Fayal",
        birth_place_clean == "Galen"                     ~ "Galena",
        birth_place_clean == "Galloway"                  ~ "Galway",
        birth_place_clean == "Gardner"                   ~ "Gardiner",
        birth_place_clean == "Geo"                       ~ "Georgia",
        birth_place_clean == "George"                    ~ "Georgia",
        birth_place_clean == "German"                    ~ "Germany",
        birth_place_clean == "Gerry"                     ~ "Germany",
        birth_place_clean == "Glascow"                   ~ "Glasgow",
        birth_place_clean == "Glocestert"                ~ "Glocester",
        birth_place_clean == "Gloster"                   ~ "Glocester",
        birth_place_clean == "Glouster"                  ~ "Glocester",
        birth_place_clean == "Gosehen"                   ~ "Goshen",
        birth_place_clean == "Gosham"                    ~ "Goshen",
        birth_place_clean == "Goshenburgh"               ~ "Gottenburg",
        birth_place_clean == "Gothenburg"                ~ "Gottenburg",
        birth_place_clean == "Gothenburgh"               ~ "Gottenburg",
        birth_place_clean == "Gottenburg`"               ~ "Gottenburg",
        birth_place_clean == "Gottenburgh"               ~ "Gottenburg",
        birth_place_clean == "Gracias"                   ~ "Graciosa",
        birth_place_clean == "Gracioza"                  ~ "Graciosa",
        birth_place_clean == "Greene"                    ~ "Green",
        birth_place_clean == "Greensborgh"               ~ "Greensborough",
        birth_place_clean == "Guadaloupe"                ~ "Guadeloupe",
        birth_place_clean == "Haddan"                    ~ "Haddam",
        birth_place_clean == "Hadlon"                    ~ "Hadley",
        birth_place_clean == "Hadlyme"                   ~ "Hadley",
        birth_place_clean == "Haerlem"                   ~ "Harlem",
        birth_place_clean == "Hagerstone"                ~ "Hagerstown",
        birth_place_clean == "Half"                      ~ "Halifax",
        birth_place_clean == "Hallwell"                  ~ "Hallowell",
        birth_place_clean == "Halowell"                  ~ "Hallowell",
        birth_place_clean == "Hamburgh"                  ~ "Hamburg",
        birth_place_clean == "Hamden"                    ~ "Hampton",
        birth_place_clean == "Hampden"                   ~ "Hampton",
        birth_place_clean == "Hampston"                  ~ "Hampton",
        birth_place_clean == "Hampten"                   ~ "Hampton",
        birth_place_clean == "Hamptonfalls"              ~ "Hampton",
        birth_place_clean == "Hanvover"                  ~ "Hanover",
        birth_place_clean == "Hardwich"                  ~ "Hardwick",
        birth_place_clean == "Harrisburgh"               ~ "Harrisburg",
        birth_place_clean == "Harwich"                   ~ "Hardwick",
        birth_place_clean == "Haverill"                  ~ "Haverhill",
        birth_place_clean == "Haverille"                 ~ "Haverhill",
        birth_place_clean == "Hebrow"                    ~ "Hebron",
        birth_place_clean == "Hemstead"                  ~ "Hempstead",
        birth_place_clean == "Herkemer"                  ~ "Herkimer",
        birth_place_clean == "Herkimmer"                 ~ "Herkimer",
        birth_place_clean == "Hillsboro"                 ~ "Hillsborough",
        birth_place_clean == "Holand"                    ~ "Holland",
        birth_place_clean == "Hawaiian"                  ~ "Hawaii",
        birth_place_clean == "Honolulu"                  ~ "Hawaii",
        birth_place_clean == "Hoosac"                    ~ "Hoosack",
        birth_place_clean == "Hoosick"                   ~ "Hoosack",
        birth_place_clean == "Houstin"                   ~ "Houston",
        birth_place_clean == "Hunington"                 ~ "Huntington",
        birth_place_clean == "Hyannis"                   ~ "Hyannes",
        birth_place_clean == "ireland"                   ~ "Ireland",
        birth_place_clean == "Isd"                       ~ "Island",
        birth_place_clean == "Isl"                       ~ "Island",
        birth_place_clean == "Isle"                      ~ "Island",
        birth_place_clean == "Islesboro"                 ~ "Isleborough",
        birth_place_clean == "Islesborough"              ~ "Isleborough",
        birth_place_clean == "Ithica"              ~ "Ithaca",
        birth_place_clean == "Jamacia"                   ~ "Jamaica",
        birth_place_clean == "Jameste"                   ~ "Jamestown",
        birth_place_clean == "Jeffries"                  ~ "Jeffrey",
        birth_place_clean == "Jerrico"                   ~ "Jericho",
        birth_place_clean == "Jinkins"                   ~ "Jenkins",
        birth_place_clean == "Johns"                     ~ "Johnston",
        birth_place_clean == "Johnson"                   ~ "Johnston",
        birth_place_clean == "Johnstown"                 ~ "Johnston",
        birth_place_clean == "Kenebunk"                  ~ "Kennebunk",
        birth_place_clean == "Kennebec"                  ~ "Kennebunk",
        birth_place_clean == "Kennebeck"                 ~ "Kennebunk",
        birth_place_clean == "Kennibec"                  ~ "Kennebunk",
        birth_place_clean == "Keen"                      ~ "Keene",
        birth_place_clean == "Kean"                      ~ "Keene",
        birth_place_clean == "Kilkenny"                  ~ "Killingly",
        birth_place_clean == "Kingstown"                 ~ "Kingston",
        birth_place_clean == "Lancashire"                ~ "Lancaster",
        birth_place_clean == "Lancester"                 ~ "Lancaster",
        birth_place_clean == "Lanchire"                  ~ "Lancashire",
        birth_place_clean == "Lanesborough"              ~ "Lanesboro",
        birth_place_clean == "Lansinburg"                ~ "Lansingburgh",
        birth_place_clean == "Lansinburgh"               ~ "Lansingburgh",
        birth_place_clean == "Lansingburg"               ~ "Lansingburgh",
        birth_place_clean == "Larnsburg"                 ~ "Lansingburgh",
        birth_place_clean == "Leabenon"                  ~ "Lebanon",
        birth_place_clean == "Lebon"                     ~ "Lebanon",
        birth_place_clean == "Leed"                      ~ "Leeds",
        birth_place_clean == "Lewistown"                 ~ "Lewiston",
        birth_place_clean == "Leyden"                    ~ "Leyton",
        birth_place_clean == "Lime"                      ~ "Lima",
        birth_place_clean == "Limorec"                   ~ "Limerick",
        birth_place_clean == "Limrick"                   ~ "Limerick",
        birth_place_clean == "Liverpoool"                ~ "Liverpool",
        birth_place_clean == "Lonaery"                   ~ "Londonderry",
        birth_place_clean == "Londary"                   ~ "Londonderry",
        birth_place_clean == "Londondary"                ~ "Londonderry",
        birth_place_clean == "Londondery"                ~ "Londonderry",
        birth_place_clean == "Londen"                    ~ "London",
        birth_place_clean == "Louville"                  ~ "Louisville",
        birth_place_clean == "Lower"                     ~ "Lowell",
        birth_place_clean == "Lubic"                     ~ "Lubec",
        birth_place_clean == "Lucerne"                   ~ "Luzerne",
        birth_place_clean == "Lyman"                     ~ "Lyme",
        birth_place_clean == "Lyon"                      ~ "Lyons",
        birth_place_clean == "Madara"                    ~ "Madeira",
        birth_place_clean == "Madaria"                   ~ "Madeira",
        birth_place_clean == "Maderia"                   ~ "Madeira",
        birth_place_clean == "Madberry"                  ~ "Madbury",
        birth_place_clean == "Madigascar"                ~ "Madagascar",
        birth_place_clean == "Madros"                    ~ "Madrid",
        birth_place_clean == "Main"                      ~ "Maine",
        birth_place_clean == "Malboroouys"               ~ "Malborough",
        birth_place_clean == "Malburgh"                  ~ "Malborough",
        birth_place_clean == "Mancheseter"               ~ "Manchester",
        birth_place_clean == "Manhattanville"            ~ "Manhattan",
        birth_place_clean == "Manila"                    ~ "Manilla",
        birth_place_clean == "Manlius"                   ~ "Manilla",
        birth_place_clean == "Marble"                    ~ "Marblehead",
        birth_place_clean == "Marie"                     ~ "Maria",
        birth_place_clean == "Marques"                   ~ "Marquesas",
        birth_place_clean == "Marqueses"                 ~ "Marquesas",
        birth_place_clean == "Marqueus"                  ~ "Marquesas",
        birth_place_clean == "Marseille"                 ~ "Marseilles",
        birth_place_clean == "Marselles"                 ~ "Marseilles",
        birth_place_clean == "Martha"                    ~ "Marthas",
        birth_place_clean == "Martinico"                 ~ "Martinique",
        birth_place_clean == "Mashby"                    ~ "Mashpee",
        birth_place_clean == "Mass"                      ~ "Massachusetts",
        birth_place_clean == "Matapoisett"               ~ "Mattapoisett",
        birth_place_clean == "Maui"                      ~ "Hawaii",
        birth_place_clean == "Me"                        ~ "Maine",
        birth_place_clean == "Medeira"                   ~ "Madeira",
        birth_place_clean == "Mederin"                   ~ "Madeira",
        birth_place_clean == "Mediera"                   ~ "Madeira",
        birth_place_clean == "Medfield"                  ~ "Medford",
        birth_place_clean == "Mellville"                 ~ "Melville",
        birth_place_clean == "Menden"                    ~ "Mendon",
        birth_place_clean == "Meridith"                  ~ "Meredith",
        birth_place_clean == "Meriden"                   ~ "Meredith",
        birth_place_clean == "Meriland"                  ~ "Maryland",
        birth_place_clean == "Merrimac"                  ~ "Merrimack",
        birth_place_clean == "Methewen"                  ~ "Methuen",
        birth_place_clean == "Middlebobough"             ~ "Middleborough",
        birth_place_clean == "Middleboro"                ~ "Middleborough",
        birth_place_clean == "Middleborourgh"            ~ "Middleborough",
        birth_place_clean == "Middlebourogh"             ~ "Middleborough",
        birth_place_clean == "Midleborough"              ~ "Middleborough",
        birth_place_clean == "Middleton"                 ~ "Middletown",
        birth_place_clean == "Millford"                  ~ "Milford",
        birth_place_clean == "Millbern"                  ~ "Milbern",
        birth_place_clean == "Milbury"                   ~ "Millbury",
        birth_place_clean == "Milltown"                  ~ "Milton",
        birth_place_clean == "Moltenboro"                ~ "Moltonborough",
        birth_place_clean == "Moltonboro"                ~ "Moltonborough",
        birth_place_clean == "Montgonery"                ~ "Montgomery",
        birth_place_clean == "Montgony"                  ~ "Montgomery",
        birth_place_clean == "Montvill"                  ~ "Montville",
        birth_place_clean == "Montreall"                 ~ "Montreal",
        birth_place_clean == "Mountpielier"              ~ "Montpelier",
        birth_place_clean == "Mowe"                      ~ "Mowee",
        birth_place_clean == "Mower"                     ~ "Mowee",
        birth_place_clean == "Mt"                        ~ "Mount",
        birth_place_clean == "Muson"                     ~ "Munson",
        birth_place_clean == "Murray"                    ~ "Murray",
        birth_place_clean == "Namtucket"                 ~ "Nantucket",
        birth_place_clean == "Nantucke"                  ~ "Nantucket",
        birth_place_clean == "Nantucker"                 ~ "Nantucket",
        birth_place_clean == "Nan"                       ~ "Nantucket",
        birth_place_clean == "Naragansett"               ~ "Narragansett",
        birth_place_clean == "Naraganset"                ~ "Narragansett",
        birth_place_clean == "Nashaway"                  ~ "Nashua",
        birth_place_clean == "Natches"                   ~ "Natchez",
        birth_place_clean == "Nelsom"                    ~ "Nelson",
        birth_place_clean == "NewBedford"                ~ "New Bedford", # Need to deal with "New"
        birth_place_clean == "Newb"                      ~ "New Bedford",
        birth_place_clean == "NewBedfprd"                ~ "New Bedford",
        birth_place_clean == "NewBedord"                 ~ "New Bedford",
        birth_place_clean == "Newbedford"                ~ "New Bedford",
        birth_place_clean == "NewArk"                    ~ "Newark",
        birth_place_clean == "NewBerry"                  ~ "Newbury",
        birth_place_clean == "NewBoston"                 ~ "Boston",
        birth_place_clean == "NewBritian"                ~ "NewBritain",
        birth_place_clean == "NewBrittain"               ~ "NewBritain",
        birth_place_clean == "NewCanan"                  ~ "NewCanaan",
        birth_place_clean == "NewCannan"                 ~ "NewCanaan",
        birth_place_clean == "NewCarman"                 ~ "NewCanaan",
        birth_place_clean == "NewCastle"                 ~ "Newcastle",
        birth_place_clean == "NewGlocester"              ~ "NewGloucester",
        birth_place_clean == "Newberg"                   ~ "Newburg",
        birth_place_clean == "NewBurg"                   ~ "Newburg",
        birth_place_clean == "Newburgh"                  ~ "Newburg",
        birth_place_clean == "NewBurgh"                  ~ "Newburg",
        birth_place_clean == "NewBern"                   ~ "Newbern",
        birth_place_clean == "NewBury"                   ~ "Newbury",
        birth_place_clean == "Newburyport"               ~ "Newbury",
        birth_place_clean == "NewBuryPort"               ~ "Newbury",
        birth_place_clean == "Newfound"                  ~ "Newfoundland",
        birth_place_clean == "Newhampshire"              ~ "New Hampshire",
        birth_place_clean == "NewHamshire"               ~ "New Hampshire",
        birth_place_clean == "NewHampden"                ~ "NewHampton",
        birth_place_clean == "Newjersey"                 ~ "New Jersey",
        birth_place_clean == "NewJersey"                 ~ "New Jersey",
        birth_place_clean == "NewLebenon"                ~ "NewLebanon",
        birth_place_clean == "Newlondon"                 ~ "NewLondon",
        birth_place_clean == "NewMarket"                 ~ "Newmarket",
        birth_place_clean == "Newmilford"                ~ "NewMilford",
        birth_place_clean == "NewOlreans"                ~ "NewOrleans",
        birth_place_clean == "Neworleans"                ~ "NewOrleans",
        birth_place_clean == "NewZealand"                ~ "New Zealand",
        birth_place_clean == "NewPort"                   ~ "Newport",
        birth_place_clean == "Newtown"                   ~ "Newton",
        birth_place_clean == "NewTown"                   ~ "Newton",
        birth_place_clean == "Newyork"                   ~ "New York",
        birth_place_clean == "NewYork"                   ~ "New York",
        birth_place_clean == "Nh"                        ~ "New Hampshire",
        birth_place_clean == "Nobleborugh"               ~ "Noblesborough",
        birth_place_clean == "Noblebourough"             ~ "Noblesborough",
        birth_place_clean == "Noblesboro"                ~ "Noblesborough",
        birth_place_clean == "Norfork"                   ~ "Norfolk",
        birth_place_clean == "Noredgewalk"               ~ "Norridgewock",
        birth_place_clean == "Normidgewalk"              ~ "Norridgewock",
        birth_place_clean == "Norridgewalk"              ~ "Norridgewock",
        birth_place_clean == "Norridgemark"              ~ "Norridgewock",
        birth_place_clean == "Norridgewock"              ~ "Norridgewock",
        birth_place_clean == "Northhampton"              ~ "Northampton",
        birth_place_clean == "Northcumberlad"            ~ "Northumberland",
        birth_place_clean == "NovaScotia"                ~ "Nova Scotia",
        birth_place_clean == "Nova"                      ~ "Nova Scotia",
        birth_place_clean == "Ny"                        ~ "New York",
        birth_place_clean == "Oahu"                      ~ "Hawaii",
        birth_place_clean == "Odgensburg"                ~ "Ogdensburg",
        birth_place_clean == "Ogdensburgh"               ~ "Ogdensburg",
        birth_place_clean == "Oh"                        ~ "Ohio",
        birth_place_clean == "Oihe"                      ~ "Hawaii",
        birth_place_clean == "Ohahie"                    ~ "Hawaii",
        birth_place_clean == "Onedia"                    ~ "Oneida",
        birth_place_clean == "Onondago"                  ~ "Onondaga",
        birth_place_clean == "Opato"                     ~ "Oporto",
        birth_place_clean == "Otaheite"                  ~ "Otahute",
        birth_place_clean == "Otahita"                   ~ "Otahute",
        birth_place_clean == "Otahite"                   ~ "Otahute",
        birth_place_clean == "Otahut"                    ~ "Otahute",
        birth_place_clean == "Otaherta"                  ~ "Otahute",
        birth_place_clean == "Oteheite"                  ~ "Otahute",
        birth_place_clean == "Ouhyee"                    ~ "Hawaii",
        birth_place_clean == "Owhyhee"                   ~ "Hawaii",
        birth_place_clean == "Owihee"                    ~ "Hawaii",
        birth_place_clean == "Owihee"                    ~ "Hawaii",
        birth_place_clean == "Owyhee"                    ~ "Hawaii",
        birth_place_clean == "Owyee"                     ~ "Hawaii",
        birth_place_clean == "Oyhee"                     ~ "Hawaii",
        birth_place_clean == "Palmer"                    ~ "Palmyra",
        birth_place_clean == "Paramus"                   ~ "Parma",
        birth_place_clean == "Paterson"                  ~ "Patterson",
        birth_place_clean == "Paukeepsie"                ~ "Poughkeepsie",
        birth_place_clean == "Pautucket"                 ~ "Pawtucket",
        birth_place_clean == "Paw"                       ~ "Pawtucket",
        birth_place_clean == "Pawcatuck"                 ~ "Pawtucket",
        birth_place_clean == "Pembrook"                  ~ "Pembroke",
        birth_place_clean == "Penbroke"                  ~ "Pembroke",
        birth_place_clean == "Perue"                     ~ "Peru",
        birth_place_clean == "Peterborough"              ~ "Petersburg",
        birth_place_clean == "Peterburg"                 ~ "Petersburg",
        birth_place_clean == "Petersburgh"               ~ "Petersburg",
        birth_place_clean == "Phil"                      ~ "Petersburg",
        birth_place_clean == "Phila"                     ~ "Philadelphia",
        birth_place_clean == "Philad"                    ~ "Philadelphia",
        birth_place_clean == "Philadeklphia"             ~ "Philadelphia",
        birth_place_clean == "Philadephia"               ~ "Philadelphia",
        birth_place_clean == "Philadlephia"              ~ "Philadelphia",
        birth_place_clean == "Philiadelphia"             ~ "Philadelphia",
        birth_place_clean == "Philipstown"               ~ "Phillipston",
        birth_place_clean == "Picko"                     ~ "Pico",
        birth_place_clean == "Pittburgh"                 ~ "Pittsburgh",
        birth_place_clean == "Pittsburg"                 ~ "Pittsburgh",
        birth_place_clean == "Pittston"                  ~ "Pittstown",
        birth_place_clean == "Platts"                    ~ "Plattsburgh",
        birth_place_clean == "Plattsburg"                ~ "Plattsburgh",
        birth_place_clean == "Plattsburgg"               ~ "Plattsburgh",
        birth_place_clean == "Platzburgh"                ~ "Plattsburgh",
        birth_place_clean == "Plimouth"                  ~ "Plymouth",
        birth_place_clean == "Plympton"                  ~ "Plymton",
        birth_place_clean == "Poquetonock"               ~ "Poquonnock", # what do do about "Port"?
        birth_place_clean == "Portigues"                 ~ "Portugal",
        birth_place_clean == "Portugese"                 ~ "Portugal",
        birth_place_clean == "Portuguese"                ~ "Portugal",
        birth_place_clean == "Portuga"                   ~ "Portugal",
        birth_place_clean == "Pokeepsie"                 ~ "Poughkeepsie",
        birth_place_clean == "Poughkipsie"               ~ "Poughkeepsie",
        birth_place_clean == "Prince"                    ~ "Princeton",
        birth_place_clean == "Princetown"                ~ "Princeton",
        birth_place_clean == "providence"                ~ "Providence",
        birth_place_clean == "Provindence"               ~ "Providence",
        birth_place_clean == "Quinsey"                   ~ "Quincy",
        birth_place_clean == "Randolf"                   ~ "Randolph",
        birth_place_clean == "Raratonga"                 ~ "Rarotonga",
        birth_place_clean == "Redding"                   ~ "Reading",
        birth_place_clean == "Rehobeth"                  ~ "Rehoboth",
        birth_place_clean == "Renselaer"                 ~ "Rensselaer",
        birth_place_clean == "Reo"                       ~ "Rio",
        birth_place_clean == "Rhode"                     ~ "Rhode Island",
        birth_place_clean == "Rhodeisland"               ~ "Rhode Island",
        birth_place_clean == "Ri"                        ~ "Rhode Island",
        birth_place_clean == "Ricnmond"                  ~ "Richmond",
        birth_place_clean == "Rochseter"                 ~ "Rochester",
        birth_place_clean == "Rockawayrothaway"          ~ "Rockaway",
        birth_place_clean == "Rochseter"                 ~ "Rochester",
        birth_place_clean == "Rotchester"                ~ "Rochester",
        birth_place_clean == "Royalstown"                ~ "Royalton",
        birth_place_clean == "Ryegate"                   ~ "Rye",  # what do do about "S"
        birth_place_clean == "Sag"                       ~ "Sag Harbor",
        birth_place_clean == "Sagharbor"                 ~ "Sag Harbor",
        birth_place_clean == "Sagharbour"                ~ "Sag Harbor",
        birth_place_clean == "Salesbury"                 ~ "Salisbury",
        birth_place_clean == "Salibury"                  ~ "Salisbury",
        birth_place_clean == "Sallisbury"                ~ "Salisbury",
        birth_place_clean == "Salsbury"                  ~ "Salisbury",
        birth_place_clean == "Salam"                     ~ "Salem",
        birth_place_clean == "Sandburton"                ~ "Sanbornton",
        birth_place_clean == "Sanborntown"               ~ "Sanbornton",
        birth_place_clean == "Sannwich"                  ~ "Sandwich",
        birth_place_clean == "Sain"                       ~ "Saint",
        birth_place_clean == "San"                       ~ "Saint",
        birth_place_clean == "Sant"                      ~ "Saint",
        birth_place_clean == "Santa"                     ~ "Saint",
        birth_place_clean == "Savanah"                   ~ "Savannah",
        birth_place_clean == "Savory"                    ~ "Savoy",
        birth_place_clean == "Saxe"                      ~ "Saxony",
        birth_place_clean == "Say"                       ~ "Saybrook",
        birth_place_clean == "Schenactady"               ~ "Schenectady",
        birth_place_clean == "Schenectedy"               ~ "Schenectady",
        birth_place_clean == "Schohairy"                 ~ "Schoharie",
        birth_place_clean == "Scholarie"                 ~ "Schoharie",
        birth_place_clean == "Scholars"                  ~ "Schoharie",
        birth_place_clean == "Scipiornew"                ~ "Scipio",
        birth_place_clean == "Scotch"                    ~ "Scotland",
        birth_place_clean == "Seakhonk"                  ~ "Seekonk",
        birth_place_clean == "Seakonk"                   ~ "Seekonk",
        birth_place_clean == "Seekhonk"                  ~ "Seekonk",
        birth_place_clean == "Seekoncounty"              ~ "Seekonk",
        birth_place_clean == "Senaca"                    ~ "Seneca",
        birth_place_clean == "Shakleigh"                 ~ "Shapley",
        birth_place_clean == "Shapleigh"                 ~ "Shapley",
        birth_place_clean == "Sharron"                   ~ "Sharon",
        birth_place_clean == "Shelburn"                  ~ "Shelburne",
        birth_place_clean == "Shelford"                  ~ "Sheffield",
        birth_place_clean == "Sipio"                     ~ "Scipio",
        birth_place_clean == "somerset"                  ~ "Somerset",
        birth_place_clean == "Sommerville"               ~ "Summerville",
        birth_place_clean == "Somerville"                ~ "Summerville",
        birth_place_clean == "So"                        ~ "South",
        birth_place_clean == "Soth"                      ~ "South",
        birth_place_clean == "Sou"                       ~ "South",
        birth_place_clean == "Southautan"                ~ "Southampton",
        birth_place_clean == "Southhampton"              ~ "Southampton",
        birth_place_clean == "Southboro"                 ~ "Southborough",
        birth_place_clean == "Springfiled"               ~ "Springfield",
        birth_place_clean == "state"                     ~ "State",
        birth_place_clean == "Statten"                   ~ "Staten",
        birth_place_clean == "Statien"                   ~ "Staten",
        birth_place_clean == "Statien"                   ~ "Staten",
        birth_place_clean == "Store"                     ~ "Storo",
        birth_place_clean == "Stonginton"                ~ "Stonington",
        birth_place_clean == "Stonignton"                ~ "Stonington",
        birth_place_clean == "Stoninginton"              ~ "Stonington",
        birth_place_clean == "Stonnington"               ~ "Stonington",
        birth_place_clean == "Stow"                      ~ "Stowe",
        birth_place_clean == "Strawsburg"                ~ "Strasburg",
        birth_place_clean == "Strawsburgh"               ~ "Strasburg",
        birth_place_clean == "Suffield"                  ~ "Suffield",
        birth_place_clean == "Suffold"                   ~ "Suffolk",
        birth_place_clean == "Sumisett"                  ~ "Summerset",
        birth_place_clean == "Summersett"                ~ "Summerset",
        birth_place_clean == "Swansey"                   ~ "Swansea",
        birth_place_clean == "Swanzey"                   ~ "Swansea",
        birth_place_clean == "Sweeden"                   ~ "Sweden",
        birth_place_clean == "Talbort"                   ~ "Talbot",
        birth_place_clean == "Talcahuan"                 ~ "Talcuana",
        birth_place_clean == "Tcera"                     ~ "Terceira",
        birth_place_clean == "Tcera"                     ~ "Terceira",
        birth_place_clean == "Teciera"                   ~ "Terceira",
        birth_place_clean == "Terceira"                  ~ "Terceira",
        birth_place_clean == "Tercoera"                  ~ "Terceira",
        birth_place_clean == "Tereira"                   ~ "Terceira",
        birth_place_clean == "Teriecia"                  ~ "Terceira",
        birth_place_clean == "Tewkesbury"                ~ "Tewksbury",
        birth_place_clean == "Thedford"                  ~ "Thetford",
        birth_place_clean == "Thomastown"                ~ "Thomaston",
        birth_place_clean == "ThompsonConn"              ~ "Thompson",
        birth_place_clean == "Thorton"                   ~ "Thornton",
        birth_place_clean == "Tippary"                   ~ "Tipperary",
        birth_place_clean == "Tisbiry"                   ~ "Tisbury",
        birth_place_clean == "TisburyMass"               ~ "Tisbury",
        birth_place_clean == "Tisbuty"                   ~ "Tisbury",
        birth_place_clean == "Tiverfton"                 ~ "Tiverton",
        birth_place_clean == "TIverton"                  ~ "Tiverton",
        birth_place_clean == "Tivertown"                 ~ "Tiverton",
        birth_place_clean == "Tompkins"                  ~ "Tompkin",
        birth_place_clean == "Trinadad"                  ~ "Trinidad",
        birth_place_clean == "Tristian"                  ~ "Tristan",
        birth_place_clean == "Tro"                       ~ "Troy",
        birth_place_clean == "Tuftonboro"                ~ "Tufton",
        birth_place_clean == "Truvo"                     ~ "Truro",
        birth_place_clean == "Tuttenboro"                ~ "Tufton",
        birth_place_clean == "Turnerme"                  ~ "Turner",
        birth_place_clean == "Vassalborough"             ~ "Vassalboro",
        birth_place_clean == "Vasselboro"                ~ "Vassalboro",
        birth_place_clean == "Vineard"                   ~ "Vineyard",
        birth_place_clean == "Vineard"                   ~ "Vineyard",
        birth_place_clean == "Vinyard"                   ~ "Vineyard",
        birth_place_clean == "Viginia"                   ~ "Virginia",
        birth_place_clean == "Virginna"                  ~ "Virginia",
        birth_place_clean == "Vt"                        ~ "Vermont",
        birth_place_clean == "Wahoo"                     ~ "Wahoo",
        birth_place_clean == "Waitsfield"                ~ "Wakefield",
        birth_place_clean == "Waohoo"                    ~ "Wahoo",
        birth_place_clean == "Wariwck"                   ~ "Warwick",
        birth_place_clean == "Wariwck"                   ~ "Warwick",
        birth_place_clean == "Warran"                    ~ "Warren",
        birth_place_clean == "Washinton"                 ~ "Washington",
        birth_place_clean == "Washoo"                    ~ "Washington",
        birth_place_clean == "Water"                     ~ "Waterbury",
        birth_place_clean == "Waterboro"                 ~ "Waterbury",
        birth_place_clean == "Waterborough"              ~ "Waterbury",
        birth_place_clean == "Waterton"                  ~ "Watertown",
        birth_place_clean == "Watterton"                 ~ "Watertown",
        birth_place_clean == "Weatherfield"              ~ "Weathersfield",
        birth_place_clean == "Wendall"                   ~ "Wendell", 
        birth_place_clean == "Weste"                     ~ "West", # what do do about "West"?
        birth_place_clean == "Westmorelond"              ~ "Westmoreland",
        birth_place_clean == "Westmorland"               ~ "Westmoreland",
        birth_place_clean == "Westmourland"              ~ "Westmoreland",
        birth_place_clean == "Westn"                     ~ "Weston",
        birth_place_clean == "Westown"                   ~ "Weston",
        birth_place_clean == "Westporte"                 ~ "Westport",
        birth_place_clean == "Wetherfield"               ~ "Wethersfield",
        birth_place_clean == "Whilte"                    ~ "White",
        birth_place_clean == "Whithall"                  ~ "Whitehall",
        birth_place_clean == "Whittingham"               ~ "Whitingham",
        birth_place_clean == "Willamsburg"               ~ "Williamsburg",
        birth_place_clean == "Williamsburgh"             ~ "Williamsburg",
        birth_place_clean == "Willington"                ~ "Wilmington",
        birth_place_clean == "Willmington"               ~ "Wilmington",
        birth_place_clean == "Wilmantown"                ~ "Wilmington",
        birth_place_clean == "Wilmantown"                ~ "Wilmington",
        birth_place_clean == "Wilmantown"                ~ "Wilmington",
        birth_place_clean == "Wiltons"                   ~ "Wilton",
        birth_place_clean == "Windham"                   ~ "Windham",
        birth_place_clean == "Winsor"                    ~ "Windsor",
        birth_place_clean == "Wiscapett"                 ~ "Wiscasset",
        birth_place_clean == "Wiscasett"                 ~ "Wiscasset",
        birth_place_clean == "Wiscassett"                ~ "Wiscasset",
        birth_place_clean == "Wisscasset"                ~ "Wiscasset",
        birth_place_clean == "Wndsor"                    ~ "Windsor",
        birth_place_clean == "Woahoo"                    ~ "Wahoo",
        birth_place_clean == "Woborn"                    ~ "Woburn",
        birth_place_clean == "Wolfborough"               ~ "Wolfsborough",
        birth_place_clean == "Wolfeborough"              ~ "Wolfsborough",
        birth_place_clean == "Woodbrige"                 ~ "Woodbridge",
        birth_place_clean == "Woods"                     ~ "Woodstock",
        birth_place_clean == "Woohoo"                    ~ "Wahoo",
        birth_place_clean == "Wooster"                   ~ "Worcester",
        birth_place_clean == "Worestester"               ~ "Worcester",
        birth_place_clean == "Wyhee"                     ~ "Hawaii",
        TRUE                                       ~ birth_place_clean
      ), 
      birth_place_clean = ifelse(birth_place_clean %in% c("At", "Capt", "Captaun", "Deserted", "Discharged", "From", 
                                                          "Negro", "Naturalized", "No", "Of", "Uncertain", "Unkinown", 
                                                          "Unknown", "Us", "Usa", "Xx"),
                                 NA_character_,
                                 birth_place_clean)
      ) |>
      inner_join(df_voyage_distinct |> select(voyage_id, year_out, year_in),
                 by = "voyage_id") |>
      mutate(birth_year = year_out - age) |>
      arrange(voyage_id, name_last_clean, name_first_clean, year_out) |>
      group_by(voyage_id) |>
      mutate(crewlist_id_set = glue_collapse(unique(crewlist_id), sep = ", ")) |> # glue_collapse() is slow
      # TODO: change the following to a more nuanced distance function to determine matches
      # TODO:   and create UUID for crew member
      distinct(name_last_clean, name_first_clean, voyage_id, .keep_all = TRUE) |> # TODO: is this necessary?
      mutate(crewentry_id_unique = paste0(voyage_id, "-", str_pad(row_number(), width = 3, side = "left", pad = "0"))) |>
      ungroup()
    # note: I'm not keeping residence_* since they probably require a lot of cleanup to be useful
    
    write_rds(df_crewlist, file = here("data/processed", "df_crewlist.rds")) # TODO: are we going to use this?
    write_rds(df_crew, file = here("data/processed", "df_crew.rds")) 
  }
}

###### 
get_crewlist <- function(crew_info) {
  # crew_info exists just to create a dependency on the crew object created by tar_make()
  read_rds(here("data/processed", "df_crewlist.rds")) 
}

###### 
get_df <- function(fname) {
  read_rds(here("data/processed", fname))
}


###### prepare df_financial_results
get_charleswmorgan_data <- function() {
  readxl::read_xlsx(here("data/financial-results", "charles-w-morgan-financial-results.xlsx"), 
                    skip = 3)
}

make_vessel <- function(voyage_distinct) {
  voyage_distinct %>%
    select(voyage_id, voyage_name, port, sailing_from, ground, year_out, day_out, year_in, day_in, vessel_id) %>%
    distinct(vessel_id, .keep_all = TRUE) # note: only includes first master
}

######
make_voyage_duration <- function(vessel, df_log) {
  vessel %>%
    mutate(date_start = ymd(day_out, quiet = TRUE),
           date_end = ymd(day_in, quiet = TRUE),
           duration_days = as.integer(difftime(date_end, date_start,
                                               units = "days")),
           decade = 10 * floor(year_out / 10)) %>%
    filter(duration_days > 0) %>% # data quality filter
    arrange(voyage_id, desc(duration_days)) %>%
    left_join(.,
              df_log %>% 
                arrange(voyage_id, desc(n_obs)) %>%
                distinct(voyage_id, .keep_all = TRUE) %>%
                select(voyage_id, n_obs, source),
              by = "voyage_id") # only a small number of voyages with full day_out and day_in are in the logbook table
}

###### make_voyage_duration_simulated so there is more data to work with
make_voyage_duration_simulated <- function(voyage_distinct, voyage_duration, vessel, df_log) {
  # defined in constants.R
  #   voyage_years_max
  #   cutoff_voyage_duration_days
  
  date_start_prob_vector <- voyage_distinct %>%
    mutate(date_start = yday(ymd(day_out))) %>%
    filter(!is.na(date_start),
           date_start < 366) %>% # it's ok if leap years are off by 1 starting March 1
    count(date_start) %>%
    left_join(tibble(date_start = 1:365),
              .,
              by = "date_start") %>%
    replace_na(list(n = 0)) %>%
    pull(n)
  
  date_end_prob_vector <- voyage_distinct %>%
    mutate(date_end = yday(ymd(day_in))) %>%
    filter(!is.na(date_end),
           date_end < 366) %>% # it's ok if leap years are off by 1 on yday starting March 1
    count(date_end) %>%
    left_join(tibble(date_end = 1:365),
              .,
              by = "date_end") %>%
    replace_na(list(n = 1)) %>%
    pull(n)
  
  voyage_length_prob_vector <- voyage_duration %>%
    mutate(bin30 = floor(duration_days / 30)) %>%
    group_by(bin30) %>%
    summarize(n = n(),
              days_med = median(duration_days),
              days_avg = mean(duration_days)
    ) %>%
    ungroup()
  
  ### Testing
  # voyage_length_prob_vector %>%
  #   ggplot(aes(bin30, n)) +
  #   geom_col()
  ### End Testing
  
  pick_dates_start_end <- function(my_year_start, my_date_start,
                                   my_year_end, my_date_end) {
    # Call this function via pmap (once for each row of data)
    
    # Fail fast
    good_year_start <- !is.na(my_year_start)
    if(!good_year_start) {
      return(c(start = NA_real_, end = my_date_end)) # no good date_start so not worth doing anything
    }
    
    my_ym_start <- ym(str_extract(my_date_start, "\\d{4} [[:alpha:]]+"), quiet = TRUE) # ignore day if it exists to avoid ym() confusion
    my_date_start <- ymd(my_date_start, quiet = TRUE)
    good_ym_start <- !is.na(my_ym_start)
    good_ymd_start <- !is.na(my_date_start)
    
    my_ym_end <- ym(str_extract(my_date_end, "\\d{4} [[:alpha:]]+"), quiet = TRUE) # ignore day if it exists to avoid ym() confusion
    my_date_end <- ymd(my_date_end, quiet = TRUE)
    good_year_end <- !is.na(my_year_end)
    good_ym_end <- !is.na(my_ym_end)
    good_ymd_end <- !is.na(my_date_end)
    
    # Happy path
    if(good_ymd_start & good_ymd_end) {
      return(c(start = my_date_start, end = my_date_end))
    }
    
    ### From here on out we may need to work on both start and end dates before returning
    ### First make sure we have a good date_start, then build date_end
    
    # TODO: should be OK to remove these two lines, since all cases are covered below
    my_date_start_sim <- my_date_start
    my_date_end_sim <- my_date_end
    
    if(!good_ymd_start & good_ym_start) {
      # add start dom
      # TODO: making sure it's before whatever end mo and dom we have
      if(!is.na(my_ym_end) && my_ym_start == my_ym_end) {
        my_date_start_sim <- my_ym_start # pick the first day of the month to minimize chance of it being after end date
      } else {
        my_date_start_sim <- my_ym_start + sample(0:27, 1) # Let's ignore 29th-31st each month for simplicity
      }
    }
    
    if(!good_ym_start & good_year_start) {
      # add start mo and dom, making sure it's before whatever end mo and dom we have
      if(good_ym_end) {
        if(my_year_start == my_year_end) {
          # Start before my_ym_end
          start_yday_max <- pmax(1, yday(my_ym_end) - 31)
          my_yday = sample(1:start_yday_max, 1, 
                           prob = date_start_prob_vector[1:start_yday_max])
          #message(paste0("my_year_start == my_year_end: ", my_year_start, ", my_yday: ", my_yday))
        } else {
          my_yday <- sample(1:(365 - 90), 1, 
                            prob = date_start_prob_vector[1:(365 - 90)])
          #message(paste0("my_year_start != my_year_end; my_year_start: ", my_year_start, "my_year_end: ", my_year_end, ", my_yday: ", my_yday))
        }
        my_date_start_sim <- as.Date(my_yday - 1, format = "%j", origin = ymd(glue("{my_year_start}-01-01"))) #TODO: why -1?
      } else {
        if(good_year_end) { 
          if(my_year_start == my_year_end) {
            my_yday <- sample(1:(365 - 270), 1, 
                              prob = date_start_prob_vector[1:(365 - 270)]) # pick a day in the first quarter
          } else {
            my_yday <- sample(1:365, 1, 
                              prob = date_start_prob_vector)
          }
          my_date_start_sim <- as.Date(my_yday - 1, format = "%j", origin = ymd(glue("{my_year_start}-01-01"))) #TODO: why -1?
        } else {
          my_yday <- sample(1:365, 1, 
                            prob = date_start_prob_vector)
          my_date_start_sim <- as.Date(my_yday - 1, format = "%j", origin = ymd(glue("{my_year_start}-01-01"))) #TODO: why -1?
        }
      }
    }
    
    ### Now we have a good date_start. If date_end is good, we're done
    
    if(good_ymd_end) {
      return(c(start = my_date_start_sim, end = my_date_end))
    }
    
    ### Now make a good date_end
    
    my_ym_start <- floor_date(my_date_start_sim, unit = "month") # in case we repaired the date_start
    
    if(good_ym_end) {
      # add end dom, making sure it's not before date_start
      if(my_ym_start == my_ym_end) {
        my_date_end_sim <- ceiling_date(my_ym_start, unit = "month") - 1 # pick the last day of the month
      } else {
        my_date_end_sim <- my_ym_end + sample(0:27, 1) # Let's ignore 29th-31st each month for simplicity
      }
    }
    
    if(!good_ym_end & good_year_end) {
      # add end dom, making sure it's not before date_start and using duration_prob_vector to pick (delta year + yday)
      if(my_year_start == my_year_end) {
        my_yday <- sample((yday(my_date_start_sim) + 1):365, 1, 
                          prob = date_end_prob_vector[(yday(my_date_start_sim) + 1):365])
      } else {
        my_yday <- sample(1:365, 1, 
                          prob = date_end_prob_vector)
      }
      my_date_end_sim <- as.Date(my_yday - 1, format = "%j", origin = ymd(glue("{my_year_end}-01-01"))) #TODO: why -1?
    } 
    
    if(!good_year_end) {
      # we don't have an end year, so use voyage_length_prob_vector to select year, month and day
      my_duration_days <- 30 * sample(2:nrow(voyage_length_prob_vector), 1, 
                                      prob = voyage_length_prob_vector$n[2:nrow(voyage_length_prob_vector)]) + 
        sample(0:27, 1)
      my_date_end_sim <- my_date_start_sim + my_duration_days
    }
    
    return(c(start = my_date_start_sim, end = my_date_end_sim))
    
  }
  
  # test
  # my_year_start <- 1788
  # my_date_start <- "1788 Mar 2"
  # my_date_start <- "1788 Apr"
  # my_date_start <- "1789"
  # my_ym_start <- "1786-02-01"
  # my_date_end <- "1789 Mar 2"
  # my_date_end <- "1789 Apr"
  # my_date_end <- "1789"
  # xx_duration_days <- replicate(10000,
  #                               30 * sample(2:nrow(voyage_length_prob_vector), 1, 
  #                                           prob = voyage_length_prob_vector$n[2:nrow(voyage_length_prob_vector)]
  #                               )
  # )
  # hist(xx_duration_days)
  # ggplot(data = tibble(x = xx_duration_days)) +
  #   geom_histogram(aes(x), binwidth = 30) +
  #   scale_x_continuous(breaks = (0:30)*100)
  
  # df_vessel_test <- tribble(
  #   ~year_out, ~day_out,         ~year_in, ~day_in,
  #   1777,      "1777 Mar 12",    1778,     "1778 Oct 2",
  #   1777,      "1777 Mar",       1778,     "1778 Oct 2",
  #   1777,      "1777",           1778,     "1778 Oct 2",
  #   1777,      "1777 Mar 12",    1777,     "1777 Oct 2",
  #   1777,      "1777 Mar",       1777,     "1777 Oct",
  #   1777,      "1777",           1777,     "1777 Oct",
  #   1777,      "1777 Mar 12",    1778,     NA_character_,
  #   1777,      "1777 Mar",       1778,     NA_character_,
  #   1777,      "1777",           NA,       NA_character_,
  #   1777,      "1777 Mar 12",    1777,     NA_character_,
  #   1777,      "1777 Mar",       1777,     "1777 Mar",
  #   1777,      "1777 Mar",       1778,     "1778 Mar",
  #   1777,      "1777",           NA,       NA_character_,
  #   1777,      "1777",           1778,     NA_character_
  # )
  
  vessel %>%
    filter(!is.na(year_out)) %>%
    #head(1000) %>% # use for debugging
    mutate(dates_start_end = pmap(list(my_year_start = year_out, 
                                       my_date_start = day_out,
                                       my_year_end = year_in,
                                       my_date_end = day_in),
                                  pick_dates_start_end),
           date_start = as.Date(unlist(dates_start_end)[c(TRUE, FALSE)], origin = "1970-01-01"),
           date_end = as.Date(unlist(dates_start_end)[c(FALSE, TRUE)], origin = "1970-01-01")
    ) %>%
    select(-dates_start_end) %>%
    filter(year_out < 1930) %>% # remove one outlier with incomplete data
    mutate(duration_days = as.integer(difftime(date_end, date_start,
                                               units = "days")) + 1, # add one day
           decade = 10 * floor(year_out / 10)
    ) %>% 
    filter(between(duration_days, cutoff_voyage_duration_days, voyage_years_max * 365)) %>% # data quality filter
    arrange(voyage_id, desc(duration_days)) %>%
    left_join(.,
              df_log %>% 
                arrange(voyage_id, desc(n_obs)) %>%
                distinct(voyage_id, .keep_all = TRUE) %>%
                select(voyage_id, n_obs, source),
              by = "voyage_id")
  
}


##### make_voyages_no_large_gaps() for plotting
make_voyages_no_large_gaps <- function(df_log, voyages_for_plot) {
  # defined in constants.R
  #   max_obs_gap
  #   voyage_set_batch
  
  voyages_no_large_obs_gap <- df_log %>%
    filter(!first_log_entry) %>%
    arrange(log_date) %>%
    group_by(voyage_id) %>%
    summarize(max_days_between_obs = max(days_between_obs, na.rm = TRUE)) %>%
    ungroup() %>%
    filter(max_days_between_obs <= max_obs_gap) %>%
    anti_join(.,
              tibble(voyage_id = bad_tracks),
              by = "voyage_id")
  
  voyages_for_plot %>%
    inner_join(.,
               voyages_no_large_obs_gap, 
               by = "voyage_id") %>%
    arrange(log_date_min) %>%
    mutate(id = row_number(),
           batch = ceiling(id / voyage_set_batch))
  
}
