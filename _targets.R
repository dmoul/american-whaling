#_target.R

library(targets)
#remotes::install_github("ropensci/tarchetypes") # need tarchetypes >= 0.6.0.9000 for tar_quarto() function
# see https://books.ropensci.org/targets/literate-programming.html
library(tarchetypes)
#library(here)
#source("./scripts/my-setup.R") replaced with functions below

options(tidyverse.quiet = TRUE,
        dplyr.summarise.inform = FALSE)


packages <- c("here",
             "readr",
             "tibble",
             "tidyr",
             "readxl",
             "attempt",
             "dplyr",
             "janitor",
             "lubridate",
             "scales",
             "ggplot2",
             "gt",
             "glue",
             "purrr",
             "forcats",
             "stringr",
             "ggridges", # for geom_density_ridges()
             "ggdark", # for dark_mode()
             "ggtext", # for element_markdown()
             "UpSetR", # for upset() intersection plot
             "broman", # for spell_out()
             "broom", # for tidy()
             "rnaturalearth", ## for landmass boundaries
             "cowplot", # for theme_map() ??
             "sf",
             "lwgeom", # for st_distance() and st_make_valid()
             "units",
             #"hrbrthemes", # for plot layout
             "ggrepel", # for geom_text_repel
             "av", # to make mp4 animations (via make_whale_yearly_video() in mapping-prep.R for whales.qmd)
             "fs", # to make mp4 animations (via make_whale_yearly_video() in mapping-prep.R for whales.qmd)
             "UpSetR", # for upset plot
             "grid", # for upset plot
             
             NULL)

  tar_option_set(
    packages = packages,
    format = "rds"
  )

invisible(lapply(packages, library, character.only = TRUE))
source("./scripts/constants.R")
source("./scripts/settings.R")
source("./scripts/functions.R")

# these functions prepare .rds files used later (if they don't exist already)
prepare_voyage_data()
prepare_logbook_data()
prepare_voyages_for_plot_data()

list(
  tar_target(my_packages, save_package_list(packages),
             format = "rds"), # use rds since this is a list, not a dataframe
  tar_target(voyage, get_df("df-voyage.rds")),
  tar_target(voyage_distinct, get_df("df-voyage-distinct.rds")),
  tar_target(log_all, get_df("df-log-all.rds")),
  tar_target(log, get_df("df-log.rds")),
  tar_target(cpi_1845, prepare_cpi("./data/cpi/inflation-cpi-1800plus.xlsx")),
  tar_target(voyages_for_plot_sightings, get_df("voyages-for-plot-sightings.rds")),
  tar_target(voyages_for_plot_strikes, get_df("voyages-for-plot-strikes.rds")),
  tar_target(voyages_for_plot_points, get_df("voyages-for-plot-points.rds")),
  tar_target(voyages_for_plot, get_df("voyages-for-plot.rds")),
  tar_target(voyages_no_large_gaps, make_voyages_no_large_gaps(log, voyages_for_plot)),
  tar_target(crew, prepare_crew_data(voyage_distinct)),
  tar_target(crew_list, get_crewlist(crew)),
  tar_target(cwm, get_charleswmorgan_data()),
  # make some dataframes
  tar_target(vessel, make_vessel(voyage_distinct)),
  tar_target(voyage_duration, make_voyage_duration(vessel, log)),
  tar_target(voyage_duration_simulated, 
             make_voyage_duration_simulated(voyage_distinct, voyage_duration, vessel, log)),
  tar_target(voyages_for_plot_nolargegaps, make_voyages_no_large_gaps(log, voyages_for_plot)),
  NULL
)
