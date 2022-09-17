# constants.R

###### PART 1: Project convention constants

# for excluding voyages for plotting that have poor data
cutoff_n_obs <- 100
cutoff_voyage_distance_km <- 200 # when creating voyages_for_plot* exclude voyages shorter than this distance (assume error)
cutoff_voyage_duration_days <- 30  # when creating voyages_for_plot* exclude voyages shorter than this duration (assume error)
voyage_years_max <- 6 # when creating df_voyages_distinct, drop voyages longer than this
days_between_obs_max <- 365; # when creating df_log, drop voyages with gaps in logs longer than this

max_obs_gap <- 7 # for make_voyages_no_large_gaps(); results in 138 good tracks
#max_obs_gap <- 14 # increases tracks to 204; add too many bad tracks
voyage_set_batch <- 5

# for creating mp4 files in make_whale_yearly_video()
dir_out_temp <- "./image-output-temp/"
dir_out <- "./figure-output/"

# map projection
#my_proj <- "WGS84"
my_proj <- "+proj=moll"

# for plots
my_caption <- "Source: American Offshore Whaling Voyages: A Database; plot: Daniel Moul"

# colors from https://colorbrewer2.org/#type=diverging&scheme=RdYlBu&n=5
my_color_scale_d5 <- c('#d7191c','#fdae61','#ffffbf','#abd9e9','#2c7bb6')
my_color_scale_d3 <- c('#d7191c','#ffffbf','#2c7bb6')
my_color_red <- "#d7191c"
my_color_blue <- "#2c7bb6"
my_color_yellow <- '#ffffbf'

my_species_color_manual <- tribble(
  ~species,               ~my_color,
  "Sperm",                 my_color_red,
  "Right",                 my_color_yellow,
  "Whale",                 my_color_blue,
  "Whale (unspecified)",   my_color_blue
)

my_product_color_manual <- tribble(
  ~product,                ~my_color,
  "sperm",                 my_color_red,
  "whale",                 my_color_yellow,
  "bone",                  my_color_blue
)

species_levels <- 
  c("Sperm",
    "Whale",
    "Right",
    "Finback",
    "Pilot",
    "Bowhead",
    "Humpback",
    "Grampus",
    "Gray",
    "Killer",
    "Blue",
    "Dolphin")

options(ggplot2.discrete.fill = my_color_scale_d3,
        ggplot2.discrete.colour = my_color_scale_d3,
        dplyr.summarise.inform = FALSE)


###### PART 2: Source data constants

# production unit conversions
# (used to get product output on somewhat similar scales as convenience when plotting)
gal_per_barrel <- 31.5 # not 42 as with petroleum; see end note reference
ton_per_lb <- 1 / 2000

min_crew_size_cutoff <- 12

# Subset of good tracks manually validated
good_tracks <- c(
  "AV05786", # New England to Hudson Bay
  "AV00436", # New England to Caribbean
  "AV00776", # North and South Atlantic
  "AV11052", # North Atlantic
  "AV01837", # North and South Atlantic
  "AV06038", # Around Cape to East Africa
  "AV06166", # Near North America
  "AV00826", # North Atlantic circle
  "AV08211", # Around the world
  "AV12950", # North Atlantic circle
  "AV13262", # North and South Atlantic
  "AV04246", # New England out and back again
  "AV04243", # North and South Atlantic
  "AV05358", # North and South Atlantic
  "AV02476", # North and South Atlantic
  "AV12846", # South Atlantic circle
  "AV00305", # North Atlantic circle
  "AV15502", # North and South Atlantic
  "AV09120", # North and South Atlantic
  "AV06598", # New England to Cape Town
  "AV12552", # North Atlantic circle
  "AV08147", # New England out and back again
  "AV09201", # North and South Atlantic and around horn
  "AV12541", # Bering Strait
  "AV08064", # Gulf of Mexico
  "AV06168", # North American Coast and North Atlantic
  "AV01286", # North America to Java
  "AV01579", # North and South America and African coast
  "AV15049", # South American and South Africa
  "AV12541", # California to Bering Strait
  "AV05729", # North America, Europe, South America, Africa
  "AV00305", # North Atlantic circle
  "AV00775" # North Atlantic and Caribbean
)

# All bad tracks, manually validated, which have bad coordinates that create tracks over land, typically multiple times
# anti-join with voyages_for_plot to get all good tracks (also with no gaps of obs > 7 days)
bad_tracks <- c(
  "AV09058",
  "AV00163",
  "AV08896",
  "AV00167",
  "AV06653",
  "AV13479",
  "AV00168",
  "AV01937",
  "AV13904",
  "AV00171",
  "AV03576",
  "AV14192",
  "AV06063",
  "AV13907",
  "AV05712",
  "AV12574",
  "AV06229",
  "AV10907",
  "AV09441",
  "AV00317",
  "AV02575",
  "AV04793",
  "AV00318",
  "AV02171",
  "AV05396",
  "AV11032",
  "AV05598",
  "AV05600",
  "AV13237",
  "AV00170",
  "AV07511",
  "AV05201",
  "AV05380",
  "AV03056",
  "AV00747",
  "AV04023",
  "AV13905",
  "AV04361",
  "AV06612",
  "AV02096",
  "AV02097",
  "AV06617",
  "AV07293",
  "AV07019",
  "AV03365",
  "AV04823",
  "AV09262",
  "AV00434"
)


# Voyage codes and their meanings from https://whalinghistory.org/av/voyages/columns/

code_return <- tribble(
  ~code, ~description,
  "A", "Abandoned",
  "B", "Burned",
  "C", "Condemned",
  "L", "Lost, sank, wrecked or missing",
  "M", "Missing",
  "S", "Seized",
  "F", "Sold, chartered, or withdrawn",  #"Chartered for freighting",
  "So", "Sold, chartered, or withdrawn",  #"Sold, in a foreign port",
  "W", "Sold, chartered, or withdrawn",  #"Withdrawn",
  "U", "Unknown",
  "prob", "Unknown", #"Uncertain",
  "?", "Unknown", #"Uncertain",
  "no code", "no code",
  "OK", "OK",
)

code_masters_fate <- tribble(
  ~code, ~description,
  "A", "Alternate master name", # Sources disagreed on who was master of the voyage and the authors were unable to resolve the disagreement with confidence. Voyages are repeated under all alternate master names.
  "D", "Died, other causes",
  "K", "Killed by whale",
  "L", "Left the ship, usually because of illness",
  "R", "Replacement master",
  "?", "Uncertain",
  "RK", "Replacement master; killed by whale",
  "RL", "Replacement master; left the ship",
  "RD", "Replacement master; died",
  "LD", "Left the ship; died"
)

code_vessel_rig <- tribble(
  ~code, ~description,
  "Bark", "Bark",
  "Brig", "Brig",
  "DSchr", "Diesel Schooner",
  "Gall", "Galliot",
  "GSchr", "Gas Schooner",
  "GYawl", "Gas Yawl",
  "SBark", "Steam Bark",
  "SBgtn", "Steam Brigantine",
  "SBktn", "Steam Barkentine",
  "Scow", "Scow",
  "Schr", "Schooner",
  "Sloop", "Sloop",
  "SSchr", "Steam Schooner",
  "Ship", "Ship",
  "Snow", "Snow",
  "Str", "Steamer"
)

code_other_abbreviations <- tribble(
  ~code, ~description,
  "aband", "abandoned",
  "cond", "condemned",
  "destr", "destroyed",
  "lat", "latitude",
  "long", "longitude",
  "mer", "merchant",
  "merch", "merchant",
  "prob", "probably",
  "p", "probably",
  "ser, serv", "service",
  "sp", "sperm",
  "str", "straits",
  "sts", "straits",
  "?", "uncertain",
  "wh", "whale",
  "withdr", "withdrawn"
)

