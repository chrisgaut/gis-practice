##############################
# 30-Minute Travel Maps
# Christopher Gauthier 07/20/2025
# Milos Popovic on YouTube "30-Minute Travel Maps: The Secret to Fast Exploration" 
# https://www.youtube.com/watch?v=peLyCb0sN4k
##############################

# Libraries
pacman::p_load(
  sf, osmdata, dodgr, tidygeocoder,
  scales, igraph, maptiles, tidyverse,
  tidyterra, viridisLite, ggspatial
)

# Parameters
city <- "Barcelona, Spain"
origin <- "Placa de Catalunya, Barcelona, Spain"
max_mins <- 30
expand_bb <- 0.05

# 1) OSM -> drivable ways
streets <- dodgr::dodgr_streetnet(
  city, expand = expand_bb,
  quiet = TRUE
)

# Keep only drivable highways
# (keep service to maintain center connectivity)

keep_hw <- c(
  "motorway", "trunk", "primary", "secondary",
  "tertiary", "unclassified", "residential",
  "living_street", "motorway_link", "trunk_link",
  "primary_link", "secondary_link", "tertiary_link",
  "service"
)

if("highway" %in% names(streets)) {
  streets <- streets[streets$highway %in% keep_hw, ]
}

# Weight for motorcar and drop non-finite/zero weights
graph_all <- dodgr::weight_streetnet(
  streets,
  wt_profile = "motorcar",
  keep_cols = "highway"
)

wt_col <- if (
  "w" %in% names(graph_all)
) "w" else "d"

graph_all <- graph_all[is.finite(
  graph_all[[wt_col]]) & graph_all[[wt_col]] > 0, ]

# 2) Keep the largest undirected component
# RESUME HERE
