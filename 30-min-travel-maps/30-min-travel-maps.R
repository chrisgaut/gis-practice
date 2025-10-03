##############################
# 30-Minute Travel Maps
# Christopher Gauthier 07/20/2025
# Milos Popovic on YouTube "30-Minute Travel Maps: The Secret to Fast Exploration" 
# https://www.youtube.com/watch?v=M-O0o3L28Hk
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
edges_basic <- data.frame(
  from = as.character(graph_all$from_id),
  to = as.character(graph_all$to_id),
  stringsAsFactors = FALSE
)

g_ug <- igraph::graph_from_data_frame(
  edges_basic, directed = FALSE
)
comp <- igraph::components(g_ug)
giant <- which.max(comp$csize)
keep_ids <- names(comp$membership)[comp$membership == giant]

graph <- graph_all[
  graph_all$from_id %in% keep_ids &
    graph_all$to_id %in% keep_ids,
]

# 3) Make graph effectively undirected
make_undirected <- function(g) {
  rev <- g
  rev$from_id <- g$to_id
  rev$to_id <- g$from_id
  
  if(
    all(
      c(
        "xfr", "yfr", "xto", "yto"
      ) %in% names(g)
    )
  ) {
    rev$xfr <- g$xto
    rev$yfr <- g$yto
    rev$xto <- g$xfr
    rev$yto <- g$yfr
  }
  keep <- intersect(names(g), names(rev))
  unique(rbind(g[, keep], rev[, keep]))
}

graph_ud <- make_undirected(graph)
verts <- dodgr::dodgr_vertices(graph_ud)

# 4) Geocode origin, snap to the component

