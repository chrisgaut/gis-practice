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
city <- "Athens, GA"
origin <- "Athens, Georgia"
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
origin_df <- tibble::tibble(addr = origin)
orig_xy <- tidygeocoder::geocode(
  origin_df, address = addr,
  method = "osm", lat = lat,
  long = lon, limit = 1, quiet = TRUE
)

origin_sf <- sf::st_as_sf(
  orig_xy,
  coords = c("lon", "lat"),
  crs = 4326
)

v_sf <- sf::st_as_sf(
  verts, 
  coords = c("x", "y"),
  crs = 4326, remove = FALSE
)

idx <- sf::st_nearest_feature(
  origin_sf, v_sf
)

from_id <- v_sf$id[idx]

# 5) Travel times (minutes) on undirected motorcar graph
if(
  "w" %in% names(graph_ud)
) {
  graph_ud$d <- graph_ud$w
}

times_mat <- dodgr::dodgr_dists(
  graph_ud, from = from_id,
  to = verts$id
)

tmins <- as.numeric(times_mat[1, ]) / 60

vtimes <- data.frame(
  id = as.character(verts$id),
  tmin = tmins,
  stringsAsFactors = FALSE
)

# 6) Build edge geometries & join times
edges_try <- try(
  dodgr::dodgr_to_sf(
    graph_ud
  ), silent = TRUE
)

edges_sf <- try(
  edges_try$edges,
  silent = TRUE
)

# Fallback: rebuild geometry if needed
if(!inherits(edges, "sf") || nrow(edges_sf) == 0) {
  v_from <- data.frame(
    from_id = as.character(verts$id),
    x_from = verts$x, y_from = verts$y
  )
  
  v_to <- data.frame(
    to_id = as.character(verts$id),
    x_to = verts$x,  y_to = verts$y
  )
  
  g_min <- data.frame(
    from_id = as.character(graph_ud$from_id),
    to_id = as.character(graph_ud$to_id)
  )
  
  g_min <- merge(
    g_min, v_from, by = "from_id",
    all.x = TRUE
  )
  
  g_min <- merge(
    g_min, v_to, by = "to_id",
    all.x = TRUE
  )
  
  keep <- is.finite(g_min$x_from) & 
    is.finite(g_min$y_from) &
    is.finite(g_min$x_to) &
    is.finite(g_min$y_to)
  
  g_min <- g_min[keep, , drop = FALSE]
  
  line_list <- lapply(
    seq_len(nrow(g_min)),
    function(i) {
      sf::st_linestring(
        matrix(c(
          g_min$x_from[i], g_min$y_from[i],
          g_min$x_to[i], g_min$y_to[i]
        ), ncol = 2, byrow = TRUE
        )
      )
    }
  )
  
  edges_sf <- sf::st_sf(
    from_id = g_min$from_id,
    to_id = g_min$to_id,
    geometry = sf::st_sfc(line_list, crs = 4326)
  )
}

# Join times from the two endpoint vertices
edges_sf$from_id <- as.character(edges_sf$from_id)
edges_sf$to_id <- as.character(edges_sf$to_id)
geom_col <- sf::st_geometry(edges_sf)
edges_df <- sf::st_drop_geometry(edges_sf)

t_lookup <- setNames(vtimes$tmin, vtimes$id)
edges_df$t_from <- unname(
  t_lookup[edges_df$from_id]
)
edges_df$t_to <- unname(
  t_lookup[edges_df$to_id]
)
edges_df$t_edge <- pmin(
  edges_df$t_from, edges_df$t_to,
  na.rm = TRUE
)
edges_df$geometry <- geom_col
edges_sf <- sf::st_as_sf(
  edges_df, sf_column_name = "geometry",
  crs = 4326
)

# Keep only edges with a finite time and within the cutoff
edges_sf <- edges_sf[is.finite(edges_sf$t_edge) &
                       edges_sf$t_edge <= max_mins, ]

# 7) Basemap tiles (Web Mercator 3857)
# Get tiles covering the selected edges
# returns a SpatRaster in EPSG:3857
tiles_3857 <- maptiles::get_tiles(
  x = edges_sf,
  provider = "CartoDB.Positron",
  zoom = 14, crop = TRUE
)

terra::plotRGB(tiles_3857)

# Transform edges to match tile CRS (EPSG:3857) for plotting
edges_3857 <- sf::st_transform(edges_sf, crs = 3857)

# 8) Discrete color scale
edges_plot <- edges_3857

# Build pretty breaks in minutes (5-min steps, include max).
breaks_min <- seq(0, max_mins, by = 5)

if (
  tail(breaks_min, 1) < max_mins
) breaks_min <- c(breaks_min, max_mins)

edges_plot$t_cat <- cut(
  edges_plot$t_edge,
  breaks = breaks_min,
  include.lowest = TRUE,
  right = TRUE, dig.lab = 3
)

# Prettify legend labels like "0-5", "5-10"
lvl <- levels(edges_plot$t_cat)
pretty_labels <- paste0(
  head(breaks_min, -1),
  "-", tail(breaks_min, -1)
)
legend_labels <- setNames(pretty_labels, lvl)

# 9) Plot with ggplot2
# linewidth is in mm; choose a sensible visual width
road_linewide_mm <- 1.25

p <- ggplot() +
  tidyterra::geom_spatraster_rgb(
    data = tiles_3857
  ) +
  geom_sf(
    data = edges_plot,
    aes(color = t_cat),
    linewidth = road_linewide_mm,
    lineend = "round",
    linejoin = "round",
    alpha = 0.9,
    show.legend = TRUE
  ) +
  scale_color_viridis_d(
    name = "Travel time (min)",
    option = "magma", direction = 1,
    drop = FALSE, breaks = lvl,
    labels = legend_labels
  ) +
  coord_sf(crs = 3857, expand = FALSE) +
  theme_void() +
  theme(
    legend.position = "right",
    legend.key.height = grid::unit(8, "mm"),
    legend.key.width = grid::unit(8, "mm"),
    legend.text = element_text(
      size = 9
    ),
    legend.title = element_text(
      size = 10, face = "bold"
    ),
    plot.title = element_text(
      size = 17, face = "bold",
      hjust = .1
    ),
    plot.subtitle = element_text(
      size = 16, face = "bold",
      hjust = .1
    )
  ) +
  ggspatial::annotation_scale(
    location = "bl", width_hint = 0.2,
    text_cex = 0.8
  ) +
  ggspatial::annotation_north_arrow(
    location = "tl", which_north = "true",
    style = ggspatial::north_arrow_fancy_orienteering
  ) +
  labs(
    title = paste0(
      "Drive-time reach (<= ", max_mins,
      " min), ", city
    ),
    subtitle = paste0("Origin: ", origin),
    caption = "Basemap (c)CartoDB/(c)OSM contributors"
  )

out_prefix <- gsub("[,]", "-", city)
out_prefix <- gsub("[ ]", "", out_prefix)
out_prefix <- tolower(out_prefix)
ggsave(
  filename = paste0(out_prefix, "-pulse-lines.png")
  , plot = p, width = 7, height = 7, dpi = 600,
  bg = "white"
)
