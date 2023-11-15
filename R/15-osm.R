library(tidyverse)
library(sf)
library(osmdata)
library(ggtext)
library(here)

# Get shape of Germany
shp <- giscoR::gisco_get_countries(country = "Germany")
st_crs(shp)
st_bbox(shp)

# Pull OSM data in chunks ------------------------------------------------------

# Split bounding box into sections
bbox_sf <- st_as_sfc(st_bbox(shp)) %>%
  st_transform(crs = 32632)
bbox_grid <- st_make_grid(bbox_sf, n = 3)
bbox_mat <- do.call("rbind", lapply(bbox_grid %>% st_transform(4326), st_bbox))
bbox_mat
bbox_mat[1, ]

# Retrieve features by section
features <- vector("list", nrow(bbox_mat))
for (i in seq_len(nrow(bbox_mat))) {
  message(bbox_mat[i, ])
  features[[i]] <- opq(bbox = bbox_mat[i, ], timeout = 1200) %>%
    add_osm_feature(key = "railway", value = "rail") %>%
    osmdata_sf(quiet = FALSE)
  write_rds(features[i],
            here("output", "osm-railway-de", sprintf("osm-railway-de-features-%s.rds", i)),
            compress = "gz")
}

# Merge linestrings with the same osm_id ---------------------------------------

features_lines <- features %>%
  map_dfr(pluck, "osm_lines")

features_lines %>%
  st_drop_geometry() %>%
  count(osm_id, sort = TRUE) %>%
  count(n)

# Distinguish main and branch lines
features_lines %>%
  st_drop_geometry() %>%
  count(usage, sort = TRUE)

# Identify lines that occur in more than one subset
features_lines_multiple_merged <- features_lines %>%
  group_by(osm_id) %>%
  filter(n() > 1) %>%
  summarize(
    geometry = st_union(geometry),
    operator = first(operator),
    usage = first(usage)
    )

# Add merged lines to the unique lines
features_lines_combined <- features_lines %>%
  select(osm_id, operator, usage, geometry) %>%
  group_by(osm_id) %>%
  filter(n() == 1) %>%
  ungroup() %>%
  bind_rows(features_lines_multiple_merged) %>%
  mutate(
    usage_main = usage == "main",
    usage_main = if_else(is.na(usage_main), FALSE, usage_main)
    )

features_lines_combined %>%
  st_drop_geometry() %>%
  count(usage_main)

# Intersect with country shape
features_lines_combined_filtered <- features_lines_combined %>%
  st_filter(shp, .predicate = st_intersects)


# City locations ---------------------------------------------------------------

calculate_bbox_center <- function(bbox) {
  x <- (bbox["x", "min"] + bbox["x", "max"]) / 2
  y <- (bbox["y", "min"] + bbox["y", "max"]) / 2
  list("x" = x, "y" = y)
}

cities <- list(
  "Berlin" = calculate_bbox_center(getbb("Berlin", limit = 1)),
  "Köln" = calculate_bbox_center(getbb("Köln", limit = 1)),
  "Hamburg" = list("x" = 9.98, "y" = 53.50), # calculate_bbox_center(getbb("Hamburg", limit = 1)),
  "Frankfurt/\nMain" = calculate_bbox_center(getbb("Frankfurt am Main", limit = 1)),
  "München" = calculate_bbox_center(getbb("München", limit = 1)),
  "Leipzig" = calculate_bbox_center(getbb("Leipzig", limit = 1))
)
cities_df <- bind_rows(cities, .id = "city") %>%
  st_as_sf(coords = c("x", "y"))
st_crs(cities_df) <- "EPSG:4326"

# Calculate distances to the cities
distances <- st_distance(features_lines_combined_filtered, cities_df)
# Shortest distance
features_lines_combined_filtered$distance <- apply(distances, 1, min)


# Map --------------------------------------------------------------------------

p <- features_lines_combined_filtered %>%
  ggplot() +
  geom_sf(
    aes(color = distance, linewidth = ifelse(usage_main, 0.33, 0.2))
  ) +
  geom_sf(
    data = cities_df,
    color = "grey2", size = 4, shape = 21, stroke = 0.7
  ) +
  annotate(
    "text",
    x = c(15, 5, 5, 5, 15, 15),
    y = c(st_coordinates(cities_df$geometry)[1:5, "Y"], 50.7),
    label = cities_df$city,
    hjust = c(0, 1, 1, 1, 0, 0),
    family = "Helvetica Neue", fontface = "bold", size = 3, color = "grey20",
    lineheight = 0.8
  ) +
  annotate(
    "segment",
    x = c(14.85, 5.15, 5.15, 5.15, 14.85, 14.85),
    xend = st_coordinates(cities_df$geometry)[, "X"] +
      0.15 * ifelse(st_coordinates(cities_df$geometry)[, "X"] < 10, -1, 1),
    y = c(st_coordinates(cities_df$geometry)[1:5, "Y"], 50.7),
    yend = st_coordinates(cities_df$geometry)[, "Y"],
    linewidth = 0.3, color = "grey20"
  ) +
  colorspace::scale_color_continuous_sequential(
    palette = "Reds 2", rev = FALSE) +
  scale_linewidth_identity() +
  coord_sf(
    xlim = c(st_bbox(shp)["xmin"] - 1.5, st_bbox(shp)["xmax"] + 1.5),
    clip = "off"
  ) +
  guides(color = "none") +
  labs(
    title = "Railway Network Germany",
    caption = "Data: OpenStreetMap contributors. Visualization: Ansgar Wolsing"
  ) +
  theme_void(base_family = "Helvetica Neue") +
  theme(
    plot.background = element_rect(color = "grey98", fill = "grey98"),
    plot.margin = margin(t = 4, l = 4, r = 4, b = 6),
    plot.title = element_text(
      family = "Helvetica Neue", face = "bold", hjust = 0.5, size = 16),
    plot.caption = element_text(hjust = 0.5, size = 7)
  )
ggsave(here("plots", "15-osm-rail.png"), width = 6, height = 6)
