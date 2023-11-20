library(tidyverse)
library(sf)
library(osmdata)
library(elevatr)
library(ggtext)
library(here)

target_crs <- "EPSG:25834"

# Country shape
shp <- giscoR::gisco_get_countries(country = "Norway", resolution = "03")
bbox_shp <- st_bbox(shp)
shp <- st_transform(shp, crs = target_crs)

# Cities
cities_points <- c("Oslo", "Bergen", "Stavanger", "Trondheim") %>%
  set_names() %>%
  map(function(x) st_centroid(getbb(x, format_out = "sf_polygon", limit = 1))) %>%
  bind_rows(.id = "name") %>%
  st_transform(crs = st_crs(shp))

# Elevation data
elev <- get_elev_raster(shp, z = 6)
elev_country <- raster::mask(elev, shp)
st_crs(elev_country)
elev_country <- raster::projectRaster(elev_country, crs = target_crs)
elev_country_df <- elev_country %>%
  raster::rasterToPoints() %>%
  as.data.frame() %>%
  rename(z = 3)


# National parks from OSM
features <- opq(bbox = bbox_shp, timeout = 1200) %>%
  add_osm_feature(key = "boundary", value = "national_park") %>%
  osmdata_sf()

# Filter national parks to shape boundaries
features_polygons_filtered <- features$osm_polygons %>%
  st_set_crs("EPSG:4326") %>%
  filter(boundary == "national_park") %>%
  st_transform(crs = target_crs) %>%
  st_make_valid() %>%
  st_filter(shp)
features_multipolygons_filtered <- features$osm_multipolygons %>%
  st_set_crs("EPSG:4326") %>%
  filter(boundary == "national_park") %>%
  st_transform(crs = target_crs) %>%
  st_make_valid() %>%
  # Some park areas extend beyond land, avoid this
  st_intersection(shp, .predicate = st_within)

# Check for overlaps by name
features_polygons_filtered %>%
  st_drop_geometry() %>%
  inner_join(st_drop_geometry(features_multipolygons_filtered), by = "name")


# Area of the national parks
features_multipolygons_filtered %>%
  mutate(area = st_area(geometry)) %>%
  st_drop_geometry() %>%
  select(name, area) %>%
  arrange(-area) %>%
  tibble()



national_park_color <- "#1fc44c"


p <- ggplot() +
  geom_raster(
    data = elev_country_df,
    aes(x, y, alpha = z),
    show.legend = FALSE, fill = "grey30"
  ) +
  geom_sf(
    data = shp,
    linewidth = 0.1, alpha = 0.3, fill = "white"
  ) +
  geom_sf(
    data = features_multipolygons_filtered,
    fill = national_park_color, color = national_park_color, alpha = 0.6
  ) +
  geom_sf(
    data = features_polygons_filtered,
    fill = national_park_color, color = national_park_color, alpha = 0.6
  ) +
  # Cities
  geom_sf(
    data = cities_points,
    shape = 21, color = "white", fill = "grey8"
  ) +
  shadowtext::geom_shadowtext(
    data = st_as_sf(data.frame(cities_points, hjust = c(-0.2, 1.1, 1.1, 1.1))),
    aes(
      x = st_coordinates(geometry)[, "X"],
      y = st_coordinates(geometry)[, "Y"],
      label = name, hjust = hjust),
    family = "Noto Sans", bg.color = "white", color = "grey10"
  ) +
  # Title
  annotate(
    GeomTextBox,
    x = -5e5, y = 7.78e6,
    label = sprintf("<span style='color:%s'>National Parks</span> of Norway",
                    national_park_color),
    hjust = 0, size = 12, family = "Playfair Display", width = 0.6,
    fill = NA, box.size = 0, lineheight = 0.96
  ) +
  # Subtitle
  annotate(
    GeomTextBox,
    x = -5e5, y = 7.53e6,
    label = "Norway has **47 national parks** - 46 on the mainland and
    1 in Svalbard, the latter not shown on the map",
    hjust = 0, size = 3, family = "Noto Sans", width = 0.42,
    fill = NA, box.size = 0
  ) +
  # Highlight parks
  ## Rondane (oldest)
  annotate(
    GeomTextBox,
    x = 1.3e5, y = 7.0e6,
    label = "**Rondane** National Park is the oldest national park in Norway,
    established in 1962.",
    hjust = 0, size = 3, family = "Noto Sans", width = 0.5,
    fill = NA, box.size = 0
  ) +
  annotate(
    "segment",
    x = 1.3e5, xend = -0.855e5,
    y = 7.0e6, yend = 6.91e6,
    linewidth = 0.25, linetype = "solid", color = "grey20"
  ) +
  annotate(
    "point",
    x = -0.855e5,
    y =  6.91e6,
    size = 0.5, color = "grey20"
  ) +
  ## Hardangervidda (largest)
  annotate(
    GeomTextBox,
    x = 1.3e5, y = 6.8e6,
    label = "**Hardangervidda** National Park is the largest national park in Norway with
    3,422 km<sup>2</sup>. It was designated in 1981.",
    hjust = 0, size = 3, family = "Noto Sans", width = 0.5,
    fill = NA, box.size = 0
  ) +
  annotate(
    "segment",
    x = 1.3e5, xend = -2.4e5,
    y = 6.8e6, yend = 6.74e6,
    linewidth = 0.25, linetype = "solid", color = "grey20"
  ) +
  annotate(
    "point",
    x = -2.4e5,
    y =  6.74e6,
    size = 0.5, color = "grey20"
  ) +
  coord_sf(crs = target_crs, clip = "off") +
  labs(
    caption = "Source: OpenStreetMap contributors, Mapzen Elevation Data, GISCO.
    Visualization: Ansgar Wolsing"
  ) +
  theme_void(base_family = "Noto Sans") +
  theme(
    plot.background = element_rect(color = "grey93", fill = "grey93"),
    plot.caption = element_markdown(hjust = 1, size = 7),
    plot.margin = margin(rep(3, 4))
  )
ggsave(here("plots", "20-outdoors.png"), width = 5, height = 5, scale = 1.2, dpi = 500)
