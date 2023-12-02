library(tidyverse)
library(sf)
library(ggtext)
library(patchwork)
library(here)


#' Projection: Lambert Conformal Conic
#' https://proj.org/en/9.3/operations/projections/lcc.html
crs <- "+proj=lcc +lon_0=-90 +lat_1=33 +lat_2=45"


# KONTUR Datasets (2023-11-01)
#' Canada: https://data.humdata.org/dataset/kontur-population-canada
#' US: https://data.humdata.org/dataset/kontur-population-united-states-of-america
#' Mexico: https://data.humdata.org/dataset/kontur-population-mexico

# Population density
kontur <- st_read(here("data", "kontur_population_CA_20231101.gpkg"))
st_crs(kontur)
st_bbox(kontur)

# Country shape
shp <- giscoR::gisco_get_countries(country = "Canada", epsg = "3857")

ggplot(shp) + geom_sf()
ggplot(shp) + geom_sf() + coord_sf(crs = crs)


# Custom theme
custom_theme <- function() {
  theme_void(base_family = "Fira Sans") +
    theme(
      plot.background = element_rect(color = "grey93", fill = "grey93"),
      text = element_text(color = "grey9"),
      plot.title = element_text(
        color = "grey2", family = "Fira Sans", face = "bold", hjust = 0.5,
        size = 24),
      plot.subtitle = element_textbox(
        lineheight = 1.2, width = 1, hjust = 0.5, halign = 0.5),
      plot.caption = element_markdown(hjust = 0.7),
      legend.position = c(0.1, 0.75),
      legend.direction = "vertical",
      legend.key.width = unit(3, "mm"),
      legend.text = element_text(size = 8),
      plot.margin = margin(c(t = 2, b = 0, l = 10, r = 10))
    )
}
theme_set(custom_theme())

# Base map without population density data
p_base <- ggplot() +
  geom_sf(
    data = shp,
    color = "grey20", fill = "grey80", linewidth = 0.1
  ) +
  scale_fill_viridis_c(breaks = c(10, 100, 1000, 5000, 20000),
                       labels = scales::number_format(), direction = 1,
                       trans = "pseudo_log", option = "plasma") +
  coord_sf(crs = crs) +
  labs(
    title = "Canada Population Density",
    subtitle = "400m hexagon population grid. Values represent number of people
    per cell.",
    caption = "Data: Kontur Population Dataset (Release 2023-11-01).
    Visualization: Ansgar Wolsing",
    fill = "Population (log)"
  )


# Add arrows for the insets to be added later ----------------------------------

# Locations of the largest cities
coords_vancouver <- st_geometry(st_point(c(-122.9, 49.3)))
coords_toronto <- st_geometry(st_point(c(-79.8, 43.7)))
coords_montreal <- st_geometry(st_point(c(-73.9, 45.6)))
coords_calgary <- st_geometry(st_point(c(-114.1, 51.05)))
coords_edmonton <- st_geometry(st_point(c(-113.8, 53.5)))

cities_df <- data.frame(geometry = c(coords_vancouver, coords_toronto, coords_montreal,
                                  coords_calgary)) %>%
  st_as_sf(crs = "EPSG:4326") %>%
  st_transform(crs = crs)

# arrows_df <- data.frame(
#   # x = c(),
#   xend = st_coordinates(cities_df)[, "X"],
#   # y = c(),
#   yend = st_coordinates(cities_df)[, "Y"]
# )

p_base +
  geom_sf(
    data = cities_df) +
  coord_sf(crs = crs)


# Add population density data to the map
p_density <- p_base +
  geom_sf(
    data = kontur,
    aes(fill = population),
    linewidth = 1e-4, color = "white") +
  coord_sf(crs = crs)


# Create insets / magnifying glass -----------------------

create_magnifying_inset <- function(coords, name, dist = 60000) {
  st_crs(coords) <- "EPSG:4326"
  area_buffer <- st_buffer(coords, dist = dist)

  kontur_area_buffer <- kontur %>%
    st_filter(st_transform(area_buffer, crs = st_crs(.)), .predicate = st_within)

  p_inset <- kontur_area_buffer %>%
    ggplot() +
    geom_sf(
      data = area_buffer,
      color = "grey10", linewidth = 0.3, fill = "grey93"
    ) +
    geom_sf(aes(fill = population), linewidth = 0, color = NA) +
    scale_fill_viridis_c(breaks = c(10, 100, 1000, 5000, 20000),
                         labels = scales::number_format(), direction = 1,
                         trans = "pseudo_log", option = "plasma") +
    coord_sf(crs = crs) +
    guides(fill = "none") +
    labs(title = name) +
    theme(
      plot.background = element_rect(color = NA, fill = NA),
      plot.title = element_text(size = 10)
    )
  p_inset
}

# Create insets for the largest cities

p_inset_vancouver <- create_magnifying_inset(coords_vancouver, "Vancouver")
p_inset_toronto <- create_magnifying_inset(coords_toronto, "Toronto")
p_inset_montreal <- create_magnifying_inset(coords_montreal, "Montréal")
p_inset_calgary <- create_magnifying_inset(coords_calgary, "Calgary")
# p_inset_edmonton <- create_magnifying_inset(coords_edmonton, "Edmonton")


# Add the insets on the map
p_combined <- p_density +
  inset_element(p_inset_vancouver, left = 0, bottom = 0.05, right = 0.20,
                top = 0.25, align_to = "full") +
  inset_element(p_inset_montreal, left = 0.75, bottom = 0.425, right = 0.95,
                top = 0.625, align_to = "full") +
  inset_element(p_inset_toronto, left = 0.75, bottom = 0.65, right = 0.95,
                top = 0.85, align_to = "full") +
  inset_element(p_inset_calgary, left = 0.26, bottom = 0.35, right = 0.46,
                top = 0.55, align_to = "full")
ggsave(here("plots", "10-north-america-ca-pop-density-with-inset-2.png"), dpi = 400,
       width = 5, height = 5, scale = 2)
