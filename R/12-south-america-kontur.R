library(tidyverse)
library(sf)
library(ggtext)
library(patchwork)
library(here)

# KONTUR Datasets (2023-11-01)
#' Brazil: https://data.humdata.org/dataset/kontur-population-canada


# Population density
kontur <- st_read(here("data", "kontur_population_BR_20231101.gpkg"))
st_crs(kontur)
st_bbox(kontur)

# Country shape
shp <- giscoR::gisco_get_countries(country = "Brazil", epsg = "3857")


# Custom theme
custom_theme <- function() {
  theme_void(base_family = "Fira Sans") +
    theme(
      plot.background = element_rect(color = "grey9", fill = "grey9"),
      text = element_text(color = "grey92"),
      plot.title = element_text(
        color = "grey98", family = "Fira Sans", face = "bold", hjust = 0.5,
        size = 24),
      plot.subtitle = element_textbox(
        lineheight = 1.2, width = 1, hjust = 0.5, halign = 0.5),
      plot.caption = element_markdown(hjust = 0.7),
      legend.position = c(0.1, 0.3),
      legend.direction = "vertical",
      legend.key.width = unit(3, "mm"),
      legend.text = element_text(size = 8),
      plot.margin = margin(c(t = 2, b = 0, l = 10, r = 10))
    )
}
theme_set(custom_theme())


p <- ggplot(kontur) +
  geom_sf(
    data = shp,
    color = "grey40", fill = "grey15", linewidth = 0.1
  ) +
  geom_sf(aes(fill = population), linewidth = 1e-4, color = "white") +
  colorspace::scale_fill_continuous_sequential(
    palette = "YlGnBu", breaks = c(10, 100, 1000, 5000, 20000), rev = FALSE,
    labels = scales::number_format(), trans = "log") +
  coord_sf(xlim = c(st_bbox(kontur)["xmin"] - 2e5, st_bbox(kontur)["xmax"] + 2e5)) +
  labs(
    title = "Brazil Population Density",
    subtitle = "400m hexagon population grid. Values represent number of people
    per cell.",
    caption = "Data: Kontur Population Dataset (Release 2023-11-01).
    Visualization: Ansgar Wolsing",
    fill = "Population (log)"
  )
ggsave(here("plots", "12-south-america-br-pop-density.png"), dpi = 200,
       width = 4, height = 4, scale = 2)


# Create insets / magnifying glass -----------------------

create_magnifying_inset <- function(coords, name, dist = 75000) {
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
    guides(fill = "none") +
    labs(title = name) +
    theme(
      plot.background = element_rect(color = NA, fill = NA),
      plot.title = element_text(size = 10)
    )
  p_inset
}

# Create insets for the 4 largest cities
coords_vancouver <- st_geometry(st_point(c(-123.0, 49.3)))
coords_toronto <- st_geometry(st_point(c(-80.0, 43.7)))
coords_montreal <- st_geometry(st_point(c(-73.9, 45.6)))
coords_edmonton <- st_geometry(st_point(c(-113.8, 53.5)))

p_inset_vancouver <- create_magnifying_inset(coords_vancouver, "Vancouver")
p_inset_toronto <- create_magnifying_inset(coords_toronto, "Toronto")
p_inset_montreal <- create_magnifying_inset(coords_montreal, "Montréal")
p_inset_edmonton <- create_magnifying_inset(coords_edmonton, "Edmonton")

p_combined <- p +
  inset_element(p_inset_vancouver, left = 0, bottom = -0.015, right = 0.25,
                top = 0.185, align_to = "full") +
  inset_element(p_inset_toronto, left = 0.75, bottom = 0.4, right = 1,
                top = 0.60, align_to = "full") +
  inset_element(p_inset_montreal, left = 0.75, bottom = 0.6, right = 1,
                top = 0.80, align_to = "full") +
  inset_element(p_inset_edmonton, left = 0.35, bottom = 0.25, right = 0.6,
                top = 0.5, align_to = "full")
ggsave(here("plots", "10-north-america-ca-pop-density-with-inset.png"), dpi = 600,
       width = 5, height = 5, scale = 2)
