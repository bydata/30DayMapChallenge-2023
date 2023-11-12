library(tidyverse)
library(sf)
library(ggtext)
library(patchwork)
library(here)

# Map projection: Brazil polyconic: EPSG:5880
crs <- "EPSG:5880"

# KONTUR Datasets (2023-11-01)
#' Brazil: https://data.humdata.org/dataset/kontur-population-canada
kontur <- st_read(here("data", "kontur_population_BR_20231101.gpkg"))

st_crs(kontur)
st_bbox(kontur)
kontur <- st_transform(kontur, crs = crs)
st_bbox(kontur)


# Country shape
shp <- giscoR::gisco_get_countries(country = "Brazil", epsg = "3857")
shp <- st_transform(shp, crs = crs)

# Americas shape
shp_americas <- giscoR::gisco_get_countries(region = "Americas", epsg = "3857")
shp_americas <- st_transform(shp_americas, crs = crs)

# Functions for custom annotations
annotate_text <- function(hjust = 0,...) {
  annotate(
    "richtext",
    lineheight = 1.05, vjust = 0.5, color = "white", family = "Fira Sans",
    hjust = hjust, fill = NA, label.size = 0, size = 3.5,
    ...)
}

annotate_segment <- function(...) {
  annotate(
    GeomSegment,
    ..., linewidth = 0.5, color = "white"
  )
}


# Transform coordinates --------------------------------------------------------

transform_coordinates <- function(lon, lat, crs_in = "EPSG:4326", crs_out = st_crs(kontur)) {
  st_point(c(lon, lat)) %>%
    st_geometry() %>%
    st_sf(crs = crs_in) %>%
    st_transform(crs = crs_out)
}

# Rio
transform_coordinates(-43.205556, -22.911111)
# Sao Paulo
transform_coordinates(-46.633333, -23.55)
# Brasília
transform_coordinates(-47.882778, -15.793889)
# Fortaleza
transform_coordinates(-38.5275, -3.7275)

# Population figures: https://en.wikipedia.org/wiki/List_of_cities_in_Brazil_by_population


# Map --------------------------------------------------------------------------

p <- ggplot(kontur) +
  geom_sf(
    data = shp_americas,
    color = "grey40", fill = "grey11", linewidth = 0.2, linetype = "dotted"
  ) +
  geom_sf(
    data = shp,
    color = "grey40", fill = "grey14", linewidth = 0.2
  ) +
  geom_sf(aes(fill = population), linewidth = 0, color = "white") +
  # Rio de Janeiro: -43.205556, -22.911111 // 6,211,423
  annotate_text(
    label = "**Rio de Janeiro**<br>pop. 6,211,000",
    x = 7.1e6, y = 7424714) +
  annotate_segment(x = 7.1e6, xend = 6.18e6, y = 7424714, yend = 7424714) +
  # Sao Paulo: -23.55,-46.633333 // 11,451,245
  annotate_text(
    label = "**Sao Paulo**<br>pop. 11,451,000",
    x = 7.1e6, y = 7e6) +
  annotate_segment(x = 7.1e6, xend = 5.8e6, y = 7e6, yend = 7375236) +
  # Brasília: 15.793889,-47.882778 // 2,817,068
  annotate_text(
    label = "**Brasília**<br>pop. 2,817,000",
    x = 7.1e6, y = 8243642) +
  annotate_segment(x = 7.1e6, xend = 5.7e6, y = 8243642, yend = 8243642) +
  # Fortaleza: -3.7275, -38.5275 // 2,428,678
  annotate_text(
    label = "**Fortaleza**<br>pop. 2,428,000",
    x = 7.1e6, y = 9572742) +
  annotate_segment(x = 7.1e6, xend = 6.7e6, y = 9572742, yend = 9572742) +
  colorspace::scale_fill_continuous_sequential(
    palette = "YlGnBu", breaks = c(10, 100, 1000, 5000, 20000), rev = FALSE,
    labels = scales::number_format(), trans = "log") +
  coord_sf(
    xlim = c(st_bbox(kontur)["xmin"] - 2e5, st_bbox(kontur)["xmax"] + 2e5),
    ylim = c(st_bbox(kontur)["ymin"] - 2e5, st_bbox(kontur)["ymax"] + 2e5)) +
  labs(
    title = "Brazil Population Density",
    subtitle = "400m hexagon population grid. Values represent number of people
    per cell.",
    caption = "Data: Kontur Population Dataset (Release 2023-11-01),
    IBGE Census (2022).
    Visualization: Ansgar Wolsing",
    fill = "Population<br>*(log)*"
  ) +
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
    legend.position = c(0.05, 0.175),
    legend.direction = "vertical",
    legend.key.width = unit(3, "mm"),
    legend.title = element_markdown(size = 9, lineheight = 1.05),
    legend.text = element_markdown(size = 8),
    plot.margin = margin(c(t = 2, b = 0, l = 10, r = 10))
  )
ggsave(here("plots", "12-south-america-br-pop-density-lo.png"), dpi = 200,
       width = 4, height = 4, scale = 2)
ggsave(here("plots", "12-south-america-br-pop-density-hi.png"), dpi = 500,
       width = 4, height = 4, scale = 2)
