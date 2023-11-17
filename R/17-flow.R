library(tidyverse)
library(ggtext)
library(sf)
library(osmdata)
# devtools::install_github("dkahle/ggmap")
library(ggmap)
library(here)


# Retrieve the Nord Stream relation from OSM
osm_query <- opq(bbox = "Nord Stream") %>%
  add_osm_feature(key = "name", value = "Nord Stream") %>%
  osmdata_sf()
st_bbox(osm_query$osm_multilines)


# Retrieve basemap from Stadia Maps --------------------------------------------

#' To retrieve Stadia Maps tile, you have to sign up for a free account
#' at https://stadiamaps.com/ and create an API key
#' Set the API key as an option: options("stadiamaps-key" = "YOUR_API_KEY")
#' and then access the key with getOption()
register_stadiamaps(getOption("stadiamaps-key"))
stadiamaps_key()
has_stadiamaps_key()

# Retrieve tiles and store them
bbox <- c(left = 11.7, bottom = 53.0, right = 33.7, top = 61.0)
maptype <- "alidade_smooth_dark"
basemap <- get_stadiamap(bbox = bbox, maptype = maptype,  zoom = 6)
write_rds(
  basemap,
  here("output", sprintf("stadia-stamenmaps-outdoors-%s-%s.rds",
                       paste(bbox, collapse = "-"), maptype)),
  compress = "gz")


# Find start and end coordinates for both pipelines
nordstream2_start_end <- osm_query$osm_lines %>%
  filter(name == "Nord Stream 2") %>%
  st_coordinates() %>%
  as.data.frame() %>%
  slice(1, nrow(.))
nordstream1_start_end <- osm_query$osm_lines %>%
  filter(name != "Nord Stream 2") %>%
  st_union() %>%
  st_coordinates() %>%
  as.data.frame() %>%
  slice_max(order_by = X, n = 1, with_ties = FALSE)


# Map --------------------------------------------------------------------------

# Labels
annotations <- data.frame(
  x = c(19.0, 17),
  xend = c(19.75, 15.7),
  y = c(57.8, 55.5),
  yend = c(57.8, 55.5),
  name = c("Nord Stream 1", "Nord Stream 2"))

ggmap(basemap) +
  geom_sf(
    data = st_union(filter(osm_query$osm_lines, name != "Nord Stream 2")),
    aes(geometry = geometry,
        color = "Nord Stream 1"),
    inherit.aes = FALSE, linewidth = 1, linetype = "solid", linejoin = "bevel") +
  geom_sf(
    data = filter(osm_query$osm_lines, name == "Nord Stream 2"),
    aes(geometry = geometry,
        color = "Nord Stream 2"),
    inherit.aes = FALSE, linewidth = 0.8, linetype = "longdash", linejoin = "bevel") +
  geom_point(
    data = nordstream1_start_end,
    aes(X, Y, color = "Nord Stream 1"), size = 1.8, inherit.aes = FALSE
  ) +
  geom_point(
    data = nordstream2_start_end,
    aes(X, Y, color = "Nord Stream 2"), size = 1.8, inherit.aes = FALSE
  ) +
  geom_label(
    data = annotations,
    aes(x, y, label = name, fill = name, hjust = c(1, 0)),
    color = "white", family = "Chivo", label.size = 0, label.r = unit(0.1, "mm")
  ) +
  geom_segment(
    data = annotations,
    aes(x, y, xend = xend, yend = yend, color = name)
  ) +
  scale_color_manual(values = c("#1AC8ED", "#E56399"), aesthetics = c("fill", "color")) +
  guides(fill = "none", color = "none") +
  labs(
    title = "Nord Stream Pipelines in the Baltic Sea.
    <span style='color:#888888'>Now defunct.</span>",
    caption = "Source: OpenStreetMap contributors, basemap: Stadia Maps.
    Visualization: Ansgar Wolsing"
  ) +
  theme_void(base_family = "Source Sans Pro") +
  theme(
    plot.background = element_rect(color = "#222222", fill = "#222222"),
    text = element_text(color = "white"),
    plot.title = element_markdown(family = "Source Sans Pro Semibold", size = 18, hjust = 0.5,
                              margin = margin(b = 3)),
    plot.caption = element_markdown(hjust = 0.5, color = "grey72", size = 8,
                                    margin = margin(t = 6)),
    plot.margin = margin(t = 0, b = 1, l = 1, r = 1),
    legend.position = "bottom"
  )
ggsave(here("plots", "17-flow.png"), width = 6, height = 4.5, scale = 1.2, dpi = 500)
