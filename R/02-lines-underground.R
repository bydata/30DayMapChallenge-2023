library(tidyverse)
library(sf)
library(osmdata)
library(rvest)
library(ggtext)
library(here)

#' Download noise pollution data (Rail Noise: Lnight) from:
#' https://data.london.gov.uk/dataset/noise-pollution-in-london
#' and unzip the archive

## Get shape of London

shp_city <- getbb("London, United Kingdom", format_out = "sf_polygon", limit = 1)
st_crs(shp_city)
st_bbox(shp_city)

# Get Underground features
underground_features <- opq(bbox = st_bbox(shp_city), timeout = 1200) %>%
  add_osm_feature(key = "railway", value = "subway") %>%
  osmdata_sf()

# Get water features
water_features <- opq(bbox = st_bbox(shp_city), timeout = 1200) %>%
  add_osm_feature(key = "water", value = "river") %>%
  osmdata_sf()


# Scrape underground lines table from Wikipedia

remove_note_indicator <- function(x) {
  str_remove_all(x, "\\[.+?\\]")
}

wiki_url <- "https://en.wikipedia.org/wiki/London_Underground"
underground_df <- read_html(wiki_url) %>%
  html_nodes(css = "table.sortable") %>%
  pluck(1) %>%
  html_table(header = TRUE) %>%
  janitor::clean_names() %>%
  filter(name != "Name") %>%
  rename(
    mapcolour = mapcolour_112,
    length_km = length,
    length_mi = length_2,
    average_weekday_ridership_2017 = average_weekday_ridership_2017_113
  ) %>%
  mutate(
    name = str_to_title(str_remove(name, "(?i)\\sLine")),
    across(everything(), remove_note_indicator),
    across(c(stations, opened, cars_per_train, average_weekday_ridership_2017,
             trips_per_year, average_trips_per_mile),
           function(x) str_remove_all(x, ",") %>% as.integer),
    across(c(length_km, length_mi), as.numeric)
  )

# Add line colours RGB codes
underground_df$mapcolour_rgb <- c("#9A5E34", "#C83C2B", "#F7CF46", "#34783B", "#DD9FAE",
                         "#7E878D", "#791B53", "#000000", "#0F0698", "#48A1DB",
                         "#86CCB3")

# Edit names of lines
unique(underground_features$osm_lines$name)
underground_df$name

lines_regex <- paste0("(?i)", paste(underground_df$name, collapse = "|"))

# underground_features$osm_lines %>%
#   transmute(line_name = str_extract(name, lines_regex))
# underground_features$osm_lines %>%
#   filter(!is.na(name)) %>%
#   transmute(
#     id = row_number(),
#     name,
#     line_name = str_extract_all(name, lines_regex)) %>%
#   unnest(cols = line_name) %>%
#   View()

underground_features_lines <- underground_features$osm_lines %>%
  filter(!is.na(name)) %>%
  mutate(
    row_id = row_number(),
    line_name = str_extract_all(name, lines_regex)) %>%
  unnest(cols = line_name) %>%
  mutate(line_name = str_to_title(line_name)) %>%
  select(row_id, osm_id, name, line_name, geometry) %>%
  inner_join(underground_df, by = join_by(line_name == name))

# Move the District Line slightly North
shift_north_value <- 0.0025
underground_features_lines$geometry[underground_features_lines$line_name == "District"] <-
  underground_features_lines$geometry[underground_features_lines$line_name == "District"] +
  shift_north_value

# Move the Circle Line slightly North
shift_north_value <- 0.0025
underground_features_lines$geometry[underground_features_lines$line_name == "Circle"] <-
  underground_features_lines$geometry[underground_features_lines$line_name == "Circle"] -
  shift_north_value


bg_color <- "grey13"

geom_line_outer_glow <- function(x, df = underground_features_lines) {
  df <- subset(df, line_name == x)
  colour <- head(df$mapcolour_rgb, 1)

  ggfx::with_outer_glow(
    geom_sf(
      data = df,
      color = colour,
      linewidth = 0.5
    ),
    expand = 7, sigma = 6,
    colour = colorspace::lighten(colour, 0.7)
  )
}

# Make the lines glow!
glowing_lines <- map(unique(underground_df$name), geom_line_outer_glow)


ragg::agg_png(here("plots", "02-lines-with-glow.png"), width = 8, height = 6,
              res = 300, units = "in", bg = bg_color)
ggplot() +
  geom_sf(
    data = shp_city,
    fill = "grey20", linewidth = 0
  ) +
  geom_sf(
    data = water_features$osm_polygons,
    fill = bg_color, linewidth = 0
  ) +
  geom_sf(
    data = water_features$osm_multipolygons,
    fill = bg_color, linewidth = 0
  ) +
  # Add the glowing lines in the plot
  glowing_lines +
  # Title inside the map
  annotate(
    "text",
    x = -0.15, y = 51.375,
    label = "LONDON\nUNDERGROUND",
    color = bg_color, size = 9, family = "Roboto Condensed", fontface = "bold",
    hjust = 0, lineheight = 0.65
  ) +
  # Legend
  annotate(
    "richtext",
    x = st_coordinates(st_centroid(shp_city))[1, "X"],
    y = 51.26,
    family = "Roboto Condensed",
    label = paste(sprintf("<span style='color:%s'>%s</span>",
                          unique(underground_df$mapcolour_rgb),
                          unique(underground_df$name)), collapse = " "),
    size = 3.5, color = "white", fill = alpha("white", 0.1),
    hjust = 0.5, label.size = 0, label.padding = unit(2, "mm")
  ) +
  scale_color_identity() +
  coord_sf(clip = "off") +
  theme_void(base_family = "Roboto Condensed") +
  theme(
    plot.background = element_rect(color = bg_color, fill = bg_color),
    panel.margin = margin(rep(5, 4)),
    text = element_text(color = "grey80"),
    plot.title = element_text(
      hjust = 0.5, size = 24, color = "grey86", family = "Roboto Condensed")
  )
dev.off()
