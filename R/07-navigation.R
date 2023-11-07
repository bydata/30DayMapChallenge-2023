library(tidyverse)
library(sf)
library(here)
library(ggtext)
library(patchwork)

df <- read_csv(here("data", "query-wikidata-stadiums.csv"))

bundesliga_clubs <- c(
  "Borussia Dortmund",
  "FC Bayern Munich",
  "VfB Stuttgart",
  "SC Freiburg",
  "SV Werder Bremen",
  "Bayer 04 Leverkusen",
  "FC Augsburg",
  "1. FC Köln",
  "Borussia Mönchengladbach",
  "TSG 1899 Hoffenheim",
  "RB Leipzig",
  "1. FC Union Berlin",
  "VfL Wolfsburg",
  "1. FSV Mainz 05",
  "VfL Bochum",
  "Eintracht Frankfurt",
  "1. FC Heidenheim",
  "SV Darmstadt 98"
)

bundesliga_clubs_shortname <- c(
  "Dortmund",
  "Munich",
  "Stuttgart",
  "Freiburg",
  "Bremen",
  "Leverkusen",
  "Augsburg",
  "Köln",
  "M'gladbach",
  "Hoffenheim",
  "Leipzig",
  "Berlin",
  "Wolfsburg",
  "Mainz",
  "Bochum",
  "Frankfurt",
  "Heidenheim",
  "Darmstadt"
)
names(bundesliga_clubs_shortname) <- bundesliga_clubs

df_buli <- subset(df, clubLabel %in% bundesliga_clubs) %>%
  group_by(clubLabel) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  st_as_sf(wkt = "coordinates", crs = "EPSG:4326") %>%
  st_transform(crs = "EPSG:4839")

# Load club icons (downloaded manually)
icons_folder <- here("input", "team_icons_2023-24")
icons_files <- list.files(icons_folder, pattern = ".png")
icons_tags <- glue::glue("<img src='{here(icons_folder, icons_files)}' height=15>")
names(icons_tags) <- bundesliga_clubs[order(bundesliga_clubs)]


## GEOMETRIES ==================================================================
## Shapefile Germany
de <- rnaturalearth::ne_countries(scale = 50, country = "Germany", returnclass = "sf")
de <- st_transform(de, crs = "EPSG:4839")


# Calculate the distances (meters) between the stadiums
df_club_only <- df_buli %>%
  st_drop_geometry() %>%
  select(club)

df_distances <- df_club_only %>%
  cross_join(df_buli, suffix = c("", ".y")) %>%
  filter(club != club.y) %>%
  inner_join(df_buli, by = "club") %>%
  select(-c(club.y, venue.x, venue.y)) %>%
  rowwise() %>%
  mutate(
    distance_oneway = as.numeric(st_distance(coordinates.x, coordinates.y)),
    distance_return = 2 * distance_oneway) %>%
  ungroup()
str(df_distances)

# Calculate the distances each club has to travel in a season
df_distances_season <- df_distances %>%
  group_by(clubLabel = clubLabel.x) %>%
  summarize(
    total_distance_oneway = sum(distance_oneway),
    total_distance_return = sum(distance_return)
    )

# Who has to travel the longest distance?
df_distances_season %>%
  arrange(-total_distance_return)

club_longest_dist <- df_distances_season$clubLabel[which.max(df_distances_season$total_distance_return)]
club_shortest_dist <- df_distances_season$clubLabel[which.min(df_distances_season$total_distance_return)]
club_longest_dist
club_shortest_dist


df_plot <- df_distances %>%
  filter(clubLabel.x %in% c(club_longest_dist, club_shortest_dist)) %>%
  inner_join(df_distances_season, by = join_by(clubLabel.x == clubLabel)) %>%
  inner_join(
    data.frame(
      clubLabel = names(icons_tags),
      icon_tag = icons_tags
    ),
    by = join_by(clubLabel.x == clubLabel)
  ) %>%
  mutate(
    facet_label = sprintf(
        paste0(
          ifelse(
            clubLabel.x == club_longest_dist,
            "The longest distance:", "The shortest distance:"),
        "<br><b style='font-size:15pt'>%s</b><br>",
        "<i>(%5.1f km)</i>"
        ),
    clubLabel.x, total_distance_return / 1000),
    coordinates.x_lon = st_coordinates(coordinates.x)[, "X"],
    coordinates.x_lat = st_coordinates(coordinates.x)[, "Y"],
    coordinates.y_lon = st_coordinates(coordinates.y)[, "X"],
    coordinates.y_lat = st_coordinates(coordinates.y)[, "Y"]
         )


bg_color <- "#0A153B"
bar_color <- "#74A4BC"

p1 <- df_plot %>%
  ggplot() +
  # Map of Germany
  geom_sf(
    data = de,
    aes(geometry = geometry),
    fill = colorspace::lighten(bg_color, 0.33)
  ) +
  geom_curve(
    aes(
      x = coordinates.x_lon, xend = coordinates.y_lon,
      y = coordinates.x_lat, yend = coordinates.y_lat),
    curvature = 0.05, linewidth = 0.2, color = "grey96",
    arrow = arrow(angle = 15, type = "closed", length = unit(1.5, "mm"))
  ) +
  # add club icons
  geom_richtext(
    data = ~distinct(., clubLabel.x, icon_tag, coordinates.x_lon, coordinates.x_lat),
    aes(coordinates.x_lon, coordinates.x_lat, label = icon_tag),
    label.size = 0, fill = NA
  ) +
  # Create to facets based on clubLabel, show the facet_label value instead of
  # clubLabel using a custom labeller function
  facet_wrap(vars(clubLabel.x), labeller = as_labeller(
    function(x) {
      unique(df_plot$facet_label[df_plot$clubLabel.x == x])
    }
  )) +
  labs(
    subtitle = "Distances for outward and return journey, measured in a straight
    line (Euclidean distance)"
  ) +
  theme_void(base_family = "Source Sans Pro", base_size = 12) +
  theme(
    plot.background = element_rect(color = bg_color, fill = bg_color),
    text = element_text(color = "grey90"),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_markdown(hjust = 0.5, margin = margin(b = 18)),
    strip.text = element_markdown(family = "Source Sans Pro", lineheight = 1.25,
                                  size = 11.5)
  )


# Chart theme for the bar charts
theme_custom <- function() {
  theme_minimal(base_family = "Source Sans Pro", base_size = 10) +
  theme(
    plot.background = element_rect(color = bg_color, fill = bg_color),
    text = element_text(color = "grey90"),
    plot.title = element_markdown(hjust = 0.5, lineheight = 1, size = 12),
    axis.text.x.top = element_text(color = "grey90"),
    axis.text.y = element_blank(),
    axis.ticks.x.top = element_line(color = "grey90", size = 0.2),
    axis.ticks.length = unit(1.5, "mm"),
    axis.title = element_blank(),
    panel.grid = element_blank()
  )
}


# Distances (return) travelled per club
p2 <- df_distances_season %>%
  mutate(clubLabel = fct_reorder(clubLabel, total_distance_return)) %>%
  ggplot(aes(clubLabel, total_distance_return / 1000)) +
  geom_col(fill = bar_color) + #2EC4B6
  geom_text(
    aes(y = 200, label = clubLabel),
    hjust = 0, vjust = 0.5, family = "Source Sans Pro", size = 3.5,
    color = "white"
  ) +
  scale_y_continuous(position = "right", breaks = seq(0, 20000, 5000),
                     minor_breaks = seq(2500, 20000, 2500)) +
  coord_flip() +
  labs(
    title = "**Total distance by club**<br>(return, in km)"
  ) +
  theme_custom()

# Shortest distances between venues
p3 <- df_distances %>%
  mutate(
    clubLabel_short.x = bundesliga_clubs_shortname[clubLabel.x],
    clubLabel_short.y = bundesliga_clubs_shortname[clubLabel.y],
    direction_label = sprintf("%s - %s",
                                   pmin(clubLabel_short.x, clubLabel_short.y),
                                   pmax(clubLabel_short.x, clubLabel_short.y))) %>%
  distinct(direction_label, distance_oneway) %>%
  slice_min(order_by = distance_oneway, n = 10) %>%
  mutate(direction_label = fct_reorder(direction_label, -distance_oneway)) %>%
  ggplot(aes(direction_label, distance_oneway / 1000)) +
  geom_segment(
    aes(
      xend = direction_label,
      y = 0, yend = distance_oneway / 1000),
    col = bar_color, linewidth = 0.8,
    arrow = arrow(angle = 15, type = "closed", length = unit(1.5, "mm"))) +
  geom_text(
    aes(x = as.numeric(direction_label) + 0.3, y = 0, label = direction_label),
    hjust = 0, vjust = 0.5, family = "Source Sans Pro", size = 3.5,
    color = "white"
  ) +
  scale_y_continuous(position = "right", breaks = seq(0, 100, 10)) +
  coord_flip() +
  labs(
    title = "**Shortest distances**<br>between stadiums (oneway, in km)"
  ) +
  theme_custom()

# Longest distances between venues
p4 <- df_distances %>%
  mutate(
    clubLabel_short.x = bundesliga_clubs_shortname[clubLabel.x],
    clubLabel_short.y = bundesliga_clubs_shortname[clubLabel.y],
    direction_label = sprintf("%s - %s",
                              pmin(clubLabel_short.x, clubLabel_short.y),
                              pmax(clubLabel_short.x, clubLabel_short.y))) %>%
  distinct(direction_label, distance_oneway) %>%
  slice_max(order_by = distance_oneway, n = 10) %>%
  mutate(direction_label = fct_reorder(direction_label, distance_oneway)) %>%
  ggplot(aes(direction_label)) +
  geom_segment(
    aes(
      xend = direction_label,
      y = 0, yend = distance_oneway / 1000),
    col = bar_color, linewidth = 0.8,
    arrow = arrow(angle = 15, type = "closed", length = unit(1.5, "mm"))) +
  geom_text(
    aes(x = as.numeric(direction_label) + 0.3, y = 0, label = direction_label),
    hjust = 0, vjust = 0.5, family = "Source Sans Pro", size = 3.5,
    color = "white"
  ) +
  scale_y_continuous(position = "right", minor_breaks = seq(0, 1000, 10)) +
  coord_flip() +
  labs(
    title = "**Longest distances**<br>between stadiums (oneway, in km)"
  ) +
  theme_custom()
p3

# Combine plots using {patchwork}
p1 + p2 + p3 + p4 +
  plot_layout(design = "
              111
              111
              234
              ") &
  plot_annotation(
    title = "Bundesliga Journeys 2023",
    caption = "Source: DFB, Wikidata. Visualization: Ansgar Wolsing",
    theme = theme(
      text = element_text(color = "white", family = "Source Sans Pro"),
      plot.title = element_text(
        hjust = 0.5, size = 28, family = "Source Sans Pro SemiBold",
        margin = margin(t = 4, b = 6)),
      plot.background = element_rect(color = bg_color, fill = bg_color)
    )
  )

ggsave(here("plots", "07-navigation.png"), width = 8 / 1.5, height = 9 / 1.5,
       scale = 1.2 * 1.5,
       bg = bg_color)
