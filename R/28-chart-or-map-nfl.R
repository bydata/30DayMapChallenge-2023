library(tidyverse)
library(sf)
library(here)
library(ggtext)
# devtools::install_github("martinjhnhadley/statesRcontiguous")
library(statesRcontiguous)


schedule_mat <- read_tsv(here("data", "nfl-schedule-2023.tsv"))
team_abbr <- read_tsv(here("data", "nfl-team-abbreviations.tsv"))
#' Stadium locations
#' Source: https://gist.github.com/rajinwonderland/80b3ac9c7dc75337594fb5e711e461a7?short_path=cd04ea5
stadiums <- st_read("https://gist.githubusercontent.com/rajinwonderland/80b3ac9c7dc75337594fb5e711e461a7/raw/6cf57ddbe377b652345a5a44bdf0c420693ef32d/stadiums.geojson")

# Correct incorrect franchise names
stadiums <- stadiums %>%
  rename(stadium_name = name1) %>%
  mutate(team = case_match(
    team,
    "Chicago Bers" ~ "Chicago Bears",
    "Washington Redskins" ~ "Washington Commanders",
    .default = team
))

# Reduced set of variables
colnames(stadiums)
stadiums_reduced <- stadiums %>%
  select(city, state, team, stadium_name, geometry)

crs <- st_crs(stadiums_reduced)
stadiums_updated <- stadiums_reduced %>%
  filter(!team %in% c("Oakland Raiders", "St. Louis Rams", "San Diego Chargers")) %>%
  # split the combined record for Giants and Jets
  separate_rows(team, sep = "/ ") %>%
  mutate(team = ifelse(team == "NewYork Jets", "New York Jets", team)) %>%
  add_row(
    city = "Paradise", state = "NV", team = "Las Vegas Raiders",
    stadium_name = "Allegiant Stadium",
    geometry = st_sfc(st_point(c(-115.183889, 36.090556)), crs = crs)
  ) %>%
  add_row(
    city = "Inglewood", state = "CA", team = "Los Angeles Rams",
    stadium_name = "SoFi Stadium",
    geometry = st_sfc(st_point(c(-118.339, 33.953)), crs = crs)
  ) %>%
  add_row(
    city = "Inglewood", state = "CA", team = "Los Angeles Chargers",
    stadium_name = "SoFi Stadium",
    geometry = st_sfc(st_point(c(-118.339, 33.953)), crs = crs)
  )

# European stadiums
stadiums_europe <- tibble(
  city = c("London", "London", "Frankfurt"),
  # state = NA,
  # team = NA,
  stadium_name = c("Tottenham Hotspur Stadium", "Wembley Stadium", "Waldstadion"),
  geometry = c(
    st_sfc(st_point(c(-0.066417, 51.60475)), crs = crs),
    st_sfc(st_point(c(-0.279444, 51.555556)), crs = crs),
    st_sfc(st_point(c(8.645415, 50.068548)), crs = crs)
  )
) %>%
  st_as_sf()


# Schedule

schedule_long <- schedule_mat %>%
  pivot_longer(cols = -TEAM, names_to = "week", values_to = "opponent") %>%
  mutate(
    week = as.integer(week),
    home_away = ifelse(str_detect(opponent, "^@"), "away", "home"),
    opponent = str_remove(opponent, "^@")) %>%
  rename(team = TEAM) %>%
  # remove bye weeks (opponent=BYE)
  filter(opponent != "BYE") %>%
  inner_join(team_abbr, by = join_by(team == code_common)) %>%
  rename(team_code = team, team_name = franchise) %>%
  inner_join(team_abbr, by = join_by(opponent == code_common)) %>%
  rename(opponent_code = opponent, opponent_name = franchise) %>%
  select(team_code, team_name, week, opponent_code, opponent_name, home_away)

schedule_long %>%
  mutate(home_team = ifelse(home_away == "home", team_name, opponent_name)) %>%
  inner_join(stadiums_updated, by = join_by(home_team == team)) %>% View()



# Super Bowl LVII Champion Kansas City Chiefs spielt am Sonntag, den 5.11.2023, gegen die Miami Dolphins. 15:30 Uhr.
# Die New England Patriots spielen am Sonntag, den 12.11.2023, gegen die Indianapolis Colts. 15:30 Uhr.
#
# The 2023 NFL International Series gets underway on Sunday, Oct. 1, with the Falcons and Jaguars
# meeting in Wembley Stadium.
# The remaining two games will be played at Tottenham Hotspur Stadium,
# and will first feature the Bills and Jaguars and then the Ravens and Titans,
# which will close the London games on Oct. 15.

europe_games <- tribble(
  ~team_code, ~team_name, ~week, ~opponent_code, ~opponent_name, ~home_away, ~stadium_name,
  "JAX", "Jacksonville Jaguars", 4, "ATL", "Atlanta Falcons", "home", "Wembley Stadium",
  "ATL", "Atlanta Falcons", 4, "JAX", "Jacksonville Jaguars", "away", "Wembley Stadium",
  "BUF", "Buffalo Bills", 5, "JAX", "Jacksonville Jaguars", "home", "Tottenham Hotspur Stadium",
  "JAX", "Jacksonville Jaguars", 5, "BUF", "Buffalo Bills", "away", "Tottenham Hotspur Stadium",
  "TEN", "Tennessee Titans", 6, "BAL", "Baltimore Ravens", "home", "Tottenham Hotspur Stadium",
  "BAL", "Baltimore Ravens", 6, "TEN", "Tennessee Titans", "away", "Tottenham Hotspur Stadium",
  "KC", "Kansas City Chiefs", 9, "MIA", "Miami Dolphins", "home", "Waldstadion",
  "MIA", "Miami Dolphins", 9, "KC", "Kansas City Chiefs", "away", "Waldstadion",
  "NE", "New England Patriots", 10, "IND", "Indianapolis Colts", "home", "Waldstadion",
  "IND", "Indianapolis Colts", 10, "NE", "New England Patriots", "away", "Waldstadion",
)

europe_games <- europe_games %>%
  inner_join(stadiums_europe, by = "stadium_name") %>%
  st_as_sf()

st_crs(stadiums_updated)
st_crs(europe_games)
st_crs(stadiums_updated)

schedule_locations <- schedule_long %>%
  # determine which team is the home team to add the stadium
  mutate(home_team = ifelse(home_away == "home", team_name, opponent_name)) %>%
  inner_join(stadiums_updated, by = join_by(home_team == team)) %>%
  # identify games played in Europe
  left_join(europe_games,
            by = join_by(team_code, team_name, week, opponent_code, opponent_name, home_away)) %>%
  mutate(
    stadium_name = ifelse(!is.na(stadium_name.y), stadium_name.y, stadium_name.x),
    geometry = ifelse(!is.na(stadium_name.y), geometry.y, geometry.x),
    city = ifelse(!is.na(stadium_name.y), city.y, city.x),
    home_away_europe = ifelse(!is.na(stadium_name.y), "Europe", home_away),
    home_away_europe = factor(home_away_europe, levels = c("home", "away", "Europe"))
  ) %>%
  select(-c(stadium_name.x, stadium_name.y, geometry.x, geometry.y,
            city.x, city.y)) %>%
  # add the away teams' location (stadium) to determine travel distance
  inner_join(select(stadiums_updated, team, geometry),
             by = join_by(team_name == team),
             suffix = c(".destination", ".start"))  %>%
  st_as_sf()

# Calculate the distance between the stadiums
matchday_distances <- schedule_locations %>%
  filter(home_away_europe %in% c("away", "Europe")) %>%
  # mutate(geometry.start = ifelse(
  #   team_code == "JAX" & week == 5, geometry.destination, geometry.start)) %>%
  rowwise() %>%
  mutate(travelled_distance = st_distance(
    geometry.start, st_sfc(geometry.destination, crs = crs))) %>%
  ungroup() %>%
  # Jacksonville played in London twice and didn't travel back to the US inbetween
  mutate(travelled_distance = ifelse(
    team_code == "JAX" & week == 5, 0, as.numeric(travelled_distance)),
    travelled_distance_km = travelled_distance / 1000,
    travelled_distance_km_return = 2 * travelled_distance_km)





shp_contiguous_states <- shp_all_us_states %>%
  filter(contiguous.united.states)
st_crs(shp_contiguous_states) <- st_crs(stadiums)


stadiums %>%
  ggplot() +
  geom_sf(
    data = shp_contiguous_states,
    linetype = "dotted"
  ) +
  geom_sf() +
  theme_void()

franchise_total_distances <- matchday_distances %>%
  st_drop_geometry() %>%
  # count(team_name, wt = travelled_distance_km_return, name = "total_distance", sort = TRUE) %>%
  group_by(team_name) %>%
  summarize(
    total_distance = sum(travelled_distance_km_return),
    had_europe_match = sum(home_away_europe == "Europe") > 0
  ) %>%
  inner_join(select(stadiums_updated, team_name = team, geometry), by = join_by(team_name)) %>%
  st_as_sf() %>%
  st_transform(crs = "EPSG:3857")


# Create polygons for Dorling cartogram
dorling <- cartogram::cartogram_dorling(franchise_total_distances,
                                        weight = "total_distance", k = 8)

p_base <- ggplot() +
  geom_sf(
    data = dorling
  ) +
  theme_void()

layer_data(p_base, 1) %>%
  bind_cols(team_name = dorling$team_name) %>%
  inner_join(st_drop_geometry(franchise_total_distances), by = "team_name") %>%
  mutate(
    area = as.numeric(st_area(geometry)),
    centroid = st_centroid(geometry),
    x = st_coordinates(centroid)[, "X"],
    y = st_coordinates(centroid)[, "Y"],
    logo_label = str_replace_all(team_name, " ", "_"),
    logo_label = sprintf("<img src='input/nfl-logos/%s.svg.png' width=%f>",
                         logo_label, round(area / 23e9))
  ) %>%
  arrange(-area) %>%
  mutate(team = fct_inorder(team_name)) %>%
  ggplot(aes(x, y, size = area)) +
  geom_point(
    aes(stroke = ifelse(had_europe_match, 1.5, 0.2)),
    shape = 21, color = "grey12", fill = "#77777744" #, stroke = 0.2
  ) +
  geom_richtext(
    aes(x, y, size = area, label = logo_label),
    fill = NA, label.size = 0
  ) +
  scale_size_area(max_size = 30) +
  coord_cartesian(clip = "off") +
  guides(size = "none") +
  labs(
    title = "NFL Travel Schedule",
    subtitle = "The size of the circles shows the total distance travelled.
    Franchises which played in Europe are highlighted with thicker line width",
    caption = "Source: . Visualization: Ansgar Wolsing"
  ) +
  theme_void(base_family = "Source Sans Pro") +
  theme(
    plot.background = element_rect(color = "grey92", fill = "grey92"),
    text = element_text(color = "grey10"),
    plot.title = element_text(
      hjust = 0, size = 24, family = "Source Sans Pro", face = "bold",
      color = "black"),
    plot.subtitle = element_textbox(
      hjust = 0, halign = 0, width = 0.95, lineheight = 1.2,
      margin = margin(t = 4, b = 12)),
    plot.caption = element_markdown(hjust = 0, margin = margin(t = 12)),
    plot.margin = margin(t = 4, b = 4, l = 30, r = 30)
  )
ggsave(here("plots", "28-chart-or-map-nfl.png"), width = 5.5, height = 4.5, scale = 1.4)


# p_barchart <- dorling %>%
#   st_drop_geometry() %>%
#   mutate(
#     logo_label = str_replace_all(team_name, " ", "_"),
#     logo_label = sprintf("<img src='input/nfl-logos/%s.svg.png' width=%f>",
#                          logo_label, 12),
#     logo_label = fct_reorder(logo_label, total_distance),
#     team_name = fct_reorder(team_name, total_distance)
#   ) %>%
#   ggplot(aes(logo_label, total_distance)) +
#   geom_col() +
#   scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
#   coord_flip() +
#   theme_minimal() +
#   theme(
#     axis.text.y.left = element_markdown()
#   )
# ggsave(here("plots", "28-chart-or-map-nfl-barchart.png"), width = 5, height = 4, scale = 1.5)


p_lollipop <- dorling %>%
  st_drop_geometry() %>%
  mutate(
    logo_label = str_replace_all(team_name, " ", "_"),
    logo_label = sprintf("<img src='input/nfl-logos/%s.svg.png' width=%f>",
                         logo_label, 18),
    logo_label = fct_reorder(logo_label, total_distance),
    team_name = fct_reorder(team_name, total_distance)
  ) %>%
  ggplot(aes(team_name, total_distance)) +
  geom_segment(
    aes(xend = team_name, y = 0, yend = total_distance)
  ) +
  geom_label(
    aes(y = 0, label = team_name),
    hjust = 0, vjust = 0, fill = alpha("grey92", 0.6), label.size = 0,
    family = "Source Sans Pro", size = 2.75
  ) +
  geom_point(
    size = 5, color = "grey92"
  ) +
  geom_richtext(
    aes(label = logo_label),
    fill = NA, label.size = 0
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.1)), position = "right",
    labels = function(x) ifelse(x == max(x), paste(x, "km"), as.character(x))) +
  coord_flip() +
  labs(
    title = "",
    subtitle = "Total distance travelled (km)",
    y = NULL
  ) +
  theme_minimal(base_family = "Source Sans Pro") +
  theme(
    plot.background = element_rect(color = "grey92", fill = "grey92"),
    axis.title.y = element_blank(),
    axis.text.y.left = element_blank(),
    axis.ticks.x.top = element_line(linewidth = 0.2)
  )
ggsave(here("plots", "28-chart-or-map-nfl-lollipop.png"), width = 4, height = 5, scale = 1.5)
