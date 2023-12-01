library(tidyverse)
library(sf)
library(ggtext)
library(here)
library(tidygeocoder)

# Country shape
shp <- giscoR::gisco_get_nuts(country = "Germany", nuts_level = "1",
                              resolution = "10", epsg = "3035")
crs <- st_crs(shp)
st_bbox(shp)

# Source: Wikipedia
teams <- read_tsv(here("data", "bundesliga-vereine-1+2.tsv"))

# Source: Wikipedia / Statistisches Bundesamt
cities <- read_tsv(here("data", "cities-de-population-2022.tsv"))
colnames(cities) <- c("rank", "name", "population")

cities_regex <- paste0("\\b(", paste(unique(cities$name), collapse = "|"), ")")

teams <- teams %>%
  mutate(
    city = str_extract(team, cities_regex),
    # manually add missing matches
    city = case_match(
      team,
      "Eintracht Frankfurt" ~ "Frankfurt am Main",
      "SC Freiburg" ~ "Freiburg im Breisgau",
      "1. FC Heidenheim" ~ "Heidenheim an der Brenz",
      "TSG 1899 Hoffenheim" ~ "Sinsheim",
      "FC Schalke 04" ~ "Gelsenkirchen",
      "Kickers Offenbach" ~ "Offenbach am Main",
      "Stuttgarter Kickers" ~ "Stuttgart",
      "KFC Uerdingen 05" ~ "Krefeld",
      "SG Wattenscheid 09" ~ "Bochum",
      "Wuppertaler SV" ~ "Wuppertal",
      "Hertha BSC" ~ "Berlin",
      "Hamburger SV" ~ "Hamburg",
      "Karlsruher SC" ~ "Karlsruhe",
      "FC St. Pauli" ~ "Hamburg",
      "SV Elversberg" ~ "Spiesen-Elversberg",
      "FC Erzgebirge Aue" ~ "Aue-Bad Schlema",
      "SV Babelsberg 03" ~ "Potsdam",
      "BSV Stahl Brandenburg" ~ "Brandenburg an der Havel",
      "Wacker Burghausen" ~ "Burghausen",
      "VfR Bürstadt" ~ "Bürstadt",
      "SC Charlottenburg" ~ "Berlin",
      "FSV Frankfurt" ~ "Frankfurt am Main",
      "Freiburger FC" ~ "Freiburg im Breisgau",
      "Hallescher FC" ~ "Halle (Saale)",
      "TSV Havelse" ~ "Garbsen",
      "FSV Salmrohr" ~ "Salmtal",
      "SV Sandhausen" ~ "Sandhausen",
      "BSV 07 Schwenningen" ~ "Villingen-Schwenningen",
      "HSV Barmbek-Uhlenhorst" ~ "Hamburg",
      "SpVgg Erkenschwick" ~ "Oer-Erkenschwick",
      "1. FC Mülheim" ~ "Mülheim an der Ruhr",
      "Spandauer SV" ~ "Berlin",
      "DSC Wanne-Eickel" ~ "Herne",
      .default = city
    )
  )

cities_with_teams <- count(teams, city, sort = TRUE)


# Geocode the cities & create sf object
if (FALSE) {
  cities_with_teams_geocoded <- geocode(cities_with_teams, city = city)
  cities_with_teams_geocoded <- cities_with_teams_geocoded %>%
    st_as_sf(coords = c("long", "lat"))
  st_crs(cities_with_teams_geocoded) <- "EPSG:4326"
  cities_with_teams_geocoded <- st_transform(cities_with_teams_geocoded, crs = crs)
  write_rds(cities_with_teams_geocoded, here("output", "cities_with_teams_geocoded.rds"))
} else {
  cities_with_teams_geocoded <- read_rds(here("output", "cities_with_teams_geocoded.rds"))
}


# Create a rectangular grid and calculate the centroids
grid <- st_make_grid(shp, n = 25) %>%
  st_as_sf() %>%
  mutate(id = row_number())
grid_shp <- st_filter(grid, shp)
ggplot(grid_shp) + geom_sf() + geom_sf_text(aes(label = id), size = 1.2)
mean(st_area(grid_shp)) / 1000^2


# Join city locations and grid
cities_within_grid <- st_join(grid_shp, cities_with_teams_geocoded)
cities_within_grid_centroids <- st_centroid(cities_within_grid) %>%
  mutate(n = replace_na(n, 0))

cities_within_grid_centroids %>%
  st_drop_geometry() %>%
  count(id, sort = TRUE, wt = n) %>%
  filter(n > 0) %>%
  nrow()

cities_within_grid_centroids <- cities_within_grid_centroids %>%
  left_join(teams, by = "city") %>%
  group_by(id, city, x) %>%
  summarize(
    n_buli1 = sum(highest_league == 1),
    n_buli2 = sum(highest_league == 2),
    n = n_buli1 + n_buli2,
    .groups = "drop"
  ) %>%
  mutate(
    n = replace_na(n, 0),
    n_buli1 = replace_na(n_buli1, 0))

cities_within_grid_centroids_agg <- cities_within_grid_centroids %>%
  group_by(id, x) %>%
  summarize(
    n = sum(n),
    n_buli1 = sum(n_buli1),
    .groups = "drop")


# Custom ggplot theme
theme_custom <- function() {
  theme_void(base_family = "Outfit") +
  theme(
    plot.background = element_rect(color = "#eef2eb", fill = "#eef2eb"),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.title = element_text(hjust = 0.5),
    legend.justification = "center",
    plot.margin = margin(rep(2, 4)),
    plot.title = element_text(family = "Outfit SemiBold", size = 16, hjust = 0.5),
    plot.subtitle = element_textbox(
      width = 1.1, hjust = 0.5, halign = 0.5, lineheight = 1.05),
    plot.caption = element_textbox(
      width = 1.1, hjust = 0.5, halign = 0.5, lineheight = 1.05)
  )
}
theme_set(theme_custom())


cities_within_grid_centroids_agg %>%
  ggplot() +
  geom_sf(
    aes(size = n, fill = n_buli1),
    shape = 21
  ) +
  scale_size_continuous(range = c(0.3, 6)) +
  scale_fill_gradient(low = "grey90", high = "grey20") +
  guides(
    size = guide_legend(title = "# teams in 1. & 2. BL",
                        title.position = "top", title.hjust = 0.5),
    fill = guide_legend(title = "# teams in 1. BL",
                        title.position = "top", nrow = 1, title.hjust = 0.5,
                        override.aes = list(size = 4))
  ) +
  labs(
    title = "Bundesliga Density",
    subtitle = "Each dot represents the number of football teams in the
    first and second division in Germany (1. & 2. Bundesliga) in that particular
    area. Each cell covers an area of approximately 890 km<sup>2</sup>.",
    caption = "Bundesliga since 1963, 2. Bundesliga since 1974, both the years
    in which the leagues were founded.
    Data: Wikipedia, GISCO. Visualization: Ansgar Wolsing"
  ) +
  theme(
    legend.key.width = unit(4, "mm"),
    legend.spacing.x = unit(2, "mm"),
    legend.box.spacing = unit(1, "mm")
  )
ggsave(here("plots", "30-my-favourite.png"), width = 3.5, height = 5, scale = 1.6)


## The same style with population ----------------------------------------------

# Population density
kontur <- st_read(here("data", "kontur_population_DE_20231101.gpkg"))
st_crs(kontur)
st_bbox(kontur)
kontur <- st_transform(kontur, crs = crs)

# Join population data and grid
kontur_grid <- st_join(grid_shp, kontur)
kontur_grid_centroids <- st_centroid(kontur_grid)

kontur_grid_centroids_agg <- kontur_grid_centroids %>%
  group_by(id, x) %>%
  summarize(population = sum(population), .groups = "drop")


kontur_grid_centroids_agg %>%
  ggplot() +
  geom_sf(
    aes(size = population, fill = population),
    shape = 21
  ) +
  scale_size_continuous(range = c(0.3, 6)) +
  scale_fill_gradient(low = "grey90", high = "grey20") +
  guides(
    size = guide_legend(title = "Total population within grid",
                        title.position = "top", title.hjust = 0.5),
    fill = guide_legend(title = "Total population within grid",
                        title.position = "top", title.hjust = 0.5)
  ) +
  labs(
    title = "Population Density",
    subtitle = "",
    caption = "Data: Kontur, GISCO. Visualization: Ansgar Wolsing"
  )
ggsave(here("plots", "30-my-favourite-population-de.png"), width = 3.5, height = 5, scale = 1.6)


## Putting it all together -----------------------------------------------------

cities_within_grid_centroids_agg %>%
  st_drop_geometry() %>%
  inner_join(kontur_grid_centroids_agg, by = "id") %>%
  mutate(club_pop_ratio = n / population * 1e5) %>%
  st_as_sf(crs = crs)%>%
  ggplot() +
  geom_sf(
    aes(size = club_pop_ratio, fill = club_pop_ratio),
    shape = 21
  ) +
  scale_size_continuous(range = c(0.3, 6)) +
  scale_fill_gradient(low = "grey90", high = "grey20") +
  guides(
    size = guide_legend(title = "Bundesliga clubs per 100,000 inhabitants",
                        title.position = "top", title.hjust = 0.5),
    fill = guide_legend(title = "Bundesliga clubs per 100,000 inhabitants",
                        title.position = "top", title.hjust = 0.5)
  ) +
  labs(
    title = "Bundesliga Population Ratio",
    subtitle = "Number of clubs to have ever played in Bundesliga or 2. Bundesliga
    in each cell per 100,000 inhabitants",
    caption = "Bundesliga since 1963, 2. Bundesliga since 1974, both the years
    in which the leagues were founded.<br>
    Data: Kontur, Wikipedia, GISCO. Visualization: Ansgar Wolsing"
  )
ggsave(here("plots", "30-my-favourite-buli-population-ratio.png"), width = 3.5, height = 5, scale = 1.6)

