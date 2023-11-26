library(tidyverse)
library(sf)
library(ggtext)
library(here)
library(tidygeocoder)

# Source: Wikipedia
teams <- read_tsv(here("data", "bundesliga-vereine-1+2.tsv"))
# Source: Wikipedia / Statistisches Bundesamt
cities <- read_tsv(here("data", "cities-de-population-2022.tsv"))
colnames(cities) <- c("rank", "name", "population")

# Shape Germany
shp <- giscoR::gisco_get_nuts(country = "Germany", nuts_level = "1",
                              resolution = "10", epsg = "3035")
crs <- st_crs(shp)
st_bbox(shp)

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

# Check if there are any teams without a matching city
# (note that it may be okay in some cases, e.g. Aue)
teams %>%
  anti_join(cities, by = join_by(city == name))

# Which cities never had a football club in 1. or 2. Bundesliga
no_buli_cities <- cities %>%
  anti_join(teams, by = join_by(name == city)) %>%
  arrange(-population) %>%
  slice_max(order_by = population, n = 18) %>%
  mutate(rank_league = rank(-population))

# Geocode the cities & create sf object
no_buli_cities_geocoded <- geocode(no_buli_cities, city = name)
no_buli_cities_geocoded <- no_buli_cities_geocoded %>%
  st_as_sf(coords = c("long", "lat"))
st_crs(no_buli_cities_geocoded) <- "EPSG:4326"
no_buli_cities_geocoded <- st_transform(no_buli_cities_geocoded, crs = crs)
write_rds(no_buli_cities_geocoded, here("output", "no_buli_cities_geocoded.rds"))

table_bottom_y <- 2.60e6
table_line_offset <- 2.6e4

no_buli_cities_geocoded %>%
  arrange(-rank) %>%
  ggplot() +
  geom_sf(
    data = shp,
    linetype = "solid", linewidth = 0.2, fill = "white"
  ) +
  geom_sf(
    aes(fill = rank_league == 1),
    size = 5.5,
    shape = 21, col = "white" #, fill = "grey8"
  ) +
  geom_sf_text(
    aes(label = rank_league),
    color = "white", family = "Chivo", size = 2.75
  ) +
  # Legend with cities
  geom_point(
    aes(
      x = 4.66e6,
      y = table_bottom_y + (18 - rank_league) * table_line_offset,
      color = rank_league == 1
    ),
    size = 4
  ) +
  geom_text(
    aes(
      x = 4.66e6,
      y = table_bottom_y + (18 - rank_league) * table_line_offset,
      label = rank_league
    ),
    family = "Chivo", size = 2, color = "white", vjust = 0.55
  ) +
  geom_richtext(
    aes(
      x = 4.67e6,
      y = table_bottom_y + (18 - rank_league) * table_line_offset,
      label = paste(name)
    ),
    family = "Source Sans Pro", size = 3.25, hjust = 0, vjust = 0.55,
    fill = NA, label.size = 0
  ) +
  geom_richtext(
    aes(
      x = 4.97e6,
      y = table_bottom_y + (18 - rank_league) * table_line_offset,
      label = scales::number(population, big.mark = ".", decimal.mark = ",")
    ),
    family = "Source Sans Pro", size = 3.25, hjust = 1, vjust = 0.5,
    fill = NA, label.size = 0
  ) +
  scale_fill_manual(values = c("grey40", "grey8"), aesthetics = c("color", "fill")) +
  coord_sf(xlim = c(4031952, 4671975 + 3e5)) +
  guides(fill = "none", color = "none") +
  labs(
    title = "Least successful German cities in association football",
    subtitle = "Largest cities by population in Germany that never had a football club
    <br>in Bundesliga (since 1963) and 2nd Bundesliga (since 1974)",
    caption = "Source: Wikipedia, Statistisches Bundesamt (2022), DFB.
    Visualization: Ansgar Wolsing"
  ) +
  theme_void(base_family = "Source Sans Pro", base_size = 12) +
  theme(
    plot.background = element_rect(color = "white", fill = "white"),
    plot.title = element_markdown(
      family = "Source Sans Pro Semibold", size = 18, hjust = 0),
    plot.subtitle = element_textbox(width = 0.85, lineheight = 1.15),
    plot.caption = element_markdown(hjust = 1),
    plot.margin = margin(rep(6, 4))
  )
ggsave(here("plots", "26-minimal.png"), width = 4, height = 4.5, scale = 1.8)
