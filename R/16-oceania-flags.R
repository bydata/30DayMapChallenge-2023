library(tidyverse)
library(sf)
library(cartogram)
# devtools::install_github("jimjam-slam/ggflags")
library(ggflags)

oceania <- giscoR::gisco_get_countries(
  resolution = "20", region = "Oceania", epsg = "3857"
)
oceania <- st_transform(oceania, crs = "+proj=moll")
glimpse(oceania)


#' Population data from {gt} package
#' Source: Worldbank
pop <- gt::countrypops
pop_latest <- pop %>%
  group_by(country_name) %>%
  filter(year == max(year)) %>%
  ungroup()
unique(pop_latest$year)

oceania_pop <- oceania %>%
  inner_join(pop_latest, by = c("ISO3_CODE" = "country_code_3"))

# 10 countries do not match --> acceptable
nrow(oceania_pop) == nrow(oceania)
unique(oceania$NAME_ENGL)
unique(oceania_pop$NAME_ENGL)
setdiff(unique(oceania$NAME_ENGL), unique(oceania_pop$NAME_ENGL))

st_crs(oceania_pop)

oceania_dorling <- cartogram_dorling(oceania_pop, weight = "population", k = 5)

p <- oceania_dorling %>%
  ggplot() +
  geom_sf()

layer_data(p, 1) %>%
  bind_cols(FID = tolower(oceania_dorling$FID)) %>%
  mutate(
    area = as.numeric(st_area(geometry)),
    centroid = st_centroid(geometry),
    x = st_coordinates(centroid)[, "X"],
    # Shift longitude for countries at 180 °W
    x = ifelse(x < 0, x + min(-xmin) + max(xmax), x),
    y = st_coordinates(centroid)[, "Y"]
  ) %>%
  st_drop_geometry() %>%
  arrange(-area) %>%
  # Change country codes to work with geom_flag
  mutate(
    FID = case_match(
      FID,
      "uk" ~ "gb",
      "el" ~ "gr",
       .default = FID
    )
  ) %>%
  mutate(x = ifelse(FID == "ru", x - 6e6, x)) %>%
  ggplot(aes(x, y, size = area)) +
  ggflags::geom_flag(
    aes(country = FID)
  ) +
  # geom_label(aes(label = FID), size = 3) +
  scale_size_area(max_size = 50) +
  coord_cartesian(clip = "off") +
  guides(size = "none") +
  labs(
    title = "Oceania",
    subtitle = "The size of the flags indicates the population of the countries (2021)",
    caption = "Source: GISCO, Worldbank. Visualization: Ansgar Wolsing"
  ) +
  theme_void(base_family = "Source Sans Pro Light") +
  theme(
    plot.background = element_rect(color = "grey12", fill = "grey12"),
    text = element_text(color = "grey90"),
    plot.title = element_text(
      hjust = 0.5, size = 36, family = "Playfair Display", face = "italic",
      color = "grey99"),
    plot.subtitle = element_text(hjust = 0.5, margin = margin(t = 12, b = 40)),
    plot.margin = margin(t = 4, b = 20, l = 60, r = 20)
  )
ggsave(file.path("plots", "16-oceania-dorling-pop.png"), width = 9, height = 7)
