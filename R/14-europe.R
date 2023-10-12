library(tidyverse)
library(sf)
library(cartogram)
# devtools::install_github("jimjam-slam/ggflags")
library(ggflags)

europe <- giscoR::gisco_get_countries(
  resolution = "20", region = "Europe", epsg = "3857"
)

glimpse(europe)

europe %>%
  ggplot() +
  geom_sf()


#' Population data from {gt} package
#' Source: Worldbank
pop <- gt::countrypops
pop_latest <- pop %>%
  group_by(country_name) %>%
  filter(year == max(year)) %>%
  ungroup()
unique(pop_latest$year)

europe_pop <- europe %>%
  inner_join(pop_latest, by = c("ISO3_CODE" = "country_code_3"))
nrow(europe_pop) == nrow(europe)
st_crs(europe_pop)

europe_dorling <- cartogram_dorling(europe_pop, weight = "population", k = 5)

p <- europe_dorling %>%
  # st_transform(crs = "EPSG:4326") %>%
  ggplot() +
  geom_sf(fill = "steelblue", col = "white") +
  geom_sf_text(
    aes(label = FID, size = population),
    col = "white", family = "Source Sans Pro SemiBold"
  ) +
  theme_void() +
  theme(
    plot.background = element_rect(color = "grey98", fill = "grey98")
  )

layer_data(p, 1) %>%
  bind_cols(FID = tolower(europe_dorling$FID)) %>%
  mutate(
    area = as.numeric(st_area(geometry)),
    centroid = st_centroid(geometry),
    x = st_coordinates(centroid)[, "X"],
    y = st_coordinates(centroid)[, "Y"]
  ) %>%
  arrange(-area) %>%
  # Change country codes to work with geom_flag
  mutate(
    FID = case_match(
      FID,
      "uk" ~ "gb",
      "el" ~ "gr",
       .default = FID
    )
  ) %>% # View()
  # slice_head(n = 12) %>%
  # move Russia to the West
  mutate(x = ifelse(FID == "ru", x - 6e6, x)) %>%
  ggplot(aes(x, y, size = area)) +
  ggflags::geom_flag(
    aes(country = FID)
  ) +
  # geom_text(aes(label = FID), size = 3) +
  scale_size_area(max_size = 50) +
  coord_cartesian(clip = "off") +
  guides(size = "none") +
  labs(
    title = "Europe",
    subtitle = "The size of the flags indicates the population (2021)",
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
    plot.margin = margin(t = 4, b = 4, l = 40, r = 40)
  )
ggsave(file.path("plots", "14-europe-dorling-pop.png"), width = 9, height = 7)
