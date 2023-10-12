library(tidyverse)
library(sf)
library(cartogram)
# devtools::install_github("jimjam-slam/ggflags")
library(ggflags)

asia <- giscoR::gisco_get_countries(
  resolution = "20", region = "Asia", epsg = "3857"
)

glimpse(asia)


#' Population data from {gt} package
#' Source: Worldbank
pop <- gt::countrypops
pop_latest <- pop %>%
  group_by(country_name) %>%
  filter(year == max(year)) %>%
  ungroup()
unique(pop_latest$year)

asia_pop <- asia %>%
  inner_join(pop_latest, by = c("ISO3_CODE" = "country_code_3"))
nrow(asia_pop) == nrow(asia)
st_crs(asia_pop)

# Create polygons for Dorling cartogram
asia_dorling <- cartogram_dorling(asia_pop, weight = "population", k = 15)

p <- asia_dorling %>%
  ggplot() +
  geom_sf()

# Extract layer data to calculate the centroid and the area of the flags
p %>%
  layer_data(1) %>%
  bind_cols(FID = tolower(asia_dorling$FID)) %>%
  mutate(
    area = as.numeric(st_area(geometry)),
    centroid = st_centroid(geometry),
    x = st_coordinates(centroid)[, "X"],
    y = st_coordinates(centroid)[, "Y"]
  ) %>%
  arrange(-area) %>%
  mutate(FID2 = fct_inorder(FID)) %>%
  ggplot(aes(x, y, size = area)) +
  ggflags::geom_flag(
    aes(country = FID)
  ) +
  scale_size_area(max_size = 60) +
  coord_cartesian(clip = "off") +
  guides(size = "none") +
  labs(
    title = "Asia",
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
ggsave(file.path("plots", "06-asia-dorling-pop.png"), width = 8, height = 7)
