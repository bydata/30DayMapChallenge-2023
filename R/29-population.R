library(tidyverse)
library(sf)
library(cartogram)
# devtools::install_github("jimjam-slam/ggflags")
library(ggflags)

world <- giscoR::gisco_get_countries(
  resolution = "20", epsg = "4326"
)
world <- filter(world, NAME_ENGL != "Antarctica")

# Add Taiwan
taiwan <- rnaturalearth::ne_countries(
  country = "Taiwan", scale = 50, returnclass = "sf")
world <- world %>%
  select(FID, ISO3_CODE, NAME_ENGL, geometry) %>%
  bind_rows(select(taiwan, FID = iso_a2, ISO3_CODE = iso_a3, NAME_ENGL = name, geometry))

# France
france <- world %>%
  filter(FID == "FR") %>%
  st_crop(
    st_as_sfc(st_bbox(c(xmin = -10, ymin = 20, xmax = 10, ymax = 50), crs = "EPSG:4326")))
ggplot(france) + geom_sf()
world <- world %>%
  filter(FID != "FR") %>%
  add_row(france)


# Transform to Mollweide projection
target_crs <- "+proj=moll"
world <- st_transform(world, crs = target_crs)

glimpse(world)

world %>%
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
pop_latest <- pop_latest %>%
  add_row(country_name = "Taiwan", country_code_2 = "TW",
          country_code_3 = "TWN", year = 2021, population = 23894394)

world_pop <- world %>%
  inner_join(pop_latest, by = c("ISO3_CODE" = "country_code_3"))
nrow(world_pop)
nrow(world)
st_crs(world_pop)

world_dorling <- world_pop %>%
  cartogram_dorling(weight = "population", k = 4)

p_pre <- world_dorling %>%
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


df_plot <- layer_data(p_pre, 1) %>%
  bind_cols(FID = tolower(world_dorling$FID)) %>%
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
  ) %>%
  # Russia
  mutate(
    x = ifelse(FID == "ru", x - 2.0e6, x),
    y = ifelse(FID == "ru", y - 0.75e6, y)
  ) %>%
  # Brazil
  mutate(
    x = ifelse(FID == "br", x - 0.65e6, x),
    y = ifelse(FID == "br", y + 0.95e6, y)
  ) %>%
  # Bolivia
  mutate(
    y = ifelse(FID == "bo", y + 0.5e6, y)
  ) %>%
  # Paraguay
  mutate(
    x = ifelse(FID == "py", x - 0.3e6, x),
    y = ifelse(FID == "py", y + 0.8e6, y)
  ) %>%
  # Argentina
  mutate(
    x = ifelse(FID == "ar", x + 0.1e6, x),
    y = ifelse(FID == "ar", y + 1.4e6, y)
  ) %>%
  # Uruguay
  mutate(
    x = ifelse(FID == "uy", x + 0.1e6, x),
    y = ifelse(FID == "uy", y + 0.7e6, y)
  ) %>%
  # Chile
  mutate(
    x = ifelse(FID == "cl", x + 0.5e6, x),
    y = ifelse(FID == "cl", y + 0.8e6, y)
  ) %>%
  # USA
  mutate(
    y = ifelse(FID == "us", y - 0.5e6, y)
  )  %>%
  # Canada
  mutate(
    x = ifelse(FID == "ca", x - 1.5e6, x),
    y = ifelse(FID == "ca", y - 0.5e6, y)
  ) %>%
  # India
  mutate(
    y = ifelse(FID == "in", y + 0.6e6, y)
  ) %>%
  # China
  mutate(
    y = ifelse(FID == "cn", y - 0.5e6, y)
  ) %>%
  # Algeria
  mutate(
    y = ifelse(FID == "dz", y - 0.5e6, y)
  )  %>%
  # Morocco
  mutate(
    y = ifelse(FID == "ma", y - 0.7e6, y)
  ) %>%
  # Tunisia
  mutate(
    x = ifelse(FID == "tn", x + 0.2e6, x),
    y = ifelse(FID == "tn", y - 0.8e6, y)
  ) %>%
  # Egypt
  mutate(
    x = ifelse(FID == "eg", x - 0.2e6, x),
    y = ifelse(FID == "eg", y - 0.3e6, y)
  ) %>%
  # Libya
  mutate(
    x = ifelse(FID == "ly", x - 0.2e6, x),
    # y = ifelse(FID == "ly", y - 0.8e6, y)
  ) %>%
  # Niger
  mutate(
    x = ifelse(FID == "ne", x - 0.2e6, x),
    y = ifelse(FID == "ne", y - 0.25e6, y)
  ) %>%
  # New Zealand
  mutate(
    y = ifelse(FID == "nz", y + 0.3e6, y)
  ) %>%
  # switch Slovakia and Czech Republic
  mutate(
    x = case_match(
      FID,
      "sk" ~ x[FID == "cz"],
      "cz" ~ x[FID == "sk"],
      .default = x
      )
  )

p <- df_plot %>%
  ggplot(aes(x, y, size = area)) +
  ggflags::geom_flag(
    aes(country = FID)
  ) +
  scale_size_area(max_size = 38) +
  coord_cartesian(xlim = c(-11e6, 15.5e6), ylim = c(-5.5e6, 7.2e6), clip = "off") +
  guides(size = "none") +
  labs(
    title = "World Population",
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
    plot.margin = margin(t = 4, b = 4, l = 10, r = 10)
  )
ggsave(file.path("plots", "29-population-dorling.png"), width = 6, height = 5,
       scale = 1.5, dpi = 600)

st_bbox(world)
