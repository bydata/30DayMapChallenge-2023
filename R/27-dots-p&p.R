library(tidyverse)
library(ggtext)
library(spocc)
library(sf)

#' Source: https://www.gbif.org/

# Penguins
penguins <- occ(query = "Spheniscidae", from = "gbif",
          date = c("2022-01-01", "2022-12-31"),
          limit = 30000,
          has_coords = TRUE)
penguins_df <- occ2df(penguins) %>%
  st_as_sf(coords = c("longitude", "latitude"))
st_crs(penguins_df) <- 4326
write_rds(penguins_df, here("output", "penguins_df.rds"))


# Polar bears
polarbears <- occ(query = "Ursus maritimus", from = "gbif",
          date = c("2022-01-01", "2022-12-31"),
          limit = 2000,
          has_coords = TRUE)
polarbears_df <- occ2df(polarbears) %>%
  st_as_sf(coords = c("longitude", "latitude"))
st_crs(polarbears_df) <- 4326
write_rds(polarbears_df, here("output", "polarbears_df.rds"))

world <- giscoR::gisco_get_coastallines(epsg = "4326")

#' Projection: Azimuthal Equidistant
#' https://proj.org/en/9.3/operations/projections/aeqd.html
penguins_df <- st_transform(penguins_df, crs = "+proj=aeqd")
polarbears_df <- st_transform(polarbears_df, crs = "+proj=aeqd")
world <- st_transform(world, crs = "+proj=aeqd")

ggplot() +
  geom_sf(
    data = world,
    linewidth = 0, fill = "#7B7E95"
  ) +
  geom_sf(
    data = penguins_df,
    size = 0.15, alpha = 0.5, color = "#F1DABF"
  ) +
  geom_sf(
    data = polarbears_df,
    size = 0.15, alpha = 0.5, color = "#00CED1"
  ) +
  labs(
    title = "<span style='color:#F1DABF'>Penguins</span> &
    <span style='color:#00CED1'>Polarbears</span>",
    subtitle = "Locations of observations in 2022",
    caption = "Source: Global Biodiversity Information Facility (GBIF).
    Visualization: Ansgar Wolsing"
  ) +
  theme_void(base_family = "Outfit") +
  theme(
    plot.background = element_rect(color = "#001040", fill = "#001040"),
    text = element_text(color = "grey90"),
    plot.title = element_markdown(hjust = 0.5, size = 18, face = "bold"),
    plot.subtitle = element_markdown(hjust = 0.5),
    plot.caption = element_markdown(hjust = 0.5, size = 7),
    plot.margin = margin(rep(4, 4))
  )
ggsave(file.path("plots", "27-dots-penguins-polarbears.png"), width = 5, height = 5)
