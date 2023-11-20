library(tidyverse)
library(sf)
library(ggtext)
library(terra)
library(here)


# Europe coastlines
coastlines <- giscoR::gisco_get_coastallines() %>%
  st_intersection(st_as_sfc(st_bbox(bbox, crs = "EPSG:4326")))
ggplot(coastlines) + geom_sf()


#' Download shipping density data from
#' https://datacatalog.worldbank.org/search/dataset/0037580/Global-Shipping-Traffic-Density
data_path <- here("data", "ShipDensity_Passenger", "ShipDensity_Passenger1.tif")
ship_raster <- terra::rast(data_path)
ship_raster

# Extent to crop
bbox <- c(xmin = -12, xmax = 29, ymin = 44, ymax = 64)
extent <- rast()
ext(extent) <- bbox
ships_cropped <- crop(ship_raster, extent)
ships_cropped_df <- as.data.frame(ships_cropped, xy = TRUE)

# Number of cells
ships_cropped_df %>%
  filter(ShipDensity_Passenger1 > 0) %>%
  nrow()
ships_cropped_df %>%
  filter(ShipDensity_Passenger1 > 2) %>%
  nrow()


# Custom palette
custom_pal <- colorRampPalette(c("blue", "green", "yellow"))


p <- ships_cropped_df %>%
  filter(ShipDensity_Passenger1 > 1) %>%
  ggplot() +
  geom_sf(
    data = coastlines,
    fill = "grey4", col = "grey30") +
  geom_raster(aes(x, y, fill = ShipDensity_Passenger1)) +
  annotate(
    GeomTextBox,
    x = 8, y = 51,
    label = "Hourly AIS positions received between January 2015 and February 2021,
    indicating total reported positions by passenger ships in 0.005° x 0.005°
    grid cells (approx. 500m x 500m at the Equator).",
    family = "Outfit Light", width = 0.47, hjust = 0, halign = 0, fill = NA,
    box.size = 0, color = "grey84", size = 2.75
  ) +
  scale_fill_gradientn(
    colours = custom_pal(20), trans = "log",
    labels = scales::label_number(accuracy = 1),
    breaks = c(30, 100, 1000, 10000)
    ) +
  coord_sf(xlim = c(NA, 27), ylim = c(48, 62)) +
  guides(
    fill = guide_colorbar(
      title = "Shipping Density *(log scale)*", title.position = "top")
  ) +
  labs(
    title = "PASSENGER SHIPS",
    caption = "Source: World Bank. Visualization: Ansgar Wolsing"
  ) +
  theme_void(base_family = "Outfit Light") +
  theme(
    plot.background = element_rect(color = "grey4", fill = "grey4"),
    text = element_text(color = "grey84"),
    plot.title = element_text(
      color = "white", family = "Outfit Medium", size = 18, hjust = 0.5,
      margin = margin(b = 8)),
    plot.caption = element_text(
      size = 6, hjust = 1, margin = margin(t = 8)),
    legend.position = c(0.655, 0.08),
    legend.direction = "horizontal",
    legend.title = element_markdown(size = 8, lineheight = 1),
    legend.text = element_text(size = 6),
    legend.key.width = unit(8, "mm"),
    legend.key.height = unit(2.5, "mm"),
    plot.margin = margin(t = 2, b = 1, l = 2, r = 2)
  )
ggsave(here("plots", "21-raster-ships.png"), width = 6, height = 4.5, dpi = 500)
