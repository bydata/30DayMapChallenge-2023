library(tidyverse)
library(sf)
library(ggtext)
library(terra)
library(here)

bbox <- c(xmin = -12, xmax = 29, ymin = 44, ymax = 64)

# Europe coastlines
coastlines <- giscoR::gisco_get_coastallines(resolution = "10") %>%
  st_intersection(st_as_sfc(st_bbox(bbox, crs = "EPSG:4326")))
ggplot(coastlines) + geom_sf()


#' Download shipping density data from
#' https://datacatalog.worldbank.org/search/dataset/0037580/Global-Shipping-Traffic-Density
data_path <- here("data", "ShipDensity_Fishing", "ShipDensity_Fishing1.tif")
ship_raster <- terra::rast(data_path)
ship_raster

# Extent to crop
# bbox <- c(xmin = -12, xmax = 29, ymin = 44, ymax = 64)
extent <- rast()
ext(extent) <- bbox
ships_cropped <- crop(ship_raster, extent)
ships_cropped_df <- as.data.frame(ships_cropped, xy = TRUE)
colnames(ships_cropped_df) <- c("x", "y", "density")

# Number of cells
ships_cropped_df %>%
  filter(density > 0) %>%
  nrow()
ships_cropped_df %>%
  filter(density > 2) %>%
  nrow()


# Custom palette
custom_pal <- colorRampPalette(c("blue", "green", "yellow"))


p <- ships_cropped_df %>%
  # slice_sample(n = 10e6) %>%
  filter(density > 0) %>%
  ggplot() +
  geom_sf(
    data = coastlines,
    fill = "grey4", col = "grey20", linewidth = 0.1) +
  geom_raster(aes(x, y, fill = density)) +
  annotate(
    GeomTextBox,
    x = 4, y = 49.5,
    label = "Hourly AIS positions received between January 2015 and February 2021,
    indicating total reported positions by passenger ships in 0.005° x 0.005°
    grid cells (approx. 500m x 500m at the Equator).",
    family = "Outfit Light", width = 0.38, hjust = 0, halign = 0, fill = NA,
    box.size = 0, color = "grey84", size = 2.75
  ) +
  scale_fill_gradientn(
    colours = custom_pal(20), trans = "log",
    labels = scales::label_number(accuracy = 1),
    breaks = c(10, 30, 100, 1000, 10000, 100000, 500000)
    ) +
  coord_sf(xlim = c(NA, 27), ylim = c(48, 62)) +
  guides(
    fill = guide_colorbar(
      title = "Shipping Density<br>(log scale)", title.position = "top")
  ) +
  labs(
    title = "Fishing Ship Movement",
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
    legend.position = c(0.92, 0.32),
    legend.direction = "vertical",
    legend.title = element_markdown(size = 7, lineheight = 1, hjust = 0),
    legend.text = element_text(size = 5),
    legend.key.width = unit(2.5, "mm"),
    legend.key.height = unit(7, "mm"),
    plot.margin = margin(t = 2, b = 1, l = 2, r = 2)
  )
ggsave(here("plots", "21-raster-ships-fishing.png"), width = 6, height = 4.5, dpi = 500)
