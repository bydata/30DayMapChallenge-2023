library(tidyverse)
library(rWind)
library(lubridate)
library(ggmap)
library(metR)
library(sf)
library(here)
library(ggtext)

# Map of Europe  ---------------------------------------------------------------

europe <- giscoR::gisco_get_countries(region = "Europe")
europe <- st_filter(
  europe,
  st_as_sfc(st_bbox(c(xmin = -19, ymin = 40, xmax = 9, ymax = 60), crs = "EPSG:4326")))


# Retrieve basemap from Stadia Maps --------------------------------------------

#' To retrieve Stadia Maps tile, you have to sign up for a free account
#' at https://stadiamaps.com/ and create an API key
#' Set the API key as an option: options("stadiamaps-key" = "YOUR_API_KEY")
#' and then access the key with getOption()
register_stadiamaps(getOption("stadiamaps-key"))
stadiamaps_key()
has_stadiamaps_key()

# Retrieve tiles and store them
bbox <- c(left = -20, bottom = 40, right = 9, top = 60)
maptype <- "alidade_smooth"
basemap <- get_stadiamap(bbox = bbox, maptype = maptype,  zoom = 5)
write_rds(
  basemap,
  here("output", sprintf("stadia-stamenmaps-alidade_smooth-%s-%s.rds",
                         paste(bbox, collapse = "-"), maptype)),
  compress = "gz")


# Retrieve wind data -----------------------------------------------------------

# Date-time range to pull the data for
dt <- seq(ymd_hms(paste(2023, 10, 31, 00, 00, 00, sep = "-")),
          ymd_hms(paste(2023, 11,  2, 21, 00, 00, sep = "-")), by = "3 hours")

#' Download the wind data and store it.
#' (TRUE to run the query. Otherwise load the data from your local disk.)
if (FALSE) {
  wind_series <- wind.dl_2(dt, lon1 = -20, lon2 = 15, lat1 = 40, lat2 = 62)
  write_rds(wind_series, file.path("data", "wind-series-20231031-20231102.rds"))
} else {
  wind_series <- read_rds(file.path("data", "wind-series-20231031-20231102.rds"))
}

# Create a dataframe from the list of rWind objects
wind_series_df <- wind_series %>%
  map(as.data.frame) %>%
  bind_rows(.id = "time")

time_with_highest_windspeed <- wind_series_df %>%
  filter(speed == max(speed)) %>%
  pull(time)

wind_peak <- wind_series[[time_with_highest_windspeed]] %>%
  as.data.frame()


# Limits for a consistent color scale // calculate min and max wind speed
speed_lower <- min(wind_peak$speed)
speed_max <- max(wind_peak$speed)


# Map --------------------------------------------------------------------------

# Breaks for the legend
legend_breaks <- seq(speed_lower, speed_max, 6)
legend_breaks_fmt <- vector("character", length(legend_breaks))
legend_breaks_fmt[1] <- as.character(round(legend_breaks[1], 2))
legend_breaks_fmt[2:length(legend_breaks)] <-
  scales::number(legend_breaks[2:length(legend_breaks)], accuracy = 1)

p <- ggplot() +
  geom_sf(
    data = europe,
    color = "grey80", fill = "grey10", linetype = "dotted"
  ) +
  geom_streamline(
    data = wind_peak,
    aes(
      x = lon, y = lat, dx = ugrd10m, dy = vgrd10m,
      col = sqrt(after_stat(dx)^2 + after_stat(dy)^2)
    ),
    L = 2, res = 2, n = 50, arrow = NULL, alpha = 0.7,
    size = 0.42
  ) +
  scale_color_viridis_c(
    option = "C", breaks = legend_breaks, labels = legend_breaks_fmt,
    limits = c(speed_lower, speed_upper)) +
  coord_sf(
    xlim = c(bbox["left"], bbox["right"]), ylim = c(bbox["bottom"], bbox["top"]),
    expand = FALSE) +
  guides(color = guide_legend(
    title = "Avg. wind speed (m/s)", title.position = "top", title.hjust = 0.5,
    label.vjust = 2, label.position = "bottom", nrow = 1,
    override.aes = list(alpha = 1, linewidth = 3, shape = 15)
  )) +
  labs(
    title = "Storm Ciarán",
    subtitle = sprintf("Streamlines for the storm that hit Western Europe
    in October/November 2023. The map shows a snapshot from<br>%s UTC.",
                       time_with_highest_windspeed),
    caption = "**Source:** NOAA/NCEP Global Forecast System (GFS) Atmospheric Model,
    <br>retrieved via {rWind} package; GISCO. **Visualization:** Ansgar Wolsing"
  ) +
  theme_void(base_family = "Lato", base_size = 9) +
  theme(
    plot.background = element_rect(color = "grey4", fill = "grey4"),
    text = element_text(color = "white", lineheight = 1.1),
    plot.title = element_text(face = "bold", hjust = 0.5, size = 24),
    plot.subtitle = element_textbox(
      hjust = 0.5, halign = 0.5, width = 1, margin = margin(t = 4, b = 8)),
    plot.caption = element_textbox(
      hjust = 0.5, halign = 0.5, width = 1, size = 7.5, margin = margin(t = 6, b = 4)),
    legend.position = "bottom",
    legend.key.height = unit(2.5, "mm"),
    legend.key.width = unit(6, "mm")
  )
ggsave(here("plots", "18-atmosphere.png"), width = 4, height = 5, scale = 1.2)



# Munch Map --------------------------------------------------------------------

# Munch Scream palette: https://loading.io/color/feature/EdvardMunch-TheScream-Skrik/
scream_colors <- c("#514134", "#e35839", "#d28d4f", "#dbae1d", "#477187", "#323a3f")
scream_pal <- colorRampPalette(scream_colors)

p +
  scale_color_gradientn(
    colors = scream_pal(20), breaks = legend_breaks, labels = legend_breaks_fmt,
    limits = c(speed_lower, speed_upper))
ggsave(here("plots", "18-atmosphere-munch-scream.png"), width = 4, height = 5, scale = 1.2)



# GIF --------------------------------------------------------------------------

for (i in 49:80) {
  id <- sprintf("%03d", i)
  wind <- wind_series[[i]]
  ggmap(basemap) +
    geom_raster(
      data = wind,
      aes(lon, lat,
          fill = stage(speed, after_scale = alpha(fill, 0.5))),
      interpolate = TRUE) +
    scale_fill_viridis_c(limits = c(speed_lower, speed_upper)) +
    coord_equal(expand = FALSE) +
    guides(color = "none") +
    labs(
      title = names(wind_series)[i]
    ) +
    theme_void(base_family = "Outfit")
  ggsave(
    file.path("plots", "18-atmosphere", paste("wind-", id, ".png", sep = "")),
    width = 1000, height = 600, units = "px", scale = 1.5)
}

library(magick)

img_files <- file.path("plots", "18-atmosphere",
                       list.files(file.path("plots", "18-atmosphere"), pattern = ".png$"))

images <- image_read(img_files)
animated <- image_animate(images)
image_write_gif(animated, file.path("plots", "18-atmosphere-ciaran.gif"))
