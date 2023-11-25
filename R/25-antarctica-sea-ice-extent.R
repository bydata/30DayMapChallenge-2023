library(tidyverse)
library(ggforce)
library(ggtext)
library(here)

#' Source:
#' https://nsidc.org/data/g02135/versions/3#anchor-1

# Download Antarctic sea ice extent
month <- "10"
data_url <- sprintf("https://noaadata.apps.nsidc.org/NOAA/G02135/south/monthly/data/S_%s_extent_v3.0.csv", month)
df <- read_csv(data_url)

# long-term average (1981-2010)
si_extent_avg <- mean(df$extent[df$year %in% 1981:2010])
si_extent_max <- max(df$extent)
si_extent_min <- min(df$extent[df$year < year(Sys.Date())])
si_extent_current <- df$extent[df$year == year(Sys.Date())]

1 - si_extent_current / si_extent_avg
1 - si_extent_current / si_extent_min

p <- ggplot() +
  geom_circle(
    data = df,
    aes(x0 = 0, y0 = 0, r = extent),
    color = "grey80", linewidth = 0.2, alpha = 0.7
  ) +
  geom_circle(
    aes(x0 = 0, y0 = 0, r = si_extent_avg),
    color = "white", linewidth = 1
  ) +
  geom_circle(
    aes(x0 = 0, y0 = 0, r = si_extent_current),
    color = "#22DDDD", linewidth = 1.25
  ) +
  annotate(
    GeomTextBox,
    x = 0, y = -1.5,
    label = "The Antarctic<br><b style='color:#22DDDD'>sea ice extent in Oktober 2023</b><br>
    is the lowest since the beginning of the measurement in 1979.<br><br>
    <span style='font-size:10pt'>
    The current extent is **10.5 %** below the long-term average<br>(1981 to 2010) and
    **5.7 %** below the previous minimum.
    <br><br>
    Each ring represents the monthly sea ice extent
    of a year.</span>",
    box.size = 0, fill = NA, hjust = 0.5, halign = 0.5, color = "white",
    family = "Source Sans Pro", size = 5, width = 0.65, lineheight = 1.1
  ) +
  coord_equal() +
  theme_void() +
  theme(
    plot.background = element_rect(color = "#152346", fill = "#152346")
  )
p

shp <- giscoR::gisco_get_countries(country = "Antarctica", resolution = "60")
shp <- st_transform(shp, crs = "+proj=stere +lat_0=-90") %>%
  st_union()

p_antartica <- ggplot(shp) +
  geom_sf(fill = NA, col = "white") +
  theme_void()
p_antartica


library(patchwork)

p + inset_element(p_antartica, left = 0.4, bottom = 0.62, right = 0.6, top = 0.87) &
  theme(plot.background = element_rect(color = "#152346", fill = "#152346"))
ggsave(here("plots", "25-antarctica-sea-ice-extent.png"), width = 5, height = 5,
       scale = 1.2)
