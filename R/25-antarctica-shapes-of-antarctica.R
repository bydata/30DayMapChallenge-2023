library(tidyverse)
library(sf)
library(patchwork)
library(elevatr)
library(here)

# Get shape
shp <- giscoR::gisco_get_countries(country = "Antarctica", resolution = "03")
st_bbox(shp)

# Which CRS is it?
st_crs(shp)

# Function to plot the shape
plot_proj <- function(crs = st_crs(shp), name) {
  shp %>%
    st_transform(crs = crs) %>%
    ggplot() +
    geom_sf(fill = NA, color = "#F2F7F2", linewidth = 0.1) +
    coord_sf(default_crs = NULL, lims_method = "geometry_bbox") +
    # labs(title = name) +
    theme_void()
}

# https://proj.org/en/9.3/operations/projections/mbtfpp.html

# WGS84
(p1 <- plot_proj("EPSG:4326", "EPSG:4326"))
# plot_proj("WGS84", "WGS")

# Web Mercator projection
(p2 <- plot_proj("EPSG:3857", "Web Mercator"))

# Mollweide
(p3 <- plot_proj("+proj=moll +x_0=0 +y_0=0 +lat_0=0 +lon_0=0", "Mollweide"))

# Lambert Azimuthal Equal Area
(p4 <- plot_proj("+proj=laea +lon_0=0 +lat_0=0 +ellps=WGS84 +no_defs",
          "Lambert Azimuthal Equal Area"))

# International Terrestrial Reference Frame 2014
(p5 <- plot_proj("EPSG:7789", "International Terrestrial Reference Frame 2014"))

#' Gall stereographic projection
#' https://proj.org/en/9.3/operations/projections/gall.html
(p6 <- plot_proj("+proj=gall", "Gall stereographic"))

#' Winkel I
#' https://proj.org/en/9.3/operations/projections/wink1.html
(p7 <- plot_proj("+proj=wink1", "Winkel I"))

#' Quadrilateralized Spherical Cube, bottom cube side
#' https://proj.org/en/9.3/operations/projections/qsc.html
(p8 <- plot_proj("+proj=qsc +lat_0=-90",
                 "Quadrilateralized Spherical Cube, \nbottom cube side"))

#' Vitkovsky I
#' https://proj.org/en/9.3/operations/projections/vitk1.html
(p9 <- plot_proj("+proj=vitk1 +lat_1=45 +lat_2=55", "Vitkovsky I"))

#' Rectangular Polyconic
#' https://proj.org/en/9.3/operations/projections/rpoly.html
(p10 <- plot_proj("+proj=rpoly", "Rectangular Polyconic"))

#' van der Grinten (I)
#' https://proj.org/en/9.3/operations/projections/vandg.html
(p11 <- plot_proj("+proj=vandg", "van der Grinten"))

#' Stereographic
#' https://proj.org/en/9.3/operations/projections/stere.html
(p12 <- plot_proj("+proj=stere +lat_0=-90", "Stereographic"))

#' Azimuthal Equidistant
#' https://proj.org/en/9.3/operations/projections/aeqd.html
(p13 <- plot_proj("+proj=aeqd +lat_0=-90", "Azimuthal Equidistant"))

#' Adams World in a Square I
#' https://proj.org/en/9.3/operations/projections/adams_ws1.html
(p14 <- plot_proj("+proj=adams_ws1"))

#'
#'
# plot_proj("")

layout <- "
112233
445566
778899
"

# p1 + p2 + p3 + p4 + p5 + p6 + p7 + p8 + p9 + p10 +
#   plot_layout(design = layout) &
#   theme(
#     plot.background = element_rect(color = "#443850", fill = "#443850")
#   )
#

## Combine plots using magick ==================================================

plot_width <- 1
plot_height <- 1
plot_path <- here("plots", "25-antarctica-plots")

ggsave(here(plot_path, "p1.png"), plot = p1, width = plot_width, height = plot_height)
ggsave(here(plot_path, "p2.png"), plot = p2, width = plot_width, height = plot_height)
ggsave(here(plot_path, "p3.png"), plot = p3, width = plot_width, height = plot_height)
ggsave(here(plot_path, "p4.png"), plot = p4, width = plot_width, height = plot_height)
ggsave(here(plot_path, "p5.png"), plot = p5, width = plot_width, height = plot_height)
ggsave(here(plot_path, "p6.png"), plot = p6, width = plot_width, height = plot_height)
ggsave(here(plot_path, "p7.png"), plot = p7, width = plot_width, height = plot_height)
ggsave(here(plot_path, "p8.png"), plot = p8, width = plot_width, height = plot_height)
ggsave(here(plot_path, "p9.png"), plot = p9, width = plot_width, height = plot_height)
ggsave(here(plot_path, "p10.png"), plot = p10, width = plot_width, height = plot_height)
ggsave(here(plot_path, "p11.png"), plot = p11, width = plot_width, height = plot_height)
ggsave(here(plot_path, "p12.png"), plot = p12, width = plot_width, height = plot_height)
ggsave(here(plot_path, "p13.png"), plot = p13, width = plot_width, height = plot_height)
ggsave(here(plot_path, "p14.png"), plot = p14, width = plot_width, height = plot_height)

