library(tidyverse)
library(sf)
library(patchwork)
library(here)


shape <- giscoR::gisco_get_countries(country = "Antarctica")

# Which CRS is it?
st_crs(shape)

# Function to plot the shape
plot_proj <- function(crs = st_crs(shape), name) {
  shape %>%
    st_transform(crs = crs) %>%
    ggplot() +
    geom_sf(color = NA, fill = "#F2F7F2") +
    labs(title = name) +
    theme_void(base_family = "Outfit") +
    theme(
      plot.background = element_rect(color = "#443850", fill = "#443850"),
      panel.background = element_rect(color = NA, fill = NA),
      plot.title = element_text(color = "#F2F7F2", hjust = 0.5)
    )
}

# https://proj.org/en/9.3/operations/projections/mbtfpp.html

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

#'
#'
plot_proj("")

#'
#'
plot_proj("")

#'
#'
plot_proj("")

layout <- "
112233
445566
778899
"

p1 + p2 + p3 + p4 + p5 + p6 + p7 + p8 + p9 +
  plot_layout(design = layout) &
  theme(
    plot.background = element_rect(color = "#443850", fill = "#443850")
  )

plot_width <- 2.5
plot_height <- 2
plot_path <- here("plots", "25-antarctica-plots")

ggsave(here(plot_path, "p1.png"), plot = p1, width = plot_width, height = plot_height)
ggsave(here(plot_path, "p2.png"), plot = p2, width = plot_width, height = plot_height)
ggsave(here(plot_path, "p3.png"), plot = p3, width = plot_width, height = plot_height)
ggsave(here(plot_path, "p4.png"), plot = p4, width = plot_width, height = plot_height)

library(magick)

img1 <- image_read(here(plot_path, "p1.png"))
img2 <- image_read(here(plot_path, "p2.png"))
img3 <- image_read(here(plot_path, "p3.png"))
img4 <- image_read(here(plot_path, "p4.png"))

img1_2 <- image_append(
  c(img1, img2), stack = FALSE)
img3_4 <- image_append(
  c(img3, img4), stack = FALSE)
combined <- image_append(c(img1_2, img3_4), stack = TRUE)
image_write(combined, here(plot_path, "combined.png"))
