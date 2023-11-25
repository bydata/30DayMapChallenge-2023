library(tidyverse)
library(sf)
library(patchwork)
library(elevatr)
library(here)

# Get shape
shp <- giscoR::gisco_get_countries(country = "Antarctica", resolution = "03")
st_bbox(shp)
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

# Create the plots for all given projections -----------------------------------
projections <- list(
  c("EPSG:4326", "WGS84"),
  c("EPSG:3857", "Web Mercator"),
  c("+proj=moll +x_0=0 +y_0=0 +lat_0=0 +lon_0=0", "Mollweide"),
  c("+proj=laea +lon_0=0 +lat_0=0 +ellps=WGS84 +no_defs", "Lambert Azimuthal Equal Area"),
  c("EPSG:7789", "International Terrestrial Reference Frame 2014"),
  c("+proj=gall", "Gall stereographic"),
  c("+proj=wink1", "Winkel I"),
  c("+proj=qsc +lat_0=-90", "Quadrilateralized Spherical Cube, \nbottom cube side"),
  c("+proj=vitk1 +lat_1=45 +lat_2=55", "Vitkovsky I"),
  c("+proj=rpoly", "Rectangular Polyconic"),
  c("+proj=vandg", "van der Grinten"),
  c("+proj=stere +lat_0=-90", "Stereographic"),
  c("+proj=aeqd +lat_0=-90", "Azimuthal Equidistant"),
  c("+proj=adams_ws1", "Adams World in a Square I")
)


plots <- map(projections, ~ plot_proj(.x[1], .x[2]))


## Save plots ==================================================

plot_width <- 1
plot_height <- 1
plot_path <- here("plots", "25-antarctica-plots")

walk2(plots, seq_along(plots), function(plot, i) {
  filename <- file.path(plot_path, paste0("p", i, ".png"))
  ggsave(filename, plot, width = plot_width, height = plot_height, units = "in",
         dpi = 300)
})
