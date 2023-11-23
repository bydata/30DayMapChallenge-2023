#' Code adapted from https://github.com/Pecners/kontur_rayshader_tutorial/blob/main/main.R

library(sf)
library(tidyverse)
library(stars)
library(rayshader)
library(here)

# Transform population data for height map -------------------------------------

target_crs <- "EPSG:25834"

# KONTUR Datasets (2023-11-01)
#' Canada: https://data.humdata.org/dataset/kontur-population-canada
kontur <- st_read(here("data", "kontur_population_NO_20231101.gpkg"))
st_crs(kontur)

# Exclude Svalbard & Jan Mayen
bb <- st_bbox(kontur)
bb["xmin"] <- 2e5
bb["ymax"] <- 12e6

kontur <- kontur %>%
  st_filter(st_as_sfc(st_bbox(bb, crs = st_crs(kontur)))) %>%
  st_transform(crs = target_crs)

# define aspect ratio based on bounding box
bb <- st_bbox(kontur)
bottom_left <- st_point(c(bb[["xmin"]], bb[["ymin"]]))  %>%
  st_sfc(crs = st_crs(kontur))
bottom_right <- st_point(c(bb[["xmax"]], bb[["ymin"]]))  %>%
  st_sfc(crs = st_crs(kontur))
top_left <- st_point(c(bb[["xmin"]], bb[["ymax"]]))  %>%
  st_sfc(crs = st_crs(kontur))
width <- st_distance(bottom_left, bottom_right)
height <- st_distance(bottom_left, top_left)

# handle conditions of width or height being the longer side
if (width > height) {
  w_ratio <- 1
  h_ratio <- as.numeric(height / width)
} else {
  h_ratio <- 1
  w_ratio <- as.numeric(width / height)
}

# convert to raster so we can then convert to matrix
size <- 5000
rast <- st_rasterize(kontur, nx = floor(size * w_ratio), ny = floor(size * h_ratio))
mat <- matrix(rast$population,
              nrow = floor(size * w_ratio),
              ncol = floor(size * h_ratio))


# Render the spike map ---------------------------------------------------------

# Custom texture on flag colours
white <- "#FFFFFF"
darkblue <- "#00205B"
red <- "#EF3340"
lightred <- lighten(leftcolor, 0.75)

custom_texture_pal <- colorRampPalette(colors = c(darkblue, white, rep(red, 3)))
swatchplot(custom_texture_pal(128))
texture <- custom_texture_pal(128)


rgl::clear3d()

mat  %>%
  height_shade(texture = texture)  %>%
  plot_3d(heightmap = mat,
          windowsize = c(600, 600),
          zoom = 0.5,
          theta = -19.5,
          phi = 22.75,
          zscale = 11,
          background = "white",
          solid = FALSE,
          shadowdepth = 0,
          shadowcolor = colorspace::darken(shadowcolor, 0.7),
          shadow_darkness = 0.75)
render_camera()
# theta       phi      zoom       fov
# -12.73399  19.43781   0.50000   0.00000
# theta       phi      zoom       fov
# -19.48477  22.75316   0.50000   0.00000

outfile <- here("plots", "23-3d-norway-pop-hi-res-10.png")

{
  start_time <- Sys.time()
  cat(crayon::cyan(start_time), "\n")
  if (!file.exists(outfile)) {
    png::writePNG(matrix(1), target = outfile)
  }
  render_highquality(
    filename = outfile,
    interactive = FALSE,
    lightdirection = 240,
    lightaltitude = c(20, 80),
    lightcolor = c(lightred, "white"),
    lightintensity = c(500, 100),
    samples = 450,
    width = 6000,
    height = 6000,
    parallel = TRUE
  )
  end_time <- Sys.time()
  diff <- end_time - start_time
  cat(crayon::cyan(diff), "\n")
}

# rgl::close3d()
