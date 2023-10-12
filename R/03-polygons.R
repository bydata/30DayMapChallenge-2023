# pacman::p_load("tidyverse", "here", "glue", "ggtext", "colorspace",
#                "sf", "osmdata", "geojsonsf", "jsonlite")
library(tidyverse)
library(here)
library(ggtext)
library(sf)
library(osmdata)
library(jsonlite)

## GEOMETRIES ==================================================================

## Area of Cologne
coords_cgn <- getbb("Cologne, Germany", format_out = "sf_polygon")
coords_cathedral <- getbb("Kölner Dom, Cologne, Germany",
                          featuretype = "church")

## GET BUILDINGS DATA ====================================================================
#' Source: https://www.offenedaten-koeln.de/dataset/adresse
#' Source: https://www.offenedaten-koeln.de/dataset/geb%C3%A4udemodell-stadt-k%C3%B6ln-2010
#'    Projection: 31466 - DHDN / Gauss-Kruger zone 2. VG

urls_buildings <- paste0("https://www.offenedaten-koeln.de/sites/default/files/dachansicht_lod2_part",
                         1:3, ".zip")
filepaths_buildings_zip <- here("input", "cologne_buildings",
                                paste0("dachansicht_lod2_part", 1:3, ".zip"))
folder_buildings <- here("input", "cologne_buildings")
filepath_buildings_dataframe <- here(folder_buildings, "cologne_buildings.rds")

if (!file.exists(filepath_buildings_dataframe)) {
  if (!file.exists(filepaths_buildings_zip[1])) {
    dir.create(folder_buildings)
    walk2(urls_buildings, filepaths_buildings_zip,
          ~download.file(url = .x, destfile = .y, mode = "wb"))
    walk(filepaths_buildings_zip, unzip, exdir =  folder_buildings)
  }
  filepaths_shp <- here(folder_buildings,
                        list.files(folder_buildings, pattern = ".*\\.shp$"))
  buildings <- map(filepaths_shp, st_read)

  # Save buildings dataframe with geometry
  write_rds(buildings, filepath_buildings_dataframe,
            compress = "gz")
} else {
  buildings <- read_rds(filepath_buildings_dataframe)
}

# Align the CRSs in the separate files
unique(map(buildings, st_crs))
buildings <- map_dfr(buildings, function(x) {
  if (is.na(st_crs(x))) {
    st_crs(x) <- "EPSG:31466"
  }
  x
})

#' Set the coordinate reference system
#' According to comments/documentation: 31466 - DHDN / Gauss-Kruger zone 2. VG
st_crs(buildings$geometry) <- "EPSG:31466"
st_crs(buildings$geometry)

buildings2 <- st_zm(buildings, drop = TRUE, what = "ZM")


## Download HAUSNUMMERN  =======================================================

# Directory to store the data (will be created if it doesn't exist)
housenumber_data_dir <- here("input", "cologne-hausnummern")
if (!dir.exists(housenumber_data_dir)) dir.create(housenumber_data_dir)

# Download the data for a single batch of 1000 records at at time
download_housenumber_data <- function(offset, data_dir, file_prefix = "hausnummern_") {
  url <- sprintf("https://geoportal.stadt-koeln.de/arcgis/rest/services/Statistische_Daten/OSM_Hausnummern/MapServer/0/query?where=objectid+is+not+null&text=&objectIds=&time=&geometry=&geometryType=esriGeometryEnvelope&inSR=&spatialRel=esriSpatialRelIntersects&relationParam=&outFields=*&returnGeometry=true&returnTrueCurves=false&maxAllowableOffset=&geometryPrecision=&outSR=4326&returnIdsOnly=false&returnCountOnly=false&orderByFields=OBJECTID&groupByFieldsForStatistics=&outStatistics=&returnZ=false&returnM=false&gdbVersion=&returnDistinctValues=false&resultOffset=%d&resultRecordCount=&f=geojson", offset)
  dest_file <- file.path(housenumber_data_dir, sprintf("%s%d.json", file_prefix, offset))
  download.file(url, destfile = dest_file)
  print(offset)
  Sys.sleep(1)
}

# Download all records
offsets <- seq(0, 2e5, 1000)
walk(offsets, download_housenumber_data, data_dir = housenumber_data_dir)


# Inspiration: https://twitter.com/dr_xeo/status/1392794246009311236

# Read in the data
read_housenumber_data <- function(offset, data_dir, file_prefix = "hausnummern_") {
  filepath <- file.path(data_dir, paste0(file_prefix, offset, ".json"))
  df <- fromJSON(filepath)$features$properties
  df
}

# Load housenumber data // ignore corrupt file idx 101
hausnummern <- map_dfr(offsets[c(1:100, 102:162)], read_housenumber_data, data_dir = housenumber_data_dir)

# merge buildings and hausnummern

buildings_hausnr <- buildings2 %>%
  mutate(HAUSNR = as.character(HAUSNR)) %>%
  left_join(hausnummern, by = c("STRID" = "nr_strasse", "HAUSNR" = "hausnr"))

# Add cathedral build year (Object ID R02EVZW)
buildings_hausnr <- buildings_hausnr %>%
  mutate(baujahr_alt = ifelse(OBJID == "R02EVZW", 1880, baujahr_alt))



# non-matching rows in buildings dataframe
buildings2 %>%
  mutate(HAUSNR = as.character(HAUSNR)) %>%
  anti_join(hausnummern, by = c("STRID" = "nr_strasse", "HAUSNR" = "hausnr")) %>%
  nrow()

# non-matching rows in house number dataframe
hausnummern %>%
  filter(!is.na(hausnr)) %>%
  mutate(hausnr = as.numeric(hausnr)) %>%
  anti_join(buildings2, by = c("nr_strasse" = "STRID", "hausnr" = "HAUSNR")) %>%
  nrow()

st_crs(buildings_hausnr)
colnames(buildings_hausnr)
glimpse(buildings_hausnr)


buildings_hausnr %>%
  st_drop_geometry() %>%
  count(baujahr_alt, sort = TRUE) %>%
  filter(!is.na(baujahr_alt)) %>%
  ggplot(aes(baujahr_alt, n)) +
  geom_col() +
  geom_vline(xintercept = 1945, color = "red")

## Filter circle around the Cologne cathedral
cathedral_point <- st_as_sfc("POINT(6.9560927 50.9412784)", crs = "EPSG:4326") %>%
  st_as_sf() %>%
  st_transform(crs = "EPSG:31466")
cathedral_point_buffered <- cathedral_point %>%
  st_buffer(1800)
buildings_hausnr_around_cathedral <- buildings_hausnr %>%
  st_filter(cathedral_point_buffered)

# color_palette <- paletteer::paletteer_d("colorBlindness::Blue2DarkRed12Steps")
# color_palette2 <- color_palette[c(2, 3, 4, 6, 7, 8, 9, 10, 12)]
# color_palette2 <- c(
#   "#7AE7C7", "#5a56bc", "#7678edff","#b79877ff",
#   "#d7a83c", "#f7b801", "#f4a001", "#f18701", "#f35b04")
color_palette2 <- c(
  "#FFFFFF", "#FF0000", "#0000FF", "#008000", "#FFD700", "#FF00FF",
  "#00FFFF", "#FFA500", "#800080")


p <- buildings_hausnr_around_cathedral %>%
  mutate(baujahr_alt_grp = cut(
    baujahr_alt, breaks = c(0, 1600, 1800, 1900, 1945, 1950, 1960, 1980, 2000, Inf),
    labels = c("1600 or earlier", "1601-1800", "1801-1900", "1901-1945", "1946-1950",
               "1951-1960", "1961-1980", "1981-2000", "after 2000")),
    baujahr_alt_grp = fct_na_value_to_level(baujahr_alt_grp, "Unknown/no data")) %>%
  ggplot() +
  geom_sf(
    aes(geometry = geometry),
    fill = "grey28") +
  geom_sf(
    data = ~mutate(., baujahr_alt_grp2 = baujahr_alt_grp) %>%
      filter(baujahr_alt_grp2 != "Unknown/no data"),
    # aes(geometry = geometry, fill = baujahr_alt_grp2),
    aes(geometry = geometry),
    fill = "#FFA500",
    color = NA
  ) +
  scale_fill_manual(values = c(unclass(color_palette2), "grey60")) +
  coord_sf() +
  facet_wrap(vars(baujahr_alt_grp2)) +
  guides(fill = guide_legend(title.position = "top", direction = "horizontal",
                             byrow = FALSE)) +
  labs(
    title = "Buildings of Cologne",
    subtitle = "Build year for the buildings in Cologne, Germany
    that exists today (2016),<br>area within 1,800 meters from the Cologne Cathedral",
    caption = "Source: Offene Daten Köln. Visualization: Ansgar Wolsing",
    fill = "Year of Construction"
  ) +
  theme_void(base_family = "Source Sans Pro") +
  theme(
    plot.background = element_rect(color = "grey20", fill = "grey20"),
    plot.margin = margin(rep(10, 4)),
    text = element_text(color = "grey80"),
    plot.title = element_text(
      size = 36, hjust = 0.5, family = "Playfair Display",
      face = "italic", color = "white"),
    plot.subtitle = element_markdown(
      lineheight = 1.3, hjust = 0.5, margin = margin(t = 10, b = 20)),
    plot.caption = element_markdown(hjust = 0.5),
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 10)
  )
ggsave(here("plots", "03-polygons.png"), dpi = 600, width = 9, height = 10)


# p <- buildings_hausnr_around_cathedral %>%
#   mutate(baujahr_alt_grp = case_when(
#     baujahr_alt <= 1945 ~ "<= 1945",
#     baujahr_alt >  1945 ~ ">1945",
#     is.na(baujahr_alt) ~ "Unknown/no data")) %>%
#   ggplot() +
#   geom_sf(
#     aes(geometry = geometry),
#     fill = "grey36") +
#   geom_sf(
#     data = ~mutate(., baujahr_alt_grp2 = baujahr_alt_grp) %>%
#       filter(baujahr_alt_grp2 != "Unknown/no data"),
#     aes(geometry = geometry, fill = baujahr_alt_grp2),
#     color = NA
#   ) +
#   scale_fill_manual(values = c("#eb925b", "#5BC0EB")) +
#   coord_sf() +
#   facet_wrap(vars(baujahr_alt_grp2)) +
#   guides(fill = guide_legend(title.position = "top", direction = "horizontal", byrow = FALSE)) +
#   labs(
#     title = "COLOGNE",
#     caption = "Source: Offene Daten Köln, OpenStreetmap contributors.
#     Visualisation: Ansgar Wolsing (Inspiration by @dr_xeo)",
#     fill = "Year of Construction"
#   ) +
#   theme_void(base_family = "Cabinet Grotesk") +
#   theme(
#     plot.background = element_rect(color = "grey20", fill = "grey20"),
#     plot.margin = margin(rep(10, 4)),
#     text = element_text(color = "grey99"),
#     plot.title = element_text(size = 36, face = "bold", hjust = 0.5, color = "white"),
#     plot.caption = element_markdown(hjust = 0.5),
#     legend.position = "bottom"
#   )
# ggsave(here("plots", "buildings01-circle-1945-split.png"), dpi = 200, width = 10, height = 7.5)
