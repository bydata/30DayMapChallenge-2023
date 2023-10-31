library(tidyverse)
library(ggtext)
library(osmdata)
library(sf)
library(here)

country <- "Germany"

# where to store the data
data_dir <- here("data")

#' Downloads and unzips country datasets from GeoNames.org
#' http://download.geonames.org/export/dump/
download_and_unzip_geonames <- function(country,
                                        data_dir) {
  # get country code from English country name
  country_code <- countrycode::countrycode(
    country, origin = "country.name", destination = "iso2c")
  geonames_url <- glue::glue("http://download.geonames.org/export/dump/{country_code}.zip")
  geonames_localfile_zip <- here::here(data_dir, glue::glue("geonames_{tolower(country_code)}.zip"))

  if (!dir.exists(data_dir)) {
    dir.create(data_dir)
  }

  download.file(geonames_url, destfile = geonames_localfile_zip)
  geonames_localfile <- unzip(geonames_localfile_zip, list = TRUE) %>%
    filter(Name != "readme.txt")
  unzip(geonames_localfile_zip, exdir = data_dir)

  c("filename" = geonames_localfile$Name[1])
}

filename <- download_and_unzip_geonames(country, data_dir = data_dir)
filename

places <- read_tsv(here::here(data_dir, filename),
                   col_names = c(
                     "geonameid",
                     "name",
                     "asciiname",
                     "alternatenames",
                     "latitude",
                     "longitude",
                     "feature_class",
                     "feature_code",
                     "country_code",
                     "cc2",
                     "admin1_code",
                     "admin2_code",
                     "admin3_code",
                     "admin4_code",
                     "population",
                     "elevation",
                     "dem",
                     "timezone",
                     "modification_date"
                   ))

# Select settlements, city, village etc., see http://www.geonames.org/export/codes.html
places <- places %>%
  filter(feature_class == "P") %>%
  arrange(name)

# Select places named "Bad ..."
bad_places <- places %>%
  filter(str_detect(name, "^Bad\\s"))

# load country shape
shp <- giscoR::gisco_get_countries(country = "Germany", resolution = "10")

bad_places <- st_as_sf(bad_places, coords = c("longitude", "latitude"))
st_crs(bad_places) <- st_crs(shp)


ggplot() +
  geom_sf(
    data = shp,
    fill = "grey95"
  ) +
  geom_sf(
    data = bad_places,
    col = "white", fill = "#0B3954", stroke = 0.4, size = 2.5, shape = 24
  ) +
  labs(
    title = "Bad Places",
    subtitle = "\"Bad\" is a frequent component of place names in the German-speaking world,
    indicating the presence of a spa, especially a health spa.",
    caption = "Data: GeoNames. Visualization: Ansgar Wolsing"
  ) +
  theme_void(base_family = "Source Sans Pro") +
  theme(
    plot.background = element_rect(color = "white", fill = "#FFFBF9"),
    text = element_text(lineheight = 1),
    plot.title = element_text(hjust = 0.5, family = "Playfair Display", size = 20),
    plot.subtitle = element_textbox(hjust = 0.5, halign = 0.5, width = 1.2),
    plot.caption = element_text(hjust = 0.5),
    plot.margin = margin(rep(4, 4))
  )
ggsave(here("plots", "04-bad.png"), width = 4.8, height = 5.8)
