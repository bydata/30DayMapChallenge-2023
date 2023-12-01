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


places_bw <- places %>%
  mutate(
    # black_white = str_extract(name, "(?i)(schwarz|wei(?:ß|ss))"),
    # black_white = str_replace(black_white, "ss", "ß"),
    black_white = str_extract(name, "(?i)(black|white)"),
    black_white = str_to_title(black_white)
    ) %>%
  # filter(population > 0) %>%
  filter(!is.na(black_white)) %>%
  st_as_sf(coords = c("longitude", "latitude"))
st_crs(places_bw) <- "EPSG:4326"


shp_contiguous_states <- statesRcontiguous::shp_all_us_states %>%
  filter(contiguous.united.states)
st_crs(shp_contiguous_states) <- "EPSG:4326"

places_bw <- st_filter(places_bw, shp_contiguous_states)


table(places_bw$black_white)



ggplot() +
  geom_sf(
    data = mutate(shp_contiguous_states, black_white = "Black"),
    fill = "white", col = "black", linewidth = 0.2, linetype = "dotted") +
  geom_sf(
    data = mutate(shp_contiguous_states, black_white = "White"),
    fill = "black", col = "white",  linewidth = 0.2, linetype = "dotted") +
  geom_sf(
    data = places_bw,
    aes(fill = black_white, col = black_white),
    shape = 21, size = 1, stroke = 0.1
  ) +
  scale_color_manual(values = c("Black" = "white", "White" = "black")) +
  scale_fill_manual(values = c("Black" = "black", "White" = "white")) +
  facet_wrap(vars(black_white), nrow = 2,
             #labeller = as_labeller(function(x) sprintf("Places called *%s*", x))
             ) +
  guides(color = "none", fill = "none") +
  labs(
    title = "Black and white places in the Contiguous U.S.",
    subtitle = "Populated places whose names contain \"black\" or \"white\"",
    caption = "Data: GeoNames. Visualization: Ansgar Wolsing"
  ) +
  theme_void(base_family = "Outfit") +
  theme(
    plot.background = element_rect(color = "white", fill = "white"),
    plot.title = element_text(family = "Outfit SemiBold", hjust = 0.5, size = 14),
    plot.caption = element_text(size = 7, hjust = 0.5),
    strip.text = element_text(size = 12, margin = margin(t = 4)),
    plot.margin = margin(rep(2, 4))
  )
ggsave(here("plots", "24-black-white-usa-placenames.png"),
       width = 4, height = 5, scale = 1.2)



## Germany ---------------------------------------------------------------------

places_bw <- places %>%
  mutate(
    black_white = str_extract(name, "(?i)(schwarz|wei(?:ß|ss))"),
    black_white = str_replace(black_white, "ss", "ß"),
    black_white = str_to_title(black_white)
  ) %>%
  # filter(population > 0) %>%
  filter(!is.na(black_white)) %>%
  st_as_sf(coords = c("longitude", "latitude"))
st_crs(places_bw) <- "EPSG:4326"


# load country shape
shp <- giscoR::gisco_get_nuts(
  country = "Germany", resolution = "10", nuts_level = 1)

places_bw <- st_filter(places_bw, shp)


table(places_bw$black_white)


ggplot() +
  geom_sf(
    data = mutate(shp, black_white = "Schwarz"),
    fill = "white", col = "black", linewidth = 0.2, linetype = "dotted") +
  geom_sf(
    data = mutate(shp, black_white = "Weiß"),
    fill = "black", col = "white",  linewidth = 0.2, linetype = "dotted") +
  geom_sf(
    data = places_bw,
    aes(fill = black_white, col = black_white),
    shape = 21, size = 1.5, stroke = 0.2
  ) +
  scale_color_manual(values = c("Schwarz" = "white", "Weiß" = "black")) +
  scale_fill_manual(values = c("Schwarz" = "black", "Weiß" = "white")) +
  facet_wrap(vars(black_white)) +
  guides(color = "none", fill = "none") +
  labs(
    title = "Black and white places in Germany",
    subtitle = "Populated places whose names contain<br>\"Schwarz\" (\"black\") or
    \"Weiß/Weiss\" (\"white\")",
    caption = "Data: GeoNames. Visualization: Ansgar Wolsing"
  ) +
  theme_void(base_family = "Outfit") +
  theme(
    plot.background = element_rect(color = "white", fill = "white"),
    plot.title = element_text(family = "Outfit SemiBold", hjust = 0.5, size = 15),
    plot.subtitle = element_markdown(
      hjust = 0.5, lineheight = 1.1, size = 11, margin = margin(t = 4, b = 8)),
    plot.caption = element_markdown(size = 7, hjust = 0.5),
    strip.text = element_text(size = 12, margin = margin(t = 4)),
    plot.margin = margin(rep(2, 4))
  )
ggsave(here("plots", "24-black-white-germany-placenames.png"),
       width = 5, height = 4.5)


kleve <- st_sfc(st_point(c(6.09, 51.79)), crs = "EPSG:4326")

distances <- places_bw %>%
  st_distance(kleve)

places_bw %>%
  bind_cols(distance = distances) %>%
  arrange(distance) %>% View()
