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
shp <- giscoR::gisco_get_nuts(
  country = "Germany", resolution = "10", nuts_level = 1)

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


## Bad place density per federal state =========================================

# Total population in Bad places per federal state (admin 1 code)
bad_places %>%
  st_drop_geometry() %>%
  count(admin1_code, wt = population)

# Federal states codes and population
#' Source: Destatis
#' https://www.destatis.de/DE/Themen/Gesellschaft-Umwelt/Bevoelkerung/Bevoelkerungsstand/Tabellen/bevoelkerung-nichtdeutsch-laender.html
#' Codes from https://download.geonames.org/export/dump/admin1CodesASCII.txt
federal_states <- tribble(
  ~admin1_code, ~federal_state, ~population,
  "10", "Schleswig-Holstein", 2953270,
  "04", "Hamburg", 1892122,
  "06", "Niedersachsen", 8140242,
  "03", "Bremen", 684864,
  "07", "Nordrhein-Westfalen", 18139116,
  "05", "Hessen", 6391360,
  "08", "Rheinland-Pfalz", 4159150,
  "01", "Baden-Württemberg", 11280257,
  "02", "Bayern", 13369393,
  "09", "Saarland", 992666,
  "16", "Berlin", 3755251,
  "12", "Brandenburg", 2573135,
  "12", "Mecklenburg-Vorpommern", 1628378,
  "13", "Sachsen", 4086152,
  "14", "Sachsen-Anhalt", 2186643,
  "15", "Thüringen", 2126846
)

shp_badplaces_pop_share <- bad_places %>%
  st_drop_geometry() %>%
  count(admin1_code, wt = population, .drop = FALSE, name = "badplaces_population") %>%
  inner_join(federal_states, by = "admin1_code")%>%
  mutate(federal_state = toupper(federal_state)) %>%
  right_join(shp, by = join_by(federal_state == NUTS_NAME)) %>%
  mutate(badplaces_pop_share = badplaces_population / population,
         badplaces_pop_share = replace_na(badplaces_pop_share, 0)) %>%
  st_as_sf()
head(shp_badplaces_pop_share)


ggplot() +
  geom_sf(
    data = shp_badplaces_pop_share,
    aes(fill = badplaces_pop_share)
  ) +
  geom_sf(
    data = bad_places,
    col = "white", fill = "#0B3954", stroke = 0.4, size = 2.5, shape = 24
  ) +
  colorspace::scale_fill_continuous_sequential(
    "Sunset", labels = scales::label_percent()) +
  guides(fill = guide_colorbar(
    title = "Share of inhabitants living in places named \"Bad\"",
    title.position = "top")) +
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
    plot.margin = margin(rep(4, 4)),
    legend.position = "bottom",
    legend.key.height = unit(2, "mm"),
    legend.key.width = unit(12, "mm"),
    legend.title.align = 0.5,
    legend.title = element_text(size = 8, hjust = 0.5),
    legend.text = element_text(size = 7)
  )
ggsave(here("plots", "04-bad-with-pop-share.png"), width = 4.8, height = 5.8)


bad_places %>%
  filter(admin1_code == "02") %>%
  st_filter(st_as_sfc(st_bbox(c(xmin = 12.5, ymin = 48, xmax = 14, ymax = 48.8),
                      crs = st_crs(bad_places)))) %>%
  mutate(
    coords = st_coordinates(geometry),
    x = coords[, "X"],
    y = coords[, "Y"]) %>%
  ggplot() +
  geom_sf() +
  ggrepel::geom_text_repel(
    aes(x, y, label = name),
    min.segment.length = 0) +
  coord_sf(xlim = c(12.2, 14), ylim = c(48.2, 48.8))

