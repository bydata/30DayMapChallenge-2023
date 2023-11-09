library(tidyverse)
library(ggtext)
library(sf)
library(here)

#' Source: https://regionalatlas.statistikportal.de/
#' Download data from Regionatlas and
#' place the file in data/regionalatlas-flaechennutzung
##' Path: Themen >> Verdienste und Einkünfte >> Gesamtbetrag der Einkünfte je Steuerpflichtigen
##' Levels:
##' Gemeinden (for the other federal states)
# Zeichenerklärung:
# 2222222222: nichts vorhanden, genau 0
# 5555555555: Zahlenwert unbekannt oder geheim zu halten
# 6666666666: Tabellenfach gesperrt, da Aussage nicht sinnvoll
# 7777777777: keine Angabe, da Zahlenwert nicht sicher genug
# 8888888888: Angabe fällt später an

# Read the files ---------------------------------------------------------------

data_path <- here("data", "regionalatlas-einkuenfte")
filenames <- list.files(data_path, pattern = "\\.geojson")
filenames <- filenames[order(filenames)]
einkuenfte_dfs <- map(here(data_path, filenames), function(x) {
  message(x); st_read(x)})
einkuenfte_dfs <- set_names(einkuenfte_dfs, filenames)


# Recode the data --------------------------------------------------------------

einkuenfte_df <- einkuenfte_dfs %>%
  bind_rows(.id = "filename") %>%
  rename(einkuenfte = ai1602) %>%
  # Recode missing values
  mutate(
    einkuenfte = na_if(einkuenfte, 2222222222),
    einkuenfte = na_if(einkuenfte, 5555555555)
  )

glimpse(einkuenfte_df)
summary(st_drop_geometry(einkuenfte_df))

# Basic map to check the data
einkuenfte_df %>%
  ggplot() +
  geom_sf(
    aes(fill = einkuenfte),
    linewidth = 0, color = NA, size = 0
    )


# HEXAGONS! --------------------------------------------------------------------

# Create the grid
grid <- st_make_grid(einkuenfte_df, n = 30, square = FALSE, what = "polygons") %>%
  st_as_sf() %>%
  mutate(idx = row_number())
grid_de <- st_filter(grid, einkuenfte_df)
ggplot(grid_de) + geom_sf()

# Join the spacial objects by the centroid location
cent_grid <- st_centroid(grid_de)
cent_merge <- st_join(cent_grid, einkuenfte_df, left = FALSE)
grid_flaechennutzung <- st_join(
  grid_de, st_transform(einkuenfte_df, st_crs(grid_de)),
  join = st_intersects)

# Aggregate values based on index and nutzungstyp
grid_einkuenfte_agg <- aggregate(
  select(grid_flaechennutzung, -c(id, idx)),
  # keep the land use type in the grouping
  by = list(idx = grid_flaechennutzung$idx),
  FUN = mean,
  na.rm = TRUE,
  do_union = FALSE
)
glimpse(grid_einkuenfte_agg)


p <- grid_einkuenfte_agg %>%
  filter(!is.nan(einkuenfte)) %>%
  ggplot() +
  geom_sf(
    aes(fill = einkuenfte),
    linewidth = 0.05, color = "grey18"
  ) +
  colorspace::scale_fill_continuous_sequential(
    "Sunset", trans = "log") +
  guides(fill = guide_colorbar(title.position = "top")) +
  labs(
    title = "Taxable Income in Germany",
    subtitle = "Average total amount of taxable income per taxpayer<br>in a grid of hexagons.",
    caption = "The original data is on municipality level (\"Gemeinden\")<br>
    Source: Regionalatlas, Statistisches Bundesamt (2015).
    Visualization: Ansgar Wolsing",
    fill = "\U00D8 taxable income (in kEUR, log)"
  ) +
  theme_void(base_family = "Outfit", base_size = 10) +
  theme(
    plot.background = element_rect(color = "grey96", fill = "grey96"),
    strip.text = element_text(size = 11, margin = margin(b = 4)),
    legend.position = "bottom",
    legend.title = element_text(hjust = 0.5, size = 7),
    legend.text = element_text(size = 7),
    legend.key.height = unit(2, "mm"),
    legend.key.width = unit(8, "mm"),
    text = element_text(color = "grey20", lineheight = 1.1),
    plot.title = element_text(family = "Outfit Semibold", hjust = 0.5),
    plot.subtitle = element_markdown(
      hjust = 0.5, margin = margin(t = 4, b = 12)),
    plot.title.position = "plot",
    plot.caption = element_markdown(hjust = 0.5, size = 7),
    plot.margin = margin(rep(4, 4))
  )
ggsave(here("plots", "09-hexagons-income.png"), width = 4, height = 4, scale = 1.2)
