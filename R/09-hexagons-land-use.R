library(tidyverse)
library(ggtext)
library(sf)
library(here)

#' Source: https://regionalatlas.statistikportal.de/
#' Download data from Regionatlas and
#' place the files in data/regionalatlas-flaechennutzung
##' Path: Themen >> Gebiet und Fläche >> Flächennutzung nach ALB
##' Levels: Kreise / kreisfreie Städte (for Saxony) +
##' Gemeinden (for the other federal states)
# Zeichenerklärung:
# 2222222222: nichts vorhanden, genau 0
# 5555555555: Zahlenwert unbekannt oder geheim zu halten
# 6666666666: Tabellenfach gesperrt, da Aussage nicht sinnvoll
# 7777777777: keine Angabe, da Zahlenwert nicht sicher genug
# 8888888888: Angabe fällt später an

# Read the files ---------------------------------------------------------------

data_path <- here("data", "regionalatlas-flaechennutzung")
filenames <- list.files(data_path, pattern = "\\.geojson")
filenames <- filenames[order(filenames)]
flaechennutzung_dfs <- map(here(data_path, filenames), function(x) {
  message(x); st_read(x)})
flaechennutzung_dfs <- set_names(flaechennutzung_dfs, filenames)


# Recode the data --------------------------------------------------------------

flaechennutzung_df <- flaechennutzung_dfs %>%
  bind_rows(.id = "filename") %>%
  mutate(
    ebene = str_extract(filename, "^(K|G)-", group = 1),
    ebene = factor(ebene, levels = c("K", "G"), labels = c("Kreis", "Gemeinde")),
    nutzungstyp = str_extract(filename, "AI010([1-4])-", group = 1),
    nutzungsanteil = case_when(
      nutzungstyp == 1 ~ ai0101,
      nutzungstyp == 2 ~ ai0102,
      nutzungstyp == 3 ~ ai0103,
      nutzungstyp == 4 ~ ai0104
    ),
    nutzungstyp = case_match(
      nutzungstyp,
      "1" ~ "Siedlung und Verkehr",
      "2" ~ "Erholung",
      "3" ~ "Landwirtschaft",
      "4" ~ "Wald"
    ),
    nutzungstyp = fct_inorder(nutzungstyp),
    nutzungstyp_en = case_match(
      nutzungstyp,
      "Siedlung und Verkehr" ~ "Urban & Traffic",
      "Erholung" ~ "Recreation",
      "Landwirtschaft" ~ "Agriculture",
      "Wald" ~ "Forest"
    ),
    nutzungstyp_en = fct_inorder(nutzungstyp_en)
  ) %>%
  select(-starts_with("ai010")) %>%
  # Keep district level (Kreis) only for Saxony (schluessel starts with "14")
  # ... and remove municipality level (Gemeinde) for Saxony
  filter(
    ebene == "Kreis" & str_sub(schluessel, 1, 2) == "14" |
      ebene == "Gemeinde" & str_sub(schluessel, 1, 2) != "14"
  ) %>%
  # Recode missing values
  mutate(
    nutzungsanteil = na_if(nutzungsanteil, 2222222222),
    nutzungsanteil = na_if(nutzungsanteil, 5555555555)
  )

glimpse(flaechennutzung_df)
summary(st_drop_geometry(flaechennutzung_df))

# Basic map to check the data
flaechennutzung_df %>%
  ggplot() +
  geom_sf(
    aes(fill = nutzungsanteil),
    linewidth = 0, color = NA, size = 0
    ) +
  facet_wrap(vars(nutzungstyp))


# HEXAGONS! --------------------------------------------------------------------

# Create the grid
grid <- st_make_grid(flaechennutzung_df, n = 25, square = FALSE, what = "polygons") %>%
  st_as_sf() %>%
  mutate(idx = row_number())
grid_de <- st_filter(grid, flaechennutzung_df)
ggplot(grid_de) + geom_sf()

# Join the spacial objects by the centroid location
cent_grid <- st_centroid(grid_de)
cent_merge <- st_join(cent_grid, flaechennutzung_df, left = FALSE)
grid_flaechennutzung <- st_join(
  grid_de, st_transform(flaechennutzung_df, st_crs(grid_de)),
  join = st_intersects)

# Aggregate values based on index and nutzungstyp
grid_flaechennutzung_agg <- aggregate(
  select(grid_flaechennutzung, -c(idx, nutzungstyp, nutzungstyp_en)),
  # keep the land use type in the grouping
  by = list(idx = grid_flaechennutzung$idx,
            nutzungstyp = grid_flaechennutzung$nutzungstyp,
            nutzungstyp_en = grid_flaechennutzung$nutzungstyp_en),
  FUN = mean,
  na.rm = TRUE,
  do_union = FALSE
)
glimpse(grid_flaechennutzung_agg)


p <- grid_flaechennutzung_agg %>%
  filter(nutzungstyp != "Erholung") %>%
  ggplot() +
  geom_sf(
    aes(fill = nutzungsanteil),
    linewidth = 0.05, color = "grey18"
  ) +
  colorspace::scale_fill_continuous_sequential(
    "Sunset", labels = function(x) paste(x, "%")) +
  facet_wrap(vars(nutzungstyp_en)) +
  guides(fill = guide_colorbar(title.position = "top")) +
  labs(
    title = "Land Use in Germany",
    subtitle = "Share of different types of land use in a grid of hexagons.",
    caption = "The original data is on municipality level (\"Gemeinden\"), except for
    Saxony which is on district level (\"Kreise\").<br>
    Source: Regionalatlas, Statistisches Bundesamt (2015).
    Visualization: Ansgar Wolsing",
    fill = "Share of land use"
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
    plot.subtitle = element_text(
      hjust = 0.5, margin = margin(t = 4, b = 12)),
    plot.title.position = "plot",
    plot.caption = element_markdown(hjust = 0.5, size = 7),
    plot.margin = margin(rep(4, 4))
  )
ggsave(here("plots", "09-hexagons.png"), width = 6.5, height = 4)
