library(tidyverse)
library(ggtext)
library(sf)
library(here)

shp <- st_read(here("data", "vg5000_12-31.gk3.shape.ebenen", "vg5000_ebenen_1231",
                    "VG5000_KRS.shp"))

data_url <- "https://www.destatis.de/DE/Themen/Gesellschaft-Umwelt/Bevoelkerung/Eheschliessungen-Ehescheidungen-Lebenspartnerschaften/Publikationen/Downloads-Eheschliessungen/statistischer-bericht-ehescheidungen-eheschliessungen-geborene-gestorbene-5126001227005.xlsx?__blob=publicationFile"
data_filepath <- here("data", "statistischer-bericht-ehescheidungen-eheschliessungen-geborene-gestorbene-5126001227005.xlsx")
download.file(data_url, data_filepath)
df_raw <- readxl::read_xlsx(data_filepath, sheet = "126xx-01", skip = 4)

df <- df_raw %>%
  select(AGS = 1, landkreis = 2, lebendgeborene = 5, gestorbene = 7) %>%
  filter(!is.na(AGS) & (str_length(AGS) >= 4 |
                        # Berlin & Hamburg
                          AGS %in% c("2", "11"))) %>%
  mutate(
    landkreis = case_match(
      AGS,
      "11" ~ "Berlin",
      "2" ~ "Hamburg",
      .default = landkreis
    ),
    # format the AGS according to the format in the shapefile
    AGS = str_pad(AGS, 5, side = "left", pad = "0"),
    AGS = case_match(
      landkreis,
      "Berlin" ~ "11000",
      "Hamburg" ~ "02000",
      .default = AGS
    ),
    geb_gest_ratio = lebendgeborene / gestorbene,
    geb_gest_ratio_log2 = log(geb_gest_ratio, 2),
    geb_gest_perc = (lebendgeborene - gestorbene) / gestorbene
    ) %>%
  na.omit() %>%
  arrange(AGS)


# Check if all AGS match with the data frame
shp %>%
  anti_join(df, by = "AGS") %>%
  nrow() == 0

shp %>%
  inner_join(df, by = "AGS") %>%
  ggplot() +
  geom_sf(
    aes(fill = geb_gest_ratio),
    linewidth = 0.00025, color = NA) +
  scale_fill_gradient2(
    midpoint = 1, breaks = seq(0.5, 1.25, 0.25),
    labels = c("Twice more **deaths**", "33 % more deaths",
               "**Same** number of<br>births and deaths", "25 % more **births**")) +
  labs(
    title = "In 387 of Germany's 400 districts more people die than babies are born",
    subtitle = "Ratio of births and deaths in the districts Germany<br>
    (\"Landkreise\" and \"kreisfreie Städte\"), 2022.",
    caption = "Source: Bundesamt für Kartographie und Geodäsie,
    Statistisches Bundesamt. Visualization: Ansgar Wolsing",
    fill = NULL
  ) +
  theme_void(base_family = "Source Sans Pro") +
  theme(
    plot.background = element_rect(color = "grey98", fill = "grey98"),
    legend.text = element_markdown(),
    legend.position = c(1.035, 0.35),
    legend.direction = "vertical",
    legend.key.width = unit(0.5, "cm"),
    plot.title = element_markdown(family = "Source Sans Pro Semibold", hjust = 0.5),
    plot.subtitle = element_markdown(hjust = 0.5, lineheight = 1),
    plot.caption = element_markdown(hjust = 0.5, lineheight = 1),
    plot.margin = margin(rep(4, 4))
  )
ggsave(here("plots", "13-choropleth.png"), width = 8, height = 7)


# How many districts with more births than deaths?
length(df$AGS[df$geb_gest_ratio > 1])

# How many babies born?
sum(df$lebendgeborene)

# How many people died?
sum(df$gestorbene)

