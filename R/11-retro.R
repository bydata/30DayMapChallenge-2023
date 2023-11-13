library(tidyverse)
library(sf)
library(ggtext)
# devtools::install_github("BjnNowak/bertin")
library(bertin)
library(tidycensus)
library(ggtext)
library(here)

# Miami Vice colors
miami_pal <- c("#EB95E3", "#60CFD1", "#413B86", "white", "black")

# Font: https://www.dafont.com/guanine.font

# Variables of the American Community Survey
acs_2021 <- load_variables(2021, "acs5")

df <- get_acs(geography = "county", variables = "B01001_022", year = 2021,
                    summary_var = "B01001_001", state = "FL", geometry = TRUE)

st_crs(df)
df_prep <- df %>%
  mutate(share = estimate / summary_est) %>%
  arrange(-share)

df_prep %>%
  ggplot() +
  geom_sf(aes(fill = share))

regions_valued <- make_points(
  polygon = df_prep,
  n = 55, # Number of points per side
  square = TRUE # Points shaped as squares (hexagons otherwise)
)

ggplot(regions_valued,aes(size=share)) +
  geom_sf(
    data = df_prep
  ) +
  geom_sf(
    aes(col = NAME == "Miami-Dade County, Florida")
  ) +
  scale_size(range = c(0.5, 5)) +
  scale_color_manual(values = c(miami_pal[2], miami_pal[1])) +
  theme_void()

regions_valued %>%
  # mutate(share_grp = cut(share, breaks = 4)) %>%
  mutate(
    share_grp = case_when(
      share < 0.02 ~ "<2 %",
      share < 0.03 ~ "<3 %",
      share < 0.04 ~ "<4 %",
      share < 0.05 ~ "<5 %",
      share > 0.05 ~ "5+ %"
    )) %>%
  ggplot() +
  geom_sf(
    data = df_prep,
    aes(fill = ifelse(NAME == "Miami-Dade County, Florida", "bg1", "bg2")),
    color = "white"
  ) +
  geom_sf(
    aes(
      size = share_grp,
      fill = ifelse(NAME == "Miami-Dade County, Florida", "fg1", "fg2"),
      color = NAME == "Miami-Dade County, Florida"),
    shape = 21, stroke = 0.2
  ) +
  annotate(
    GeomTextBox,
    x = -87.5, y = 28.85,
    label = "The two main actors in the 1980s TV series
    <span style='font-family:Guanine'>MIAMI VICE</span> are now
    73 (Don Johnson) and 74 years old (Philip Michael Thomas) respectively.
    This map in the style of Jacques Bertin shows the **proportion of men aged
    between 70 and 74** in the total population of the counties in Florida.
    The size of the dots indicates the share of that demographic.
    In <b style='color:#60CFD1'>Miami-Dade</b>, where the series was set,
    the proportion is comparatively low.",
    hjust = 0, vjust = 1, family = "Avenir", size = 4.5, box.size = 0,
    fill = NA, width = 0.52
  ) +
  annotate(
    GeomTextBox,
    x = -87.5, y = 24.8,
    label = "Source: American Community Survey 2021. Visualization: Ansgar Wolsing",
    hjust = 0, vjust = 1, family = "Avenir", size = 3.5, box.size = 0,
    fill = NA, width = 0.7
  ) +
  scale_size_manual(values = c(1.5, 2, 2.5, 3.5, 4.5)) +
  # scale_fill_manual(values = c("bg1" = miami_pal[1], "fg2" = miami_pal[2],
  #                              "bg2" = "white", "fg1" = miami_pal[3])) +
  scale_fill_manual(values = c("bg1" = miami_pal[2], "fg2" = miami_pal[3],
                               "bg2" = miami_pal[1], "fg1" = miami_pal[3])) +
  scale_color_manual(values = c("TRUE" = miami_pal[5], "FALSE" = miami_pal[4])) +
  guides(
    fill = "none", color = "none",
    size = guide_legend(title = "Share of population", title.position = "top")) +
  theme_void(base_family = "Avenir") +
  theme(
    plot.background = element_rect(color = "white", fill = "white"),
    plot.title = element_markdown(family = "guanine"),
    plot.margin = margin(rep(4, 4)),
    legend.position = c(0.305, 0.23),
    legend.direction = "horizontal"
  )
ggsave(here("plots", "11-retro.png"), width = 4, height = 4, scale = 1.8)
