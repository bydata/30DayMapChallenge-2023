library(tidyverse)
library(ggtext)
library(sf)
library(here)
library(grid)

#' Quote from
#' https://www.bbc.com/future/article/20160614-maps-have-north-at-the-top-but-it-couldve-been-different

world <- giscoR::gisco_coastallines

ragg::agg_png(here("plots", "22-north-is-not-always-up.png"), res = 300,
              width = 6, height = 6, units = "in", bg = "grey9")
ggplot(world) +
  geom_sf(fill = "grey70", col = NA, size = 0.05, linewidth = 0) +
  coord_sf(crs = "+proj=laea +lon_0=220 +lat_0=-84 +ellps=WGS84 +no_defs") +
  labs(
    title = "NORTH IS NOT ALWAYS UP",
    caption = "Data: **GISCO**. Visualization: **Ansgar Wolsing**") +
  cowplot::theme_map() +
  theme(plot.background = element_rect(color = NA, fill = "grey9"),
        text = element_text(family = "Montserrat", color = "grey80"),
        plot.title = element_markdown(
          face = "italic", color = "white", size = 24, hjust = 0.5),
        plot.caption = element_markdown(size = 7, hjust = 0.5)
  )
# Draw background for the textbox
grid.rect(
  x = 0.5, y = 0.8,
  width = 0.46, height = 0.15,
  gp = gpar(fill = "#BBBBBB88", lty = "dashed", col = "white", lwd = 0.5)
)
# Add text
grid.text(
  "Is it time to start embracing\na different view of the planet\nfrom the one we are used to?",
  x = 0.5,
  y = 0.8,
  hjust = 0.5,
  gp = gpar(col = "white", fontfamily = "Montserrat Semibold")
)
dev.off()

bg_color <- "grey9"

ragg::agg_png(here("plots", "22-north-is-not-always-up.png"), res = 300,
              width = 6, height = 6, units = "in", bg = bg_color)
ggplot(world) +
  geom_sf(fill = "grey70", col = NA, size = 0.05, linewidth = 0) +
  coord_sf(crs = "+proj=laea +lon_0=220 +lat_0=-84 +ellps=WGS84 +no_defs") +
  labs(
    # Have the word "UP" in the same color as the background
    title = sprintf("NORTH IS NOT ALWAYS <span style='color:%s'>UP</span>", bg_color),
    subtitle = "Is it time to start embracing a different view\nof the planet
    from the one we are used to?",
    caption = "Data: **GISCO**. Visualization: **Ansgar Wolsing**") +
  cowplot::theme_map() +
  theme(plot.background = element_rect(color = NA, fill = bg_color),
        text = element_text(family = "Montserrat", color = "grey80"),
        plot.title = element_markdown(
          face = "italic", color = "white", size = 26, hjust = 0.5),
        plot.subtitle = element_text(family = "Montserrat Medium", hjust = 0.5,
                                     lineheight = 1),
        plot.caption = element_markdown(size = 7, hjust = 0.5)
  )
# Add upside-down text
grid.text(
  "UP",
  x = 0.867,
  y = 0.964,
  hjust = 0.5, rot = 180,
  gp = gpar(col = "white", fontfamily = "Montserrat", fontface = "italic",
            fontsize = 26)
)
dev.off()

