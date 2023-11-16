library(tidyverse)
library(sf)
library(marmap)
library(here)

# Download bathymetric data for Mariana Islands
bathy <- getNOAA.bathy(lon1 = 135, lon2 = 155, lat1 = 8, lat2 = 26,
                       antimeridian = FALSE, resolution = 1, keep = TRUE)


# blues <- colorRampPalette(c("red","purple","blue", "cadetblue1","white"))
# blues <- colorRampPalette(c("red","purple", "darkblue", "#313695", "#74add1","white"))
# blues <- colorRampPalette(c("#eb348f", "#3609ba", "#313695", "#4575b4", "#74add1", "#ffffff"))
blues <- colorRampPalette(c("#eb348f", "#3609ba", "#0f2aa3", "#74add1", "#ffffff"))


# Plot bathymetric map
ragg::agg_png(here("plots", "16-oceania-mariana-trench.png"),
              width = 4, height = 4, units = "in", res = 600)
par(family = "Outfit", cex = 0.5, mar = c(8, 2, 4.5, 2) + 0.1)
plot(bathy, image = TRUE,
     land = TRUE,
     bpal = list(
       c(min(bathy), 0, blues(200)),  # sea
       c(0, max(bathy), "grey50") # land
     ),
     deep = c(-10000, -6000, 0),
     shallow = c(-1000, -10, 0),
     step = c(1000, 1000, 0),
     lwd = c(0.1, 0.1, 0.3), lty = c(1, 1, 1),
     col = c("lightblue", "grey73", "black"),
     drawlabel = c(TRUE, TRUE, FALSE),
     axes = FALSE, xlab = NA, ylab = NA,
     main = "Mariana Trench", cex.main = 7
)
# Place point at Challenger Deep
points(142.591667, 11.373333, col = "#faec2a", cex = 0.8, pch = 15)
text(139.8, 11.85, "Challenger Deep", col = "#faec2a", cex = 1.25)
# Add scale
scaleBathy(bathy, deg = 2, x = "bottomright", inset = 5, family = "Outfit",
           cex = 1)
mtext("The Mariana Trench is the deepest part of the world's oceans,
reaching a maximum known depth of about 10,994 meters
in the Challenger Deep.", side = 1, line = 4, cex = 0.67, adj = 0.5)
mtext("Source: NOAA National Centers for Environmental Information. 2022.
      Visualization: Ansgar Wolsing", side = 1, line = 6, cex = 0.3)
dev.off()
