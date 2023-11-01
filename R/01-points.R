library(tidyverse)
library(lubridate)
library(here)
library(ggtext)
#' https://github.com/brry/rdwd
# rdwd::updateRdwd()
library(rdwd)

# Index with metadata about all stations
data(metaIndex)
glimpse(metaIndex)

table(metaIndex$var)

# Select stations which have been active at least since min_date and are currently active
min_date <- as_date("1961-01-01")
active_stations_meta <- metaIndex %>%
  filter(von_datum <= min_date & var == "kl" & res == "monthly"
         & per == "recent" & hasfile)
n_active_stations <- nrow(active_stations_meta)


# Get the download URLs for all stations that match the criteria
station_ids <- unique(active_stations_meta$Stations_id)

# Update only recent files // run if you have pulled the data at least once before
update_only_recent <- TRUE
if (update_only_recent) {
  message("Updating recent files.")
  data_urls <- map(
    station_ids,
    ~selectDWD(id = .x, res = "daily", var = "kl",
               per = "r", # only recent
               exactmatch = TRUE))
} else {
  data_urls <- map(
    station_ids,
    ~selectDWD(id = .x, res = "daily", var = "kl", per = "hr",
               exactmatch = TRUE))
  message("Updating historic and recent files.")
}

# Download datasets, returning the local storage file name
files <- map(
  data_urls,
  ~dataDWD(.x, dir = file.path("data", "dwd-stations"), read = FALSE, force = TRUE))

# Add the filepath of historical file (required if you only update the recent files)
files_hist_and_recent <- vector("list", length = length(files))
files_local_hist <- here("data", "dwd-stations",
                         list.files(here("data", "dwd-stations"),
                                    pattern = "daily_kl_historical"))
files_hist_and_recent <- map2(files, seq_along(files), function(x, i) {
  # find the historical data for this station
  station_id <- str_extract(x, "_(\\d{5})_akt.zip", group = 1)
  file_hist <- files_local_hist[str_detect(files_local_hist, sprintf("tageswerte_KL_%s_", station_id))]

  c(x, file_hist)
})

# Read the files from the zip archives
dfs <- map(files_hist_and_recent, readDWD, varnames = TRUE)

combine_historical_and_recent <- function(x) {
  df <- bind_rows(x)
  colnames(df) <- tolower(colnames(df))
  df <- df %>%
    # there is a certain overlap between the two data sources,
    # remove those duplicates
    group_by(mess_datum) %>%
    slice_head(n = 1) %>%
    ungroup() %>%
    transmute(
      stations_id,
      date = lubridate::as_date(as.character(mess_datum)),
      tmk.lufttemperatur, txk.lufttemperatur_max, tnk.lufttemperatur_min
    ) %>%
    na.omit() %>%
    mutate(month = month(date, label = TRUE),
           year = year(date),
           decade = year %/% 10 * 10)
  df
}

# Combine recent and historical data - this takes a couple of minutes
dfs_combined <- map(dfs, combine_historical_and_recent, .progress = TRUE)
dfs_combined <- set_names(dfs_combined, unique(active_stations_meta$Stationsname))
write_rds(dfs_combined, here("output", "dwd-stations-pre.rds"))


# Add metadata to weather data
add_station_metadata <- function(x, metadata = active_stations_meta) {
  x %>%
    inner_join(metadata, by = join_by(stations_id == Stations_id)) %>%
    select(-c(res, var, per, hasfile))
}

dfs_prep <- map(dfs_combined, add_station_metadata, .progress = TRUE)
glimpse(dfs_prep[[1]])

# Calculate historical temperature records (pre-2023) by month per station
calculate_historical <- function(x, exclude_current_year = TRUE) {
  if (exclude_current_year) {
    current_year <- year(Sys.Date())
    x <- x[x$year < current_year, ]
  }
  x %>%
    group_by(month) %>%
    arrange(date, .by_group = TRUE) %>%
    slice_max(txk.lufttemperatur_max, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    select(stations_id, year, month, date,
           tmk.lufttemperatur, txk.lufttemperatur_max,
           lat = geoBreite, lon = geoLaenge, station_elevation = Stationshoehe,
           stations_name = Stationsname)
}

calculate_historical_average <- function(
    x,
    reference_period_start_year = 1961,
    reference_period_end_year = 1990) {
  x %>%
    filter(year >= reference_period_start_year & year <= reference_period_end_year) %>%
    group_by(stations_id, Stationsname, geoBreite, geoLaenge, month) %>%
    summarize(
      reference_avg_lufttemperatur = mean(tmk.lufttemperatur, na.rm = TRUE),
      reference_sd_lufttemperatur = sd(tmk.lufttemperatur, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      reference_period_start_year = reference_period_start_year,
      reference_period_end_year = reference_period_end_year
    ) %>%
    select(stations_id, month,
           reference_avg_lufttemperatur, reference_sd_lufttemperatur,
           lat = geoBreite, lon = geoLaenge, stations_name = Stationsname,
           reference_period_start_year, reference_period_end_year)
}

calculate_recent <- function(x) {
  current_year <- year(Sys.Date())
   x %>%
    filter(year == current_year) %>%
    group_by(stations_id, month) %>%
    summarize(
      recent_max_txk.lufttemperatur_max = max(txk.lufttemperatur_max, na.rm = TRUE),
      recent_mean_tmk.lufttemperatur = mean(tmk.lufttemperatur),
      .groups = "drop")


}

dfs_historical <- map(dfs_prep, calculate_historical, .progress = TRUE)
dfs_historical_avg <- map(dfs_prep, calculate_historical_average,
                          # reference_period_start_year = 1991,
                          # reference_period_end_year = 2020,
                          reference_period_start_year = 1961,
                          reference_period_end_year = 1990,
                          .progress = TRUE)
dfs_recent <- map(dfs_prep, calculate_recent, .progress = TRUE)


compare_recent_to_historical <- function(recent, historical) {
  historical %>%
    inner_join(recent, by = join_by(stations_id, month)) %>%
    mutate(
      diff_recent_txk = recent_max_txk.lufttemperatur_max - txk.lufttemperatur_max,
      diff_recent_tmk = recent_mean_tmk.lufttemperatur - tmk.lufttemperatur
    ) %>%
    filter(diff_recent_txk > 0)
}

compare_recent_to_historical_avg <- function(recent, historical_avg,
                                             keep_recent_below_avg = TRUE) {
  df <- historical_avg %>%
    inner_join(recent, by = join_by(stations_id, month)) %>%
    mutate(
      diff_recent = recent_mean_tmk.lufttemperatur - reference_avg_lufttemperatur,
      diff_recent_as_sd = diff_recent / reference_sd_lufttemperatur,
    )

  if (!keep_recent_below_avg) {
    df <- df %>%
      filter(diff_recent > 0)
  }

  df
}

dfs_comparison_avg <- map2(dfs_recent, dfs_historical_avg,
                           compare_recent_to_historical_avg, .progress = TRUE)

## Shape Germany
shp_de <- giscoR::gisco_get_countries(resolution = "20", country = "Germany")
centroid_de <- sf::st_centroid(shp_de) %>%
  sf::st_coordinates()

dfs_comparison_avg %>%
  bind_rows() %>%
  mutate(month_name = factor(month.name[month], levels = month.name)) %>%
  ggplot(aes(lon, lat)) +
  geom_sf(
    data = shp_de,
    inherit.aes = FALSE,
    fill = "grey95", col = "grey20", linewidth = 0.1
  ) +
  geom_point(
    aes(fill = diff_recent_as_sd),
    shape = 21, col = "grey20", stroke = 0.1, size = 1.75) +
  geom_richtext(
    data = ~group_by(., month_name) %>%
      summarize(avg_reference = mean(reference_avg_lufttemperatur),
                avg_recent = mean(recent_mean_tmk.lufttemperatur)),
    aes(x = centroid_de[, "X"] + 4, y = 45.65,
        label = sprintf("\U00D8 reference: **%2.1f** °C<br>2023: **%2.1f** °C",
                        avg_reference, avg_recent)),
    # fill = "#FFFFFFAA",
    fill = NA,
    family = "Source Sans Pro", label.size = 0, size = 3.25, vjust = 0, hjust = 1
  ) +
  colorspace::scale_fill_continuous_diverging(
    "Blue-Red 2", breaks = seq(-3, 3, 1),
    labels = function(x) ifelse(x > 0, paste0("+", x), x)) +
  coord_sf(ylim = c(45.65, NA)) +
  facet_wrap(vars(month_name), labeller = as_labeller(toupper)) +
  labs(
    title = "2023 has been warmer in most places in Germany",
    subtitle = glue::glue(
    "Each point represents one of {n_active_stations} weather stations
    which have been maintained at least since {year(min_date)}.
    The points are coloured by difference between the average temperature in the
    particular month in 2023 and the long-term average in the reference period,
    measured in standard deviations. (Standard deviation is a statistical measure
    that quantifies the amount of variation in a set of data points.)<br>
    The reference period is 1961 to 1990."),
    caption = "Source: DWD Open Data. Visualization: Ansgar Wolsing",
    fill = "Difference to reference temperature<br>*(standard deviations)*"
  ) +
  theme_void(base_family = "Source Sans Pro") +
  theme(
    plot.background = element_rect(color = "grey90", fill = "grey90"),
    plot.margin = margin(rep(4, 4)),
    plot.title = element_text(face = "bold", size = 20, hjust = 0.5),
    plot.subtitle = element_textbox(
      width = 1.0, hjust = 0.5, halign = 0.5, lineheight = 1,
      margin = margin(t = 8, b = 20)),
    legend.position = c(0.75, 0.18),
    legend.title = element_markdown(),
    strip.text = element_text(family = "Source Sans Pro Semibold", size = 11,
                              color = "grey40")
  )
ggsave(file.path("plots", "01-points.png"), width = 7, height = 10)
ggsave(file.path("plots", "01-points-2.png"), width = 5, height = 5/7 * 10, scale = 7/5)

