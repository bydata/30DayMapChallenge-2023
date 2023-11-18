library(tidyverse)

# Read data
# Source: https://www.bundeswahlleiterin.de/bundestagswahlen/2021/ergebnisse/opendata/csv/
df <- read_csv2(
  "https://www.bundeswahlleiterin.de/bundestagswahlen/2021/ergebnisse/opendata/csv/kerg2.csv",
  skip = 9, name_repair = janitor::make_clean_names)

# Filter data for AfD party vote share and store CSV
df %>%
  filter(gebietsart == "Wahlkreis", stimme == 2, gruppenart == "Partei", gruppenname == "AfD") %>%
  select(gebietsname, gruppenname, stimme, prozent) %>%
  write_csv(file.path("data", "btw-2021-zweitstimme-afd.csv"))
