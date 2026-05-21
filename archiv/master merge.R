library(dplyr)
library(here)

# all rds files

rds_files <- list.files(
  here("Organisation_processed_data"),
  pattern = "\\.rds$",
  recursive = TRUE,
  full.names = TRUE)

# Kontrolle
print(rds_files)

# Load and merge

all_data <- lapply(rds_files, readRDS) %>%
  bind_rows()

# Rremove duplicate

all_data <- all_data %>%
  distinct(primaryid, .keep_all = TRUE)

# Save master data set

saveRDS(
  all_data,
  here("Organisation_processed_data", "faers_master.rds"))
