library(dplyr)
library(here)

# Input / Output

zip_path <- here("Organisation_Rohdaten", "faers_data", "faers_ascii_2019Q1.zip")

out_dir <- here("Organisation_processed_data", "2019Q1")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

tmp <- tempfile()
dir.create(tmp)

# Unzip

unzip(zip_path, exdir = tmp)

ascii_dir <- file.path(tmp, "ascii")

files <- list.files(ascii_dir,
                    full.names = TRUE,
                    recursive = TRUE)

txt_files <- files[grepl("\\.txt$", files, ignore.case = TRUE)]

#sep/delimit $

read_faers <- function(path) {
  
  read.table(
    path,
    sep = "$",
    header = TRUE,
    fill = TRUE,
    quote = "",
    stringsAsFactors = FALSE,
    na.strings = c("", "NA", "."),
    comment.char = "",
    fileEncoding = "latin1")}

# FILES

demo_file <- txt_files[grepl("DEMO", basename(txt_files))][1]
drug_file <- txt_files[grepl("DRUG", basename(txt_files))][1]
reac_file <- txt_files[grepl("REAC", basename(txt_files))][1]

# READ DATA

demo <- read_faers(demo_file)
drug <- read_faers(drug_file)
reac <- read_faers(reac_file)

# FIX: STANDARDIZE COLUMN NAMES

names(demo) <- tolower(names(demo))
names(drug) <- tolower(names(drug))
names(reac) <- tolower(names(reac))

# FIX: STANDARDIZE KEY TYPE (IMPORTANT!)

demo$primaryid <- as.character(demo$primaryid)
drug$primaryid <- as.character(drug$primaryid)
reac$primaryid <- as.character(reac$primaryid)

# DRUG AGGREGATION

drug_clean <- drug %>%
  group_by(primaryid) %>%
  summarise(
    n_drugs = n(),
    drugs = paste(unique(drugname), collapse = "; "),
    ingredients = paste(unique(prod_ai), collapse = "; "),
    .groups = "drop")

# REAC AGGREGATION

reac_clean <- reac %>%
  group_by(primaryid) %>%
  summarise(
    n_reac = n(),
    reactions = paste(unique(pt), collapse = "; "),
    .groups = "drop")


# FINAL MERGE

faers_final <- demo %>%
  left_join(drug_clean, by = "primaryid") %>%
  left_join(reac_clean, by = "primaryid")

# SAVE OUTPUT

saveRDS(
  faers_final,
  file.path(out_dir, "faers_2019Q1_clean.rds"))

# CLEANUP

unlink(tmp, recursive = TRUE)

message("Fertig: FAERS 2019Q1 erfolgreich verarbeitet")
