library(dplyr)
library(here)

zip_dir <- here("Organisation_Rohdaten", "faers_data")
out_dir <- here("Organisation_processed_data")

zips <- list.files(zip_dir, pattern = "\\.zip$", full.names = TRUE)

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

process_quarter <- function(zip_path) {
  
  q_name <- tools::file_path_sans_ext(basename(zip_path))
  message("Verarbeite: ", q_name)
  
  tmp <- tempfile()
  dir.create(tmp)
  
  unzip(zip_path, exdir = tmp)
  
  ascii_dir <- file.path(tmp, "ascii")
  
  files <- list.files(ascii_dir,
                      full.names = TRUE,
                      recursive = TRUE)
  
  txt_files <- files[grepl("\\.txt$", files, ignore.case = TRUE)]
  
  demo_file <- txt_files[grepl("DEMO", basename(txt_files))][1]
  drug_file <- txt_files[grepl("DRUG", basename(txt_files))][1]
  reac_file <- txt_files[grepl("REAC", basename(txt_files))][1]
  
  demo <- read_faers(demo_file)
  drug <- read_faers(drug_file)
  reac <- read_faers(reac_file)
  
  names(demo) <- tolower(names(demo))
  names(drug) <- tolower(names(drug))
  names(reac) <- tolower(names(reac))
  
  demo$primaryid <- as.character(demo$primaryid)
  drug$primaryid <- as.character(drug$primaryid)
  reac$primaryid <- as.character(reac$primaryid)
  
  drug_clean <- drug %>%
    group_by(primaryid) %>%
    summarise(
      n_drugs = n(),
      drugs = paste(unique(drugname), collapse = "; "),
      ingredients = paste(unique(prod_ai), collapse = "; "),
      .groups = "drop")
  
  reac_clean <- reac %>%
    group_by(primaryid) %>%
    summarise(
      n_reac = n(),
      reactions = paste(unique(pt), collapse = "; "),
      .groups = "drop")
  
  faers_final <- demo %>%
    left_join(drug_clean, by = "primaryid") %>%
    left_join(reac_clean, by = "primaryid")
  
  out_path <- file.path(out_dir, q_name)
  dir.create(out_path, recursive = TRUE, showWarnings = FALSE)
  
  saveRDS(faers_final,
          file.path(out_path, paste0(q_name, "_clean.rds")))
  
  unlink(tmp, recursive = TRUE)
  
  message("Fertig: ", q_name)}

# run all
lapply(zips, process_quarter)