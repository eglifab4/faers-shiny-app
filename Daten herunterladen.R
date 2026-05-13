library(rvest)
library(stringr)

url <- "https://fis.fda.gov/extensions/FPD-QDE-FAERS/FPD-QDE-FAERS.html"

page <- read_html(url)

links <- page |>
  html_elements("a") |>
  html_attr("href")

links <- links[!is.na(links)]

#Ascii zip dateien

zip_links <- links[
  str_detect(
    links,
    regex(
      "faers_ascii_.*\\.zip$",
      ignore_case = TRUE))]

zip_links <- zip_links[
  str_detect(
    zip_links,
    "20(1[2-9]|[2-9][0-9])")]

zip_links <- unique(zip_links)

#Zielordner

base_dir <- "Organisation_Rohdaten/faers_data"

dir.create(
  base_dir,
  recursive = TRUE,
  showWarnings = FALSE)

options(timeout = 600)

#Quartal id

extract_id <- function(x) {
  
  str_extract(
    tolower(x),
    "20[0-9]{2}q[1-4]")}

existing_ids <- extract_id(
  list.files(base_dir))


# Download + unzip
for (link in zip_links) {
  
  # vollständige URL
  if (!str_detect(link, "^http")) {
    file_url <- paste0(
      "https://fis.fda.gov",
      link)} 
  else {file_url <- link}
  
  file_name <- basename(file_url)
  
  id <- extract_id(file_name)
  
  # überspringen falls vorhanden
  if (id %in% existing_ids) {
    
    message(
      "Bereits vorhanden: ",
      id)
    
    next}
  
  zip_path <- file.path(
    base_dir,
    file_name)
  
  out_dir <- file.path(
    base_dir,
    str_replace(
      file_name,
      "\\.zip$",
      ""))
  
  message(
    "⬇ Download: ",
    file_name)
  
  tryCatch({
    
    download.file(
      file_url,
      destfile = zip_path,
      mode = "wb",
      method = "libcurl")
    
    dir.create(
      out_dir,
      showWarnings = FALSE)
    
    unzip(
      zip_path,
      exdir = out_dir)
    
  }, error = function(e) {
    
    message(
      "Fehler bei: ",
      file_name)
    
    # defekte Dateien entfernen
    if (file.exists(zip_path)) {
      file.remove(zip_path)}
    
    if (dir.exists(out_dir)) {
      unlink(out_dir, recursive = TRUE)}})}