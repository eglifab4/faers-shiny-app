#Erste Datenerkundung
#

library(rvest)
library(stringr)
library(xml2)

url <- "https://fis.fda.gov/extensions/FPD-QDE-FAERS/FPD-QDE-FAERS.html"
page <- read_html(url)
links <- page |> 
  html_elements("a") |> 
  html_attr("href")
links <- links[!is.na(links)]
links_2012 <- links[str_detect(links, "20(1[2-9]|[2-9][0-9])")]
q1_links <- links_2012[str_detect(links_2012, "Q1")]
q1_links <- tail(q1_links, 1)
dir.create("Rohdaten_faers_data", showWarnings = FALSE)
file_url <- q1_links[1]
dest_file <- file.path("Organisation_Rohdaten/faers_data", basename(file_url))
options(timeout = 600)

download.file(file_url, destfile = dest_file, mode = "wb", method = "libcurl")

unzip(dest_file, exdir = "Organisation_Rohdaten/faers_data/q1")


doc <- read_xml("Organisation_Rohdaten/faers_data/faers_xml_2019Q1/xml/1_ADR19Q1.xml")

xml_name(xml_root(doc))
xml_children(xml_root(doc))


case1 <- xml_children(xml_root(doc))[2]

xml_name(case1)
xml_children(case1)

xml_find_all(case1, ".//*")

