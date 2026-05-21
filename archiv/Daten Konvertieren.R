install.packages("fst")
library(fst)
library(data.table)
library(here)

app_data_dir <- here("app_data")
dateien <- list.files(app_data_dir, pattern = "\\.rds$", full.names = TRUE)

for (f in dateien) {
  t0 <- Sys.time()
  message("Konvertiere: ", basename(f), " ...")
  dt <- as.data.table(readRDS(f))
  out <- sub("\\.rds$", ".fst", f)
  write_fst(dt, out, compress = 50)
  message("  -> ", basename(out),
          " (", round(file.info(out)$size / 1024^2, 1), " MB, ",
          round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1), "s)")
  rm(dt); gc(verbose = FALSE)
}

message("FERTIG! Jetzt liegen in app_data/ sowohl .rds als auch .fst Dateien.")



