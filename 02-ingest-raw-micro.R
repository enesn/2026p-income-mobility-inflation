## ==================================================================================================#
# Relative Income Mobility around an Inflationary Shock
# May 2025
# EI
## ==================================================================================================#

## ==================================================================================================#
# The releases are fixed once downloaded, so the merged
# result is cached as a parquet file: when that cache exists the raw folders are not touched at all.
# Set rebuild_raw_cache <- TRUE before sourcing (or delete the cache file) to rebuild from raw.

step_header("02 | Ingest: SILC panel 2008-2024, merged from the raw TUIK releases")

RAW_CACHE_FILE <- "cached-micro-data/silc0824-from-raw.parquet"

if (!exists("rebuild_raw_cache")) rebuild_raw_cache <- FALSE

if (!rebuild_raw_cache && file.exists(RAW_CACHE_FILE)) {

  cat("⚡ Reading cached panel:", RAW_CACHE_FILE, "\n")
  silc0824 <- arrow::read_parquet(RAW_CACHE_FILE)

} else {

  ## ==================================================================================================#
  # Every release holds the same four files, so all panels are read and merged the same way. The
  # release list (raw_panel_releases) and the file-name matching live in _setup.R, because 00-run-all.R
  # needs them too when it checks whether the raw releases are present at all.

  read_silc_panel <- function(panel_dir, sep) {
    read_part <- function(suffix) {
      path <- raw_panel_part_path(panel_dir, suffix)
      stopifnot(length(path) == 1)
      if (is.na(sep)) read_csv(path) else read.csv(path, sep = sep)
    }

    panel <- right_join(read_part("f"), read_part("fk"),
                        by = c("FKIMLIK", "HKIMLIK", c("FB010" = "FK010"))) %>%
      left_join(read_part("h"), by = c("HKIMLIK", c("FB010" = "HB010"))) %>%
      right_join(read_part("hk"), by = c("HKIMLIK", c("FB010" = "HK010")))

    # Column types harmonised to character before stacking
    panel[] <- lapply(panel, as.character)
    panel
  }

  cat("⏳ Building panel from raw releases (one-off):", RAW_CACHE_FILE, "\n")

  silc_panels <- pmap(raw_panel_releases, read_silc_panel)

  ## ==================================================================================================#

  step_note("Stacking the releases, dropping the person-years they share, counting years per person")

  silc0824 <- bind_rows(silc_panels) %>%
    distinct(FKIMLIK, FB010, .keep_all = TRUE)

  silc0824 <- silc0824 %>%
    left_join(count(silc0824, FKIMLIK, name = "times_seen"), by = "FKIMLIK")

  dir.create(dirname(RAW_CACHE_FILE), showWarnings = FALSE, recursive = TRUE)
  write_parquet(silc0824, RAW_CACHE_FILE, compression = "zstd")
  saved_note(RAW_CACHE_FILE)

  rm(read_silc_panel, silc_panels)
}

rm(rebuild_raw_cache)
