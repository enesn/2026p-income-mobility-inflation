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
  # Each TUIK panel release ships the same four files (f = individual income, fk = individual register,
  # h = household income, hk = household register), so all panels are read and merged the same way.
  # The folder layout and the delimiter differ between releases.

  panels <- tribble(
    ~panel_dir,                                                 ~sep,
    "raw-micro-data/GYKA_Panel_2008-2011/turkce/downloads",      NA,
    "raw-micro-data/GYKA_Panel_2010-2013/downloads",             ";",
    "raw-micro-data/GYKA_Panel_2012-2015/downloads",             ";",
    "raw-micro-data/GYKA_Panel_2014-2017/turkce",                ";",
    "raw-micro-data/GYKA_Panel_2015-2018/turkce",                ";",
    "raw-micro-data/GYKA_Panel_2016-2019/Turkce",                ",",
    "raw-micro-data/GYKA_Panel_2017-2020/veri_seti/csv",         ",",
    "raw-micro-data/GYKA_Panel_2018-2021",                       ",",
    "raw-micro-data/GYKA_Panel_2019-2022/csv",                   ",",
    "raw-micro-data/GYKA_Panel_2020-2023/TURKÇE/csv",            ",",
    "raw-micro-data/GYKA_Panel_2021-2024/csv_TURKÇE",            ","
  )

  read_silc_panel <- function(panel_dir, sep) {
    # File names are matched case-insensitively: the 2008-2011 release stores its
    # individual income file as GYK08091011_F.csv while every other file is lower case.
    read_part <- function(suffix) {
      path <- list.files(
        panel_dir,
        pattern = paste0("^gyk.*_", suffix, "\\.csv$"),
        ignore.case = TRUE,
        full.names = TRUE
      )
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

  silc_panels <- pmap(panels, read_silc_panel)

  ## ==================================================================================================#

  step_note("Stacking the releases, dropping the person-years they share, counting years per person")

  silc0824 <- bind_rows(silc_panels) %>%
    distinct(FKIMLIK, FB010, .keep_all = TRUE)

  silc0824 <- silc0824 %>%
    left_join(count(silc0824, FKIMLIK, name = "times_seen"), by = "FKIMLIK")

  dir.create(dirname(RAW_CACHE_FILE), showWarnings = FALSE, recursive = TRUE)
  write_parquet(silc0824, RAW_CACHE_FILE, compression = "zstd")

  rm(panels, read_silc_panel, silc_panels)
}

rm(rebuild_raw_cache)
