## ==================================================================================================#
# Inflation and redistribution in Turkey
# May 2025
# EI
## ==================================================================================================#

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

silc_panels <- pmap(panels, read_silc_panel)

## ==================================================================================================#

silc0824 <- bind_rows(silc_panels) %>%
  distinct(FKIMLIK, FB010, .keep_all = TRUE)

silc0824 <- silc0824 %>%
  left_join(count(silc0824, FKIMLIK, name = "times_seen"), by = "FKIMLIK")

rm(panels, read_silc_panel, silc_panels)
