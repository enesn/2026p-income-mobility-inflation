## ==================================================================================================#
# Relative Income Mobility around an Inflationary Shock
# May 2025
# EI
## ==================================================================================================#

#Packages used across the project.
attached_packages <- c(
  "tidyverse", "openxlsx", "readxl", "Hmisc", "purrr", "lfe", "kableExtra",
  "dplyr", "tidyr", "knitr", "httr2", "jsonlite", "readr", "arrow", "duckdb",
  "DBI", "haven"
)

#Only ever called as pkg::fun() — fixest, modelsummary and tinytable in 10, stargazer in 10 and 11,
#getPass in 02-ingest-combined-micro — so they must be installed but are deliberately not attached.
#tinytable ships with modelsummary and renders its tables; 10 reaches for it directly to group the
#columns and to resize the widest table to the text width.
namespace_packages <- c("fixest", "modelsummary", "tinytable", "stargazer", "getPass")

required_packages <- c(attached_packages, namespace_packages)

#A mirror has to be named explicitly: Rscript starts with repos unset ("@CRAN@") and
#install.packages() cannot fall back to a menu in a non-interactive session.
cran_mirror <- getOption("repos")["CRAN"]
if (is.na(cran_mirror) || !nzchar(cran_mirror) || cran_mirror == "@CRAN@") {
  options(repos = c(CRAN = "https://cloud.r-project.org"))
}

install_if_missing <- function(packages) {
  is_available <- function(p) vapply(p, requireNamespace, logical(1), quietly = TRUE)

  missing <- packages[!is_available(packages)]
  if (length(missing) == 0) return(invisible(NULL))

  message("Installing missing packages: ", paste(missing, collapse = ", "))
  install.packages(missing)

  failed <- missing[!is_available(missing)]
  if (length(failed) > 0) {
    stop("Could not install: ", paste(failed, collapse = ", "),
         ". Install these manually and re-run.", call. = FALSE)
  }
  invisible(NULL)
}

#With renv active the pinned versions in renv.lock are the source of truth, so restore those rather
#than pulling whatever CRAN serves today. A fresh clone therefore only needs source("_setup.R"):
#.Rprofile activates renv, and the restore below installs every version recorded in the lockfile.
#The test is the library path rather than the lockfile, because a session started with --vanilla
#skips .Rprofile: renv is then not active and restore() would have no project library to write to.
renv_is_active <- any(grepl("renv/library", .libPaths(), fixed = TRUE))

if (renv_is_active) {
  renv::restore(prompt = FALSE)
} else {
  message("renv is not active; installing current CRAN versions rather than the pinned ones.")
  install_if_missing(required_packages)
}

invisible(lapply(attached_packages, library, character.only = TRUE))

rm(cran_mirror, install_if_missing, renv_is_active)
## ==================================================================================================#

options(scipen = 999, digits = 10)

## ==================================================================================================#

dominance_threshold_theta <- 0.5

persistence_threshold <- 2

## ==================================================================================================#
#Progress headers. Each script announces itself with step_header(), and the longer ones mark their
#internal stages with step_note(), so a full run reads as an outline of the analysis rather than a
#silent wait. Named this way rather than step() to leave stats::step() alone.
step_header <- function(...) cat("\n==> ", paste(...), "\n", sep = "")

step_note <- function(...) cat("    - ", paste(...), "\n", sep = "")

#ggsave() and write_parquet() are silent, so a run would otherwise never say where anything landed.
#The file is checked rather than assumed, so a path that has drifted from the write above it says so
#instead of confirming a file that is not there.
saved_note <- function(path) {
  if (file.exists(path)) step_note("saved:", path) else step_note("NOT saved:", path)
}

## ==================================================================================================#
#Tables are written next to the figures in outputs-included/, so a run leaves a .tex the paper can
#\input{} rather than latex to copy out of the console. The table object is returned visibly, so
#wrapping a call in save_tex() still prints it as before. stargazer writes its own file through out=
#and is the one table function called without this helper.
save_tex <- function(x, path) {
  writeLines(as.character(x), path)
  x
}

## ==================================================================================================#
#Raw TUIK panel releases. Each release ships the same four files (f = individual income,
#fk = individual register, h = household income, hk = household register); only the folder layout and
#the delimiter differ. The table lives here rather than in 02-ingest-raw-micro.R because 00-run-all.R
#also needs it, to decide whether this machine actually has the raw releases to ingest.
raw_panel_releases <- tribble(
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

raw_panel_parts <- c("f", "fk", "h", "hk")

#File names are matched case-insensitively: the 2008-2011 release stores its individual income file
#as GYK08091011_F.csv while every other file is lower case.
raw_panel_part_path <- function(panel_dir, suffix) {
  list.files(
    panel_dir,
    pattern = paste0("^gyk.*_", suffix, "\\.csv$"),
    ignore.case = TRUE,
    full.names = TRUE
  )
}

#TRUE only when every release is present with all four of its files, i.e. when 02-ingest-raw-micro.R
#could actually run. A half-extracted raw-micro-data/ counts as not available: it would otherwise
#fail deep inside the merge rather than at the choice of ingest route.
raw_micro_data_complete <- function() {
  release_is_complete <- function(panel_dir) {
    dir.exists(panel_dir) &&
      all(vapply(raw_panel_parts,
                 function(suffix) length(raw_panel_part_path(panel_dir, suffix)) == 1,
                 logical(1)))
  }

  all(vapply(raw_panel_releases$panel_dir, release_is_complete, logical(1)))
}

## ==================================================================================================#
##to sum when some variables have missing values
`%+%` <- function(x, y)  mapply(sum, x, y, MoreArgs = list(na.rm = TRUE))


weighted_ntile <- function(x, weights, n) {
  stopifnot(length(x) == length(weights), n > 0)
  
  # Identify non-missing x
  valid <- !is.na(x)
  
  # Prepare result vector
  result <- rep(NA_integer_, length(x))
  
  # Proceed only on valid entries
  x_valid <- x[valid]
  weights_valid <- weights[valid]
  
  # Order by x
  ord <- order(x_valid)
  x_sorted <- x_valid[ord]
  weights_sorted <- weights_valid[ord]
  
  # Compute cumulative weight proportions
  cum_weights <- cumsum(weights_sorted)
  total_weight <- sum(weights_sorted)
  cum_prop <- cum_weights / total_weight
  
  # Define quantile cut points
  breaks <- seq(0, 1, length.out = n + 1)
  
  # Assign tile numbers (1 to n)
  tiles_sorted <- findInterval(cum_prop, vec = breaks, rightmost.closed = TRUE)
  
  # Convert any 0s to 1
  tiles_sorted[tiles_sorted == 0] <- 1
  
  # Reorder to match original valid x
  tiles_valid <- integer(length(x_valid))
  tiles_valid[ord] <- tiles_sorted
  
  # Insert into result
  result[valid] <- tiles_valid
  
  return(result)
}


weighted_median <- function(x, w) {
  df <- data.frame(x = x, w = w)
  df <- df[order(df$x), ]
  cum_weights <- cumsum(df$w) / sum(df$w)
  df$x[which(cum_weights >= 0.5)[1]]
}

weighted_se <- function(x, w) {
  x_bar <- sum(w * x) / sum(w)
  var_w <- sum(w * (x - x_bar)^2) / sum(w)
  n_eff <- (sum(w))^2 / sum(w^2)
  sqrt(var_w / n_eff)
}