## ==================================================================================================#
# Inflation and redistribution in Turkey
# May 2025
# EI
## ==================================================================================================#

#Packages used across the project.
required_packages <- c(
  "tidyverse", "openxlsx", "readxl", "Hmisc", "purrr", "lfe", "kableExtra",
  "dplyr", "tidyr", "knitr", "httr2", "jsonlite", "readr", "arrow", "duckdb",
  "DBI", "haven"
)

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

install_if_missing(required_packages)

invisible(lapply(required_packages, library, character.only = TRUE))

rm(cran_mirror, install_if_missing)
## ==================================================================================================#

options(scipen = 999, digits = 10)

## ==================================================================================================#

dominance_threshold_theta <- 0.5

persistence_threshold <- 2

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