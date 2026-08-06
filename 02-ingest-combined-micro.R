# ============================================================
# LOCAL CACHE
# ============================================================
CACHE_DIR <- "cached-micro-data"
dir.create(CACHE_DIR, showWarnings = FALSE, recursive = TRUE)

# ============================================================
# AUTH
# ============================================================
# Credentials are never stored in this script. They are prompted for
# interactively the first time this runs, then cached locally (gitignored,
# owner-read-only) so later runs are not prompted again. Delete
# cached-micro-data/.dropbox_auth or cached-micro-data/.dropbox_refresh to
# force a fresh prompt.
#
# AUTH_CACHE_FILE holds a raw access-token header (replicator mode): once
# it expires there is nothing to renew it with, so the prompt returns.
# REFRESH_CACHE_FILE holds refresh credentials (pipeline mode): Dropbox
# refresh tokens don't expire, so it lets every future run silently mint
# a new access token with no prompt at all.
AUTH_CACHE_FILE <- file.path(CACHE_DIR, ".dropbox_auth")
REFRESH_CACHE_FILE <- file.path(CACHE_DIR, ".dropbox_refresh")

prompt_secret <- function(prompt) {
  if (requireNamespace("getPass", quietly = TRUE)) {
    getPass::getPass(prompt)
  } else {
    readline(prompt)
  }
}

# Probes the API with a full Authorization header value (e.g. "Bearer xxx")
# to check whether it is still accepted by Dropbox. Access tokens are
# short-lived, so a cached header can go stale between runs.
auth_header_valid <- function(header) {
  resp <- request("https://api.dropboxapi.com/2/check/user") |>
    req_method("POST") |>
    req_headers(
      Authorization = header,
      "Content-Type" = "application/json"
    ) |>
    req_body_json(list(query = "ping")) |>
    req_error(is_error = function(resp) FALSE) |>
    req_perform()

  resp_status(resp) == 200
}

# Token type is determined by behavior, not by format: Dropbox does not
# guarantee token prefixes, so we probe the API instead of parsing the string.
works_as_access_token <- function(tok) {
  auth_header_valid(paste("Bearer", tok))
}

# Exchanges a refresh token for a fresh short-lived access token. Used both
# for the initial refresh-token prompt and for silent renewal on later runs.
refresh_access_token <- function(key, secret, refresh_token) {
  refresh_resp <- request("https://api.dropboxapi.com/oauth2/token") |>
    req_auth_basic(key, secret) |>
    req_body_form(
      grant_type = "refresh_token",
      refresh_token = refresh_token
    ) |>
    req_error(is_error = function(resp) FALSE) |>
    req_perform()

  refresh_data <- resp_body_json(refresh_resp)

  if (resp_status(refresh_resp) != 200 || is.null(refresh_data$access_token)) {
    stop(paste(
      "Refresh token exchange failed.",
      "Check the app key/secret/refresh token (may be expired or revoked),",
      "or delete cached-micro-data/.dropbox_refresh to re-enter them."
    ))
  }

  paste("Bearer", refresh_data$access_token)
}

# Returns the "Bearer <token>" Authorization header. Prompts for
# credentials only when no cached refresh token or access token is
# available, or when a cached access token has expired.
get_dropbox_auth <- function() {
  if (file.exists(REFRESH_CACHE_FILE)) {
    creds <- readLines(REFRESH_CACHE_FILE)
    cat("↩️ Renewing Dropbox access token from cached refresh token\n")
    return(refresh_access_token(creds[1], creds[2], creds[3]))
  }

  if (file.exists(AUTH_CACHE_FILE)) {
    cached_header <- readLines(AUTH_CACHE_FILE, n = 1)

    if (auth_header_valid(cached_header)) {
      cat("↩️ Reusing cached Dropbox credentials\n")
      return(cached_header)
    }

    cat("⚠️ Cached Dropbox credentials expired — re-authenticating\n")
    file.remove(AUTH_CACHE_FILE)
  }

  cat("Analyzing token signature...\n")
  token <- prompt_secret("Dropbox token: ")

  if (works_as_access_token(token)) {
    cat("▶ Token accepted as ACCESS token (replicator mode)\n")

    token_header <- paste("Bearer", token)

    writeLines(token_header, AUTH_CACHE_FILE)
    Sys.chmod(AUTH_CACHE_FILE, mode = "0600")
  } else {
    cat("▶ Direct auth failed — attempting REFRESH exchange (pipeline mode)...\n")

    key <- prompt_secret("Dropbox app key: ")
    secret <- prompt_secret("Dropbox app secret: ")

    token_header <- refresh_access_token(key, secret, token)

    # Cache the refresh credentials (not just the resulting access token)
    # so future runs can silently mint new access tokens with no prompt,
    # since Dropbox refresh tokens themselves don't expire.
    writeLines(c(key, secret, token), REFRESH_CACHE_FILE)
    Sys.chmod(REFRESH_CACHE_FILE, mode = "0600")
  }

  token_header
}

# Authentication is deferred until an API call actually needs it (folder
# navigation, or downloading a file that isn't cached yet), so runs that
# only read already-cached data never prompt or hit the network.
token_header <- NULL

get_token_header <- function() {
  if (is.null(token_header)) {
    cat("\nConnecting to Dropbox...\n")
    token_header <<- get_dropbox_auth()
  }
  token_header
}

# ============================================================
# LIST DROPBOX FOLDER CONTENTS
# ============================================================
list_dropbox_entries <- function(path = "", recursive = TRUE) {
  resp <- request("https://api.dropboxapi.com/2/files/list_folder") |>
    req_method("POST") |>
    req_headers(
      Authorization = get_token_header(),
      "Content-Type" = "application/json"
    ) |>
    req_body_json(list(path = path, recursive = recursive)) |>
    req_perform()

  data <- resp_body_json(resp)
  entries <- data$entries

  # Dropbox paginates results: keep fetching until has_more is FALSE
  while (isTRUE(data$has_more)) {
    resp <- request("https://api.dropboxapi.com/2/files/list_folder/continue") |>
      req_method("POST") |>
      req_headers(
        Authorization = get_token_header(),
        "Content-Type" = "application/json"
      ) |>
      req_body_json(list(cursor = data$cursor)) |>
      req_perform()

    data <- resp_body_json(resp)
    entries <- c(entries, data$entries)
  }

  entries
}

# ============================================================
# FOLDER NAVIGATION
# ============================================================
# Interactive browser over the Dropbox tree. Type an entry's number to
# open a folder or select a file, ".." to go up one level, "q" to quit.
# Returns the API path of the selected file (or NULL if quit), so the
# result can be passed directly to read_dropbox_file().
navigate_dropbox <- function(start_path = "") {
  current <- start_path

  repeat {
    entries <- list_dropbox_entries(current, recursive = FALSE)

    cat(sprintf("\n📂 %s\n", ifelse(current == "", "/ (root)", current)))

    if (length(entries) == 0) {
      cat("  (empty folder)\n")
    } else {
      for (i in seq_along(entries)) {
        icon <- ifelse(identical(entries[[i]]$`.tag`, "folder"), "📁", "📄")
        stamp <- ""
        if (!is.null(entries[[i]]$server_modified)) {
          stamp <- sprintf(
            "  (uploaded: %s)",
            sub("Z", "", sub("T", " ", entries[[i]]$server_modified))
          )
        }
        cat(sprintf("  [%d] %s %s%s\n", i, icon, entries[[i]]$name, stamp))
      }
    }

    choice <- trimws(readline("Number to open, '..' = up, 'q' = quit: "))

    if (tolower(choice) == "q") {
      return(invisible(NULL))
    }

    if (choice == "..") {
      if (current == "") {
        cat("Already at root.\n")
      } else {
        # Drop the last path segment; "" means back at root
        current <- sub("/[^/]+$", "", current)
      }
      next
    }

    idx <- suppressWarnings(as.integer(choice))

    if (is.na(idx) || idx < 1 || idx > length(entries)) {
      cat("Invalid selection, try again.\n")
      next
    }

    entry <- entries[[idx]]

    if (identical(entry$`.tag`, "folder")) {
      current <- entry$path_lower
    } else {
      cat("✔ Selected:", entry$path_display, "\n")
      return(entry$path_lower)
    }
  }
}

cache_path_for <- function(api_path) {
  file.path(CACHE_DIR, gsub("[^A-Za-z0-9._-]+", "_", sub("^/+", "", api_path)))
}

# ============================================================
# UNIVERSAL FILE READER
# ============================================================
# col_select (tidy-select, e.g. c(ID, YEAR)) reads only those columns
# into memory; it applies to csv and parquet and is ignored (with a
# warning) for other formats. Extra arguments in ... are forwarded to
# the format's reader.
#
# Caching: downloads are stored in CACHE_DIR. If a cached copy already
# exists and use_cache = TRUE (the default), it is used as-is with no
# Dropbox contact at all — so no authentication is triggered. Set
# use_cache = FALSE to force a fresh download (e.g. the source file
# changed on Dropbox and the cache needs to be refreshed).
#
# For csv files the download is not the slow part on a cached run —
# re-parsing hundreds of MB of text on every run is. So the first csv read
# also writes a parquet sidecar beside the cached file, and later runs read
# that instead: same columns and same types (the sidecar is written from
# the readr parse, not re-inferred by another reader), but far less work,
# and col_select is pushed down so unused columns are never read at all.
# The sidecar is rebuilt whenever the csv is newer, and is bypassed when
# extra reader arguments are supplied, since those can change how the csv
# parses and the sidecar records only the default parse.
read_dropbox_file <- function(api_path, col_select = NULL, ..., use_cache = TRUE) {
  local_file <- cache_path_for(api_path)

  if (use_cache && file.exists(local_file)) {
    cat("🗂️ Using cache:", local_file, "\n")
  } else {
    cat("📥 Downloading:", api_path, "\n")

    resp <- request("https://content.dropboxapi.com/2/files/download") |>
      req_headers(
        Authorization = get_token_header(),
        "Dropbox-API-Arg" = jsonlite::toJSON(list(path = api_path), auto_unbox = TRUE)
      ) |>
      req_perform()

    writeBin(resp_body_raw(resp), local_file)
  }

  ext <- tolower(tools::file_ext(api_path))

  if (ext == "csv") {
    parquet_sidecar <- paste0(local_file, ".parquet")
    sidecar_usable <- ...length() == 0

    if (sidecar_usable &&
        file.exists(parquet_sidecar) &&
        file.mtime(parquet_sidecar) >= file.mtime(local_file)) {
      cat("⚡ Reading parquet sidecar:", parquet_sidecar, "\n")
      return(arrow::read_parquet(parquet_sidecar, col_select = {{ col_select }}))
    }

    if (!sidecar_usable) return(read_csv(local_file, col_select = {{ col_select }}, ...))

    # Building the sidecar needs every column, so this one run parses the
    # whole csv even when col_select was given; the columns are dropped
    # after, and every later run reads only what it asked for.
    cat("⏳ Parsing csv and writing parquet sidecar (one-off):", parquet_sidecar, "\n")
    parsed <- read_csv(local_file)
    arrow::write_parquet(parsed, parquet_sidecar, compression = "zstd")

    if (!missing(col_select)) parsed <- dplyr::select(parsed, {{ col_select }})
    parsed
  } else if (ext %in% c("xlsx", "xls")) {
    if (!missing(col_select)) warning("col_select is ignored for Excel files")
    read_excel(local_file, ...)
  } else if (ext == "json") {
    if (!missing(col_select)) warning("col_select is ignored for JSON files")
    fromJSON(local_file, ...)
  } else if (ext == "parquet") {
    # arrow restores R attributes stored in the file's metadata (haven
    # variable/value labels, labelled classes); DuckDB drops them.
    arrow::read_parquet(local_file, col_select = {{ col_select }}, ...)
  } else {
    warning(paste("Unsupported format:", ext))
    readBin(local_file, "raw", file.size(local_file))
  }
}

# ============================================================
# SELECT & READ
# ============================================================
# The interactively chosen path is remembered in the cache dir. On later
# runs, if that file is already cached, the navigator is skipped and the
# data is read straight from the cache with no Dropbox contact — so a
# fully-cached run needs no authentication at all. Delete the last_path
# file, or run navigate_dropbox() manually, to pick anew.
LAST_PATH_FILE <- file.path(CACHE_DIR, "last_path")

selected_path <- NULL

if (file.exists(LAST_PATH_FILE)) {
  remembered <- readLines(LAST_PATH_FILE, n = 1)
  if (file.exists(cache_path_for(remembered))) {
    cat("↩️ Reusing cached selection:", remembered, "\n")
    selected_path <- remembered
  }
}

if (is.null(selected_path)) {
  selected_path <- navigate_dropbox()
  if (!is.null(selected_path)) writeLines(selected_path, LAST_PATH_FILE)
}

if (!is.null(selected_path)) silc0824 <- read_dropbox_file(selected_path)
