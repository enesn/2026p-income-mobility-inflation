## ==================================================================================================#
# Relative Income Mobility around an Inflationary Shock
# May 2025
# EI
## ==================================================================================================#
# Run-all orchestrator. Sources _setup.R and every numbered script in order, in one workspace, and
# reports how long each step took. Nothing else in the project sources anything: the scripts still
# run one by one by hand exactly as before, this file only chains them.
#
#   source("00-run-all.R")     from an R session, or     Rscript 00-run-all.R
#
# The one decision a run needs is where the micro data comes from — rebuilt from the raw TUIK
# releases, or read as the already-merged file. It is asked once, on the first run, and remembered
# in cached-micro-data/ingest-source so later runs are not asked again.
## ==================================================================================================#

RUN_ALL_STARTED <- Sys.time()

## ==================================================================================================#
# Progress and timing
## ==================================================================================================#
# Each step prints a bar before it starts, so a long run shows where it is, and its elapsed time when
# it finishes; the same numbers are repeated as a table at the end.

format_duration <- function(seconds) {
  if (seconds < 60) return(sprintf("%.1fs", seconds))
  sprintf("%dm %04.1fs", as.integer(seconds) %/% 60, seconds %% 60)
}

progress_bar <- function(done, total, width = 30) {
  filled <- round(width * done / total)
  sprintf("[%s%s] %3.0f%%", strrep("#", filled), strrep(".", width - filled), 100 * done / total)
}

rule <- function(char = "-") cat(strrep(char, 100), "\n", sep = "")

step_timings <- data.frame(script = character(), seconds = numeric())

# Steps are run for their side effects on the global workspace: each script builds objects the next
# one uses, so source() writes into globalenv() (its default) rather than into this function.
run_step <- function(script, position, total) {
  cat("\n")
  rule("=")
  cat(progress_bar(position - 1, total), sprintf("  step %d/%d  %s\n", position, total, script), sep = "")
  rule("=")

  started <- Sys.time()
  outcome <- try(source(script), silent = TRUE)
  elapsed <- as.numeric(difftime(Sys.time(), started, units = "secs"))

  step_timings <<- rbind(step_timings, data.frame(script = script, seconds = elapsed))

  if (inherits(outcome, "try-error")) {
    cat("\n✖ ", script, " failed after ", format_duration(elapsed), "\n\n", sep = "")
    stop(conditionMessage(attr(outcome, "condition")), call. = FALSE)
  }

  cat("\n✔ ", script, " done in ", format_duration(elapsed), "\n", sep = "")
  invisible(NULL)
}

## ==================================================================================================#
# Environment
## ==================================================================================================#
# _setup.R is timed like any other step because it is not always quick: on a fresh clone it restores
# the whole renv library before anything else can run.

cat("\n")
rule("=")
cat("Relative Income Mobility around an Inflationary Shock — full run\n")
cat(format(RUN_ALL_STARTED, "%Y-%m-%d %H:%M:%S"), "\n", sep = "")
rule("=")

setup_started <- Sys.time()
source("_setup.R")
step_timings <- rbind(
  step_timings,
  data.frame(script = "_setup.R",
             seconds = as.numeric(difftime(Sys.time(), setup_started, units = "secs")))
)
cat("\n✔ _setup.R done in ",
    format_duration(step_timings$seconds[step_timings$script == "_setup.R"]), "\n", sep = "")
rm(setup_started)

## ==================================================================================================#
# Where the micro data comes from
## ==================================================================================================#
# Two ingest routes produce the same silc0824 panel:
#
#   raw       02-ingest-raw-micro.R       merges the eleven raw TUIK releases in raw-micro-data/.
#                                         Only possible on a machine that holds those releases,
#                                         which are not redistributable and so are not in the repo.
#   combined  02-ingest-combined-micro.R  reads the already-merged panel as a single file, from the
#                                         local cache or, failing that, from Dropbox.
#
# The answer is a property of the machine, not of the run, so it is asked once and written to
# INGEST_PREF_FILE. Delete that file to be asked again. To override for a single run without
# changing the stored answer, set ingest_source <- "raw" (or "combined") before sourcing this file.

INGEST_PREF_FILE <- "cached-micro-data/ingest-source"

INGEST_SCRIPTS <- c(raw = "02-ingest-raw-micro.R", combined = "02-ingest-combined-micro.R")

ask_ingest_source <- function() {
  cat("\nThe raw TUIK releases are present in raw-micro-data/. How should the micro data be built?\n\n")
  cat("  [1] Combine the raw releases from scratch   (", INGEST_SCRIPTS[["raw"]], ")\n", sep = "")
  cat("  [2] Read the already-merged panel instead   (", INGEST_SCRIPTS[["combined"]], ")\n", sep = "")
  cat("\nAsked once only — the answer is remembered in ", INGEST_PREF_FILE, ".\n", sep = "")

  repeat {
    choice <- trimws(readline("Choice [1/2]: "))
    if (choice == "1") return("raw")
    if (choice == "2") return("combined")
    cat("Please answer 1 or 2.\n")
  }
}

resolve_ingest_source <- function() {
  # An ingest_source set by hand before sourcing wins, and is deliberately not written to the
  # preference file: a one-off override should not silently become the machine's stored answer.
  if (exists("ingest_source", envir = globalenv(), inherits = FALSE)) {
    override <- get("ingest_source", envir = globalenv())
    stopifnot(override %in% names(INGEST_SCRIPTS))
    cat("\n▶ Ingest source set for this run: ", override, " (stored preference not changed)\n", sep = "")
    return(override)
  }

  if (file.exists(INGEST_PREF_FILE)) {
    remembered <- trimws(readLines(INGEST_PREF_FILE, n = 1, warn = FALSE))
    if (remembered %in% names(INGEST_SCRIPTS)) {
      cat("\n↩️ Ingest source remembered from a previous run: ", remembered,
          "  (delete ", INGEST_PREF_FILE, " to be asked again)\n", sep = "")
      return(remembered)
    }
    cat("\n⚠️ Ignoring unreadable ", INGEST_PREF_FILE, " — asking again\n", sep = "")
  }

  chosen <- if (!raw_micro_data_complete()) {
    # Nothing to ask: without the raw releases the merged file is the only route.
    cat("\n▶ Raw TUIK releases not found in raw-micro-data/ — reading the already-merged panel\n")
    "combined"
  } else if (interactive()) {
    ask_ingest_source()
  } else {
    # Rscript cannot answer a prompt, and 02-ingest-combined-micro.R would block on its Dropbox
    # browser if its cache were cold. With the raw releases on disk, rebuilding is the route that
    # is guaranteed to finish unattended.
    cat("\n▶ Non-interactive session with the raw releases present — combining them from scratch\n")
    "raw"
  }

  dir.create(dirname(INGEST_PREF_FILE), showWarnings = FALSE, recursive = TRUE)
  writeLines(chosen, INGEST_PREF_FILE)
  cat("   Remembered in ", INGEST_PREF_FILE, " — delete that file to be asked again.\n", sep = "")

  chosen
}

ingest_source <- resolve_ingest_source()

## ==================================================================================================#
# The run
## ==================================================================================================#

analysis_steps <- c(
  "01-macro-trend.R",
  INGEST_SCRIPTS[[ingest_source]],
  "03-sample-restriction.R",
  "04-define-income.R",
  "05-income-decomposition.R",
  "06-class-mobility.R",
  "07-composition-contributions.R",
  "08-class-mobility-extended.R",
  "09-mean-delta-rank.R",
  "10-baseline-models.R",
  "11-appendix.R"
)

missing_steps <- analysis_steps[!file.exists(analysis_steps)]
if (length(missing_steps) > 0) {
  stop("Missing script(s): ", paste(missing_steps, collapse = ", "), call. = FALSE)
}

for (i in seq_along(analysis_steps)) {
  run_step(analysis_steps[i], position = i, total = length(analysis_steps))
}

## ==================================================================================================#
# Summary
## ==================================================================================================#

total_seconds <- as.numeric(difftime(Sys.time(), RUN_ALL_STARTED, units = "secs"))

cat("\n")
rule("=")
cat(progress_bar(1, 1), "  run complete in ", format_duration(total_seconds), "\n", sep = "")
rule("=")

# A bar of where the time went. It is scaled to the slowest step rather than to the total: with a
# dozen steps no single one is a large share of the run, so bars drawn against the total all come out
# the same length and say nothing.
step_timings$share <- step_timings$seconds / max(step_timings$seconds)

for (i in seq_len(nrow(step_timings))) {
  cat(sprintf("  %-32s %12s  %s\n",
              step_timings$script[i],
              format_duration(step_timings$seconds[i]),
              strrep("#", max(1, round(20 * step_timings$share[i])))))
}

rule("-")
cat("  Ingest source : ", ingest_source, " (", INGEST_SCRIPTS[[ingest_source]], ")\n", sep = "")
cat("  Figures/tables: outputs-included/\n")
cat("  Finished      : ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n", sep = "")
rule("=")

# ingest_source goes too: left behind, a second run in the same session would read it as a one-off
# override of itself rather than consulting the preference file. step_timings is kept, so the run can
# be inspected afterwards.
rm(RUN_ALL_STARTED, total_seconds, analysis_steps, missing_steps, i, ingest_source,
   ask_ingest_source, resolve_ingest_source, run_step)
