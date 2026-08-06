# Relative Income Mobility around an Inflationary Shock

Replication package for the paper. The code builds a balanced four-year panel from the Turkish
Statistical Institute (TUIK) SILC/GYKA panel releases, classifies households by the income source
they persistently live on, and estimates how the 2018–2023 inflation episode moved those classes
around the income distribution.

Running the pipeline reproduces every figure and table in the paper into [outputs-included/](outputs-included/).

---

## 1. Requirements

| | |
|---|---|
| R | 4.4.2 (the version recorded in [renv.lock](renv.lock)) or use the Docker container |
| Package management | [renv](https://rstudio.github.io/renv/) — activated automatically by [.Rprofile](.Rprofile) |
| Disk | ~2 GB for the raw releases plus caches |
| Platform | macOS/Linux/Windows; nothing platform-specific is used |

No packages need to be installed by hand. [_setup.R](_setup.R) calls `renv::restore()` on a fresh
clone and installs exactly the versions pinned in `renv.lock`. If the session was started with
`--vanilla` (so `.Rprofile` never ran and renv is not active), `_setup.R` falls back to installing
current CRAN versions and says so.

---

## 2. How to run the pipeline

### The whole thing

From the project root, either

```r
source("00-run-all.R")     # from an interactive R session, or
```

```sh
Rscript 00-run-all.R       # from the shell
```

[00-run-all.R](00-run-all.R) sources [_setup.R](_setup.R) and then every numbered script in order in
one workspace, printing a progress bar per step, the elapsed time of each step, and a timing table at
the end that shows where the time actually went. It stops at the first script that errors and reports
which one.

### One step at a time

The orchestrator is only a convenience — nothing else in the project sources anything, so the scripts
still run by hand exactly as they did before. Source `_setup.R` once, then the numbered scripts in
order in the same session; each one leaves objects in the global workspace that the next one uses.


### The steps

| Script | What it does | Output |
|---|---|---|
| [_setup.R](_setup.R) | Packages, global options, θ and persistence thresholds, helper functions, the raw-release manifest | — |
| [01-macro-trend.R](01-macro-trend.R) | Annual inflation 2013–2023 and the three inflation regimes | Fig 1 |
| [02-ingest-raw-micro.R](02-ingest-raw-micro.R) | Merges the eleven raw TUIK releases into the `silc0824` panel | — |
| [02-ingest-combined-micro.R](02-ingest-combined-micro.R) | Alternative: reads the same panel as one already-merged file | — |
| [03-sample-restriction.R](03-sample-restriction.R) | Balanced four-year panel of adults in size-stable households; panel weights | — |
| [04-define-income.R](04-define-income.R) | Personal, household and equal-split income, deflated to real terms | — |
| [05-income-decomposition.R](05-income-decomposition.R) | Income deciles, decile transitions, and the income changes behind them | — |
| [06-class-mobility.R](06-class-mobility.R) | Social classes: the income source each person persistently lives on | Fig 2 |
| [07-composition-contributions.R](07-composition-contributions.R) | Which sources and margins drive the moves up and down | Fig 5 |
| [08-class-mobility-extended.R](08-class-mobility-extended.R) | Adds head's education and occupation, and the minimum wage | — |
| [09-mean-delta-rank.R](09-mean-delta-rank.R) | How far each class moves in the distribution, year by year | Fig 3 |
| [10-baseline-models.R](10-baseline-models.R) | Baseline estimation samples and models | Fig 4, Tables 1–3 |
| [11-appendix.R](11-appendix.R) | Appendix tables | Tables A1–A4 |

Everything is written to [outputs-included/](outputs-included/): figures as `.pdf`, tables as `.tex`
files the paper can `\input{}` directly. Each script prints the path of every file it writes, and says
`NOT saved` rather than confirming a file that is not on disk.

---

## 3. Data, and the two ingest routes

The pipeline needs three kinds of input. Only the third is in this repository.

**a. Raw TUIK micro data — not redistributable, not in the repo.**
Eleven overlapping GYKA panel releases (2008–2011 through 2021–2024) in
[raw-micro-data/](raw-micro-data/), each shipping four files (`_f` individual income, `_fk`
individual register, `_h` household income, `_hk` household register). These are obtained from TUIK
under a micro data access agreement and cannot be shared by us. The expected folder layout and
delimiters are listed in `raw_panel_releases` in [_setup.R:101](_setup.R#L101) — keep the release
folders exactly as TUIK distributes them and the paths will match.

**b. The already-merged panel — served online, access-controlled.**
The same eleven releases pre-merged into one file, fetched over the Dropbox API. This route exists so
that a replicator or a collaborator who already holds valid official TUIK micro data access does not have to re-download
and re-merge eleven releases. See §4 for credentials.

**c. Auxiliary macro data — included in the repo.**
[other-input-data/](other-input-data/) holds `evds_inflation_fxrate.xlsx` (CBRT EVDS inflation and FX),
`tuik_cpi.xlsx` (CPI deflator) and `minimum_wage.xlsx`. These are public series and need no credentials.

### Choosing the route

**The ingest source is an option that depends on what data you have access to.** The two `02-` scripts
are interchangeable: they produce the same `silc0824` panel, and everything downstream is identical.

| Route | Script | Requires | Use when |
|---|---|---|---|
| `raw` | [02-ingest-raw-micro.R](02-ingest-raw-micro.R) | All eleven releases present in `raw-micro-data/` | You hold the TUIK releases locally |
| `combined` | [02-ingest-combined-micro.R](02-ingest-combined-micro.R) | Dropbox credentials (§4), or an already-populated cache | You have official micro data access but not the eleven releases on this machine |

`00-run-all.R` resolves this once and remembers the answer, because it is a property of the machine
rather than of the run:

- If the raw releases are **not** complete in `raw-micro-data/`, there is nothing to ask — it uses
  `combined`.
- If they are complete and the session is **interactive**, it asks once, `[1]` raw or `[2]` combined.
- If they are complete and the session is **non-interactive** (`Rscript`), it uses `raw`, which is the
  route guaranteed to finish unattended — `combined` could otherwise block on a credential prompt.

The answer is written to `cached-micro-data/ingest-source` (gitignored). **Delete that file to be asked
again.** To override for a single run without changing the stored answer, set the variable before
sourcing:

```r
ingest_source <- "combined"   # or "raw"
source("00-run-all.R")
```

### Caching

Both routes cache aggressively, so ingest is a one-off cost and later runs skip it:

- `raw` writes the merged panel to `cached-micro-data/silc0824-from-raw.parquet`. When that file
  exists the raw folders are not touched at all. Set `rebuild_raw_cache <- TRUE` before sourcing, or
  delete the file, to rebuild from raw.
- `combined` caches the downloaded file in `cached-micro-data/`, and for csv sources also writes a
  parquet sidecar beside it so later runs never re-parse hundreds of MB of text. A fully-cached run
  makes no network calls and therefore **never prompts for credentials at all**.
- `cached-micro-data/last_path` records which file on Dropbox was selected, so the interactive folder
  browser is skipped on later runs. Delete it to pick a different file.

The cache directory is gitignored. A missing cache is not an error — it just means the next run pays
the ingest cost again.

**To start from a clean slate, delete `cached-micro-data/`.** That clears everything the project
remembers about a machine at once: the cached panel and parquet sidecars, the remembered ingest route
(`ingest-source`), the remembered Dropbox selection (`last_path`), and the cached credentials
(`.dropbox_auth` / `.dropbox_refresh`). The next run then asks every question again from scratch.
Individual files can be deleted on their own if only one preference needs resetting. Note that
`last_path` is tracked in git, so restore it with `git checkout cached-micro-data/last_path` to get
the May 2025 selection back without browsing for it.

### Which version of the data to select

The Dropbox folder holds the merged panel under dated folders, one per vintage. **The published
results come from the May 2025 vintage: `/202505/silc0824.parquet`.** When the folder browser opens,
open `202505` and select `silc0824.parquet`; picking any other vintage will not reproduce the numbers
in the paper. That path is the one committed in [cached-micro-data/last_path](cached-micro-data/last_path),
so it is also what a run reuses automatically once the file is cached.

---

## 4. Credentials for the online ingest route

Nothing secret is stored in this repository, and no key or token is committed anywhere.

The `combined` route reaches a copy of the merged panel held on Dropbox. **That copy is only made
available to replicators who already hold valid, current official TUIK micro data access.** It is not
an alternative to the TUIK access agreement — it is a convenience for people who are already entitled
to the data.

**To replicate the paper via this route, request the token / app key and secret from the corresponding
author, and include evidence of your TUIK micro data access agreement in the request.** Credentials
will not be issued otherwise. The corresponding author's contact address is the one listed on the
paper.

[02-ingest-combined-micro.R](02-ingest-combined-micro.R) accepts two credential shapes and works out
which one it was handed by probing the API, not by parsing the string:

| Mode | You provide | Behaviour |
|---|---|---|
| **Replicator** | A short-lived Dropbox access token | Cached in `cached-micro-data/.dropbox_auth`. When it expires you are prompted again — there is nothing to renew it with. |
| **Pipeline** | A refresh token, plus app key and secret | Cached in `cached-micro-data/.dropbox_refresh`. Dropbox refresh tokens do not expire, so every later run silently mints a fresh access token with no prompt. |

Both cache files are written owner-read-only (`0600`) and are gitignored. Delete either file to force
a fresh prompt. Credentials are prompted for through `getPass` so they are not echoed to the terminal
or captured in the session history.

Note that `Rscript 00-run-all.R` cannot answer a prompt. On a machine that needs the `combined` route
with a cold cache, run the ingest once interactively to populate the cache and the credential file;
after that, unattended runs work.

---

## 5. Versions

**`main`** is the baseline analysis as it stood **before the third revision was submitted to *Oxford
Economic Papers***, and is the reference implementation.

**`cosmetic-for-better-replication`** (this branch) is cosmetic and speed work only — **no estimate,
sample, specification or reported number changes.** Every refactor was made to produce output
identical to `main`:

- **Reproducibility** — `renv` lockfile so package versions are pinned; `00-run-all.R` runs the whole
  pipeline in order (on `main` the scripts were sourced one at a time by hand); the online ingest
  route and the ingest-source choice (§3) are new, `main` could only rebuild from the raw releases.
- **Speed** — within-person lags in `05` rewritten as a single column shift (`lag_person()`, ~100×
  faster on the heaviest step); parquet caching so ingest is paid once; `10` builds each estimation
  sample once instead of repeating the filter chain inside every `lm()`; dead code and obsolete input
  files removed.
- **Readability** — the eleven repeated raw-release blocks collapsed into one loop over a manifest;
  seven repeated dominance/persistence definitions in `06` collapsed into one `across()`; progress
  headers and `saved` notes throughout; tables written to `.tex` instead of only printed.

To reproduce the pre-revision baseline instead, check out `main` and source the scripts by hand in
numeric order — `main` has no lockfile, so package versions are whatever is installed.

