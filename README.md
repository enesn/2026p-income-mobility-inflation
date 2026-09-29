# Relative Income Mobility around an Inflationary Shock

Replication files for the paper. The code uses the Turkish Statistical Institute (TUIK) SILC panel surveys to build a balanced four-year panel grouped by each household's persistent income source, and estimates how the 2018-2023 inflation episode shifted households' income rank.

Running the code reproduces all the figures and tables in the paper. All results are saved in [outputs-included/](outputs-included/): figures as PDF and tables as LaTeX.

## Requirements

R 4.6. On the first run, the exact versions of the packages used in the paper will be installed, so there is no need to install them manually. 

## Data

Things you will need:

1. TUIK micro data (not provided). The eleven overlapping panel releases, from 2008-2011 to 2021-2024. These are available from TUIK under a micro data access agreement, and we are unable to share them. Place them in [raw-micro-data/](raw-micro-data/), with the folder structure and filenames TUIK provides.
2. Public macro series (provided). [other-input-data/](other-input-data/) contains inflation and exchange rates from the Central Bank's EVDS, the TUIK consumer price index used as the deflator, and the minimum wage.

If you have access to TUIK micro data, we can provide you with access to a single file with the eleven releases already merged, to replicate the results in the paper. When the code prompts you to choose which file to use, choose `202505/silc0824.parquet` (the May 2025 version). Other versions will not reproduce the published numbers.

## Running the code

From the project folder, in R:

```r
source("00-run-all.R")
```

This will run all the scripts in order and print the running time of each step. It will ask you to choose between building the panel from the eleven TUIK releases or from the merged file. The two options yield identical data. The code will remember your choice and save the prepared panel, so subsequent runs will be much faster. To change the choice, delete the folder `cached-micro-data/`.

You can also run the scripts individually: run [_setup.R](_setup.R) first, then the numbered scripts in order, in the same R session.

## What each script does

| Script | Content | Output |
|---|---|---|
| [_setup.R](_setup.R) | Packages, thresholds for income-source dominance and persistence, shared functions | |
| [01-macro-trend.R](01-macro-trend.R) | Annual inflation 2013-2023 and the three inflation periods | Figure 1 |
| [02-ingest-raw-micro.R](02-ingest-raw-micro.R) | Merges the eleven TUIK releases into one panel | |
| [02-ingest-combined-micro.R](02-ingest-combined-micro.R) | Alternative to the above; reads the already-merged file | |
| [03-sample-restriction.R](03-sample-restriction.R) | Balanced four-year panel of adults in households of stable size; panel weights | |
| [04-define-income.R](04-define-income.R) | Personal, household and equal-split income in real terms | |
| [05-income-decomposition.R](05-income-decomposition.R) | Income deciles, decile transitions and the income changes behind them | |
| [06-class-mobility.R](06-class-mobility.R) | Classes defined by persistent main income source | Figure 2 |
| [07-composition-contributions.R](07-composition-contributions.R) | Which income sources drive upward and downward moves | Figure 5 |
| [08-class-mobility-extended.R](08-class-mobility-extended.R) | Adds the household head's education and occupation, and the minimum wage | |
| [09-mean-delta-rank.R](09-mean-delta-rank.R) | Average rank change of each class, year by year | Figure 3 |
| [10-baseline-models.R](10-baseline-models.R) | Baseline regressions | Figure 4, Tables 1-3 |
| [11-appendix.R](11-appendix.R) | Appendix tables | Tables A1-A6 |

## Version history

1. Original code: [44a670f](https://github.com/enesn/2026p-income-mobility-inflation/commit/44a670ffe80a9986bbefd572b9893ad36ed3e643). This is the code as I wrote it for the first submission and the first two OEP revision rounds.
2. Reproducibility revision (branch `cosmetic-for-better-replication`): [34 commits](https://github.com/enesn/2026p-income-mobility-inflation/compare/44a670ffe80a9986bbefd572b9893ad36ed3e643...632f81e81060ff4fa8fc7a7688a513f77a2b8a49). I reworked the original code with an AI coding assistant to make it easier to reproduce. The changes add a one-command pipeline, renv, cached data ingestion and progress output.
3. OEP R&R, round three (branch `oep-RR-round-three`): [PR #1](https://github.com/enesn/2026p-income-mobility-inflation/pull/1). This covers the changes in response to the third-round reviewer comments.