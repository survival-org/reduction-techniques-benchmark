# Benchmark for Reduction Techniques Paper

This benchmark uses [mlr3](https://mlr3.mlr-org.com/) for task and learner setup and [batchtools](https://mllg.github.io/batchtools/index.html) for controlling the experiment execution on a local machine.


## File Structure

- `config.yaml`: Defines general experiment settings for different scenarios, i.e. production mode, trial mode or debug mode
  - Loaded automatically via `.Rprofile` using the [`config`](https://rstudio.github.io/config/) package and stored in `conf` list.
  - Config modes (set in `.Renviron`):
    - Debug mode does not use fallbacks and encapsulation, so this is where error emssages are easiest to find
    - Trial mode uses a small tuning budget, allowing for testing in otherwise production-ready circumstances
    - Production mode is meant for the actual experiment
    - "Default" case is equivalent to production mode, just using a different name.
- `benchmark.R`: Main entrypoint -- defines the experiment and creates batchtools registry (sourcing other scripts).
  - `tasks.R`: Loads datasets from `datasets/`, converts them to mlr3 tasks, and creates resamplings which are saved to `resamplings`
  - `learners.R: Defines base learner and tuner configuration including preprocessing pipelines and tuning budget.
  - `registries/`: Holds batchtools registries, separately for configuration modes.
- `submit.R`: Script to submit and manage experiment jobs.
- `results.R`: Collect results and score them using defined measures.
- `datasets/`: Holds survival datasets as `.rds` files. All files there will be used as inputs for `tasks.R`.
- `R/helpers.R`: General helper functions
- `batchtools.conf.R`: Batchtools config file ensuring the experiment can be run on the local machine without using excessive resources.

## Setup

Currently there is no [renv]() setup yet to control dependencies, so for initialization you should at least install the following:

```r
if (!requireNamespace("pak", quietly = TRUE)) {
  install.packages("pak")
}

pak::pak(c(
  # infrastructure
  "batchtools",
  "config",
  "cli",
  "fs",
  # mlr3 ecosystem
  "mlr3verse",
  "mlr3mbo",
  "mlr3extralearners",
  "mlr3proba",
  # learners
  "glmnet",
  "xgboost",
  "ranger",
  "randomForestSRC"
))
```



1. Adjust preprocessing pipelines as needed in `learners.R`
2. Ensure selected tasks are set up as needed in `tasks.R`
3. Adapt `benchmark.R` for learners and tuning spaces

## Running the experiment

1. Source `batchmark.R`, creating a batchtools registry.
2. Submit josb via `submitJobs()`, depending on available hardware etc.
3. Collect and score results in `results.R`
