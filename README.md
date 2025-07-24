# Benchmark for Reduction Techniques Paper

This benchmark uses [mlr3](https://mlr3.mlr-org.com/) for task and learner setup and [batchtools](https://mllg.github.io/batchtools/index.html) for controlling the experiment execution on a local machine.


## File Structure

- `config.yml`: Defines general experiment settings for different scenarios, i.e. production mode, trial mode or debug mode
  - Loaded automatically via `.Rprofile` using the [`config`](https://rstudio.github.io/config/) package and stored in `conf` list.
  - Config modes (set in `.Renviron`):
    - Debug mode does not use fallbacks and encapsulation, so this is where error emssages are easiest to find
    - Trial mode uses a small tuning budget, allowing for testing in otherwise production-ready circumstances
    - Production mode is meant for the actual experiment
    - "Default" case is equivalent to production mode, just using a different name.
- `benchmark.R`: Main entrypoint -- defines the experiment and creates batchtools registry (sourcing other scripts).
  - `tasks.R`: Loads datasets from `datasets/`, converts them to mlr3 tasks
      - creates resamplings which are saved to `resamplings`, and if stored resamplings exist there it will use these instead of recreating them
        (this ensures robustness with regard to all learners being guaranteed to see the same data / resampling splits)
  - `learners.R: Defines base learner and tuner configuration including preprocessing pipelines and tuning budget.
  - `registries/`: Holds batchtools registries, separately for configuration modes.
- `submit.R`: Script to submit and manage experiment jobs.
- `results.R`: Collect results and score them using defined measures.
- `datasets/`: Holds survival datasets as `.rds` files. All files there will be used as inputs for `tasks.R`.
- `R/helpers.R`: General helper functions
- `batchtools.conf.R`: Batchtools config file ensuring the experiment can be run on the local machine without using excessive resources.

## Setup

[renv](https://docs.posit.co/ide/user/ide/guide/environments/r/renv.html) controls dependencies, and when you start an R session in the project root it will bootstrap itself and prompt you to run `renv::restore()` to install all required packages. This may take a moment.

If ytou encounter issues with renv you cannot resolve, comment out the line `source("renv/activate.R")` in `.Rprofile` and restart the R session. You will need to install packages manually, and in particular we rely on `mlr3tuning@1.3.0` instead of the latest CRAN release, so please ensure you install the correct version, for example using `pak` like this:

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
  "mlr3",
  "mlr3learners",
  "mlr3tuning@1.3.0",
  "mlr-org/mlr3mbo",
  "mlr3extralearners",
  "mlr-org/mlr3proba",
  # learners
  "glmnet",
  "xgboost",
  "randomForestSRC"
))
```
## Running the experiment

1. Source `benchmark.R`, creating a batchtools registry.
2. Submit jobs via `submitJobs()`, depending on available hardware etc.
3. Collect and score results in `results-processing.R`
4. Create figures and tables with `results-processing.R`
