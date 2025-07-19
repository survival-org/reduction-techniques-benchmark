# Circumvent srcref issue https://github.com/rstudio/renv/issues/1713
options("install.opts" = "--without-keep.source")
options("renv.config.pak.enabled" = TRUE)

if (file.exists("renv/activate.R")) {
  source("renv/activate.R")
} else {
  message("Found no renv/activate.R!")
}

# Using active config as set per R_CONFIG_ACTIVE environment var, see config.yml
# See https://rstudio.github.io/config/articles/config.html
if (
  requireNamespace("config", quietly = TRUE) &
    requireNamespace("cli", quietly = TRUE)
) {
  local({
    config_profile = Sys.getenv('R_CONFIG_ACTIVE', unset = 'default')
    cli::cli_alert_info("Loading config {.val {config_profile}}")
  })
  conf = config::get()
} else {
  warning("Install the 'config' and 'cli' packages first!")
}


# Trying to ensure learners don't use more resources than they should
# For e.g. XGBoost
Sys.setenv(OMP_NUM_THREADS = 1)
Sys.setenv(OPENBLAS_NUM_THREADS = 1)
Sys.setenv(OMP_THREAD_LIMIT = 1)
# MKL is an Intel-specific thing
Sys.setenv(MKL_NUM_THREADS = 1)
if (requireNamespace("data.table", quietly = TRUE)) {
  try(data.table::setDTthreads(1))
}

options(
  datatable.print.class = TRUE,
  datatable.print.keys = TRUE,
  batchtools.progress = TRUE
)

# Load script with helpers here to ensure its contents are available always
source("R/helpers.R")

# Make renv pick up dependencies
if (FALSE) {
  library("survival")
  library("randomForestSRC")
  library("ranger")
  library("pammtools")
  library("xgboost")
  library("mlr3learners")
  library("mlr3mbo")
  library("DiceKriging") # mbo
  library("rgenoud") # mbo
  library("mlr3proba")
  library("mlr3extralearners")
  library("callr")
  require("config")
  library("pracma")
  library("mlr3viz")
  library("patchwork")
  library("adegenet") # for a synthetic dataset eHGDP
}
