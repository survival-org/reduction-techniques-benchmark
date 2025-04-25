# Settings are defined in `config.yaml`, loaded via `config` R pkg in .Rprofile
# Result is `conf` object used throughout to retrieve relevant settings

# Packages ----------------------------------------------------------------

# Dependencies managed via renv. Manually update as necessary via renv::update()
# renv::restore(prompt = FALSE)
# rs = renv::status()
# if (!rs$synchronized) {
#   cli::cli_alert_danger("renv library not synchronized!")
# }

library(mlr3)
library(mlr3proba)
library(mlr3learners)
library(mlr3pipelines)
library(mlr3tuning)
library(mlr3mbo)
library(batchtools, warn.conflicts = FALSE)
library(mlr3batchmark)
requireNamespace("mlr3extralearners")

# Create Registry ---------------------------------------------------------
# Uses batchtools to submit jobs on local machine
# See batchtools.conf.R
# Relevant arguments:
# - ncpus: Number of CPU cores to use -> number of simultaneous jobs
# - max.load: Maximum system load after which no jobs will be submitted until reduces
#             Prevents overloading the system, e.g. for a 10 core machine set this to 8 or 9.
if (!fs::dir_exists(fs::path_dir(conf$reg_dir))) {
  fs::dir_create(fs::path_dir(conf$reg_dir))
}

if (fs::dir_exists(conf$reg_dir)) {
  if (config::is_active("production")) {
    cli::cli_abort(
      "Refusing to delete existing registry {.file {fs::path_rel(conf$reg_dir)}} in production mode!"
    )
  } else {
    cli::cli_alert_warning(
      "Deleting registry at {.file {fs::path_rel(conf$reg_dir)}}"
    )
    fs::dir_delete(conf$reg_dir)
  }
}

cli::cli_alert_info("Creating new registry {.val {conf$reg_name}}!")
reg = makeExperimentRegistry(
  conf$reg_dir,
  work.dir = here::here(),
  seed = conf$seed
)

# Tasks ---------------------------------------------------------------------------------------
source(here::here("tasks.R"))


# Learner / Tuner setup --------------------------------------------------

source(here::here("learners.R"))

# Set tuning measures -----------------------------------------------------
measures = list(
  msr("surv.cindex", id = "harrell_c"),
  msr("surv.brier", id = "isbs", p_max = 0.8, proper = FALSE, ERV = FALSE)
)

# Assemble learners -------------------------------------------------------
for (measure in measures) {
  cli::cli_h1("Assembling learners for {.val {measure$id}}")

  learners = list(
    KM = bl("surv.kaplan", id = "kaplan"),

    NEL = bl("surv.nelson", id = "nelson"),

    GLMN = wrap_auto_tune(
      bl("surv.cv_glmnet", id = "cv_glmnet", .encode = TRUE),
      cv_glmnet.alpha = p_dbl(0, 1)
    ),

    Pen = wrap_auto_tune(
      bl("surv.penalized", id = "penalized"),
      penalized.lambda1 = p_dbl(-10, 10, trafo = function(x) 2^x),
      penalized.lambda2 = p_dbl(-10, 10, trafo = function(x) 2^x)
    ),

    RFSRC = wrap_auto_tune(
      # Fixing ntime = 150 (current default) just to be explicit, as ranger's time.interest
      # is set to a non-default value and we ensure both use 150 time points for evaluation
      bl("surv.rfsrc", id = "rfsrc", ntree = 1000, ntime = 150),
      rfsrc.splitrule = p_fct(c("bs.gradient", "logrank")),
      rfsrc.mtry.ratio = p_dbl(0, 1),
      rfsrc.nodesize = p_int(1, 50),
      rfsrc.samptype = p_fct(c("swr", "swor")),
      rfsrc.sampsize.ratio = p_dbl(0, 1)
    ),

    RAN = wrap_auto_tune(
      # Adjusting time.interest (new as of 0.16.0) to 150, same as current RFSRC default
      bl("surv.ranger", id = "ranger", num.trees = 1000, time.interest = 150),
      ranger.splitrule = p_fct(c("C", "maxstat", "logrank")),
      ranger.mtry.ratio = p_dbl(0, 1),
      ranger.min.node.size = p_int(1, 50),
      ranger.replace = p_lgl(),
      ranger.sample.fraction = p_dbl(0, 1)
    ),

    # XGB/cox, uses breslow estimator internally via mlr3proba
    XGBCox = wrap_auto_tune(
      bl(
        "surv.xgboost.cox",
        id = "xgb_cox",
        tree_method = "hist",
        booster = "gbtree",
        early_stopping_rounds = 50,
        .encode = TRUE
      ),
      xgb_cox.nrounds = p_int(
        upper = 5000,
        tags = "internal_tuning",
        aggr = function(x) as.integer(mean(unlist(x)))
      ),
      xgb_cox.max_depth = p_int(1, 20),
      xgb_cox.subsample = p_dbl(0, 1),
      xgb_cox.colsample_bytree = p_dbl(0, 1),
      xgb_cox.eta = p_dbl(0, 1),
      xgb_cox.grow_policy = p_fct(c("depthwise", "lossguide"))
    )
  )

  mlr3misc::imap(learners, function(l, id) l$id = id)

  cli::cli_h2("Cleaning up and adding to registry")

  if (measure$id == "isbs") {
    cli::cli_alert_warning("Skipping {.val MBSTAFT} for ISBS measure!")
    learners$MBSTAFT = NULL

    cli::cli_alert_warning("Skipping {.val RRT} for ISBS measure!")
    learners$RRT = NULL

    cli::cli_alert_warning("Skipping {.val XGBAFT} for ISBS measure!")
    learners$XGBAFT = NULL

    cli::cli_alert_warning("Skipping {.val SSVM} for ISBS measure!")
    learners$SSVM = NULL
  }

  # custom grid design (with instantiated resamplings)
  grid = mlr3misc::cross_join(
    list(task = tasks, learner = learners),
    sorted = FALSE
  )
  grid$resampling = rep(resamplings, each = length(learners))
  # If we want to keep the AutoTuner's tuning instance we have to set store_models
  # here, which counter intuitively is different from the `store_models` option in auto_tuner()
  # Similarly we also set it to TRUE if we want to store the models in general
  ids = batchmark(
    design = grid,
    store_models = conf$store$tuning_instance | conf$store$models
  )
  # Tagging with the measure is used to disambiguate jobs with identical learner/task but different
  # tuning measure. Not sure if "cleaner" solution available?
  addJobTags(ids, measure$id)

  # also tag jobs which have been skipped because they are not wrapped
  # into a AutoTuner (and as such don't differ)
  learners_skipped = mlr3misc::ids(learners)[
    !mlr3misc::map_lgl(learners, inherits, "AutoTuner")
  ]
  ids = findExperiments(algo.pars = learner_id %in% learners_skipped)
  addJobTags(ids, measure$id)
}

# Sanity checking at the end to ensure all learners are accounted for correctly
experiments = summarizeExperiments(by = c("task_id", "learner_id"))

cli::cli_alert_success(
  "Added {.val {sum(experiments$.count)}} experiments to registry {.val {conf$reg_name}}"
)
cli::cli_li("{length(unique(experiments$learner_id))} learners")
cli::cli_li("{length(unique(experiments$task_id))} tasks")

# Table with all jobs and metadata for tasks
tab = collect_job_table()
