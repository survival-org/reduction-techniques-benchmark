library(mlr3proba)
library(batchtools)
library(data.table)


# Process registry -------------------------------------------------------
reg = loadRegistry(conf$reg_dir, writeable = FALSE)
tab = collect_job_table(
  optional_columns = c("submitted", "done", "time.running", "memory")
)

# Store reduced table with runtime info
tab_runtime = tab[, .(
  learner_id,
  task_id,
  measure,
  job.id,
  repl,
  submitted,
  done,
  time.running
)]
tab_runtime[, time.running.hours := as.numeric(time.running, unit = "hours")]
save_obj(tab_runtime, "runtime")

# Retrieve actual results, separated by tuning measure
# this counts learners like KM and RIDGE separately for each result sets as they are untuned, so they are technically duplicated
# but it's easier/more consistent this way
# ijoin(tab, findDone())[, .N, by = .(learner_id, task_id)]

# fmt: skip
eval_measures = list(
 msr("surv.cindex",      id = "harrell_c",                          label = "Harrell's C"),
 msr("surv.brier",       id = "isbs",     p_max = 0.8, ERV = FALSE, label = "Integrated Survival Brier Score (ISBS)")
)

for (tune_meas_idx in c("harrell_c", "isbs")) {
  cli::cli_h2("Processing results for tuning measure {.val {tune_meas_idx}}")
  cli::cli_progress_step("Creating bmr")

  # Locally set progress option to FALSE for speedup
  withr::with_options(list("batchtools.progress" = FALSE), {
    bmr = mlr3batchmark::reduceResultsBatchmark(ijoin(
      findDone(),
      findTagged(tune_meas_idx)
    ))
  })

  cli::cli_progress_step("Scoring results")
  scores = bmr$score(eval_measures, conditions = TRUE)
  # Cleanup to remove large R6 objects (saves multiple GB of space and RAM)
  scores = as.data.table(scores)
  scores[, task := NULL]
  scores[, learner := NULL]
  scores[, resampling := NULL]
  scores[, prediction_test := NULL]
  scores[, resampling_id := NULL]
  # convert warnings/errors to counts
  scores[, warnings := vapply(warnings, length, FUN.VALUE = integer(1))]
  scores[, errors := vapply(errors, length, FUN.VALUE = integer(1))]
  save_obj(scores, prefix = tune_meas_idx)

  cli::cli_progress_step("Extracting tuning archives")
  archive = mlr3tuning::extract_inner_tuning_archives(
    bmr,
    unnest = "",
    exclude_columns = c(
      "uhash",
      "x_domain",
      "acq_ei",
      ".already_evaluated"
    )
  )
  if (nrow(archive) > 0) {
    archive[, resampling_id := NULL]
    archives = archive_to_list(archive)
    save_obj(archives, prefix = tune_meas_idx)
  } else {
    cli::cli_alert_warning("No tuning archives found!")
  }

  cli::cli_process_done()
}
