library(data.table)
# Helpers run pre-benchmark -------------------------------------------------------------------

#' Store instantiated resamplings as portable CSV files to `here::here("resamplings"`)
#'
#' @param resampling Object of class `Resampling`, has to be instantiated.
#' @param task (`mlr3proba::TaskSurv`) Task object where `$id` will correspond to
#'   `<resampling_dir>/<task_id>.csv` file.
#' @param resampling_dir (`here::here("resamplings")`) Path of folder containing resampling CSVs.
save_resampling = function(
  resampling,
  task,
  resampling_dir = here::here("resamplings")
) {
  ensure_directory(resampling_dir)
  mlr3::assert_resampling(resampling, instantiated = TRUE)
  mlr3::assert_task(task, task_type = "surv")

  file_csv <- fs::path(resampling_dir, task$id, ext = "csv")
  cli::cli_alert_info("Saving resampling to {.file {fs::path_rel(file_csv)}}")

  resampling_tab = as.data.table(resampling)
  readr::write_csv(x = resampling_tab, file = file_csv, append = FALSE)
}

#' Reconstruct a Resampling object from stored resampling CSV
#'
#' Using tasks-specific resampling stored at `resampling_dir`.
#' @param task (`mlr3proba::TaskSurv`) Task object where `$id` is expected to correspond to
#'   `<resampling_dir>/<task_id>.csv` file.
#' @param resampling_dir (`here::here("resamplings")`) Path of folder containing resampling CSVs.
#'
#' @return Object of class `mlr3::ResamplingCustomCV` reconstructing the stored resampling folds.
#'
#' @example
#' data = readRDS("datasets/cost.rds")
#' task = as_task_surv(data, target = "time", event = "status", id = "cost")
#' task$set_col_roles("status", add_to = "stratum")
#'
#' create_resampling_from_csv(task)
create_resampling_from_csv = function(
  task,
  resampling_dir = here::here("resamplings")
) {
  mlr3::assert_task(task, task_type = "surv")

  resampling_csv_path = fs::path(resampling_dir, task$id, ext = "csv")
  checkmate::assert_file_exists(resampling_csv_path)

  # Read stored resampling, sort by row_id for easier assignment of folds in row_id order
  resampling_csv = data.table::fread(
    resampling_csv_path,
    key = c("set", "iteration")
  )

  checkmate::assert_data_table(
    resampling_csv,
    min.rows = 1L,
    col.names = "unique",
    any.missing = FALSE
  )
  checkmate::assert_names(
    names(resampling_csv),
    permutation.of = c("set", "iteration", "row_id")
  )
  checkmate::assert_set_equal(unique(resampling_csv$set), c("train", "test"))
  checkmate::assert_integerish(
    resampling_csv$iteration,
    lower = 1L,
    any.missing = FALSE
  )
  checkmate::assert_integerish(resampling_csv$row_id, any.missing = FALSE)

  row_id = NULL
  resampling = ResamplingCustom$new()
  resampling$instance = list(
    train_sets = resampling_csv[
      list("train"),
      list(ids = list(row_id)),
      by = "iteration"
    ]$ids,
    test_sets = resampling_csv[
      list("test"),
      list(ids = list(row_id)),
      by = "iteration"
    ]$ids
  )
  resampling
}

#' Store additional data for tasks
#'
#' Useful to augment job table and later results with e.g. n, p, number of unique event times, ...
#' Repeatedly refreshed in case there are changes with the included tasks in the benchmark.
#' Written to CSV such that possibly unintended changes are immediately obvious via `git status`.
#'
#' @param tasks List of `TaskSurv` objects.
#' @param path Path to store CSV file of results.
#' @example
#'
#' tasks = load_task_data()
#' save_tasktab(tasks)
save_tasktab = function(tasks, path = here::here("tables", "tasktab.csv")) {
  ensure_directory(path)
  task_names = names(tasks)

  # Save overview of tasks with some metadata which comes in handy later
  tasktab = data.table::rbindlist(lapply(seq_along(tasks), \(x) {
    task_data = tasks[[x]]

    if (!(inherits(task_data, "TaskSurv"))) {
      task = as_task_surv(
        task_data,
        target = "time",
        event = "status",
        id = task_names[x]
      )
      task$set_col_roles("status", add_to = "stratum")
    } else {
      task = task_data
    }

    data.table::data.table(
      task_id = task$id,
      n = task$nrow,
      p = length(task$feature_names),
      n_uniq_t = length(unique(task$data(cols = "time")[[1]])),
      events = sum(task$data(cols = "status")[[1]] == 1),
      censprop = round(mean(task$data(cols = "status")[[1]] == 0), 4)
    )
  }))
  tasktab[, uniq_t_rank := data.table::frank(n_uniq_t)]

  write.csv(tasktab, path, row.names = FALSE)
  tasktab
}

#' Load tasks as they were before resampling
#'
#' To just get the tasks as a named list of `data.table` objects
load_task_data = function() {
  files = dir(here::here("datasets"), pattern = "\\.rds$", full.names = TRUE)
  names = stringi::stri_sub(basename(files), 1, -5)
  tasks = mlr3misc::named_list(names)

  for (i in seq_along(files)) {
    tasks[[i]] = readRDS(files[i])
  }

  tasks
}

#' Assign number of repeats
#'
#' Number of repeats depending on events in dataset.
#' @param num_events Integer number of events
#'
#' @return Integer number of repeats
assign_repeats = function(num_events) {
  data.table::fcase(
    num_events < 500,
    3,
    num_events >= 500 & num_events < 1000,
    2,
    num_events > 1000,
    1
  )
}


# Utilities for analysis ----------------------------------------------------------------------
# fmt: skip
#' List of included learners with short IDs and some extra info
#'
#' @param path Path to store CSV file.
#'
#' @example
#' save_lrntab()
save_lrntab <- function(path = here::here("tables", "learners.csv")) {
  require(mlr3proba)
  requireNamespace("mlr3extralearners", quietly = TRUE)
  ensure_directory(path)

  lrntab <- mlr3misc::rowwise_table(
    ~id,       ~base_id,      ~base_lrn,            ~params, ~encode, ~internal_cv, ~grid,  ~scale,
    "KM"       , "kaplan"     , "surv.kaplan"       , 0 ,    FALSE , FALSE ,        FALSE, FALSE,
    "RIDGE"    , "cv_glmnet"  , "surv.cv_glmnet"    , 0 ,    FALSE , FALSE ,        FALSE, FALSE,
    "GLMN"     , "cv_glmnet"  , "surv.cv_glmnet"    , 1 ,    FALSE , TRUE  ,        FALSE, FALSE,

    "RFSRC"    , "rfsrc"      , "surv.rfsrc"        , 5 ,    FALSE , FALSE ,        FALSE, FALSE,
    "RFSRC_DT" , "rfsrc_dt"   , "surv.rfsrc"        , 5 ,    FALSE , FALSE ,        FALSE, FALSE,

    "XGBCox"   , "xgb_cox"    , "surv.xgboost.cox"  , 5 ,    TRUE  , FALSE ,        FALSE, FALSE,
    "XGB_PEM"  , "xgb_pem"    , "regr.xgboost"      , 5 ,    TRUE  , FALSE ,        FALSE, FALSE,
    "XGB_DT"   , "xgb_dt"     , "classif.xgboost"   , 5 ,    TRUE  , FALSE ,        FALSE, FALSE
  )

  lrntab$has_threads = vapply(lrntab$base_lrn, \(x) {
    "threads" %in% unlist(lrn(x)$param_set$tags)
    }, logical(1))

  if (fs::file_exists(path)) {
    cli::cli_alert_warning("Overwriting {.val {fs::path_rel(path)}}")
  }

  readr::write_csv(x = lrntab, file = path, append = FALSE)

  lrntab
}

load_lrntab = function(path = here::here("tables", "learners.csv")) {
  ensure_directory(path)
  cli::cli_alert_info("Loading {.file {path}}")
  data.table::fread(path, na.strings = "")
}

load_tasktab = function(path = here::here("tables", "tasktab.csv")) {
  ensure_directory(path)
  cli::cli_alert_info("Loading {.file {path}}")
  data.table::fread(path)
}


# Utilities for job management ----------------------------------------------------------------

#' Assemble augmented batchtools job table
#'
#' Includes regular `getJobTable()` info but also task data (n, p, ...)
#'
#' @param reg Registry, defaulting to `getDefaultRegistry()`.
#' @param keep_columns Character vector of columns from `getJobtTable()` to keep.
#'
#' @return A data.table keyed with `job.id`.
#'
#' @example
#' collect_job_table()
collect_job_table = function(
  reg = batchtools::getDefaultRegistry(),
  keep_columns = c(
    "job.id",
    "repl",
    "tags",
    "task_id",
    "learner_id",
    "log.file",
    "job.name"
  ),
  optional_columns = c("batch.id", "comment", "memory")
) {
  tab = unwrap(getJobTable(reg = reg))
  checkmate::assert_data_table(tab, min.rows = 1)

  tab = tab[,
    c(keep_columns, optional_columns[optional_columns %in% names(tab)]),
    with = FALSE
  ]

  data.table::setnames(tab, "tags", "measure")
  tasktab = load_tasktab()
  tab = ljoin(tab, tasktab, by = "task_id")
  data.table::setkey(tab, job.id)

  tab
}


# Debug utilities -----------------------------------------------------------------------------

#' Get the object size of job results
#'
#' In the context of the known issue with the serialization of R6 objects,
#' this `loadResult()`s a job result and uses `pryr::object_size` to get its size.
#'
#' @param ids [`findDone()`] `job.ids` to get result sizes for.
#' @return A data.table with columns `job.id`, `size` (in MiB).
#' @example
#' res_size = check_result_sizes()
#' jobs_done = tab[findDone(), ]
#' jobs_done = jobs_done[res_size, ]
#' jobs_done[, .(avg_size = mean(size)), by = c("learner_id")]
check_result_sizes = function(ids = batchtools::findDone()) {
  if (nrow(ids) > 0) {
    sizes = vapply(
      unlist(ids),
      \(x) {
        batchtools::loadResult(x) |>
          pryr::object_size() |>
          as.numeric()
      },
      FUN.VALUE = 1
    )
    ids[, size_bytes := prettyunits::pretty_bytes(bytes = sizes)]
    ids[, size := sizes / 1024^2][]
  } else {
    message("Don't know yet.")
  }
}

#' For quicker aggregation
aggr_result_sizes = function(ids = batchtools::findDone(), by = "learner_id") {
  res_size = check_result_sizes(ids = ids)
  jobs_done = unwrap(getJobTable())[findDone(), ]
  jobs_done = jobs_done[res_size, ]
  jobs_done[, .(avg_size = mean(size)), by = by][]
}


# Misc utils ----------------------------------------------------------------------------------

#' Ensure a directory exists
#'
#' Creates the directory `x`, or if `x` is a file, creates
#' the enclosing directory to ensure file `x` can be created.
#' @param x `character()` A filsystem path to a file or directory.
#' @return `TRUE` if the (enclosing) directory exists, `FALSE` otherwise
ensure_directory = function(x) {
  # If x is not a directory already and has no file extension
  if (!fs::is_dir(x) & fs::path_ext(x) != "") {
    x = fs::path_dir(x)
  }

  if (!fs::dir_exists(x)) {
    fs::dir_create(x, recurse = TRUE)
  }

  fs::dir_exists(x)
}

#' Aggregate extracted inner tuning archives from bmr to list of flat data.tables for easier viewing/storage
#' One list item -> One learner (all tasks in one)
archive_to_list = function(archive) {
  archive_tmp = copy(archive)
  # unnesting the list of internal tuned vals is cumbersome
  archive_tmp[,
    internal_tuned_values := lapply(archive_tmp$internal_tuned_values, \(x) {
      if (length(x) == 0) list(dummy = 0) else x
    })
  ]
  unnested <- rbindlist(archive_tmp$internal_tuned_values, fill = TRUE)
  archive_tmp <- cbind(
    archive_tmp[, -"internal_tuned_values"],
    unnested[, -"dummy"]
  )

  # drop all columns that are all NA
  archive_list = lapply(unique(archive_tmp$learner_id), \(lrn_idx) {
    tmp = archive_tmp[learner_id == lrn_idx]
    tmp[, .SD, .SDcols = colSums(!is.na(tmp)) > 0]
  })
  names(archive_list) = unique(archive_tmp$learner_id)

  archive_list
}

#' Utility to save result object to .rds file
#' @param obj Some R object, preferably a data.frame-like one.
#' @param name Optional string to name the file, will be `<name>.rds` or otherwise the name of provided `obj`.
#'
#' Results are saved to ./results/<config-mode>/<name>.rds
save_obj = function(obj, name = NULL, prefix = "") {
  ensure_directory(conf$result_path)
  xname = deparse(substitute(obj))
  name = name %||% xname
  if (prefix != "") {
    name = paste0(prefix, "_", name)
  }

  file_out = fs::path(conf$result_path, name, ext = "rds")
  cli::cli_progress_step(
    "Saving {.val {xname}} to {.file {fs::path_rel(file_out)}}"
  )
  saveRDS(object = obj, file = file_out)
}

#' Utility to save ggplot object as png
#' @param plot A ggplot2 plot.
#' @param name Name of output file. `.tex` will be appended automatically.
#' @param width,heigh,... Passed to `ggplot2::ggsave()`
save_plot = function(plot, name, width = 9, height = 6, ...) {
  result_dir = fs::path(conf$result_path, "figures")
  ensure_directory(result_dir)
  file_out = fs::path(result_dir, name, ext = "png")
  ggplot2::ggsave(
    filename = file_out,
    plot = plot,
    width = width,
    height = height,
    bg = "white", # transparency can become an issue
    ...
  )
}

#' Utility to save LaTeX tables as standalone .tex files
#' Used in conjunction with kableExtra
#' @param tbl A LaTeX-formatted table as produced by e.g. knitr::kable() and kableExtra
#' @param name Name of output file. `.tex` will be appended automatically.
save_table = function(tbl, name) {
  result_dir = fs::path(conf$result_path, "tables")

  ensure_directory(result_dir)
  file_out = fs::path(result_dir, name, ext = "tex")
  writeLines(tbl, con = file_out)
}
