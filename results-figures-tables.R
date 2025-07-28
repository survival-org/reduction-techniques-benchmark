library(ggplot2)
library(dplyr, warn.conflicts = FALSE)
scores_file = fs::path(here::here("results", conf$reg_name), "scores.rds")

if (!file.exists(scores_file)) {
  cli::cli_abort(c(
    x = "Scores file not found at {.file {fs::path_rel(scores_file)}}",
    i = "Run {.file results-processing.R} first to aggregate results from batchtools!"
  ))
}

scores = readRDS(scores_file)

scores_long = scores |>
  tidyr::pivot_longer(
    cols = c("harrell_c", "isbs", "ipa"),
    names_to = "eval_measure",
    values_to = "score"
  ) |>
  filter(
    # Only keep matching measures
    tuning_measure == eval_measure |
      (tuning_measure == "isbs" & eval_measure == "ipa")
  ) |>
  select(-tuning_measure, -uhash)

# scores_long |>
#   count(tuning_measure, eval_measure)

task_ids = c(
  "synthetic-breakpoint",
  "synthetic-hdi",
  "synthetic-tve",
  "cat_adoption",
  "wa_churn",
  "std",
  "mgus",
  "nafld1",
  "nwtco",
  "tumor"
)

learner_ids = c(
  "KM",
  "RIDGE",
  "GLMN",
  "RFSRC",
  "RFSRC_DT",
  "XGBCox",
  "XGB_PEM",
  "XGB_DT"
)

# https://coolors.co/686868-abede2-47d7bf-fe7171-a20101-86a4d5-5982c5-2a4879
learner_colors = c(
  "KM" = "#686868",
  "RIDGE" = "#89E6D7",
  "GLMN" = "#47D7BF",
  "RFSRC" = "#FE7171",
  "RFSRC_DT" = "#A20101",
  "XGBCox" = "#86A4D5",
  "XGB_PEM" = "#5982C5",
  "XGB_DT" = "#2A4879"
)

measures_labels = c(
  "harrell_c" = "Harrell's C",
  "isbs" = "ISBS",
  "ipa" = "IPA"
)

captions = c(
  "harrell_c" = "Higher is better",
  "isbs" = "Lower is better",
  "ipa" = "Higher is better"
)

subtitles = c(
  c(
    "harrell_c" = "Baseline (KM) score: 50",
    "isbs" = "",
    "ipa" = "Baseline (KM) score: 0"
  )
)

# Plots ------------------------------------------------------------------

# Scores boxplots for learners, agrgegated over tasks, separately for eval measures
for (eval_meas_idx in c("harrell_c", "isbs", "ipa")) {
  p = scores_long |>
    filter(
      .data[["eval_measure"]] == eval_meas_idx,
      # Don't need constant KM 0 for IPA
      !(learner_id == "KM" & eval_measure %in% c("harrell_c", "ipa"))
    ) |>
    mutate(
      task_id = factor(task_id, levels = task_ids),
      learner_id = factor(learner_id, levels = rev(learner_ids))
    ) |>
    mutate(score = 100 * score) |>
    ggplot(aes(
      y = learner_id,
      x = score,
      fill = learner_id,
      color = after_scale(colorspace::darken(fill, amount = 0.2))
    )) +
    geom_boxplot(alpha = 2 / 3, show.legend = FALSE) +
    scale_color_manual(
      values = learner_colors,
      aesthetics = c("color", "fill")
    ) +
    labs(
      title = glue::glue(
        "Aggregated scores for learners tuned & evaluated with {measures_labels[eval_meas_idx]}"
      ),
      subtitle = subtitles[[eval_meas_idx]],
      x = glue::glue("{measures_labels[[eval_meas_idx]]} (%)"),
      y = NULL,
      caption = captions[[eval_meas_idx]]
    ) +
    theme_minimal(base_size = 14) +
    theme(plot.title.position = "plot")

  if (interactive()) {
    print(p)
  }

  save_plot(
    p,
    name = glue::glue("aggr_{eval_meas_idx}"),
    width = 9,
    height = 6
  )
}

# Separately by task
for (eval_meas_idx in c("harrell_c", "isbs", "ipa")) {
  p = scores_long |>
    filter(
      .data[["eval_measure"]] == eval_meas_idx,
      # Don't need constant KM 0 for IPA
      !(learner_id == "KM" & eval_measure %in% c("harrell_c", "ipa"))
    ) |>
    mutate(
      task_id = factor(task_id, levels = task_ids),
      learner_id = factor(learner_id, levels = rev(learner_ids))
    ) |>
    mutate(score = 100 * score) |>
    ggplot(aes(
      y = learner_id,
      x = score,
      fill = learner_id,
      color = after_scale(colorspace::darken(fill, amount = 0.2))
    )) +
    facet_wrap(vars(task_id), ncol = 2, scales = "free") +
    geom_boxplot(alpha = 2 / 3, show.legend = FALSE) +
    scale_color_manual(
      values = learner_colors,
      aesthetics = c("color", "fill")
    ) +
    labs(
      title = glue::glue(
        "Scores for learners tuned & evaluated with {measures_labels[eval_meas_idx]}"
      ),
      subtitle = subtitles[[eval_meas_idx]],
      x = glue::glue("{measures_labels[[eval_meas_idx]]} (%)"),
      y = NULL,
      caption = captions[[eval_meas_idx]]
    ) +
    theme_minimal(base_size = 14) +
    theme(plot.title.position = "plot")

  if (interactive()) {
    print(p)
  }

  save_plot(
    p,
    name = glue::glue("scores_per_task_{eval_meas_idx}"),
    width = 9,
    height = 12
  )
}

# Tables -----------------------------------------------------------------

# Tasks

tasktab = load_tasktab()
tasktab |>
  arrange(task_id) |>
  select(task_id, n, p, events, censprop) |>
  mutate(
    censprop = round(100 * censprop, 2),
    repeats = assign_repeats(events)
  ) |>
  kableExtra::kbl(
    col.names = c("Task", "N", "p", "Events", "Cens. %", "Repeats"),
    caption = "Tasks used in benchmark comparison \\label{tab:bm-tasks}",
    booktabs = TRUE,
    format = "latex",
    linesep = ""
  ) |>
  save_table(name = "tasks")

lrntab = load_lrntab()
lrntab |>
  select(id, package, base_lrn, params) |>
  kableExtra::kbl(
    col.names = c("ID", "Package", "mlr3 ID", "# Parameters"),
    caption = "Learnes and associated mlr3 IDs and source R packages used in benchmark comparison \\label{tab:bm-learners}",
    booktabs = TRUE,
    format = "latex"
  ) |>
  save_table(name = "learners")

# Scores aggregated by learner, task
tbl_scores = scores_long |>
  group_by(learner_id, task_id, eval_measure) |>
  summarise(
    score_mean = mean(score),
    score_sd = sd(score),
    .groups = "drop"
  ) |>
  mutate(
    score_fmt = glue::glue(
      "{round(100 * score_mean, 2)} ({round(100 * score_sd, 2)})"
    )
  ) |>
  tidyr::pivot_wider(
    id_cols = c("learner_id", "task_id"),
    names_from = "eval_measure",
    values_from = "score_fmt"
  ) |>
  select(task_id, learner_id, harrell_c, isbs, ipa) |>
  mutate(
    task_id = factor(task_id, levels = task_ids),
    learner_id = factor(learner_id, levels = learner_ids)
  ) |>
  arrange(task_id, learner_id) |>
  ungroup()

tbl_scores |>
  select(-task_id) |>
  kableExtra::kbl(
    caption = "Mean (SD) of evaluation scores for each learner and task. Scores scaled by 100 for readability.\\label{tab:bm-scores}",
    col.names = c(
      "Learner",
      "Harrell's C",
      "ISBS",
      "IPA"
    ),
    booktabs = TRUE,
    format = "latex"
  ) |>
  kableExtra::kable_styling(latex_options = c("striped")) |>
  kableExtra::pack_rows(index = table(tbl_scores$task_id)) |>
  save_table(name = "scores")

# Aggregated by learner

tbl_aggr = scores_long |>
  group_by(learner_id, eval_measure) |>
  summarise(
    score_mean = mean(score),
    score_sd = sd(score),
    .groups = "drop"
  ) |>
  mutate(
    score_fmt = glue::glue(
      "{round(100 * score_mean, 2)} ({round(100 * score_sd, 2)})"
    )
  ) |>
  tidyr::pivot_wider(
    id_cols = c("learner_id"),
    names_from = "eval_measure",
    values_from = "score_fmt"
  ) |>
  select(learner_id, harrell_c, isbs, ipa) |>
  mutate(
    learner_id = factor(learner_id, levels = learner_ids)
  ) |>
  arrange(learner_id) |>
  ungroup()

tbl_aggr |>
  kableExtra::kbl(
    caption = "Mean (SD) of evaluation scores aggregated by learner. Scores scaled by 100 for readability.\\label{tab:bm-aggr}",
    col.names = c(
      "Learner",
      "Harrell's C",
      "ISBS",
      "IPA"
    ),
    booktabs = TRUE,
    linesep = "",
    format = "latex"
  ) |>
  kableExtra::kable_styling() |>
  save_table(name = "aggr")

# Error counting in archives -----------------------------------------------
archives_harrell_c = readRDS(fs::path(
  conf$result_path,
  "harrell_c_archives.rds"
))

archives_isbs = readRDS(fs::path(
  conf$result_path,
  "isbs_archives.rds"
))

list(archives_harrell_c, archives_isbs) |>
  lapply(\(archive_list) {
    lapply(archive_list, \(archive) {
      archive |>
        group_by(learner_id, task_id) |>
        summarize(
          iters_with_errors = sum(errors > 0),
          iters_total = n(),
          error_rate = iters_with_errors / iters_total,
          .groups = "drop"
        ) |>
        mutate(
          tune_measure = ifelse(
            "harrell_c" %in% names(archive),
            "harell_c",
            "isbs"
          )
        ) |>
        filter(error_rate > 0)
    }) |>
      rbindlist()
  }) |>
  rbindlist() |>
  mutate(
    err_fmt = glue::glue(
      "{iters_with_errors} / {iters_total} ({round(100 * error_rate, 1)}%)"
    )
  ) |>
  select(learner_id, task_id, tune_measure, err_fmt) |>
  arrange(learner_id, task_id, tune_measure) |>
  kableExtra::kbl(
    caption = "Errors encountered during tuning procedure. Showing total number of evaluations with at least one error and total number of tuning evaluations and error rate.\\label{tab:bm-errors}",
    col.names = c(
      "Learner",
      "Task",
      "Tuning Measure",
      "Error Rate"
    ),
    booktabs = TRUE,
    linesep = "",
    format = "latex"
  ) |>
  kableExtra::kable_styling() |>
  save_table("errors")
