library(mlr3proba)
library(batchtools)
library(dplyr)

reg = loadRegistry(conf$reg_dir, writeable = FALSE)
tab = collect_job_table(
  optional_columns = c("submitted", "done", "time.running", "memory")
)

done_count = ijoin(tab, findDone())[, .N, by = .(learner_id, task_id)]

bmr_harrell_c = mlr3batchmark::reduceResultsBatchmark(ijoin(
  findDone(),
  findTagged("harrell_c")
))
bmr_isbs = mlr3batchmark::reduceResultsBatchmark(ijoin(
  findDone(),
  findTagged("isbs")
))

archive_harrell_c = mlr3tuning::extract_inner_tuning_archives(bmr_harrell_c)
archive_isbs = mlr3tuning::extract_inner_tuning_archives(bmr_isbs)

# fmt: skip
eval_measures = list(
 msr("surv.cindex",      id = "harrell_c",                      label = "Harrell's C"),
#  msr("surv.intlogloss",  id = "isll",     ERV = FALSE, label = "Integrated Survival Log-Likelihood (ISLL)"),
#  msr("surv.intlogloss",  id = "isll_erv", ERV = TRUE,  label = "Integrated Survival Log-Likelihood (ISLL) [ERV]"),
 msr("surv.brier",       id = "isbs",     p_max = 0.8, ERV = FALSE, label = "Integrated Survival Brier Score (ISBS)")
#  msr("surv.brier",       id = "isbs_erv", p_max = 0.8, ERV = TRUE,  label = "Integrated Survival Brier Score (ISBS) [ERV]")
)

scores_harrell_c = bmr_harrell_c$score(eval_measures, conditions = TRUE)
scores_isbs = bmr_isbs$score(eval_measures, conditions = TRUE)

scores_harrell_c[harrell_c == .5]
scores_isbs[harrell_c == .5]

scores = rbind(
  scores_harrell_c[, tuning_measure := "harrel_c"],
  scores_isbs[, tuning_measure := "isbs"]
)[, .(nr, task_id, learner_id, iteration, tuning_measure, harrell_c, isbs)]

library(ggplot2)

scores |>
  tidyr::pivot_longer(
    cols = c("harrell_c", "isbs"),
    names_to = "eval_measure",
    values_to = "score"
  ) |>
  ggplot(aes(x = learner_id, y = score)) +
  facet_grid(
    cols = vars(tuning_measure),
    rows = vars(eval_measure),
    labeller = label_both,
    scales = "free_y"
  ) +
  geom_boxplot() +
  theme_minimal(base_size = 14)


scores |>
  tidyr::pivot_longer(
    cols = c("harrell_c", "isbs"),
    names_to = "eval_measure",
    values_to = "score"
  ) |>
  group_by(learner_id, task_id, tuning_measure, eval_measure) |>
  summarise(score_mean = mean(score)) |>
  tidyr::pivot_wider(names_from = "eval_measure", values_from = "score_mean") |>
  filter(harrell_c == .5 & learner_id != "KM")
