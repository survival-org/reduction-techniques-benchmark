library(mlr3proba)

bmr_harrell_c = mlr3batchmark::reduceResultsBatchmark(findTagged("harrell_c"))
bmr_isbs = mlr3batchmark::reduceResultsBatchmark(findTagged("isbs"))

# fmt: skip
eval_measures = list(
 msr("surv.cindex",      id = "harrell_c",                      label = "Harrell's C"),
#  msr("surv.intlogloss",  id = "isll",     ERV = FALSE, proper = FALSE, label = "Integrated Survival Log-Likelihood (ISLL)"),
#  msr("surv.intlogloss",  id = "isll_erv", ERV = TRUE,  proper = FALSE, label = "Integrated Survival Log-Likelihood (ISLL) [ERV]"),
 msr("surv.brier",       id = "isbs",     p_max = 0.8, proper = FALSE,  ERV = FALSE, label = "Integrated Survival Brier Score (ISBS)"),
#  msr("surv.brier",       id = "isbs_erv", p_max = 0.8, proper = FALSE,  ERV = TRUE,  label = "Integrated Survival Brier Score (ISBS) [ERV]")
)

scores_harrell_c = bmr_harrell_c$score(eval_measures)
scores_isbs = bmr_isbs$score(eval_measures)
