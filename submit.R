# Submit jobs after benchmark is set up
library(batchtools)
reg = loadRegistry(conf$reg_dir, writeable = FALSE)
tab = collect_job_table()

# Create random subset of jobs for testing
ids = tab[,
  .SD[sample(nrow(.SD), 1)],
  by = c("task_id", "learner_id")
]

cats = dplyr::filter(ids, task_id == "cat_adoption")
cats |> dplyr::select(job.id, task_id, learner_id, n, p, events, measure)
submitJobs(cats)
