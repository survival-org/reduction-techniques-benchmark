# Submit jobs after benchmark is set up

# Create random subset of jobs for testing
ids = tab[,
  .SD[sample(nrow(.SD), 1)],
  by = c("task_id", "learner_id")
]
submitJobs(ids)
