# SSH clusterfunctions with localhost worker are useful for more robust computation on a single/local machine
# max.load allows to control how much CPU is "left" for other stuff, e.g. "don't submit jobs if load is over 60"
cluster.functions <- makeClusterFunctionsSSH(
   list(Worker$new("localhost", ncpus = 20, max.load = 44)),
   fs.latency = 0
)

# Multicore CF don't allow killing jobs and are less robust, no real reason to prefer them over above config
#cluster.functions <- makeClusterFunctionsMulticore(ncpus = 8)

default.resources = list(measure.memory = TRUE)
