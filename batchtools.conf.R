# cluster.functions <- makeClusterFunctionsSSH(
#   list(Worker$new("localhost", ncpus = 6, max.load = 10)),
#   fs.latency = 0
# )

cluster.functions <- makeClusterFunctionsMulticore(ncpus = 6)

default.resources = list(measure.memory = TRUE)
