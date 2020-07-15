library(BBmisc)
library(devtools)
library(llama)
library(stringr)
library(mlr)
library(ParamHelpers)
library(aslib)
source("defs.R")

ds.dirs = list.files(coseal.data.dir, full.names = TRUE)
ds.dirs = ds.dirs[!str_detect(ds.dirs, "TSP-|README.md")]
print(ds.dirs)
#ds.dirs = list(ds.dirs[2])
asscenarios = lapply(ds.dirs, parseASScenario)

learners = list(
  # classif
  makeLearner("classif.rpart"),
  makeLearner("classif.randomForest"),
  makeLearner("classif.ksvm"),
  # regr
  makeLearner("regr.lm"),
  makeLearner("regr.rpart"),
  makeLearner("regr.randomForest"),
  # makeLearner("regr.mars")
  # cluster
  makeLearner("cluster.XMeans", H = 30) # increase upper limit of clusters
)

wrapped.learners = lapply(learners, function(learner) {
  makeImputeWrapper(learner = learner,
    classes = list(numeric = imputeMean(), integer = imputeMean(), logical = imputeMode(),
      factor = imputeConstant("NA"), character = imputeConstant("NA")))
})

par.sets = list(
  # classif
  classif.rpart = makeParamSet(),
  classif.randomForest = makeParamSet(
    makeIntegerParam("ntree", lower = 10, upper = 200),
    makeIntegerParam("mtry", lower = 1, upper = 30)
  ),
  classif.ksvm = makeParamSet(
    makeNumericParam("C",     lower = -12, upper = 12, trafo = function(x) 2^x),
    makeNumericParam("sigma", lower = -12, upper = 12, trafo = function(x) 2^x)
  ),
  # regr
  regr.lm = makeParamSet(),
  regr.rpart = makeParamSet(),
  regr.randomForest = makeParamSet(
    makeIntegerParam("ntree", lower = 10, upper = 200),
    makeIntegerParam("mtry", lower = 1, upper = 30)
  ),
  # regr.earth = makeParamSet(
  #   makeIntegerParam("degree", lower = 1, upper = 3)
  #   makeNumericParam("penalty", lower = 2, upper = 4)
  #   makeIntegerParam("penalty", lower = 0.5, upper = 5)
  #   makeLogicalParam("prune"),
  #   makeLogicalParam("forward.step")
  # # )
  # cluster
  cluster.XMeans = makeParamSet()
)

reg = runLlamaModels(asscenarios, learners = wrapped.learners,
  par.sets = par.sets, rs.iters = 250L, n.inner.folds = 3L)

# testJob(reg, 5, external = FALSE)

# jobs should be run with 2gig mem
# run time of all jobs
# summary(getJobInfo(reg)$time.running)
   # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
      # 9      18      30     161      50    6320
# can be run on SLURM in a few hours in total

# stop("we dont auto submit :)")

walltime = '100:00:00'
memory = '20gb'
ncpus = 15

unwrap(getJobPars(reg = reg))
info = unwrap(getJobPars(reg = reg))
repls = batchtools:::mergedJobs(reg = reg, ids = findSubmitted(), cols = c("repl", "job.id"))

submitJobs(reg = reg, ids = findNotSubmitted(), resources = list(ncpus = ncpus, walltime = walltime, memory = memory))
waitForJobs(reg = reg, ids = findSubmitted())

aggrShort = function(job, res) {
    return(list(succ = res$succ, par10 = res$par10, mcp = res$mcp))
}

d = reduceResultsDataTable(reg = reg, ids = findSubmitted(), fun = aggrShort,
                           missing.val = list(succ = 0, par10 = Inf, mcp = Inf))
d = merge(info, d, by = "job.id")
d = merge(repls, d, by = "job.id")

# rename columns so they match columns from old results
colnames(d) = c("id", "repl", "prob", "algo", "type", "result")
d = d[, c(1, 3, 4, 5, 2, 6)]
d$mcp = NA
d$par10 = NA
d$succ = NA

# unlist result
for(i in seq_along(1:nrow(d))) {
  d$mcp[i] = d[i, ]$result[[1]]$mcp
  d$par10[i] = d[i, ]$result[[1]]$par10
  d$succ[i] = d[i, ]$result[[1]]$succ
}
d$result = NULL

e = reduceResultsList(reg = reg, ids = findDone())
save2(file = "llama_results.RData", res = d, resLong = e)
