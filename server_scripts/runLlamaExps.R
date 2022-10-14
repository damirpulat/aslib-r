library(BBmisc)
library(devtools)
library(llama)
library(stringr)
library(mlr)
library(ParamHelpers)
library(aslib)
library(data.table)
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

submitJobs(reg = reg, ids = findNotSubmitted(), resources = list(ncpus = ncpus, walltime = walltime, memory = memory))
waitForJobs(reg = reg, ids = findSubmitted())

aggrShort = function(job, res) {
  return(list(models = res$retval$models, predictions = res$predictions))
}

ids = findSubmitted()
info = unwrap(getJobPars(reg = reg))
info$fold = str_extract(info$problem, "_[0-9]*")
info$fold = as.numeric(str_remove(info$fold, "_"))
info$problem = str_remove(info$problem, "_[0-9]*")
repls = getJobTable(reg = reg, ids = ids)
repls = repls[, c("job.id", "repl")]

d = reduceResultsDataTable(reg = reg, ids = findSubmitted(), fun = aggrShort,
                           missing.val = list(predictions = NULL, models = NULL))
d = merge(info, d, by = "job.id")
d = merge(repls, d, by = "job.id")

# stack all predictions
e = d[, .(preds = list(do.call(rbind, lapply(result, function(x) { x$predictions }))), 
          models=list(result[[1]]$model)), by=.(problem, algorithm)]

# scenarios and their names
l = data.table(problem=sapply(asscenarios, function(x) x$desc$scenario_id), 
               data=sapply(asscenarios, convertToLlamaCVFolds))
e = merge(e, l)

# compute statistics (mcp, par10, success, rmse)
s1 = c()
s2 = c()
s3 = c()
s4 = c()
for(i in seq_along(1:nrow(e))) {
  m = list(predictions = e$preds[[i]])
  attr(m, "hasPredictions") = TRUE
  s1 = c(s1, mean(misclassificationPenalties(e$data[[i]], m)))
  s2 = c(s2, mean(parscores(e$data[[i]], m)))
  s3 = c(s3, mean(successes(e$data[[i]], m)))
  s4 = c(s4, mean(aslib:::compute_rmse(e$data[[i]], m$predictions)))
}

s = e[, .(problem, algorithm)]
s$success = s3
s$par10 = s2
s$mcp = s1
s$rmse = s4

#e = reduceResultsList(reg = reg, ids = findDone())
save2(file = "llama_results.RData", res = s, resLong = e)
