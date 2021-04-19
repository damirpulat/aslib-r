library(BBmisc)
library(devtools)
library(llama)
library(stringr)
library(batchtools)
library(checkmate)
library(aslib)
source("defs.R")

source("searchSequential.R")
source("searchSequentialObjective.R")

ds.dirs = list.files(coseal.data.dir, full.names = TRUE)
ds.dirs = ds.dirs[!str_detect(ds.dirs, "README.md")]
# ds.dirs = ds.dirs[4]
print(ds.dirs)
#ds.dirs = ds.dirs[7]
asscenarios = lapply(ds.dirs, parseASScenario)

unlink("run_selection_exps-files", recursive = TRUE)
reg = makeRegistry("run_selection_exps", seed = 123,
                   packages = c("llama", "mlr", "aslib", "batchtools", "parallelMap", "checkmate", "BBmisc", "ParamHelpers"),
                   source = c("searchSequential.R", "searchSequentialObjective.R")
)

learner = makeImputeWrapper(learner = makeLearner("regr.randomForest"),
                            classes = list(numeric = imputeMean(), integer = imputeMean(), logical = imputeMode(),
                                           factor = imputeConstant("NA"), character = imputeConstant("NA")))

# FIXME: we need to store the names of all features and solvers in the result in the correct order!
#browser()
batchMap(fun = function(ast, learner) {
  ctrl = makeSSControl(method = "sfs")
  ldf = convertToLlamaCVFolds(ast)
	if (is.null(ldf$algorithmFeatures)) {
    n.bits = length(ldf$features)
    ldf.features = convertToLlamaCVFolds(ast, feature.steps = names(lapply(ast$desc$feature_steps, function(x) x$provides)))
    n.bits.features = length(ldf.features$features)
	} else {
	  n.bits = length(ldf$features) + length(ldf$algorithmFeatures)
    ldf.features = convertToLlamaCVFolds(ast, feature.steps = c(names(lapply(ast$desc$feature_steps, function(x) x$provides)), 
	               																		names(lapply(ast$desc$algorithm_feature_steps, function(x) x$provides))))
    n.bits.features = length(ldf.features$features) + length(ldf.features$algorithmFeatures)
	}

	parallelStartMulticore(cpus = 16L)
  feats = searchSequential(searchSequentialObjectiveFeatures, n.bits.features, control = ctrl, scenario = ast, ldf = ldf.features,
                           llama.model.fun = regression, mlr.learner = learner)
  n.bits = length(ldf$performance)
  solvs = searchSequential(searchSequentialObjectiveSolvers, n.bits, control = ctrl, scenario = ast, ldf = ldf,
                           llama.model.fun = regression, mlr.learner = learner)
  parallelStop()
  list(id = ast$desc$scenario_id, feats = feats, solvs = solvs)
}, asscenarios, reg = reg, more.args = list(learner = learner))


walltime = '168:00:00'
memory = '30gb'
ncpus = 30

submitJobs(reg = reg, ids = findNotSubmitted(), resources = list(ncpus = ncpus, walltime = walltime, memory = memory))
waitForJobs(reg = reg, ids = findSubmitted())

# enrich results with all feat names and solver names posthoc, see FIXME above...
#reg = loadRegistry("run_selection_exps-files")
reg = loadRegistry("run_selection_exps")
d = getJobPars(reg = reg)

res = reduceResultsList(reg = reg, ids = findDone())
for (i in 1:length(res)) {
  r = res[[i]]
  ast = Filter(function(ast) ast$desc$scenario_id == r$id, asscenarios)[[1L]]
  if (is.null(ast$algorithm.feature.values)) {
    ldf = convertToLlamaCVFolds(ast, feature.steps = names(lapply(ast$desc$feature_steps, function(x) x$provides)))
    r$all.feats = ldf$features
	} else {
	  ldf = convertToLlamaCVFolds(ast, feature.steps = c(names(lapply(ast$desc$feature_steps, function(x) x$provides)), 
	               																		names(lapply(ast$desc$algorithm_feature_steps, function(x) x$provides))))
    r$all.feats = c(ldf$features, ldf$algorithmFeatures)

	}	
  r$all.solvers = ldf$performance
  res[[i]] = r
}

# make named list
names(res) = seq_along(1:length(res))

save2(file = "selection_results.RData", res = res)
