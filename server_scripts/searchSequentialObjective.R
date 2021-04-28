searchSequentialObjectiveFeatures = function(xs, scenario, ldf, llama.model.fun, mlr.learner) {
  scores = parallelMap(function(x) {
    sel = (x == 1)
    if (!any(sel))
      return(Inf)
    ldf2 = ldf
		if (is.null(ldf2$algorithmFeatures)) {
		  ldf2$features = ldf$features[sel]
		} else {
      ldf2$features = ldf$features[sel[1:length(ldf$features)]]
	    ldf2$algorithmFeatures = ldf$algorithmFeatures[sel[(length(ldf$features) + 1):length(sel)]]	
		}
    # print(ldf2$features)
    #print(paste("selected:", paste(ldf2$features, collapse=", ")))
    model = llama.model.fun(mlr.learner, ldf2)
    ldf3 = fixFeckingPresolve(scenario, ldf2)
    score = mean(parscores(ldf3, model))
    return(score)
  }, xs, simplify = TRUE)
  return(scores)
}

searchSequentialObjectiveSolvers = function(xs, scenario, ldf, llama.model.fun, mlr.learner) {
  scores = parallelMap(function(x) {
    sel = (x == 1)
    if (!any(sel))
      return(Inf)
    ldf2 = ldf
		if (is.null(ldf2$algorithmFeatures)) {
      ldf2$performance = ldf$performance[sel]
		  ldf2$success = ldf$success[sel]
		} else {
      ldf2$algorithmNames = ldf$algorithmNames[sel] 
		  ldf2$data = ldf2$data[ldf2$data$algorithm %in% ldf2$algorithmNames, ]
			# reset folds
			folds = scenario$cv.splits
			nfolds = length(unique(folds$fold))
			rownames(folds) = folds$instance_id
      splitFactors = folds[match(ldf2$data$instance_id, folds$instance_id), "fold"]
			parts = split(1:nrow(ldf2$data), splitFactors)
			ldf2$train = lapply(1:nfolds, function(x) {
								return(unlist(parts[-x]))
              })
			ldf2$test = lapply(1:nfolds, function(x) {
								return(parts[[x]])
							})
		}
    model = llama.model.fun(mlr.learner, ldf2)
    ldf3 = fixFeckingPresolve(scenario, ldf2)
    score = mean(parscores(ldf3, model))
    return(score)
  }, xs, simplify = TRUE)
  return(scores)
}
