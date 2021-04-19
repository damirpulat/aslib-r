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
	    ldf2$algorithmFeatures = ldf$algorithmFeatuers[sel[(length(ldf$features) + 1):length(sel)]]	
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
    ldf2$performance = ldf$performance[sel]
    ldf2$success = ldf$success[sel]
    model = llama.model.fun(mlr.learner, ldf2)
    ldf3 = fixFeckingPresolve(scenario, ldf2)
    score = mean(parscores(ldf3, model))
    return(score)
  }, xs, simplify = TRUE)
  return(scores)
}
