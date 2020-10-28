#' Returns feature names of scenario.
#'
#' @param asscenario [\code{\link{ASScenario}}]\cr
#'   Algorithm selection scenario.
#' @return [\code{character}].
#' @export
getFeatureNames = function(asscenario) {
  assertClass(asscenario, "ASScenario")
  c(asscenario$desc$instance_features_deterministic, asscenario$desc$instance_features_stochastic)
}
