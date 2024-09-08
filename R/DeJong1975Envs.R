
#' Generate DeJong's 1975 environments.
#'
#' @param rndtrials    Number of random trials.
#' @param rndrepExp    Number repetitions.
#' @param stochastic   Boolean. Default: \code{TRUE}. 
#'                     Kenneth De Jong uses F4 with a Gaussion noise term. 
#'                     If \code{FALSE} we use the deterministic version F4det.     
#' @param envWeights   The weights of the problem environments. Default: \code{rep(1.0, 5}. 
#'
#' @return A named list with elements 
#'           \itemize{
#'             \item \code{$EnvList} List of 5 environments with a reference value for 
#'                   random performance added. 
#'             \item \code{$envWeights}   Weights of the environment in the meta GA experiment.
#'            }
#'
#'@export
DeJongEnvs<-function(rndtrials, rndrepExp, stochastic=TRUE, envWeights=rep(1,5))
{
#  Empty environment list
EnvList<-list()
#  Environment[1]: generate smoof-function and add Rnd performance measures for 1000 trials, 100 repetitions. 
EnvList[[1]]<-rndPerformance(DeJongF1Factory(), 
                   trials=rndtrials, repExp=rndrepExp, executionModel="MultiCore")
EnvList[[2]]<-rndPerformance(DeJongF2Factory(), 
                   trials=rndtrials, repExp=rndrepExp, executionModel="MultiCore")
EnvList[[3]]<-rndPerformance(DeJongF3Factory(), 
                   trials=rndtrials, repExp=rndrepExp, executionModel="MultiCore")
if (stochastic) 
{
EnvList[[4]]<-rndPerformance(DeJongF4Factory(), 
                   trials=rndtrials, repExp=rndrepExp, executionModel="MultiCore")
}
if (!stochastic) 
{
EnvList[[4]]<-rndPerformance(DeJongF4detFactory(), 
                   trials=rndtrials, repExp=rndrepExp, executionModel="MultiCore")
}
EnvList[[5]]<-rndPerformance(DeJongF5Factory(), 
                   trials=rndtrials, repExp=rndrepExp, executionModel="MultiCore")

EnvWeights<-c(1.0, 1.0, 1.0, 1.0, 1.0)
l<-list(); l$EnvList<-EnvList; l$EnvWeights<-EnvWeights
return(l)
}

