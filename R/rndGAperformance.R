
#
# (c) 2021 Andreas Geyer-Schulz
#          Simple Genetic Algorithm in R. V 0.1
#          Problem environments for simple binary or real coded GAs 
#          Package:
#

# TODO: Investigate variance of pure random solutions. 
#       Use `a stable' reference! Move out of loop!

#
# Note, we have rewritten all problem environments as function factories.
#

#' Make a repeatable environment for random trials.
#'
#' @param penv               A problem environment. 
#'                           Needed: \code{penv$max()} which returns \code{TRUE} or \code{FALSE}.
#' @param trials             Number of random trials. 
#' @param executionModel     Default: "Sequential". 
#'                           Available: e.g. "MultiCore". 
#'                           See xegaRun()
#' @param verbose            Default: 0. 
#'                           See xegaRun()
#'
#' @return  A problem environment for xegaRun.
#'
#' @examples
#' F3<-smoofWrapperFactory(makeSphereFunction(4))
#' y<-makeRepEnvFactory(F3, 1000, "MultiCore", 0)
#' y$f(2)
#' @importFrom xega xegaRun
#'@export
makeRepEnvFactory<-function(penv, trials=1000, executionModel="Sequential", verbose=0)
{
  self<-list()
  self$name<-xegaSelectGene::parm("repEnv")
  self$bitlength<-function() {rep(10,2)}
  self$genelength<-function() {sum(self$bitlength())}
  self$lb<-function() {rep(0,2)}
  self$ub<-function() {rep(2,2)}
  self$f<-function(parm, gene=0, lF=0)
   {
     r<-xega::xegaRun(penv=penv, algorithm="sga", max=penv$max(), elitist=FALSE, verbose=verbose, 
                       popsize=trials, generations=1, executionModel)
     return(r$solution$fitness)
   }
  return(self)
}

#' Add the best fitness of \code{trials} random trials to a problem environment. 
#'
#' @description We compute (and add) the 
#'              \enumerate{
#'              \item The best result of a single run of \code{trials}
#'                    trials as well as
#'              \item The mean of the best results of \code{repExp} 
#'                    repeated rund of \code{trials} trials.
#'              }
#' 
#'
#' @param penv         A problem environment.
#' @param trials       Number of random trials of the experiment.
#'                     Default: 1000. 
#' @param repExp       Number of repetitions of  
#'                     experiment with \code{k} trials.
#'                     Default: 100.
#' 
#' @param executionModel Execution model for xegaRun. (Default: "Sequential")
#' @param verbose      Level of console output. 
#'                     \code{verbose=0} (Default): Quiet (no output).
#'
#' @return A problem environment with 
#'     \itemize{
#'       \item \code{$rndBest()}: Best fitness of \code{trials} random trials. 
#'       \item \code{$ERndBest()}: Expected best fitness of k random trials
#'                                 in \code{repExp} repetitions. 
#'       \item \code{$stdRndBest()}: Standard deviation of  
#'                                 best fitness of k random trials
#'                                 in \code{repExp} repetitions. 
#'       \item \code{$rndTrials}:  Number of trails \code{trials}.
#'       \item \code{$rndRepExp}:  Number of repeated experiments \code{repExp}.
#' }
#' 
#' @references 
#'    Grefenstette, John G. (1986)
#'    Optimization of Control Parameters for Genetic Algorithms.
#'    IEEE Transactions on Systems, Man, and Cybernetics, 16(1), 122-128.
#'
#' @family Performance Measure
#'
#' @examples
#' F3<-smoofWrapperFactory(makeSphereFunction(4))
#' F3<-rndPerformance(F3, trials=10, repExp=10, executionModel="Sequential", verbose=0)
#' @importFrom xegaSelectGene parm 
#' @importFrom xega xegaRun
#' @export
rndPerformance<-function(penv, trials=1000, repExp=100, 
        executionModel="Sequential", verbose=0)
{ 
     r1<-xega::xegaRun(penv=penv, algorithm="sga", max=penv$max(), 
                       elitist=FALSE, verbose=verbose, 
                       popsize=trials, generations=1, executionModel)

     repEnv<-makeRepEnvFactory(penv=penv, trials=trials, 
                      executionModel=executionModel, verbose=verbose)
     rRepExp<-xega::xegaRun(penv=repEnv, popsize=1, max=TRUE, 
                      generations=1, evalrep=repExp, 
                      executionModel=executionModel, verbose=0)

     newPenv<-penv
     newPenv$rndBest<-xegaSelectGene::parm(r1$solution$fitness)
     newPenv$ERndBest<-xegaSelectGene::parm(rRepExp$solution$fitness)
     newPenv$stdRndBest<-xegaSelectGene::parm(rRepExp$solution$genotype$sigma)
     newPenv$rndTrials<-xegaSelectGene::parm(trials)
     newPenv$rndRepExp<-xegaSelectGene::parm(repExp)
     t<-force(newPenv)              # force.
     t<-newPenv$rndBest()
     t<-newPenv$ERndBest()
     t<-newPenv$stdRndBest()
     t<-newPenv$rndTrials()
     t<-newPenv$rndRepExp()
     return(newPenv)
}

#' Report rndBest and E(rndBest)
#'
#' @param penv A problem environment.
#'
#' @return A data frame with columns Attribute and Value
#'
#' @examples
#' F3<-smoofWrapperFactory(makeSphereFunction(4))
#' F3new<-rndPerformance(F3, trials=10, repExp=10, executionModel="Sequential", verbose=0)
#' reportEnv(F3new)
#'@export
reportEnv<-function(penv)
{
 Attribute<-c(penv$name(), "$globalOptimum()$value", "rndBest()", "$ERndBest()")
 Attribute<-c(Attribute, "$stdRndBest()", "$rndTrials()", "$rndRepExp")   
 Value<-c(NA, penv$globalOptimum()$value, penv$rndBest(), penv$ERndBest())
 Value<-c(Value, penv$stdRndBest(), penv$rndTrials(), penv$rndRepExp())  
 return(data.frame(Attribute, Value)) 
}

