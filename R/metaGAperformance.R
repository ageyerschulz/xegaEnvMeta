
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

#' Compute the online performance and the offline performance for 
#' maximization and minimization of a genetic algorithm for a 
#' problem environment.
#'
#' @param solution    Solution of the genetic algorithm.
#' @param stepsT      Number of time steps. Shorter series are padded 
#'                    with last element.
#' @param performanceMeasure  Default \code{NA}: Return named list.
#'                    Available: "Online" and "Offline": 
#'                    Return value of measure. 
#'
#' @return List with
#'     \itemize{
#'       \item \code{penv$OnlineGAFitness}: Online performance of 
#'                                        GA:
#'                                        Average fitness over run.
#'       \item \code{penv$OfflineGAFitness}:  Offline performance of 
#'                         GA: Average of the best fitness vector of all 
#'                             generations. 
#' }
#'    Or value of selected measure.
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
#' F3sol<-xegaRun(F3, algorithm="sga", max=FALSE)
#' F3P<-GAPerformance(F3sol)
#' F3P$OnlineGAFitness
#' F3P$OfflineGAFitness
#' @importFrom utils tail
#' @export
GAPerformance<-function(solution, stepsT=0, performanceMeasure=NA)
{
  fill<-function(vec, stepsT)
      { if (stepsT==0) {return(vec)} 
        l<-length(vec)
        if (stepsT<=l) {return(vec)}
        return(c(vec, rep(utils::tail(vec,1), (stepsT-l))))
      } 

  if (is.na(performanceMeasure))
  {
  r<-list()
     r$OnlineGAFitness<-mean(fill(solution$popStat[,1],stepsT))
     r$OfflineGAFitness<-mean(fill(solution$popStat[,6],stepsT))
  return(r)
  }

  if (performanceMeasure=="Online") 
  { # Online performance: Average fitness of a run. 
     return(mean(fill(solution$popStat[,1],stepsT)))
  }
  if (performanceMeasure=="Offline") 
  { # Offline performance: Mean of the best fitnesses.
     {return(mean(fill(solution$popStat[,6],stepsT)))}
  }

  stop("Label", performanceMeasure, "does not exist in GAperformance.")
}

#' Performance comparison for a problem environment. 
#'
#' @description Print table with known minima, maxima, and 
#'              the online and offline performance measurements 
#'              of random search and a genetic algorith.
#'
#' @details TODO: parameter popsize, generations, ...
#'
#' @param penv        A problem environment.
#' @param trials      Number of random trials.
#'                    Default: \code{NA}. 
#'                    \code{trials} is set to \code{popsize*generations}.
#' @param repExp      Number of repetitions of GA and rnd experiments.
#' @param executionModel  Default: "Sequential". Available: E.g. 
#'                        "MultiCore". See \code{xega::xegaRun()}.
#' @param popsize     Population size.
#' @param generations Number of generations.
#' @param mutrate     Mutation rate.
#' @param bitmutrate  Bit mutation rate.
#' @param crossrate   Crossover rate. 
#' @param verbose     Level of console output. 
#'                    \code{verbose=0}: Quiet (no output).
#'
#' @return A dataframe
#' 
#' @family Performance Measure
#'
#' @examples
#' PerformanceComparison(penv=smoofWrapperFactory(makeSchwefelFunction(2)),
#'                       trials=NA, executionModel="Sequential",
#'                       popsize=10, generations=10, 
#'                       mutrate=1.0, bitmutrate=0.005, crossrate=0.2,
#'                       verbose=0)
#' @importFrom xega xegaRun
#' @export
PerformanceComparison<-function(penv, trials=NA, repExp=100, 
       executionModel="Sequential",
       popsize, generations, mutrate, bitmutrate, crossrate, verbose=0)
{ 
if (is.na(trials)) {trials<-popsize*generations}
newPenv<-rndPerformance(penv=penv, trials=trials, repExp=repExp, 
                        executionModel=executionModel, verbose=verbose)

gaSolution<-xega::xegaRun(
        penv=penv, algorithm="sga", max=penv$max(), 
        executionModel=executionModel, verbose=verbose, evalrep=repExp,
	popsize=popsize, 
	generations=generations, 
	mutrate=mutrate, 
	bitmutrate=bitmutrate, 
	crossrate=crossrate, 
        terminationCondition="AbsoluteError", terminationEps=0.01)

GA<-GAPerformance(gaSolution)

# 
Attribute<-c("Optimum", "Repetitions", "E(GA Online)", "E(GA Offline)") 
Attribute<-c(Attribute, "RndBest", "E(RndBest)", "Sigma(RndBest)" )
Opt<-unlist(rep(list("min", "max")[1+penv$max()], 7))
Value<-c(penv$globalOptimum()$value, repExp,  newPenv$rndBest(), newPenv$ERndBest())
Value<-c(Value, newPenv$stdRndBest(), GA$OnlineGAFitness, GA$OfflineGAFitness)

df<-data.frame(Attribute, Opt, Value)

return(df)
}

#' Signed L2 Distance between GA performance and RND performance.
#'
#' @description If we compare fitness values, set \code{max=TRUE}!
#'
#' @param GAp   GA performance.
#' @param Rndp  Rnd performance.
#' @param max   Boolean. TRUE: Maximized! FALSE: Minimized!
#'
#' @return Signed L2 distance:
#'         \itemize{
#'         \item Positive sign: GA performance higher than RND performance.
#'         \item Negative sign: GA performance lower than RND performance.
#'         }
#'
#' @family Performance Measure
#'
#' @examples
#' F3<-smoofWrapperFactory(makeSphereFunction(4))
#' F3new<-rndPerformance(F3, trials=1000, repExp=1)
#' F3sol<-xegaRun(F3new, algorithm="sga", max=FALSE, popsize=10, generations=10)
#' F3P<-GAPerformance(F3sol)
#' signedL2Dist(GAp=F3P$OnlineGAFitness, Rndp=F3new$rndBest(), max=TRUE)
#' signedL2Dist(GAp=F3P$OfflineGAFitness, Rndp=F3new$ERndBest(), max=TRUE)
#'
#'@export
signedL2Dist<-function(GAp, Rndp, max=TRUE)
{
# cat("Gap:", GAp, "Rndp:", Rndp, "max:", Max, "\n") 
if ((GAp>Rndp) & (max)) {s<-1.0}
if ((!GAp>Rndp) & (max)) {s<-(-1.0)}
if ((GAp>Rndp) & (!max)) {s<-(-1.0)}
if ((!GAp>Rndp) & (!max)) {s<-1.0}

return(s*((GAp-Rndp)^2))
}

