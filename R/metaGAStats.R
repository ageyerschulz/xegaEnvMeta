
#' Initialize meta-GA statistics
#'
#' @param envList   List of problem environments.
#' @param envW      Weight vector of problem environments.
#'
#' @return An empty data frame of the meta-GA statistics 
#'         with the following columns:
#'         \itemize{
#'           \item \code{$Fit} Fitness of GA run with \code{penv} for metaGA:
#'                 Weighted signed L2-distance of \code{GAPerf} and
#'                 \code{RndPerf}.
#'           \item \code{$Time} Execution time of GA.
#'           \item \code{$RndPerf} Expected performance of random search.
#'           \item \code{$GAPerf}  Expected performance of GA.
#'           \item \code{$GAfit}   E(fitness) of penv for GA run.
#'           \item \code{$GAstd}   std(fitness).
#'           \item \code{$gOpt}    Optimal solution of penv.
#'           \item \code{$early}   If \code{TRUE}, \code{xegaRun()} 
#'                                 terminated early
#'                                 and reached environment of \code{gOpt}
#'                                 specified by the termination condition.
#'           \item \code{$gen}     Number of generations. (Resource limit).
#'           \item \code{$agen}    Actual number of generations used by GA.
#'           \item \code{$feval}   Number of fitness function evaluations.
#'                                 (Resource limit).
#'           \item \code{$afeval}  Actual number of fitness function 
#'                                 evaluations.
#'           \item \code{$penvn}    Name of problem \code{penv}.
#'           \item \code{$envW}    Weights of problem environment.
#'           \item \code{$Obs}     Number of observations.
#'         }
#'
#'@export
InitMetaGAStats<-function(envList, envW)
{ v<-rep(0,length(envList))
  Obs<-rep(1, length(envList))
  stdTimeVec<-gOpt<-GAstd<-GAfit<-GAPerf<-RndPerf<-Time<-Fit<-v
  penvn<-afeval<-feval<-agen<-gen<-early<-Fit
  return(data.frame(Fit=Fit, Time=Time, stdTimeVec=stdTimeVec, 
         RndPerf=RndPerf, GAPerf=GAPerf, GAfit=GAfit, GAstd=GAstd, gOpt=gOpt,
         early=early, gen=gen, agen=agen, feval=feval, afeval=afeval,
         penvn=penvn, envW=envW, Obs=Obs)) 
}

#' Update the meta-GA statistics.
#'
#' @param row   Index of problem environment 
#' @param metaGAstats   Data frame with meta-GA statistics.
#' @param penv      Problem environment.
#' @param solution   Solution of GA run for \code{penv}.
#' @param stepsT     Number of steps for computing GA performance.
#' @param performanceMeasure  Function for performance measure.
#'
#' @inherit InitMetaGAStats return
#'
#'@export
UpdateMetaGAStats<-function(row, metaGAstats, 
                            penv, solution, stepsT, performanceMeasure)
{ 
s<-metaGAstats
RndPerf<-penv$ERndBest()
GAPerf<-GAPerformance(solution, stepsT=stepsT, 
             performanceMeasure=performanceMeasure)
s$RndPerf[row]<-RndPerf
s$GAPerf[row]<-GAPerf
s$Fit[row]<-s$envW[row]*signedL2Dist(GAPerf, RndPerf)
s$Time[row]<-solution$timer$tMainLoop
s$penvn[row]<-penv$name()
s$GAfit[row]<-solution$solution$genotype$fit
if (!is.null(solution$solution$genotype$sigma))
       {s$GAstd[row]<-solution$solution$genotype$sigma}
s$gOpt[row]<-penv$globalOptimum()$value
gens<-1+solution$GAenv$generations
s$gen[row]<-gens-1
agens<-nrow(solution$popStat)
s$agen[row]<-agens
s$early[row]<-(agens<gens)
s$feval[row]<-gens*solution$GAenv$popsize
s$afeval[row]<-agens*solution$GAenv$popsize
return(s)
}

