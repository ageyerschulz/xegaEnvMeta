
#' Run meta GA.
#'
#' @param name           Name of meta GA experiment.
#' @param metaGAFactory  Meta GA factory used for the definition 
#'                       of the meta GA experiment.
#' @param EnvList        List of problem environments.
#' @param EnvWeights     Weights of environments in meta GA experiment. 
#' @param pnames         Hyper parameter name vector.
#' @param bitlength      Vector of bit lengths.
#' @param lb             Vector of lower bounds. 
#' @param ub             Vector of upper bounds. 
#' @param stepsT         Length of best fitness vector. Should match the number of generations.
#' @param terminationCondition  Early termination condition. Default: \code{"AbsoluteError"}.
#' @param terminationEps Termination interval 
#'                       (\code{[gOpt-TerminationEps, gOpt+TerminationEps]}.
#'                       Default: \code{0.1}.
#' @param popsize        Population size of meta GA.
#' @param generations    Number of generations of meta GA.
#' @param evalrep        Number of repeated evaluations of a hyper parameter point.
#' @param path           Path for writing output files. Default: \code{""}.
#'
#' @inherit metaGApostProcessing return
#'
#'@export
metaGARun<-function(name, metaGAFactory, EnvList, EnvWeights,
                    pnames, bitlength, lb, ub, stepsT,   
                    terminationCondition="AbsoluteError", 
                    terminationEps=0.1, 
                    popsize, generations, evalrep, path="")
{
# Generate Factory for 1 run of a GA per parameter set, 35 repetitions.
IV<-metaGAFactory(EnvList, EnvWeights, name=name, 
         repExp=1, executionModel="Sequential", 
         pnames=pnames, bitlength=bitlength, lb=lb, ub=ub, stepsT=stepsT,
         terminationCondition=terminationCondition, 
         terminationEps=terminationEps, log=1, path=path)

OK<-metaGAFactoryCheckSGA(IV)

# Run meta GA for finding the best parameter set. 
a<-xegaRun(penv=IV, algorithm="sga", max=TRUE,  
            popsize=popsize, generations=generations, evalrep=evalrep, 
            bitmutrate=0.05,
            executionModel="MultiCoreHet", profile=TRUE, verbose=2,
            logevals=TRUE, batch=TRUE, path=path)

cat("\n meta GA finished\n")

rMeta<-metaGApostProcessing(solution=a, path=path)

cat("\n meta GA post processing finished!\n")

return(rMeta)
}
 
