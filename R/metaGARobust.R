
#' Analysis of the robustness of the hyper parameters of a meta GA solution.
#'
#' @description \code{metaGARobust()} 
#'              \itemize{
#'              \item checks the robustness
#'              of the hyper parameters of a meta GA solution or
#'              \item evaluates the hyper parameters of a meta GA solution 
#'              on an evaluation set of problem environments. 
#'              }
#'
#' @param name  Name of the robustness analysis.
#' @param metaGAFactory  Meta GA factory used for the definition 
#'                       of the meta GA experiment. 
#' @param rMeta          A solution of a meta GA experiment.
#' @param hyperIndex     Index of hyper parameter point.   
#' @param reps           Number of repeated GA runs. Default: \code{16}.
#' @param envListEval    List of problem environments for evaluation.
#'                       Default: \code{list()}         
#' @param path           Path for writing result file(s). Default: \code{""}.
#'
#' @return Named list rMeta 
#'
#'@export
metaGARobust<-function(name, metaGAFactory, rMeta,  
                       reps=16, hyperIndex=0, envListEval=list(), path="")
{
pn<-rMeta$solution$GAenv$penv$pnames()
bl<-rMeta$solution$GAenv$penv$bitlength()
if (hyperIndex==0)
{ lb<-rMeta$solution$solution$phenotype
ub<-rMeta$solution$solution$phenotype }
else 
{lb<-unlist(rMeta$details[hyperIndex,(6:(ncol(rMeta$details)))])
ub<-unlist(rMeta$details[hyperIndex,(6:(ncol(rMeta$details)))]) }
stepsT<-rMeta$solution$GAenv$penv$stepsT()

if (0==length(envListEval))
{ # Robustness evaluation.
EnvList<-rMeta$solution$GAenv$penv$envList} 
else
{ # Evaluation of parameters on new environments
EnvList<-envListEval} 

EnvWeights<-rMeta$solution$GAenv$penv$envWeights()

IV<-metaGAFactory(EnvList, EnvWeights, name=name,
         repExp=1, executionModel="Sequential",
         pnames=pn, bitlength=bl, lb=lb, ub=ub, stepsT=stepsT,
         terminationCondition="AbsoluteError", 
         terminationEps=0.1, log=1,
         path=path)

a<-xegaRun(penv=IV, algorithm="sga", max=TRUE,
         #   popsize=50, generations=20, evalrep=10, 
             popsize=reps, generations=1, evalrep=1,
            executionModel="MultiCoreHet", profile=TRUE, verbose=4,
            logevals=TRUE, batch=TRUE, path=path)

nrMeta<-metaGApostProcessing(solution=a, path)

# cat("\n")
#
#metaGAExperiment(nrMeta)
#
#opt<-options(width=140, scipen=999)
#
#np<-min(5, length(nrMeta$experiment))
#
#for (i in (1:np))
#{
#cat("\n Analysis of hyper parameter point:", nrMeta$solution$solution$phenotype, "\n\n")
#print(nrMeta$details[i,])
#cat("\n Analysis of hyper parameter point:", 
#nrMeta$solution$solution$phenotype, "\n\n")
#df<-metaGASolutionQuality(nrMeta, i)
#print(df) }

cat("\n Robustness Analysis of ", name, "finished!\n")

return(nrMeta)
}

