
#' Sensitivity analysis of the hyper parameters of a meta GA solution.
#'
#' @description \code{metaGASensitivity()} 
#'              \itemize{
#'              \item performa sensitivity analysis.
#'              }
#'
#'              TBD: Simplify File I/O.
#'
#' @param name  Name of the robustness analysis.
#' @param metaGAFactory  Meta GA factory used for the definition 
#'                       of the meta GA experiment. 
#' @param solution       A solution of a meta GA experiment.
#' @param deltas         Vector of allowed deviations from solution parameter.
#' @param reps           Number of repeated GA runs. Default: \code{16}.
#' @param envListEval    List of problem environments for evaluation.
#'                       Default: \code{list()}         
#'
#' @return Named list rMeta 
#'
#'@export
metaGASensitivity<-function(name, metaGAFactory, solution, deltas 
                       reps=16, envListEval=list())
{
pn<-solution$GAenv$penv$pnames()
bl<-solution$GAenv$penv$bitlength()
#### proper upper and lower bounds. 
nlb<-solution$solution$phenotype-deltas
lb<-solution$solution$phenotype-deltas
lb<-solution$solution$phenotype-deltas
nub<-solution$solution$phenotype+deltas
ub<-solution$solution$phenotype+deltas
ub<-solution$solution$phenotype+deltas
###
stepsT<-solution$GAenv$penv$stepsT()
if (0==length(envListEval))
{ # Robustness evaluation.
EnvList<-solution$GAenv$penv$envList} 
else
{ # Evaluation of parameters on new environments
EnvList<-envListEval} 

EnvWeights<-solution$GAenv$penv$envWeights()

IV<-metaGAFactory(EnvList, EnvWeights, name=name,
         repExp=1, executionModel="Sequential",
         pnames=pn, bitlength=bl, lb=lb, ub=ub, stepsT=stepsT,
         terminationCondition="AbsoluteError", terminationEps=0.1, log=1)

a<-xegaRun(penv=IV, algorithm="sga", max=TRUE,
         #   popsize=50, generations=20, evalrep=10, 
             popsize=reps, generations=1, evalrep=1,
            executionModel="MultiCoreHet", profile=TRUE, verbose=2,
            logevals=TRUE, batch=TRUE)

rMeta<-metaGApostProcessing(solution=a)

cat("\n")

metaGAExperimentDoc(a)

opt<-options(width=140, scipen=999)

np<-min(5, length(rMeta$experiment))

for (i in (1:np))
{cat("\n Analysis of hyper parameter point:", rMeta$solution$solution$phenotype, "\n\n")
df<-metaGASolutionQuality(rMeta, i)
print(df) }

cat("Robustness Analysis of ", name, "finished!\n")
return(rMeta)
}

