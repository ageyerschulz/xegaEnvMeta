
#' The environments of the meta GA experiment.
#'
#' @description Generate a dataframe which 
#'              describes the dataframes in the meta GA experiment.
#'
#' @param solution  A solution of the meta GA.
#'
#' @return Data frame with the columns
#'         \itemize{
#'         \item Environment: The name of the problem environment.
#'         \item Max!:        \code{1}, if maximization. \code{0}, if minimization.
#'         \item gOpt:        Global Optimum.
#'         \item Trials:      Number of random trials.
#'         \item Rep:         Number of repetitions of \code{Trials} random trials.
#'         \item rndBest:     Best result in \code{Trials} random trials without repetition.
#'         \item E(RndBest):  Mean of \code{Rep} best random results of \code{Trials}.
#'         \item std(RndBest): Standard deviation of \code{Rep} best random results of \code{Trials}.
#'         }

#'
#'@export
metaGAhyperEnv<-function(solution)
{
envs<-solution$GAenv$penv$envList
l<-length(envs)
n<-rep("",l)
rndBest<-ERndBest<-stdRndBest<-Rep<-Trials<-max<-gOpt<-rep(0,l)
for (i in 1:l)
{
penv<-envs[[i]]
n[i]<-penv$name()
max[i]<-penv$max()
if (penv$max()) {sign<-1} else {sign<-(-1)}
gOpt[i]<-penv$globalOptimum()$value
Trials[i]<-penv$rndTrials()
Rep[i]<-penv$rndRepExp()
rndBest[i]<-sign*penv$rndBest()
ERndBest[i]<-sign*penv$ERndBest()
stdRndBest[i]<-penv$stdRndBest()
}

df<-data.frame(n, max, gOpt, Trials, Rep, rndBest, ERndBest, stdRndBest)
names(df)<-c("Environment", "Max!", "gOpt", "Trials", "Rep", "rndBest", "E(rndBest)", "sigma(RndBest)") 
return(df)
}

#' The hyper parameter space of the meta GA experiment.
#'
#' @description Without all the constant parameters of \code{xega::xegaRun}. 
#'
#' @param solution  A solution of the meta GA.
#'
#' @return Data frame with the columns
#'         \itemize{
#'         \item MetaEnvironment: The name of the problem environment for the meta GA experiment.
#'         \item Parameters:      The parameters of \code{xega::xegaRun} used in the experiment.
#'         \item Lower:           The lower bound of the parameter space.
#'         \item Upper:           The upper bound of the parameter space.
#'         \item Bits:            Number of bits for coding the parameter.
#'         }
#'
#'@export
metaGAhyperParameterSpace<-function(solution)
{
metapenv<-solution$GAenv$penv
metapnames<-metapenv$pnames()
metaenvname<-rep(metapenv$name(), length(metapnames))
lb<-metapenv$lb()
ub<-metapenv$ub()
bitlength<-metapenv$bitlength()
df<-data.frame(metaenvname, metapnames,lb, ub, bitlength)
names(df)<-c("MetaEnvironment", "Parameters", "Lower", "Upper", "Bits")
return(df)
}

#' The parameters of the meta GA.
#'
#' @param solution  A solution of the meta GA.
#'
#'@export
metaGAParameters<-function(solution)
{
pnames<-c("penv", "algorithm", "max", "popsize", "generations", 
        "crossrate", "mutrate", "bitmutrate", "elitist", "evalrep", "excutionModel")

metapenv<-solution$GAenv$penv

vals<-c( metapenv$name(), solution$GAenv$algorithm, solution$GAenv$max, solution$GAenv$popsize,
solution$GAenv$generations, solution$GAenv$crossrate, solution$GAenv$mutrate, solution$GAenv$bitmutrate,
solution$GAenv$elitist, solution$GAenv$evalrep, solution$GAenv$executionModel
)

# df<-data.frame(pnames, pvalues)
df<-data.frame(pnames, vals)
names(df)<-c("Meta GA Parameters", "Values")
return(df)
}


#' The description of a meta GA experiment.
#'
#' @param rMeta   The result of a meta-GA run.
#'
#' @family meta-GA documentation
#'
#' @return invisible(NULL)
#'@export
metaGAExperiment<-function(rMeta)
{
opt<-options(width=120, scipen=999)
on.exit<-options(opt)

cat("The meta GA experiment ",
     rMeta$solution$GAenv$penv$name()," (",
     format(Sys.time(), "%a %b %d %X %Y"),
     "):\n")

cat("\n The environments and the performance of random search:\n\n")
print(metaGAhyperEnv(rMeta$solution))

cat("\n The hyper parameter space:\n\n")
print(metaGAhyperParameterSpace(rMeta$solution))

cat("\n The parameters of the  meta GA:\n\n")
print(metaGAParameters(rMeta$solution))

return(invisible(NULL))
}
