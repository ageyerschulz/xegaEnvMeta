
#
# (c) 2024 Andreas Geyer-Schulz
#          Meta GA examples.
#

#' Factory for optimizing the basic parameters of a GA with parameterized uniform crossover.
#'
#' @description The function factory \code{UPCMetaGAFactory} sets up the problem environment 
#'              for a binary coded meta GA for a vector of problem environments.
#'              The genetic operators of the GA are bit-mutation and
#'              parameterized uniform crossover.  
#'              The GA parameters optimized are
#'              \enumerate{
#'		\item Population size \code{popsize} in \code{[10, 500]}.
#'		\item Generations \code{generations} in \code{[10, 100]}.
#'		\item Mutation rate (global) \code{mutrate} in \code{[0.001, 1.0]}.
#'		\item Bit mutation rate (local) \code{bitmutrate} in \code{[0.0001, 0.5]}.
#'		\item Crossover rate \code{crossrate} (global) in \code{[0.001, 0.5]}.
#'              \item Swap rate \code{uCrossSwap} (local) in \code{[0.001, 0.5]}.
#'              }
#'
#' @details The factory contains all functions needed for 
#'          the genetic algorithm with binary coded genes 
#'          of package \code{xega}.
#'
#' @param  envList   List of problem environments.
#' @param  name      Name of of the meta genetic algorithm problem environment.
#' @param  repExp    Number of repeated runs of the genetic algorithm.         
#' @param  example   Boolean. If \code{TRUE}, use small population size and
#'                   and a small number of generations.
#' @param performanceMeasure  Default \code{NA}: Return named list.
#'                    Available: "Online" and "Offline": 
#'                    Return value of measure. 
#' @param terminationCondition    Default: "AbsoluteError". 
#'                   See \code{xega::xegaRun()}.
#' @param terminationEps    Default: 0.01 
#'                   See \code{xega::xegaRun()}.
#' @param evalmethod Default: "Deterministic".
#'                   See \code{xega::xegaRun()}.
#' @param executionModel  Default: "Sequential". Available: E.g. 
#'                        "MultiCore". 
#'                   See \code{xega::xegaRun()}.
#' @param verbose    Boolean. If \code{TRUE}, print fitnesses 
#'                   of inner and outer GA on console.
#' @param log        Default: \code{0}. 
#'                   Print console log.
#'
#' @return A meta GA problem environment which is  list of functions
#'         from which we can dispatch the following functions:
#'         \itemize{
#'         \item \code{$name()}: Returns the name of the problem environment.
#'         \item \code{$bitlength()}: Returns a vector whose 
#'               elements specify the bit length of each parameter.
#'         \item \code{$genelength()}: Returns the length of the gene.
#'         \item \code{$lb()}: The vector of lower bounds of parameters. 
#'         \item \code{$ub()}: The vector of upper bounds of parameters. 
#'         \item \code{$f(parm, gene, lF)}: Returns the function value f at point parm.
#'         \item \code{$repExp()}: The number of repeated GA runs.
#'         \item \code{$envList()}: The list of problem environments of the meta GA.
#'         }
#' 
#' @family MetaGA-Factory
#'
#' @examples
#' EnvList<-list()
#' EnvList[[1]]<-rndPerformance(smoofWrapperFactory(makeShekelFunction(5)), trials=10, repExp=5)
#' UPC<-UPCmetaGAFactory(EnvList, "P5Shekel5d", repExp=1, example=TRUE, log=1)
#' a<-xegaRun(penv=UPC, algorithm="sga", max=TRUE, 
#'            popsize=2, generations=2, 
#'            executionModel="Sequential", profile=TRUE, verbose=0)
#' # EnvList[[2]]<-rndPerformance(smoofWrapperFactory(makeSchwefelFunction(2)))
#' # P6<-P5metaGAFactory(EnvList, "P6Shekel2dSchwefel2d", repExp=1, example=TRUE,
#' #                    verbose=FALSE)
#' # b<-xegaRun(penv=P6, algorithm="sga", max=TRUE, 
#' #           popsize=2, generations=2, 
#' #           executionModel="Sequential", profile=TRUE)
#'
#' @importFrom xegaSelectGene parm 
#' @importFrom xega xegaRun 
#' @export
UPCmetaGAFactory<-function(envList, name="UPC",
             repExp=100, executionModel="Sequential",
             evalmethod="Deterministic",
             performanceMeasure="Offline", 
             terminationCondition="AbsoluteError", terminationEps=0.01,
             example=FALSE, verbose=0, log=0)
{
self<-list()
self$name<-xegaSelectGene::parm(paste("metaGA", name))
self$bitlength<-function() {rep(64,6)}
self$genelength<-function() {sum(self$bitlength())}
self$pnames<-function() {
     c("popsize", "generations", "mutrate", 
       "bitmutrate", "crossrate", "uCrossSwap")}
self$lb<-function() {c(10, 10, 0.001, 0.0001, 0.001, 0.0001)}
if (example)
 {self$ub<-function() {c(15, 15, 1.0, 0.5, 0.5, 0.5)}
  self$repExp<-xegaSelectGene::parm(repExp)
  self$stepsT<-xegaSelectGene::parm(15)
 }
else
 {self$ub<-function() {c(1000, 1000, 1.0, 0.5, 0.5, 0.5)}
  self$repExp<-xegaSelectGene::parm(repExp)
  self$stepsT<-xegaSelectGene::parm(1000)
}
self$verbose=xegaSelectGene::parm(verbose)
self$log=xegaSelectGene::parm(log)
self$executionModel<-xegaSelectGene::parm(executionModel)
self$evalmethod<-xegaSelectGene::parm(evalmethod)
self$terminationCondition<-xegaSelectGene::parm(terminationCondition)
self$terminationEps<-xegaSelectGene::parm(terminationEps)
self$performanceMeasure<-xegaSelectGene::parm(performanceMeasure)

###

self$envList=envList
### fitness function.
self$f=function(parm, gene=0, lF=0) 
	{
        ### set up parameters
		popsize=as.integer(parm[1])
		generations=as.integer(parm[2])
		mutrate=parm[3]
		bitmutrate=parm[4]
		crossrate=parm[5]
                uCrossSwap=parm[6]

	FitVec<-rep(0, length(self$envList))
	TimeVec<-rep(0, length(self$envList))
	RndPerfVec<-rep(0, length(self$envList))
	GAPerfVec<-rep(0, length(self$envList))
	GAfitVec<-rep(0, length(self$envList))
	GAstdVec<-rep(0, length(self$envList))
	gOptVec<-rep(0, length(self$envList))
	penvVec<-rep(NA, length(self$envList))
	for (j in 1:length(self$envList))
	{ penv<-self$envList[[j]] 
        #  cat("calling xegaRun with penv", penv$name(),"\n")
	  solution<-xega::xegaRun(
             penv=penv,	algorithm="sga", 
             evalmethod=self$evalmethod(),
             elitist=TRUE, 
	     generations=generations, popsize=popsize, 
             crossover="UPCross2Gene",
	     crossrate=crossrate, uCrossSwap=uCrossSwap,
	     mutrate=mutrate, bitmutrate=bitmutrate,
	     max=penv$max(), evalrep=self$repExp(), 
             terminationCondition=self$terminationCondition(), 
             terminationEps=self$terminationEps(),
             verbose=self$verbose(), 
             executionModel=self$executionModel()
               )
        #  cat("Return from xegaRun with penv", penv$name(),"\n")
          RndPerf<-penv$ERndBest()
          GAPerf<-GAPerformance(solution, stepsT=self$stepsT(), 
                   performanceMeasure=self$performanceMeasure()) 
          RndPerfVec[j]<-RndPerf 
          GAfitVec[j]<-solution$solution$genotype$fit
          if (!is.null(solution$solution$genotype$sigma)) 
                  {GAstdVec[j]<-solution$solution$genotype$sigma}
          GAPerfVec[j]<-GAPerf 
          gOptVec[j]<-penv$globalOptimum()$value
          penvVec[j]<-penv$name()
          TimeVec[j]<-solution$timer$tMainLoop
          FitVec[j]<-signedL2Dist(GAPerf, RndPerf)    
        }

        GAtime<-sum(TimeVec)
        GAfit<-sum(FitVec)

if (self$log()==1)
{
        df<-data.frame(penvVec, gOptVec, GAfitVec, GAstdVec, 
                       GAPerfVec, RndPerfVec, FitVec, TimeVec)

#        cat("Path:\n")
#        cat("UPC$f Path:", lF$path(),  "\n")
               

        metaGAReporter(name=self$name(), parm=parm, GAfit=GAfit, 
                             GAtime=GAtime, experiment=df, lF=lF)
}

return(GAfit)
}
#### end of fitness


return(self)
}

