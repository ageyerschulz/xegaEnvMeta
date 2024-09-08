
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
#' @param envWeights Weights of environments. Default: \code{1.0}.
#' @param  name      Name of of the meta genetic algorithm problem environment.
#' @param  repExp    Number of repeated runs of the genetic algorithm.         
#' @param performanceMeasure  Default \code{NA}: Return named list.
#'                    Available: "Online" and "Offline": 
#'                    Return value of measure. 
#' @param pnames     Vector of parameter names.
#' @param bitlength  Vector of bitlengths.
#' @param lb         Vector of lower bounds.
#' @param ub         Vector of upper bounds.
#' @param stepsT     Length of best fitness vectors. Should match the number of generations.
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
#' @param path       Path for result files. Default: \code{""}
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
#' pn<-c("popsize", "generations", "mutrate", "bitmutrate",  
#'       "crossrate", "uCrossSwap")
#' bl<-rep(64,6); lb<-c(rep(10, 2), rep(0.00001, 4)); ub<-c(rep(15, 2), rep(1.0, 4))
#' stepsT<-15
#' path<-xegaTmpDir()
#' UPC<-UPCmetaGAFactory(EnvList, name="UPCShekel5d", 
#'      pnames=pn, bitlength=bl, lb=lb, ub=ub, stepsT=stepsT,
#'      repExp=1, log=1, path=path)
#' a<-xegaRun(penv=UPC, algorithm="sga", max=TRUE, 
#'            popsize=2, generations=2, 
#'            executionModel="Sequential", profile=TRUE, verbose=0, 
#'            path=path)
#'
#' @importFrom xegaSelectGene parm 
#' @importFrom xega xegaRun 
#' @export
UPCmetaGAFactory<-function(envList, 
             envWeights=rep(1.0,1), 
             name="UPC",
             repExp=100, 
             executionModel="Sequential",
             evalmethod="Deterministic",
             performanceMeasure="Offline", 
             pnames,
             bitlength,
             lb,
             ub,
             stepsT,
             terminationCondition="AbsoluteError", 
             terminationEps=0.01,
             verbose=0, 
             log=0,
             path="")
{
self<-list()
self$name<-xegaSelectGene::parm(paste("metaGA", name, sep=""))
self$pnames<-xegaSelectGene::parm(pnames)
self$bitlength<-xegaSelectGene::parm(bitlength)
self$genelength<-function() {sum(self$bitlength())}
self$lb<-xegaSelectGene::parm(lb)
self$ub<-xegaSelectGene::parm(ub)
self$stepsT<-xegaSelectGene::parm(stepsT)
self$repExp<-xegaSelectGene::parm(repExp)
self$verbose=xegaSelectGene::parm(verbose)
self$log=xegaSelectGene::parm(log)
self$executionModel<-xegaSelectGene::parm(executionModel)
self$evalmethod<-xegaSelectGene::parm(evalmethod)
self$terminationCondition<-xegaSelectGene::parm(terminationCondition)
self$terminationEps<-xegaSelectGene::parm(terminationEps)
self$performanceMeasure<-xegaSelectGene::parm(performanceMeasure)
self$path<-xegaSelectGene::parm(path)

###
self$envList=envList
envW<-envWeights
if (all(envWeights==1.0)) {envW<-rep(1.0, length(envList))}
self$envWeights=xegaSelectGene::parm(envW)
t<-self$envWeights()

### get a solution of the inner GA.
penv<-self$envList[[1]]
self$InnerGASolution<-xega::xegaRun(
             penv=penv,
             algorithm="sga",
             evalmethod=self$evalmethod(),
             elitist=TRUE,
             # hyper paramerers start
             popsize=as.integer(self$lb()[1]),
             generations=as.integer(self$lb()[2]),
             mutrate=self$lb()[3],
             bitmutrate=self$lb()[4],
             crossrate=self$lb()[5],
             uCrossSwap=self$lb()[6],
             crossover="UPCross2Gene", 
             # hyper parameters end
             max=penv$max(), evalrep=self$repExp(),
             terminationCondition=self$terminationCondition(),
             terminationEps=self$terminationEps(),
             verbose=self$verbose(),
             executionModel=self$executionModel(),
             path=self$path()
               )

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

      metaGAstats<-InitMetaGAStats(self$envList, self$envWeights())

	for (j in 1:length(self$envList))
	{ penv<-self$envList[[j]] 
	  solution<-xega::xegaRun(
             penv=penv,	algorithm="sga", 
             evalmethod=self$evalmethod(),
             elitist=TRUE, 
             ### Hyperparameter start.   
	     generations=generations, popsize=popsize, 
	     mutrate=mutrate, bitmutrate=bitmutrate,
	     crossrate=crossrate, uCrossSwap=uCrossSwap,
             crossover="UPCross2Gene",
             ### Hyperparameters end.
	     max=penv$max(), evalrep=self$repExp(), 
             terminationCondition=self$terminationCondition(), 
             terminationEps=self$terminationEps(),
             verbose=self$verbose(), 
             executionModel=self$executionModel(),
             path=lF$path()
               )

                  metaGAstats<-UpdateMetaGAStats(
                         row=j,
                         metaGAstats=metaGAstats,
                         penv=penv,
                         solution=solution,
                         stepsT=self$stepsT(),
                   performanceMeasure=self$performanceMeasure())
        }

        GAtime<-sum(metaGAstats$Time)
        GAfit<-sum(metaGAstats$Fit)

if (self$log()==1)
{
 metaGAReporter(name=self$name(), parm=parm, GAfit=GAfit, GAtime=GAtime, 
                experiment=metaGAstats, lF=lF) }

return(GAfit)
}
#### end of fitness
### force promises.
t<-self$name(); t<-self$pnames(); t<-self$bitlength(); t<-self$genelength();
t<-self$lb(); t<-self$ub(); t<-self$stepsT(); t<-self$repExp();
t<-self$verbose(); t<-self$log(); t<-self$executionModel();
t<-self$evalmethod(); t<-self$terminationCondition(); t<-self$terminationEps();
t<-self$path()
return(self)
}

