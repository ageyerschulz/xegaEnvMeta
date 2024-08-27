
#
# (c) 2024 Andreas Geyer-Schulz
#          Meta GA examples.
#

#' Meta GA filenames.
#'
#' @param name   Name of meta GA experiment.
#' @param path   Directory path. Default: "".
#'
#' @return Name list of
#'         \itemize{
#'         \item \code{$fnsolution}  File name of \code{xega::xegaRun()} 
#'               meta GA solution.
#'         \item \code{$fnexperiment} File name of list of 
#'                 results of all hyper parameter points evaluated
#'                 for solving all problem environments in the experiment.
#'         \item \code{$fndetails}  Filename of data frame of 
#'                 aggregated experiments.
#'         \item \code{$fnlog} Preamble of log file name. 
#'               Each log contains the result of the experiment for 
#'               one hyper parameter point.
#'          } 
#'
#' @export
metaGAfn<-function(name, path="")
{ translate<-function(fn) {return(chartr(old=" :", new="--", fn))}
  ts<-Sys.time()
  fnsolution<-paste(path,"xega", name,"-Solution-", ts, ".rds", sep="")
  fnexperiment<-paste(path,"xega", name,"-Experiment-", ts, ".rds", sep="")
  fndetails<-paste(path,"xega", name,"-Details-", ts, ".rds", sep="")
  fnlog<-paste(path,"xega", name,"-Log-", sep="")
  l<-list()
  l$solutionFn<-translate(fnsolution)
  l$log<-chartr(old=" ", new="-", fnlog)
  l$experiment<-translate(fnexperiment)
  l$details<-translate(fndetails)
  return(l) 
}

#' Logs results of a repeated single Meta-GA experiment.
#'
#' @param name   Name of experiment.
#' @param GAfit  GA performance: The sum of the squared difference between
#'               the GA performance and the performance of a random 
#'               search for a problem environment summed over all 
#'               problem environments of the experiment. 
#' @param GAtime Execution time of the experiment.  
#' @param parm  The parameters of the experiment.
#' @param experiment  The results of the experiment as a data frame with the following columns:
#'               \itemize{
#'               \item \code{penvVec}
#'               \item \code{gOptvec}
#'               \item \code{GAFitVec}
#'               \item \code{GAstdVec}
#'               \item \code{GAPerfVec}
#'               \item \code{RndPerfVec}
#'               \item \code{FitVec}
#'               \item \code{TimeVec}
#'               }
#' @param lF     The local function configuration.
#' 
#' @return \code{NULL} invisibly.
#'
#' @family Reporter
#'
#'@export
metaGAReporter<-function(name, GAfit, GAtime, parm, experiment, lF)
{
#   cat("Reporter\n")
   r<-list()
   r$name<-name
   r$GAfit<-GAfit
   r$GAtime<-GAtime
   r$param<-parm
   r$experiment<-experiment
 #  cat("fn: \n")
 #  cat("Path exists:",("path" %in% names(lF)), "\n")
 #  fn<-paste(lF$path(),"xega", name,"-",Sys.time(), ".rds", sep="") # nocov
fn<-paste(metaGAfn(name, path=lF$path())$log, Sys.time(), ".rds", sep="") # nocov
   fn<-chartr(old=" :", new="--", fn)                  # nocov
   saveRDS(object=r, file=fn)                    # nocov
}

