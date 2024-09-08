
#
# (c) 2024 Andreas Geyer-Schulz
#          Meta GA examples.
#

#' Evaluate a meta GA hyper parameter point. 
#' 
#' @param rMeta   Meta GA experiment data.
#' @param index   Index into sorted list of hyper parameter points. 
#'
#' @return A dataframe with the following columns:
#'         \itemize{
#'         \item \code{f}  Name of the problem environment.
#'         \item \code{Opt!} Max! or Min! 
#'         \item \code{gOpt} Global Optimum.
#'         \item \code{E(GAopt)} Average optimum solution 
#'                of repeated runs of a genetic algorithm 
#'                              at a hyper parameter point.
#'         \item \code{sigma(GAopt)} Its standard deviation.
#'         \item \code{E(L2-Error)} Average least squares optimization error.
#'         \item \code{\%-L2-Error} Error share of the L2 error of 
#'               the experiment for the problem environment.
#'         \item \code{E(L1-Error)} Average L1-optimization error.
#'         \item \code{E(t)} Average GA execution time in seconds.
#'         \item \code{sigma(t)} Its standard deviation.
#'         \item \code{n} Number of trials (evaluations) of hyper parameter point. 
#'         }
#'
#'@importFrom stats sd
#'@export
metaGASolutionQuality<-function(rMeta, index=1)
{
envList<-rMeta$solution$GAenv$penv$envList
exps<-rMeta$experiment[[index]]$exp
# cat("MetaGASolutionQuality\n")
# print(names(exps))
# aggregate results per environment.
### TBD
envs<-unique(exps$penvn)
#cat("penvs:\n")
#print(envs)
nenvs<-length(envs)
aexps<-exps[1:nenvs,]
aexps<-data.frame(matrix(0, nrow=nrow(aexps), ncol=(ncol(aexps))))
names(aexps)<-names(exps)
aexps$penvn<-envs
# cat("aggregated:\n")
# print(aexps)
for (i in (1:nenvs)) 
{  
   env<-envs[i]
#   cat("env:", env, "\n")
   m<-exps$penvn %in% env
#   print(m)
   aexps$envW[i]<-unique(exps$envW[m])
   aexps$gOpt[i]<-unique(exps$gOpt[m])
   tv<-exps$Time[m]; 
   aexps$Time[i]<-mean(tv); aexps$Obs[i]<-length(tv)
   aexps$stdTimeVec[i]<-stats::sd(tv)
   if (envList[[i]]$max()) {sign<-1; aexps$OPT[i]<-"Max!"} else {sign<-(-1);aexps$OPT[i]<-"Min!"}
   aexps$Fit[i]<-mean(exps$Fit[m])
   GAopt<-sign*exps$GAfit[m]
   aexps$GAfit[i]<-mean(GAopt)
   aexps$GAstd[i]<-stats::sd(GAopt)
   aexps$RndPerf[i]<-mean(exps$RndPerf[m])
   aexps$GAPerf[i]<-mean(exps$GAPerf[m])
   aexps$L1err[i]<-mean(abs(exps$gOpt[m] - GAopt))
   aexps$L2err[i]<-mean((exps$gOpt[m] - GAopt)^2)
   aexps$L1errstd[i]<-stats::sd(abs(exps$gOpt[m] - GAopt))
   aexps$early[i]<-sum(exps$early[m])
   aexps$Psuccess[i]<-aexps$early[i]/aexps$Obs[i]
   aexps$gen[i]<-unique(exps$gen[m])
   aexps$agen[i]<-mean(exps$agen[m])
   aexps$feval[i]<-unique(exps$feval[m])
   aexps$afeval[i]<-mean(exps$afeval[m])
   aexps$terminate[i]<-rMeta$solution$GAenv$penv$terminationCondition()
   aexps$Eps[i]<-rMeta$solution$GAenv$penv$terminationEps()
}
aexps$SSshare<-aexps$L2err/sum(aexps$L2err)
#df<-data.frame(envs, OPT, gOpt, GAo, GAostd, L2err, SSshare, L1err, L1errstd, tV, stdtV, Obs)
#names(df)<-c("f", "Opt!", "gOpt", "E(GAopt)", "sigma(GAopt)", "E(L2-Error)", "%-L2",
#             "E(L1-Error)", "sigma(L1-error)", "E(t)", "sigma(t)", "n")
return(aexps)
}

