
#' Print meta-GA detail tables.
#'
#' @param rMeta   Meta GA experiment data.
#' @param index   Index into sorted list of hyper parameter points.
#'                Default: \code{1}.
#' @param table   A string for the table type:
#'                \itemize{
#'                \item \code{"Summary"}  Summary report. (Default.)
#'                \item \code{"MetaGA"}  Report meta-GA  fitness.
#'                \item \code{"Error"}  Report GA errors (L1, L2, and
#'                      probability that GA solution was within an 
#'                      epsilon environment of the global optimal 
#'                      solution.
#'                \item \code{"Resources"} Probability of termination 
#'                      with an epsilon environmanr of the globael 
#'                      optimal solution and number of actual function 
#'                      evaluations and actual generations until 
#'                      termination.
#'                }
#' 
#' @return Invisible \code{NULL}. 
#'
#'@export
metaGAtables<-function(rMeta, table="Summary", index=1)
{
Options<-options(width=140, scipen=999)
on.exit(options(Options))
full<-metaGASolutionQuality(rMeta, index=1)
h1<-c("OPT", "penvn", "envW", "gOpt")
h1n<-c("Opt!", "f", "w", "gOpt")
# Summary
if (table=="Summary") 
{
h2 <-c("GAfit", "GAstd", "Time", "stdTimeVec", "Obs")
h2n<-c("E(fit) [GA]", "sigma(fit) [GA]", "E(t)", "sigma(t)", "n")
cols<-c(h1, h2)
df<-full[cols]
names(df)<-c(h1n, h2n)
print(df)
return(invisible(NULL))
}
# Meta GA performance
if (table=="MetaGA") 
{
h2 <-c("GAfit", "Fit", "RndPerf", "GAPerf", "Obs")
h2n<-c("E(fit) [GA]", "E(fit) [Meta]", 
       "E(RndPerformance)", "E(GAPerformance)", "n")
cols<-c(h1, h2)
df<-full[cols]
names(df)<-c(h1n, h2n)
print(df)
return(invisible(NULL))
}
# GA Errors
if (table=="Error") 
{
h2<-c("GAfit", "Psuccess", "L1err", "L1errstd", "L2err", "SSshare")
h2n<-c("E(fit) [GA]", "P(d(gOpt, GAopt)<Eps)", "E(L1err)", 
        "sigma(L1err)", "E(L2err)", "share(SS)")
cols<-c(h1, h2)
df<-full[cols]
names(df)<-c(h1n, h2n)
print(df)
return(invisible(NULL))
}
# GA Errors
if (table=="Resources") 
{
h2<-c("GAfit", "Psuccess", "Eps", "afeval", "feval", "agen")
h2n<-c("E(fit) [GA]", "P(d(gOpt, GAopt)<Eps)", "Eps", "E(actual fevals)", 
        "Max(fevals)", "E(actual generations)")
cols<-c(h1, h2)
df<-full[cols]
names(df)<-c(h1n, h2n)
print(df)
return(invisible(NULL))
}

stop("metaGAtables: table ", table, "does not exist!")
}


