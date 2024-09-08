
#' Show the k best solutions of a meta GA run.
#'
#' @param rMeta   The result of a metaGA run.
#' @param k       The number of solutions shown
#'
#' @family meta-GA documentation
#'
#'@export
metaGAkBest<-function(rMeta, k=10)
{
opt<-options(width=140, scipen=999)
on.exit(options(opt))

if (k>nrow(rMeta$details)) {k<-nrow(rMeta$details)}

cat("The best ", k, 
    "results of meta-GA experiment ", 
    rMeta$solution$GAenv$penv$name(),"\n") 

print(rMeta$details[1:k,])

}

