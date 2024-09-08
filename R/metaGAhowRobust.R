
#' How robust is a solution of a meta GA.
#'
#' @param rMetaRobust   The result of a metaGARobust run.
#'
#' @return Invisible NULL.
#'
#' @family meta-GA documentation
#'
#'@export
metaGAhowRobust<-function(rMetaRobust)
{
opt<-options(width=140, scipen=999)
on.exit(options(opt))

cat("\n Analysis of hyper parameter point:\n", 
     rMetaRobust$solution$solution$phenotype, "\n\n")

print(rMetaRobust$details[1,])

df<-metaGASolutionQuality(rMetaRobust, index=1)

print(df)

return(invisible(NULL))
}

