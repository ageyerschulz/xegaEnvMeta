
#' Check consistency of meta GA environment definition for sga
#'
#' @param self  Meta GA environment.
#'
#' @return \code{TRUE}, if \code{self} is consistent.
#'
#'@export
metaGAFactoryCheckSGA<-function(self)
{
pnms<-names(self)
needed<-c("name", "pnames", "lb", "ub", "bitlength") 
nmsexist<-needed %in% pnms
if (!all(nmsexist)) 
{stop("penv ", self$name(),". Missing penv elements: ", toString(needed[!nmsexist]))}  
l<-length(self$pnames())
if (!l==length(self$lb()))
{stop("penv ", self$name(),". Length of self$pnames() must equal length of self$lb()")}  
if (!l==length(self$ub()))
{stop("penv ", self$name(),". Length of self$pnames() must equal length of self$ub()")}  
if (!l==length(self$bitlength()))
{stop("penv ", self$name(),". Length of self$pnames() must equal length of self$bitlength()")}  
return(TRUE)
}
