
#' Factory for function F3 (5-dimensional step function)
#'
#' @description This function factory sets up the problem environment
#'              for De Jong's function F3.
#' F3 is a 5-dimensional step function.
#' It is a discontinuous, non-convex, unimodal function
#' which is piecewise constant:
#' There are infinitely many points with the optimal value.
#' For the given bounds: Minima: \eqn{x_{i}<5} and Maxima: \eqn{x_{i}>5} 
#' forall i.
#'
#' @references 
#' De Jong. Kenneth A. (1975):
#' \emph{An Analysis of the Behavior of a Class of Genetic Adaptive Systems.}
#' PhD thesis, Michigan, Ann Arbor, pp. 200-204.
#'
#' @inherit DeJongF1bFactory return
#'
#' @family problem environments
#' @family De Jong functions
#'
#' @examples
#' DeJongF3<-DeJongF3Factory()
#' DeJongF3$name()
#' DeJongF3$bitlength()
#' DeJongF3$genelength()
#' DeJongF3$lb()
#' DeJongF3$ub()
#' DeJongF3$f(c(2.01, -1.05, 4.95, -4.3, -3.0))
#' DeJongF3$describe()
#' DeJongF3$solution()
#' @importFrom stats runif
#' @export
DeJongF3Factory<-function() {
self<-list()
self$name<-function() {"DeJongF3"}
self$bitlength<-function() {c(64, 64, 64, 64, 64)}
self$genelength<-function() {sum(self$bitlength())}
self$lb<-function() {rep(-5.12,5)}
self$ub<-function() {rep(5.12,5)}
self$f<-function(parm, gene=0, lF=0) {  sum(floor(parm)) }
self$describe=function() {
cat("See", "\n")
cat("De Jong (1975) ")
cat("An Analysis of the Behavior of a Class of Genetic Adaptive Systems.", "\n")
cat("PhD thesis, Michigan, Ann Arbor, pp. 200-204", "\n\n")
cat("F3 is a 5-dimensional step function.", "\n")
cat("It is a discontinuous, non-convex, unimodal function", "\n")
cat("which is piecewise constant:", "\n")
cat("There are infinitely many points with the optimal value.", "\n")
cat("For the given bounds: Minima: $x_{i}<5$ and Maxima: $x_{i}>5$ forall i.", "\n")
return(invisible(NULL))}
self$minp=function()
	{ rep(-5.0-(0.12*stats::runif(1))-10E-7)}
self$maxp=function()
	{ rep(5.0+(0.12*stats::runif(1))+10E-7)}
self$solution=function() {
            s<-list()
            s[["minimum"]]<--30
            s[["minpoints"]]<-
            list(rep(-5.12,5), 
		 lapply((1:5), FUN=function(x){self$minp()}))
            s[["maximum"]]<-25
            s[["maxpoints"]]<-
	    list(rep(5.12,5), 
	     lapply((1:5), FUN=function(x){self$maxp()}))
            return(s)
            }
self$max<-function() {return(FALSE)}
self$globalOptimum<-function()
{l<-list();l$param<-rep(-5.12, 5); l$value<--30; l$is.minimum<-TRUE;return(l)}
t<-self$name(); t<-self$bitlength(); t<-self$genelength(); t<-self$lb()
t<-self$ub(); t<-self$max(); t<-self$globalOptimum(); t<-self$describe()
t<-self$solution(); t<-self$f(rep(1,5))
return(self)
}
