
#' Factory for function F2 (Rosenbrock's saddle)
#'
#' @description This function factory sets up the problem environment
#'              for De Jong's function F2.  
#'          F2 is a standard test function in optimization 
#'          first proposed by Rosenbrock (1960). 
#'          F2 is a quartic function with a deep parabolic valley.
#'          along the curve \code{x[1]^2==x[1]^2}. 
#'  It is a continuous, non-convex, unimodal, low-dimensional quartic function. 
#'          Its minimum is \code{0} at point \code{c(1, 1)}.
#'
#' @references Rosenbrock, H. H. (1960):
#'  \emph{An automatic method for finding the greatest or least value of 
#'        a function}. The Computer Journal. Vol. 3, p. 175.
#'
#' @inherit DeJongF1Factory return
#'
#' @family De Jong benchmark functions
#'
#' @examples
#' DeJongF2<-DeJongF2Factory()
#' DeJongF2$name()
#' DeJongF2$bitlength()
#' DeJongF2$genelength()
#' DeJongF2$lb()
#' DeJongF2$ub()
#' DeJongF2$f(c(2.01, -1.05))
#' DeJongF2$describe()
#' DeJongF2$solution()
#' @export
DeJongF2Factory<-function() {
self<-list()
self$name<-function() {"DeJongF2"}
self$bitlength<-function() {c(64, 64)}
self$genelength<-function() {sum(self$bitlength())}
self$lb<-function() {c(-2.048, -2.048)}
self$ub<-function() {c(2.048, 2.048)}
self$f<-function(parm, gene=0, lF=0) 
	  { #cat("F2 parm:\n")
            #print(parm)
           (100*(parm[1]^{2}-parm[2])^{2})+(1-parm[1])^{2}
          }
self$describe<-function() {
cat("See", "\n")
cat("De Jong (1975) ")
cat("An Analysis of the Behavior of a Class of Genetic Adaptive Systems.", "\n")
cat("PhD thesis, Michigan, Ann Arbor, pp. 197-202", "\n\n")
cat("F2 is a standard test function in optimization first proposed by Rosenbrock (1960)", "\n")
cat("F2 is a quartic function with a deep parabolic value along the curve x_{2}= x_{1}^2.", "\n")
cat("It is a continuous, non-convex, unimodal, low-dimensional quartic function.", "\n")
return(invisible(NULL))}
self$solution<-function() {
                 s<-list()
                 s[["minimum"]]<-0
                 s[["minpoints"]]<-list(list(c(1, 1)))
                 s[["maximum"]]<-3905.926
                 s[["maxpoints"]]<-list(list( c(-2.048, -2.048)))
                 return(s)
                   }
self$max<-function() {return(FALSE)}
self$globalOptimum<-function()
{l<-list();l$param<-rep(1.0, 2); l$value<-0.0; l$is.minimum<-TRUE;return(l)}
t<-self$name(); t<-self$bitlength(); t<-self$genelength(); t<-self$lb()
t<-self$ub(); t<-self$max(); t<-self$globalOptimum(); 
t<-self$solution(); t<-self$f(rep(0,1))
return(self)
}
