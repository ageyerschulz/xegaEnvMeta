
#' Factory for function F5 (Shekel's Fox Holes)
#'
#' @description This function factory sets up the problem environment
#'              for De Jong's function F5.
#' F5 is a 2-dimensional multimodal function suggested by Shekel (1971) 
#' called Shekel's foxholes.
#' It is a continuous, non-convex, multimodal, non-quadratic function
#' with 25 local minima defined by the 25 columns of the matrix A.
#' The minimum of this function is \code{1} at point \code{c(1,1)}.
#'
#' @references De Jong, Kenneth A. (1975):
#' \emph{An Analysis of the Behavior of a Class of Genetic Adaptive Systems.}
#' PhD thesis, Michigan, Ann Arbor, pp. 203-210.
#'
#' @references Shekel, J. (1971):
#' \emph{Test functions for mutimodal search techniques}.
#' Fifth Annual Princeton Conference on Information Science and Systems.
#'
#' @inherit DeJongF1bFactory return
#' 
#' @family problem environments
#' @family De Jong functions
#'
#' @examples
#' DeJongF5<-DeJongF5Factory()
#' DeJongF5$name()
#' DeJongF5$bitlength()
#' DeJongF5$genelength()
#' DeJongF5$lb()
#' DeJongF5$ub()
#' DeJongF5$f(c(2.01, -1.05, 4.95, -4.3, -3.0))
#' DeJongF5$describe()
#' DeJongF5$solution()
#' @export
DeJongF5Factory<-function() {
self<-list()
self$name<-function() {"DeJongF5"}
self$bitlength<-function() {rep(64,2)}
self$genelength<-function() {sum(self$bitlength())}
self$lb<-function() {rep(-65.536,2)}
self$ub<-function() {rep(65.536,2)}
self$f<-function(parm, gene=0, lF=0) 
    {  K<-1/500
       b<-c(-32,-16,0, 16, 32)
       A<-matrix(c(rep(b,5),unlist(lapply(b, rep, 5))), nrow=2, byrow=TRUE)
       fj<-function(j, x, y, a) {1/(j+(x-a[1,j])^{6}+(y-a[2,j])^{6})}
       F<-K+ sum(unlist(lapply((1:25), fj, x=parm[1], y=parm[2], a=A)))
       return(1/F) }
self$describe<-function() {
cat("See", "\n")
cat("De Jong (1975) ")
cat("An Analysis of the Behavior of a Class of Genetic Adaptive Systems.", "\n")
cat("PhD thesis, Michigan, Ann Arbor, pp. 203-210", "\n\n")
cat("F5 is a 2-dimensional multimodal function suggested by Shekel (1971) called Shekel's foxholes.", "\n")
cat("It is a continuous, non-convex, multimodal, non-quadratic function", "\n")
cat("with 25 local minima defined by the 25 columns of the matrix A", "\n")
return(invisible(NULL))}
self$solution<-function() {
                 s<-list()
                 s[["minimum"]]<-0.998
                 s[["minpoints"]]<-list(list(c(-32, -32)))
                 s[["maximum"]]<-499.999
                 s[["maxpoints"]]<-list( c(64, 64),
                                      c(-64, -64))
                 return(s)
                   }
self$max<-function() {return(FALSE)}
self$globalOptimum<-function()
{l<-list();l$param<-rep(1.0, 2); l$value<-1.0; l$is.minimum<-TRUE;return(l)}
t<-self$name(); t<-self$bitlength(); t<-self$genelength(); t<-self$lb()
t<-self$ub(); t<-self$max(); t<-self$globalOptimum(); t<-self$describe()
t<-self$solution(); t<-self$f(rep(0,1))
return(self)
}
