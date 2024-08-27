
#
# (c) 2021 Andreas Geyer-Schulz
#          Simple Genetic Algorithm in R. V 0.1
#          Problem environments for simple binary or real coded GAs 
#          Package: envDeJongFunctions 
#

#
# Note, we have rewritten all problem environments as function factories.
#

#' Factory for function DeJong's F1
#'
#' @description This function factory sets up the problem environment 
#'              for De Jong's function F1 which is a 3-dimensional
#'              quadratic parabola. 
#'              The function has a minimum of \code{0} at the 
#'              point \code{c(0, 0, 0)}.
#'
#' @details The factory contains all functions needed for 
#'          the simple genetic algorithm with binary coded genes 
#'          of package \code{sga}.
#'
#' @return A list of functions (
#'         \code{$name()},   
#'         \code{$bitlength()},   
#'         \code{$genelength()},   
#'         \code{$lb()},   
#'         \code{$ub()},   
#'         \code{$f(parm, gene=0, lF=0)}, ...),
#'         \code{$max()},
#'         \code{$globalOptimum()}.
#'         For details, see the interface description 
#'         in the package description.
#'
#' @family De Jong benchmark functions
#'
#' @examples
#' DeJongF1<-DeJongF1Factory()
#' DeJongF1$name() 
#' DeJongF1$bitlength()
#' DeJongF1$genelength()
#' DeJongF1$lb()
#' DeJongF1$ub()
#' DeJongF1$max()
#' DeJongF1$globalOptimum()
#' DeJongF1$f(c(2.2, 1.0, -3.5))
#' @export
DeJongF1Factory<-function(){
self<-list()
self$name<-function() {"DeJongF1"}
self$bitlength<-function() {c(64, 64, 64)}
self$genelength<-function() {sum(self$bitlength())}
self$lb<-function() {c(-5.12, -5.12, -5.12)}
self$ub<-function() {c(5.12, 5.12, 5.12)}
self$f<-function(parm, gene=0, lF=0) {sum(parm^{2})}
self$max<-function() {return(FALSE)}
self$globalOptimum<-function() 
{l<-list();l$param<-rep(0.0,3); l$value<-0.0;l$is.minimum<-TRUE;return(l)}
self$describe<-function() {
cat("See", "\n")
cat("De Jong (1975) ")
cat("An Analysis of the Behavior of a Class of Genetic Adaptive Systems.", "\n")
cat("PhD thesis, Michigan, Ann Arbor, pp. 196-199", "\n\n")
cat("F1 is a 3-dimensional parabola with spherical constant-cost contours.", "\n")
cat("It is a continuous, convex, unimodal, low-dimensional quadratic function.", "\n")
invisible(NULL)
}
self$solution<-function() { s<-list()
                 s[["minimum"]]<-0
                 s[["minpoints"]]<-list(list(c(0, 0, 0)))
                 s[["maximum"]]<-78.6432
                 s[["maxpoints"]]<-list( c(5.12, 5.12, 5.12),
                   c(5.12, 5.12, -5.12), c(5.12, -5.12, 5.12),
                   c(5.12, -5.12, -5.12), c(-5.12, 5.12, 5.12),
                   c(-5.12, 5.12, -5.12), c(-5.12, -5.12, -5.12),
                   c(-5.12, -5.12, -5.12))
                 return(s) } 
# force!
t<-self$name(); t<-self$bitlength(); t<-self$genelength(); t<-self$lb()
t<-self$ub(); t<-self$max(); t<-self$globalOptimum();
t<-self$solution(); t<-self$f(rep(1,3))
return(self)
}

