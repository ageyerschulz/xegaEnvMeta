library(TSP)
library(xega)

newTSP<-function(D, Cities=0, Name, Solution=NA, Path=NA)
{
  parm<-function(x){function() {return(x)}}	
  d<-dim(D)
  try (if (!length(d)==2) stop("n times n matrix expected"))
  try (if (!d[1]==d[2]) stop("n times n matrix expected"))
  # constant functions
  self<-list()
  self$name<-parm(Name)
  self$genelength<- parm(d[1])
  self$dist<-parm(D)
  if (identical(Cities,0)) {cit<-1:length(Path)} else {cit<-Cities}
  self$cities<-parm(cit)
  self$path<-parm(Path)
  # f
  self$f<-function(permutation, gene=0, lF=0, tour=TRUE)
  { cost<-0
  l<-length(permutation)-1
  for (i in 1:l)
  { cost<- cost+self$dist()[permutation[i], permutation[i+1]]}
  if (tour==TRUE) {cost<-cost+self$dist()[permutation[l+1], permutation[1]]}
  return(cost)}
  self$max<-function() {return(FALSE)}
  self$globalOptimum<-function() 
  {l<-list()
  l$param<-rep(NA,d[1]); l$value<-Solution;l$is.minimum<-TRUE;return(l)}
  
  # show. p is a path.
  self$show<-function(p)
  { l<-length(p)-1
  pl<-0
  for (i in 1:l)
  {d<-self$dist()[p[i], p[i+1]]
  pl<-pl+d
  cat(i,"Length:", pl, 
      " from:", self$cities()[p[i]], " to ", self$cities()[p[i+1]],
      " Distance: ", d, "\n")}
  d<-self$dist()[p[l+1], p[1]]
  pl<-pl+d
  cat(i,"Length:", pl, 
      " from:", self$cities()[p[l+1]], " to ", self$cities()[p[1]],
      " Distance: ", d, "\n")}
  
  self$greedy<-function(startPosition, k)
  { # local functions
    without<-function(set, element) {set[!set==element]}
    # v a vector
    findMinIndex<-function(indexSet, v)
    {v<-indexSet[v==min(v)]
    return(v[sample(1:length(v),1)]) }
    
    nextPosition<-startPosition
    path<-as.vector(startPosition)
    indexSet<-without(1:self$genelength(), nextPosition)
    for (i in 1:k)
    {nextPosition<-findMinIndex(indexSet, self$dist()[nextPosition, indexSet])
    path<-c(path, nextPosition)
    indexSet<-without(indexSet, nextPosition)}
    return(path)}
  
  self$kBestGreedy<-function(k, tour=TRUE)
  { l<-self$genelength()
  best<-self$greedy(1, k)
  costBest<-self$f(best, tour)
  for (i in 2:l)
  {
    new<-self$greedy(i, k)
    costNew<-self$f(new, tour)
    if (costNew<costBest) {best<-new; costBest<-costNew}
  }
  
  return(best)
  }
  
  self$rnd2Opt<-function(permutation, maxTries=5)
  {
    
    randomSplit<-function(l)
    { kpos<-sample(1:l, 2, replace=FALSE)
    if (kpos[1]>kpos[2]) {kpos[c(2, 1)]<-kpos}
    if ((kpos[2]==l) & (kpos[1]==1)) {kpos[2]<-l-1}
    if ((kpos[2]-kpos[1])==1)
    {if (kpos[2]==l) {kpos[1]<-kpos[1] -1} else {kpos[2]<-kpos[2]+1}}
    x1<-1:kpos[1]
    if (kpos[2]<l) {x2<-(kpos[2]+1):l} else {x2<-rep(0,0)}
    y<-(kpos[1]+1):kpos[2]
    return(c(x2, x1, y[length(y):1])) }
    
    cost1<-self$f(permutation)
    l<-length(permutation)
    for (i in 1:maxTries)
    { newpermutation<-permutation[randomSplit(l)]
    cost2<-self$f(newpermutation)
    if (cost1>cost2) {return(newpermutation)}
    if (i==maxTries) {break} }
    return(permutation)
  }
  
  self$LinKernighan<-function(permutation, maxTries=5, show=FALSE)
  {
    epsilon<-parm(0.000001)
    newpermutation<-permutation; i<-1
    repeat
    { c1<-self$f(newpermutation); i<-i+1
    newpermutation<-self$rnd2Opt(newpermutation, maxTries)
    c2<-self$f(newpermutation)
    if (show) {
      cat(i,"p: ", newpermutation, 
          "c1:", c1, "c2:", c2, "diff:", (c1-c2), "\n")}
    if (abs(c1-c2)<epsilon()) {break}
    }
    return(newpermutation)
  }
  
  
  a<-force(self$name())
  a<-force(self$genelength())
  a<-force(self$dist())
  a<-force(self$cities())
  a<-force(self$path())
  a<-force(self$max())
  a<-force(self$globalOptimum())
  return(self) }

envs <- list()
f <- c("gr17.tsp", "gr48.tsp", "gr120.tsp", "brg180.tsp", "si535.tsp", "si1032.tsp")
solutions <- c(2085, 5046, 6942, 1950, 48450, 92650)
for (i in 1:6) {
  data <- as.matrix(read_TSPLIB(f[i]))
  envs[[i]] <- newTSP(data, Name=f[i], Solution=solutions[i])
}

a<-xegaRun(penv=envs[[1]], max=FALSE, algorithm="sgperm", genemap="Identity", generations=100, popsize=100, mutation="MutateGeneMix", verbose=1)
a$solution
