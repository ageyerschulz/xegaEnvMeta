library(TSP)
library(xegaEnvMeta)

envs <- list()
f <- c("gr17.tsp", "gr48.tsp", "gr120.tsp", "brg180.tsp", "si535.tsp", "si1032.tsp")
solutions <- c(2085, 5046, 6942, 1950, 48450, 92650)
for (i in 1:6) {
  data <- as.matrix(read_TSPLIB(f[i]))
  envs[[i]] <- newTSP(data, Name=f[i], Solution=solutions[i])
}



a<-xegaRun(penv=envs[[1]], max=FALSE, algorithm="sgperm", genemap="Identity", generations=100, popsize=100, mutation="MutateGeneMix", verbose=1)
a$solution
