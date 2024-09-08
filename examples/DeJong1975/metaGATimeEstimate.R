
library(xegaEnvMeta)

source("DeJongF1.R")
source("DeJongF2.R")
source("DeJongF3.R")
source("DeJongF4.R")
source("DeJongF5.R")


### Large/Small.
production<-TRUE

if (!production)
{ # test
rndtrials<-10
rndrepExp<-10
example<-TRUE
popsize<-3
generations<-2
evalrep<-2
}

if (production)
{ # production
rndtrials<-1000
rndrepExp<-100
example<-FALSE
popsize<-50
generations<-50
evalrep<-10
}

#  Empty environment list
EnvList<-list()
#  Environment[1]: generate smoof-function and add Rnd performance measures for 1000 trials, 100 repetitions. 
EnvList[[1]]<-rndPerformance(DeJongF1Factory(), 
                   trials=rndtrials, repExp=rndrepExp, executionModel="MultiCore")
cat("env 1\n")
EnvList[[2]]<-rndPerformance(DeJongF2Factory(), 
                   trials=rndtrials, repExp=rndrepExp, executionModel="MultiCore")
cat("env 2\n")
EnvList[[3]]<-rndPerformance(DeJongF3Factory(), 
                   trials=rndtrials, repExp=rndrepExp, executionModel="MultiCore")
cat("env 3\n")
EnvList[[4]]<-rndPerformance(DeJongF4Factory(), 
                   trials=rndtrials, repExp=rndrepExp, executionModel="MultiCore")
cat("env 4\n")
EnvList[[5]]<-rndPerformance(DeJongF5Factory(), 
                   trials=rndtrials, repExp=rndrepExp, executionModel="MultiCore")
cat("env 5\n")

EnvWeights<-c(1.0, 1.0, 1.0, 1.0, 1.0)

# Generate Factory for 1 run of a GA per parameter set, 35 repetitions.
IV<-IVmetaGAFactory(EnvList, EnvWeights, name="IVDeJong1975", 
         repExp=1, example=example, executionModel="Sequential", 
         terminationCondition="AbsoluteError", terminationEps=0.1, verbose=1, log=1)

cat("IV \n")
# Execute one run with at the upper bound.
lF<-list(); lF$path<-function() {""}

a<-IV$f(IV$ub(), gene=0, lF=lF)


