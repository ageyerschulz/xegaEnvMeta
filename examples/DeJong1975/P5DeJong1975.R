
library(xegaEnvMeta)

source("DeJongF1.R")
source("DeJongF2.R")
source("DeJongF3.R")
source("DeJongF4.R")
source("DeJongF5.R")

#  Empty environment list
EnvList<-list()
#  Environment[1]: generate smoof-function and add Rnd performance measures for 1000 trials, 100 repetitions. 
EnvList[[1]]<-rndPerformance(DeJongF1Factory(), 
                   trials=1000, repExp=100, executionModel="MultiCore")
EnvList[[2]]<-rndPerformance(DeJongF2Factory(), 
                   trials=1000, repExp=100, executionModel="MultiCore")
EnvList[[3]]<-rndPerformance(DeJongF3Factory(), 
                   trials=1000, repExp=100, executionModel="MultiCore")
EnvList[[4]]<-rndPerformance(DeJongF4Factory(), 
                   trials=1000, repExp=100, executionModel="MultiCore")
EnvList[[5]]<-rndPerformance(DeJongF5Factory(), 
                   trials=1000, repExp=100, executionModel="MultiCore")

# Generate Factory for 1 run of a GA per parameter set, 35 repetitions.
P5<-P5metaGAFactory(EnvList, "P5DeJong1975", 
         repExp=2, example=TRUE, executionModel="Sequential", 
         terminationCondition="AbsoluteError", terminationEps=0.1, log=1)
# Run GA for finding the best parameter set. 
a<-xegaRun(penv=P5, algorithm="sga", max=TRUE, 
         #   popsize=50, generations=20, evalrep=10, 
             popsize=5, generations=2, evalrep=2, 
            executionModel="MultiCoreHet", profile=TRUE, verbose=3,
            logevals=TRUE, batch=TRUE)

cat("P5metaGA finished\n")

details<-metaGApostProcessing(solution=a)

cat("P5 The 10 best results:\n")
print(details[1:10,])

cat("DeJong (P5) 1975 finished!\n")

