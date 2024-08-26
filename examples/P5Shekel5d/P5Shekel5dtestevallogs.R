
library(smoof)
library(xegaEnvMeta)

#  Empty environment list
EnvList<-list()
#  Environment[1]: generate smoof-function and add Rnd performance measures for 1000 trials, 100 repetitions. 
EnvList[[1]]<-rndPerformance(smoofWrapperFactory(makeShekelFunction(5)), 
                   trials=100, repExp=10, executionModel="MultiCore")
# Generate Factory for 1 run of a GA per parameter set, 35 repetitions.
P5<-P5metaGAFactory(EnvList, "P5Shekel5d", repExp=1, example=FALSE, executionModel="Sequential", 
                    terminationCondition="AbsoluteError", terminationEps=0.1, log=1)
# Run GA for finding the best parameter set. 
a<-xegaRun(penv=P5, algorithm="sga", max=TRUE, 
            popsize=5, generations=2, evalrep=3, 
            executionModel="MultiCoreHet", profile=TRUE, verbose=3,
            logevals=TRUE, batch=TRUE)

