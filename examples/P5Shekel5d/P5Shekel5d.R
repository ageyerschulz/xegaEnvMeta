
library(smoof)
library(xegaEnvMeta)

#  Empty environment list
EnvList<-list()
#  Environment[1]: generate smoof-function and add Rnd performance measures for 1000 trials, 100 repetitions. 
EnvList[[1]]<-rndPerformance(smoofWrapperFactory(makeShekelFunction(5)), 
                   trials=1000, repExp=100, executionModel="MultiCore")

# Generate Factory for 1 run of a GA per parameter set, 35 repetitions.
P5<-P5metaGAFactory(EnvList, "P5Shekel5d", repExp=1, example=FALSE, executionModel="Sequential", 
                    terminationCondition="AbsoluteError", terminationEps=0.1, log=1)
# Run GA for finding the best parameter set. 
a<-xegaRun(penv=P5, algorithm="sga", max=TRUE, 
            popsize=50, generations=20, evalrep=10, 
            executionModel="MultiCoreHet", profile=TRUE, verbose=3,
            logevals=TRUE, batch=TRUE)

details<-aggregateP5MetaResults("xegaP5*")
saveRDS(details, "P5MetaGAShekel5dDetails.rds")

detdf<-convertP5MetaResults(details)
saveRDS(detdf, "P5MetaGAShekel5ddetailsdf.rds")

cat("P5Shekel5d finished!\n")

