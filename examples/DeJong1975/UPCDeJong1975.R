
library(smoof)
library(xegaEnvMeta)
source("DeJongF1.R")
source("DeJongF2.R")
source("DeJongF3.R")
source("DeJongF4.R")
source("DeJongF5.R")

#  Empty environment list
EnvList<-list()
#  Environment[1]: generate smoof-function and add Rnd performance measures for 1000 trials, 100 repetitions. 
trials<-1000
repExp<-100
EnvList[[2]]<-rndPerformance(DeJongF1Factory(), 
                   trials=trials, repExp=repExp, executionModel="MultiCore")
EnvList[[1]]<-rndPerformance(DeJongF2Factory(), 
                   trials=trials, repExp=repExp, executionModel="MultiCore")
EnvList[[3]]<-rndPerformance(DeJongF3Factory(), 
                   trials=trials, repExp=repExp, executionModel="MultiCore")
EnvList[[4]]<-rndPerformance(DeJongF4Factory(), 
                   trials=trials, repExp=repExp, executionModel="MultiCore")
EnvList[[5]]<-rndPerformance(DeJongF5Factory(), 
                   trials=trials, repExp=repExp, executionModel="MultiCore")
cat("Environments finished\n")

# Generate Factory for 1 run of a GA per parameter set, 35 repetitions.
UPC<-UPCmetaGAFactory(EnvList, "DeJong1975", repExp=1, example=FALSE, executionModel="Sequential", 
                    terminationCondition="AbsoluteError", terminationEps=0.1, log=1)
cat("UPCmetaGAFactory finished\n")

# Run GA for finding the best parameter set. 
a<-xegaRun(penv=UPC, algorithm="sga", max=TRUE, 
            popsize=100, generations=10, evalrep=1, 
         #    popsize=5, generations=2, evalrep=2, 
            executionModel="MultiCoreHet", profile=TRUE, verbose=3,
            logevals=TRUE, batch=TRUE)

details<-aggregateUPCMetaResults("xegaUPC*")
saveRDS(details, "UPCMetaGADeJong1975Details.rds")

detdf<-convertUPCMetaResults(details)
saveRDS(detdf, "UPCMetaGADeJong1975detailsdf.rds")

cat("The 10 best results:\n")
print(detdf[1:10,])

cat("DeJong (UPC) 1975 finished!\n")

