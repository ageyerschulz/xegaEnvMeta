
library(xegaEnvMeta)


path=""
robustReps<-10
rMeta<-readRDS("xegametaGA-UPC4DeJong1975-rMeta-2024-09-05-08-03-19.472883.rds")
rMeta$solution$GAenv$penv$stepsT<-function() {return(500)}

n<-"UPC4DeJong1975RobustOpt"

for (i in (1:5))

{
newn<-paste(n, i, sep="")

rMetaRobust<-metaGARobust(
      name=newn,
      metaGAFactory=UPCmetaGAFactory, 
      rMeta=rMeta,
      hyperIndex=i,
      reps=robustReps,
      path=path
      )

cat("\n\n Analyses of ", newn, "\n\n")

metaGAhowRobust(rMeta=rMetaRobust)
metaGAtables(rMeta=rMetaRobust, table="Summary")
metaGAtables(rMeta=rMetaRobust, table="MetaGA")
metaGAtables(rMeta=rMetaRobust, table="Error")
metaGAtables(rMeta=rMetaRobust, table="Resources")
}

### TBD Evaluation needs a second list envs: envListEval.


