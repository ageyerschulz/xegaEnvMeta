
library(xegaEnvMeta)

         
### Large/Small.
stochastic<-FALSE
production<-TRUE

### Define Experiment

if (!production)
{ # test
rndtrials<-10
rndrepExp<-10
popsize<-3
generations<-2
evalrep<-2
pn<-c("popsize", "generations", "mutrate", "bitmutrate", "crossrate", "uCrossSwap")
bl<-rep(64,6); lb<-c(rep(10, 2), rep(0.00001, 4)); ub<-c(rep(15, 2), rep(1.0, 4))
stepsT<-15
robustReps<-16
path<-"UPC4test/"
}


if (production)
{ # production
rndtrials<-1000
rndrepExp<-100
popsize<-50
generations<-20
evalrep<-1
pn<-c("popsize", "generations", "mutrate", "bitmutrate", "crossrate", "uCrossSwap")
bl<-c(rep(10,2),rep(8,4)); lb<-c(rep(10, 2), rep(0.00001, 4)); ub<-c(rep(1000, 2), rep(1.0, 4))
stepsT<-1000
robustReps<-200
path<-"UPC4prod/"
}

### Build environment list

envs<-DeJongEnvs(rndtrials=rndtrials, rndrepExp=rndrepExp, stochastic)
EnvList<-envs$EnvList; EnvWeights<-envs$EnvWeights


ind4<-c(1, 2, 3, 5)
envs<-envs[ind4]
EnvList<-EnvList[ind4]

### Run meta GA

rMeta<-metaGARun(
       name="UPC4DeJong1975",
       metaGAFactory=UPCmetaGAFactory,
       EnvList=EnvList,
       EnvWeights=EnvWeights,
       pnames=pn,
       bitlength=bl,
       lb=lb,
       ub=ub,
       stepsT=stepsT,
       popsize=popsize,
       generations=generations,
       evalrep=evalrep,
       path=path
       )


metaGAExperiment(rMeta=rMeta)
metaGAkBest(rMeta=rMeta, k=5)

cat("\n DeJong (UPC) 1975 finished!\n")


### Run robustness check of best solution.

rMetaRobust<-metaGARobust(
      name="UPC4DeJong1975RobustOpt",
      metaGAFactory=UPCmetaGAFactory, 
      rMeta=rMeta,
      reps=robustReps,
      path=path
      )


metaGAhowRobust(rMeta=rMetaRobust)
metaGAtables(rMeta=rMetaRobust, table="Summary")
metaGAtables(rMeta=rMetaRobust, table="MetaGA")
metaGAtables(rMeta=rMetaRobust, table="Error")
metaGAtables(rMeta=rMetaRobust, table="Resources")


### TBD Evaluation needs a second list envs: envListEval.

#
# Robustness of the best 5 points.
#

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


