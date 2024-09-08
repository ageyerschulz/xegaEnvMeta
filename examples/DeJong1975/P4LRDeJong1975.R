
library(xegaEnvMeta)

### Large/Small.
stochastic<-FALSE
production<-TRUE

### Define experiment

if (!production)
{ # test
rndtrials<-10
rndrepExp<-10
popsize<-3
generations<-2
evalrep<-2
maxevals<-10000
pn<-c("popsize", "mutrate", "bitmutrate", "crossrate")
bl<-c(rep(10,1), rep(8,3)); lb<-c(rep(10, 1), rep(0.00001, 3)); ub<-c(rep(15, 1), rep(1.0, 3))
stepsT<-15
robustReps<-16
path<-"P4LRtest/"
}

if (production)
{ # production
rndtrials<-1000
rndrepExp<-100
popsize<-100
generations<-30
evalrep<-10
maxevals<-10000
pn<-c("popsize", "mutrate", "bitmutrate", "crossrate")
bl<-c(rep(12,1), rep(10,3))
lb<-c(rep(10, 1), rep(0.00001, 3))
ub<-c(rep(1000, 1), rep(1.0, 3))
stepsT<-1000
robustReps<-200
path<-"P4LRprod/"
}

### Build environment list

envs<-DeJongEnvs(rndtrials=rndtrials, rndrepExp=rndrepExp, stochastic)
EnvList<-envs$EnvList; EnvWeights<-envs$EnvWeights

ind4<-c(1, 2, 3, 5)
envs<-envs[ind4]
EnvList<-EnvList[ind4]

### Run meta GA

rMeta<-metaGARun(
       name="P4LRDeJong1975",
       metaGAFactory=P5LRmetaGAFactory,
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
cat("\n DeJong (P4LR) 1975 finished!\n")

### Run robustness check of best solution.

rMetaRobust<-metaGARobust(
      name="P4LRDeJong1975RobustOpt",
      metaGAFactory=P5LRmetaGAFactory, 
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

