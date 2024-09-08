
library(xegaEnvMeta)

### Large/Small.
stochastic<-FALSE
production<-FALSE

### Define experiment

if (!production)
{ # test
rndtrials<-10
rndrepExp<-10
popsize<-3
generations<-2
evalrep<-2
pn<-c("popsize", "generations", "mutrate", "bitmutrate", "crossrate")
bl<-c(rep(10,2), rep(8,3)); lb<-c(rep(10, 2), rep(0.00001, 3)); ub<-c(rep(15, 2), rep(1.0, 3))
stepsT<-15
robustReps<-16
path<-"P5test/"
}

if (production)
{ # production
rndtrials<-1000
rndrepExp<-100
popsize<-100
generations<-30
evalrep<-1
pn<-c("popsize", "generations", "mutrate", "bitmutrate", "crossrate")
bl<-c(rep(10,2), rep(8,3)); lb<-c(rep(10, 2), rep(0.00001, 3)); ub<-c(rep(1000, 2), rep(1.0, 3))
stepsT<-1000
robustReps<-200
path<-"P5prod/"
}

### Build environment list

envs<-DeJongEnvs(rndtrials=rndtrials, rndrepExp=rndrepExp, stochastic)
EnvList<-envs$EnvList; EnvWeights<-envs$EnvWeights

### Run meta GA

rMeta<-metaGARun(
       name="P5DeJong1975",
       metaGAFactory=P5metaGAFactory,
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
cat("\n DeJong (P5) 1975 finished!\n")

### Run robustness check of best solution.

rMetaRobust<-metaGARobust(
      name="P5DeJong1975RobustOpt",
      metaGAFactory=P5metaGAFactory, 
      rMeta=rMeta,
      reps=robustReps,
      path=path
      )


metaGAhowRobust(rMeta=rMetaRobust)
metaGAtables(rMeta=rMetaRobust, table="Summary")
metaGAtables(rMeta=rMetaRobust, table="Error")
### Run robustness check of 5th best solution.

rMetaRobust5<-metaGARobust(
      name="P5DeJong1975RobustOpt",
      metaGAFactory=P5metaGAFactory, 
      rMeta=rMeta,
      reps=robustReps,
      hyperIndex=5,
      path=path
      )

metaGAhowRobust(rMeta=rMetaRobust5)
metaGAtables(rMeta=rMetaRobust5, table="MetaGA")
metaGAtables(rMeta=rMetaRobust5, table="Resources")

### TBD Evaluation needs a second list envs: envListEval.

