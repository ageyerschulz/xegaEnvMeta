
library(xegaEnvMeta)

###

### Large/Small.
stochastic<-FALSE
production<-FALSE

### Define experiment

if (!production)
{ # test
rndtrials<-10
rndrepExp<-10
example<-TRUE
popsize<-3
generations<-2
evalrep<-2
pn<-c("popsize", "generations", "mutrate", "mutrate2", "bitmutrate", "bitmutrate2", 
      "crossrate", "crossrate2", "cutOffFit")
bl<-rep(64,9); lb<-c(rep(10, 2), rep(0.00001, 7)); ub<-c(rep(15, 2), rep(1.0, 7))
stepsT<-15
robustReps<-16
path<-"IV5test/"
}

if (production)
{ # production
rndtrials<-1000
rndrepExp<-100
example<-FALSE
popsize<-50
generations<-20
evalrep<-1
pn<-c("popsize", "generations", "mutrate", "mutrate2", "bitmutrate", "bitmutrate2", 
      "crossrate", "crossrate2", "cutOffFit")
bl<-c(rep(10,2),rep(8,2)); lb<-c(rep(10, 2), rep(0.00001, 7)); ub<-c(rep(1000, 2), rep(1.0, 7))
stepsT<-1000
robustReps<-100
path<-"IV5prod/"
}

### Build environment list

envs<-DeJongEnvs(rndtrials=rndtrials, rndrepExp=rndrepExp, stochastic)
EnvList<-envs$EnvList; EnvWeights<-envs$EnvWeights

### Run meta GA

rMeta<-metaGARun(
       name="IV5DeJong1975",
       metaGAFactory=IVmetaGAFactory, 
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
cat("\n DeJong (IV) 1975 finished!\n")

### Run robustness check of best solution.

rMetaRobust<-metaGARobust(
      name="IV5DeJong1975RobustOpt",
      metaGAFactory=IVmetaGAFactory, 
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


