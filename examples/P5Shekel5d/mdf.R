
convertP5MetaResults<-function(l)
{
# extract all fields of the same type.
extractParms<-function(elem)
{  return(elem$param)  }

extractGAfit<-function(elem)
{  return(elem$GAfit)  }

extractGAstd<-function(elem)
{  return(elem$GAstd)  }

extractGAobs<-function(elem)
{  return(elem$GAobs)  }

tmp<-lapply(l, FUN=extractParms)
df1<-data.frame(tmp)
df3<-t(df1)


fit<-unlist(lapply(l, FUN=extractGAfit))
std<-unlist(lapply(l, FUN=extractGAstd))
obs<-unlist(lapply(l, FUN=extractGAobs))

df2<-data.frame(fit, std, obs)

result<-cbind(df2, df3)
cn<-c("Fit", "sigma(Fit)", "Obs")
cn<-c(cn, "popsize", "generations", "mutrate", "bitmutrate", "crossrate")
row.names(result)<-NULL
names(result)<-cn

return(result)
}


