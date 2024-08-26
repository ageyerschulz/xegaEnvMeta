
aggregateP5MetaResults<-function(pattern)
{
# read all files in local directory
a<-list.files(pattern=pattern)
b<-lapply(a, FUN=readRDS)

# extract all fields of the same type.
extractParms<-function(elem)
{  return(elem$param)  }

extractParms<-function(elem)
{  return(elem$param)  }

extractGAfit<-function(elem)
{  return(elem$GAfit)  }

extractExperiment<-function(elem)
{  return(elem$experiment)  }

# append data frames 
append<-function(df1, df2)
{return (rbind(df1, df2))}

c<-lapply(b, extractParms)
fit<-lapply(b, extractGAfit)
exp<-lapply(b, extractExperiment)

d<-unique(c)

r<-list()

# construct new list. Keep raw data.
for (i in 1:length(d))
{
el<-list()
el$param<-d[[i]]
m<-c %in% d[i]
tmp<-unlist(fit[m])
el$GAfitVec<-tmp
el$GAfit<-mean(tmp)
el$GAstd<-sd(tmp)
el$GAobs<-length(tmp)
el$exp<-Reduce(append, exp[m])
r[[i]]<-el
}

s<-sort(unlist(lapply(r, extractGAfit)), decreasing=TRUE, index.return=TRUE)

result<-r[s$ix]
return(result)
}

