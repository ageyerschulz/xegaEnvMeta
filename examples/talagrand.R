
# Hilbert space.

predictSup<-function(ustart, delta, rndmean, rndsd, max=-1)
{
# mean correction.
u<-ustart-(rndmean*max)

p<-2*exp((-1)*((u^2)/(rndsd^2)))

# add the mean.
gopt<-(max*rndmean)+u

cat("u", u, "p(u<gopt)", p, "gsup", gopt, "\n")
return(gopt)
}

