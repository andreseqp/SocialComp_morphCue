library(data.table)
library(here)
here()

source("C:/Users/a.quinones/Dropbox/R_files/posPlots.R")

RBF<-function(x,xCenter,sigSq){
  return(exp(-(x-xCenter)^2/(2*sigSq)))
}


totRBF<-function(x,xCenter,sigSq,featWeig){
  tmp<-sapply(xCenter, RBF,x=x,sigSq=sigSq)
  tmp<-apply(tmp, 1, FUN=crossprod,y=featWeig)
  #tmp<-apply(tmp,1,FUN = sum)
  return(tmp)
}

nCenters<-5
interv<-1/nCenters
centers<-interv*0.5+interv*seq(0,nCenters-1)
weights<-runif(nCenters)#RBF(centers,0.5,0.05)
rangx<-seq(0,1,length=1000)

par(plt=posPlot())
plot(totRBF(rangx,centers,0.01,weights)~rangx,type='l',col=1,
     xlab="x",ylab="response",ylim=c(0,3),lwd=3)
points(y=weights,x=centers,cex=3)
