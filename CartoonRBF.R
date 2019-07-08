library(data.table)
library(here)
here()

source("C:/Users/a.quinones/Dropbox/R_files/posPlots.R")


png(here("cartoonRBF.png"),width=800,height = 800)
nCenters<-5
interv<-1/nCenters
centers<-interv*0.5+interv*seq(0,nCenters-1)
weights<-runif(nCenters,min=-1,max=1)#RBF(centers,0.5,0.05)
rangx<-seq(0,1,length=1000)

par(plt=posPlot(numploty = 2,idploty = 2))
plot(logist(totRBF(rangx,centers,0.01,weights),alpha = 0,beta=1)~rangx,type='l',col=1,
     xlab="",ylab="p(Dove)",ylim=c(0,1),lwd=3,xaxt="n",cex.lab=1.5)
points(y=logist(weights,alpha = 0,beta=1),x=centers,cex=3)


weights<-runif(nCenters)#RBF(centers,0.5,0.05)

par(plt=posPlot(numploty = 2,idploty = 1),new=T)
plot(totRBF(rangx,centers,0.01,weights)~rangx,type='l',col=1,
     xlab="Badge",ylab="Estimated value",ylim=c(0,1),lwd=3,cex.lab=1.5)
points(y=logist(weights,alpha = 0,beta=1),x=centers,cex=3)
dev.off()


# cartoon mean

randWeigh<-matrix(runif(50,-1,1),ncol = 10)

nCenters<-5
interv<-1/nCenters
centers<-interv*0.5+interv*seq(0,nCenters-1)
rangx<-seq(0,1,length=1000)
means<-apply(randWeigh, MARGIN = 1, FUN = mean)
matplot(x=centers,y=randWeigh,pch = 19)
points(x=centers,y=means,pch=19,col=2,cex=3)


IndRBF<-sapply(as.list(1:10),
               function(x){x=
                 totRBF(rangx,
                        centers,0.01,
                        as.double(
                          randWeigh[,x]))})

str(IndRBF)
matlines(x=rangx,y=IndRBF,lty = 1)
lines(x=rangx,y=totRBF(rangx,centers,0.01,means),col=2,lwd=2)

