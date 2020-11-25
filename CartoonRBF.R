library(data.table)
library(here)
here()

source("../R_files/posPlots.R")
source(here("AccFunc.R"))

png(here("Images","cartoonRBF.png"),width=800,height = 800)
nCenters<-6
interv<-1/(nCenters-1)
centers<-interv*seq(0,nCenters-1)
sigSq<-0.001
weights<-rep(0,nCenters)
#  Random weights
weights<-runif(nCenters,min=-1,max=1)#RBF(centers,0.5,0.05)
rangx<-seq(0,1,length=1000)

par(plt=posPlot(numploty = 2,idploty = 2),las=1)
plot(logist(totRBF(rangx,centers,sigSq,weights),alpha = 0,beta=1)~rangx,type='l',col=1,
     xlab="",ylab="p(Dove)",ylim=c(0,1),lwd=3,xaxt="n",cex.lab=1.5,cex.axis=1.3)
points(y=logist(weights,alpha = 0,beta=1),x=centers,cex=3)


weights<-runif(nCenters)#RBF(centers,0.5,0.05)

par(plt=posPlot(numploty = 2,idploty = 1),new=T)
plot(totRBF(rangx,centers,0.01,weights)~rangx,type='l',col=1,
     xlab="Badge",ylab="Estimated value",lwd=3,cex.lab=1.5,cex.axis=1.3,
     ylim=range(c(weights,totRBF(rangx,centers,0.01,weights))))
points(y=weights,x=centers,cex=3)
# dev.off()

png(here("Images","cartoonRBF_InitAct.png"),width=1000,height = 700)
nCenters<-6
interv<-1/(nCenters-1)
centers<-interv*seq(0,nCenters-1)
sigSq<-0.01
weights<-data.table(neutral=rep(0,nCenters),agressive=rep(-2,nCenters),
        peaceful=rep(2,nCenters),clever=rep(-0.69,nCenters))
rangx<-seq(0,1,length=1000)

par(plt=posPlot(numploty = 2,idploty = 2),las=1)
actor<-sapply(weights, Actor,centers=centers)

as.matrix(actor)

par(plt=posPlot())
matplot(x=rangx,y=as.matrix(actor),type='l',col=c("black","red","blue","green"),lty=1,
     xlab="Badge size",ylab="p(Dove)",ylim=c(0,1.1),lwd=3,xaxt="s",cex.lab=1.5,cex.axis=1.3)
points(y=logist(weights$neutral,alpha = 0,beta=1),x=centers,cex=3)
text(y = actor[1,]+0.1,x=rep(0.5,4),col=c("black","red","blue","green"),
     labels = names(weights),cex=2)
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

# Cartoon Logist

logist(0.5,alpha = 5,beta = 10)

plot(logist(seq(0,1,length.out = 1000),alpha = 0,beta = 1)~
       seq(0,1,length.out = 1000),type="l",ylim=c(0,1))
lines(logist(seq(0,1,length.out = 1000),alpha =1,beta = 1)~
        seq(0,1,length.out = 1000),col="red")
lines(logist(seq(0,1,length.out = 1000),alpha =-1,beta = 1)~
        seq(0,1,length.out = 1000),col="blue")

plot(logist(seq(0,1,length.out = 1000),alpha = 3,beta = 6)~
       seq(0,1,length.out = 1000),type="l")

hist(rnorm(1000,mean = 0,sd = 0.05))
hist(rnorm(1000,mean = 0,sd = 0.1))
hist(rnorm(1000,mean = 0,sd = 0.15))
hist(rnorm(1000,mean = 0,sd = 0.2))
