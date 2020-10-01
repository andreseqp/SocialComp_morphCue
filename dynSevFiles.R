# Varying parameter of a single scenario ---------------------------------------

# Required libraries -----------------------------------------------------------

library(here)
here()
source(here("AccFunc.R"))
library("plotrix")

# Scenario to be plotted - corresponds to folders where simulations are stored

scenario<-"nIntGroupEvolLear"
variable<-gsub("[^[:alpha:]]",scenario,replacement = '')
variable<-gsub("Evol",variable,replacement = '')
extSimsDir<-paste0("e:/BadgeSims/",scenario,"_")

# Load files -------------------------------------------------------------------

(listTest<-list.files(extSimsDir,full.names = TRUE))
(evolList<-grep("evolLearn",listTest,value=TRUE))
(indList<-grep("indLearn",listTest,value=TRUE))

nampar<-gsub("[^[:alpha:]]",gsub(".txt","",tail(strsplit(indList[1],"_")[[1]],1)),
             replacement = "")


# (listTest<-list.files(here("Simulations",paste0(scenario,"_"))))
# (sdList<-grep("evol",listTest,value=TRUE))

evol<-do.call(rbind,lapply(evolList,filesScenar,scenario,full.name=TRUE))

pop<-do.call(rbind,lapply(indList, filesScenar,scenario,full.name=TRUE))

# Extract means and IQR for the dynamic variables ------------------------------

evolStats<-evol[,.(m.freqGenHawk=mean(freqGenHawks),
                   upIQR.freqGenHawk=fivenum(freqGenHawks)[4],
                   lowIQR.freqGenHawk=fivenum(freqGenHawks)[2],
                   m.freqGenDove=mean(freqGenDove),
                   upIQR.freqGenDove=fivenum(freqGenDove)[4],
                   lowIQR.freqGenDove=fivenum(freqGenDove)[2],
                   m.freqEval=mean(freqGenEval),
                   upIQR.freqGenEval=fivenum(freqGenEval)[4],
                   lowIQR.freqGenEval=fivenum(freqGenEval)[2], 
                   m.freqFenHawk=mean(freqFenHawks),
                   upIQR.freqFenHawk=fivenum(freqFenHawks)[4],
                   lowIQR.freqFenHawk=fivenum(freqFenHawks)[2],
                   m.freqFenDove=mean(freqFenDoves),
                   upIQR.freqFenDove=fivenum(freqFenDoves)[4],
                   lowIQR.freqFenDove=fivenum(freqFenDoves)[2],
                   m.meanAlpha=mean(meanAlpha),
                   upIQR.alpha=fivenum(meanAlpha)[4],
                   lowIQR.alpha=fivenum(meanAlpha)[2],
                   m.meanBeta=mean(meanBeta),
                   upIQR.beta=fivenum(meanBeta)[4],
                   lowIQR.beta=fivenum(meanBeta)[2],
                   m.freqHH = mean(freqHH),
                   m.freqHD = mean(freqHD),
                   m.freqDD = mean(freqDD),
                   upIQR.freqHH = fivenum(freqHH)[4],
                   upIQR.freqHD = fivenum(freqHD)[4],
                   upIQR.freqDD = fivenum(freqDD)[4],
                   lowIQR.freqHH = fivenum(freqHH)[2],
                   lowIQR.freqHD = fivenum(freqHD)[2],
                   lowIQR.freqDD = fivenum(freqDD)[2],
                   m.weightAct_0=mean(WeightAct_0),
                   m.weightAct_1=mean(WeightAct_1),
                   m.weightAct_2=mean(WeightAct_2),
                   m.weightAct_3=mean(WeightAct_3),
                   m.weightAct_4=mean(WeightAct_4),
                   m.weightCrit_0=mean(WeightCrit_0),
                   m.weightCrit_1=mean(WeightCrit_1),
                   m.weightCrit_2=mean(WeightCrit_2),
                   m.weightCrit_3=mean(WeightCrit_3),
                   m.weightCrit_4=mean(WeightCrit_4)),
                by=.(time,get(nampar))]

setnames(evolStats,"get",nampar)

timeChoi<-max(evolStats[,time])

# Plot mean and IQRs of the genotypes and phenotypes ----------------------------

# png(here("Simulations",paste0(scenario,"_"),"effectQualVariance.png"),height = 800,width = 800)

evol[time==timeChoi,]
with(evol[time==timeChoi],{
boxplot(freqFenHawks~get(nampar),
       xlab='',ylab='',cex.axis=1.3,yaxt='s')
points(x=posX,y=freqFenHawks,pch=21)
  })



par(plt=posPlot(numploty = 2,idploty = 2),xaxt="n",las=1)
with(evolStats[time==timeChoi],{
  plotCI(x=as.factor(get(nampar)),y=m.freqFenHawk,
         ui = upIQR.freqFenHawk,li=lowIQR.freqFenHawk,
         pch=16,xlab='',ylab='',
         col=colTypesLin[1],
         sfrac=0.005,cex.axis=1.3,yaxt='s',ylim=c(0,0.8))
  lines(x=c(0,max(get(nampar))),y=c(0.66,0.66),col='grey')
  par(new=T)
  plotCI(x=as.factor(get(nampar)),y=m.freqFenDove,new=T,
         ui = upIQR.freqFenDove,li=lowIQR.freqFenDove,
         pch=16,xlab='',ylab='',
         col=colTypesLin[2],ylim=c(0,0.8),
         sfrac=0.005,cex.axis=1.3,yaxt='s')
  
})
legend("bottomright",legend=c("Hawk","Dove"),
       lty=c(1,1,1),lwd=2,col=colTypesLin,bty="o",cex=1.15)


par(plt=posPlot(numploty = 2,idploty = 1),xaxt="s",las=1,new=TRUE,xpd=T)
with(evolStats[time==timeChoi],{
  plotCI(x=get(nampar),y=m.freqHH,
         ui = upIQR.freqHH,li=lowIQR.freqHH,
         pch=16,xlab=expression(sigma^2~"of the quality distribution")
         ,ylab='',col=colTypesLin[1],
         sfrac=0.005,cex.axis=1.3,yaxt='s',ylim=c(0,0.5))
  par(new=T)
  plotCI(x=get(nampar),y=m.freqHD,
         ui = upIQR.freqHD,li=lowIQR.freqHD,
         pch=16,xlab='',ylab='',
         col=colTypesLin[2],
         sfrac=0.005,cex.axis=1.3,yaxt='s',ylim=c(0,0.5))
  par(new=T)
  plotCI(x=get(nampar),y=m.freqDD,
         ui = upIQR.freqDD,li=lowIQR.freqDD,
         pch=16,xlab='',ylab='',
         col=colTypesLin[3],
         sfrac=0.005,cex.axis=1.3,yaxt='s',ylim=c(0,0.5))
  lines(x=c(0,max(get(nampar))),y=c(0.4356,0.4356),col='grey')
})
legend("bottomright",legend=c("HH","HD","DD"),
       lty=c(1,1,1),lwd=2,col=colTypesLin,bty="o",cex=1.)

# dev.off()


## Box plots -----------------------------------------------------------

pdf(paste0(extSimsDir,"/summ_",nampar,".pdf"))

# png(here("Simulations",paste0(scenario,"_"),
#          paste0("BehavIntALL",".png")),
#     width = 1400,height = 500)


# Jitter for the replicates
evol[,posX:=match(get(nampar),sort(unique(get(nampar))))+
       runif(length(get(nampar)),
             min = -0.3,max = 0.3)]
cexAxis<-1.5;cexPoints<-1.5
yLim<-as.numeric(evol[,.(min(c(freqHH,freqHD,freqDD)),
              max(c(freqHH,freqHD,freqDD)))])+c(0,0.02)
par(mfrow=c(1,3),xaxt="s",las=1,plt=posPlot())

# frequency of interactions types

bbHH<-boxplot(freqHH~get(nampar),data=evol,
         pch=16,cex.lab=2,
         xlab=variable,
         #xlab="Badge cost",
         ylab='',ylim=fivenum(evol$freqHH)[c(1,5)]+c(0,0.005),
         cex.axis=cexAxis,yaxt='s')
with(evol,{
  points(x=posX,y=freqHH,
         col = colSeeds[match(seed,unique(seed))],pch = 20,cex=cexPoints)
  # lines(y=rep(0.666^2,2),x=c(0.5,3.5))
  text(x=posX+0.2,y=freqHH,labels = seed,cex=0.8)
  })
mtext("HH",3,line = -2.5,cex=2)

par(xaxt="s",las=1)
bbHH<-boxplot(freqHD~get(nampar),data=evol[time>max(time)/2],
              pch=16,ylim=fivenum(evol$freqHD)[c(1,5)]+c(0,0.005),
              #xlab="Badge cost",
              xlab=nampar,
              ylab='',cex.lab=2,
              cex.axis=cexAxis)
# axis(2,line = -2)
with(evol[time>max(time)/2],{
  points(x=posX,y=freqHD,col = colSeeds[match(seed,unique(seed))],pch = 20,
         cex=cexPoints)
  # lines(y=rep(2*0.666*0.333,2),x=c(0.5,3.5))
  #text(x=posX+0.1,y=freqHD,labels = seed)
})
mtext("HD",3,line = -2.5,cex=2)

bbHH<-boxplot(freqDD~get(nampar),data=evol[time>max(time)/2],
              pch=16,ylim=fivenum(evol$freqDD)[c(1,5)],#+c(0,0.0001),
              ylab='',cex.lab=2,
              xlab=variable,
              #xlab="Badge cost",
              cex.axis=cexAxis)
 # axis(2,line = -2)
with(evol[time>max(time)/2],{
  points(x=posX,y=freqDD,col = colSeeds[match(seed,unique(seed))],pch = 20,
         cex=cexPoints)
  lines(y=rep(0.333^2,2),x=c(0.5,3.5))
  #text(x=posX+0.1,y=freqDD,labels = seed)
})
mtext("DD",3,line = -2.5,cex=2)

# dev.off()
# 
# png(here("Simulations",paste0(scenario,"_"),
#          paste0("CueALL",".png")),
#     width = 1400,height = 500)


par(mfrow=c(1,2),xaxt="s",las=1)
bpCue<-boxplot(meanCue~get(nampar),data=evol[time>max(time)/2],
              pch=16,xlab=variable,
              #xlab = "Badge cost",
              ylab='',ylim=c(0,1.1),
              cex.axis=cexAxis,yaxt='s')
# axis(2,line = -2)
with(evol[time>max(time)/2],{
  points(x=posX,y=meanCue,col = colSeeds[match(seed,unique(seed))],pch = 20,
         cex=cexPoints)
  #text(x=posX+0.1,y=meanCue,labels = seed)
})
mtext("Mean cue size",3,line = -2,cex=2)


bpCue<-boxplot(sdCue~get(nampar),data=evol[time>max(time)/2],
               pch=16,xlab=variable,
               #xlab = "Badge cost",
               ylab='',
               cex.axis=cexAxis,yaxt='s')

with(evol[time>max(time)/2],{
  points(x=posX,y=sdCue,col = colSeeds[match(seed,unique(seed))],pch = 20,
         cex=cexPoints)
  #text(x=posX+0.1,y=sdCue,labels = seed)
})
mtext("Cue sd",3,line = -2,cex=2)

# dev.off()

bpCue<-boxplot(meanAlpha~get(nampar),data=evol[time>max(time)/2],
               pch=16,xlab=variable,
               #xlab = "Badge cost",
               ylab='',ylim=as.numeric(evol[,.(min(meanAlpha),
                                                     max(meanAlpha))])+c(0,0.4),
               cex.axis=0.8,yaxt='n')
axis(2)
with(evol[time>max(time)/2],{
  points(x=posX,y=meanAlpha,col = colSeeds[match(seed,unique(seed))],pch = 20)
  #text(x=posX+0.1,y=meanAlpha,labels = seed)
})
mtext("Alpha mean",3,line = -3,cex=1)


bpCue<-boxplot(sdAlpha~get(nampar),data=evol[time>max(time)/2],
               pch=16,xlab=variable,
               ylab='',ylim=as.numeric(evol[,.(min(sdAlpha),
                                               max(sdAlpha))])+c(0,0.4),
               cex.axis=0.8,yaxt='n')
axis(2)
with(evol[time>max(time)/2],{
  points(x=posX,y=sdAlpha,col = colSeeds[match(seed,unique(seed))],pch = 20,
         cex=cexPoints)
  #text(x=posX+0.1,y=meanAlpha,labels = seed)
})
mtext("Alpha sd",3,line = -3,cex=1)


bpCue<-boxplot(meanBeta~get(nampar),data=evol[time>max(time)/2],
               pch=16,xlab=variable,
               ylab='',ylim=as.numeric(evol[,.(min(meanBeta),
                                               max(meanBeta))])+c(0,0.4),
               cex.axis=0.8,yaxt='n')
axis(2)
with(evol[time>max(time)/2],{
  points(x=posX,y=meanBeta,col = colSeeds[match(seed,unique(seed))],pch = 20)
  #text(x=posX+0.1,y=meanBeta,labels = seed)
})
mtext("Beta mean",3,line = -3,cex=1)


bpCue<-boxplot(sdBeta~get(nampar),data=evol[time>max(time)/2],
               pch=16,xlab=variable,
               ylab='',ylim=as.numeric(evol[,.(min(sdBeta),
                                               max(sdBeta))])+c(0,0.4),
               cex.axis=0.8,yaxt='n')
axis(2)
with(evol[time>max(time)/2],{
  points(x=posX,y=sdBeta,col = colSeeds[match(seed,unique(seed))],pch = 20)
  #text(x=posX+0.1,y=meanBeta,labels = seed)
})
mtext("Beta sd",3,line = -3,cex=1)

par(plt=posPlot())
plot(data=evol[time!=0],freqHH~sdCue,
     col=colTypesLin[match(get(nampar),sort(unique(get(nampar))))],pch=20)
legend("bottomright",legend = sort(evol[,unique(get(nampar))]),pch=20,
       col=colTypesLin)

par(plt=posPlot())
plot(data=evol[time!=0],freqHH~abs(meanBeta),
     col=colTypesLin[match(get(nampar),sort(unique(get(nampar))))],pch=20)
legend("bottomright",legend = sort(evol[,unique(get(nampar))]),pch=20,
       col=colTypesLin)


dev.off()



