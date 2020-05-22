# Varying parameter of a single scenario ---------------------------------------

# Required libraries -----------------------------------------------------------

library(here)
here()
source(here("AccFunc.R"))
library("plotrix")

# Scenario to be plotted - corresponds to folders where simulations are stored

scenario<-"initAct"
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
                by=.(time,get(variable))]

setnames(evolStats,"get",variable)

timeChoi<-max(evolStats[,time])

# Plot mean and IQRs of the genotypes and phenotypes ----------------------------

# png(here("Simulations",paste0(scenario,"_"),"effectQualVariance.png"),height = 800,width = 800)


par(plt=posPlot(numploty = 2,idploty = 2),xaxt="n",las=1)
with(evolStats[time==timeChoi],{
  plotCI(x=as.factor(get(variable)),y=m.freqFenHawk,
         ui = upIQR.freqFenHawk,li=lowIQR.freqFenHawk,
         pch=16,xlab='',ylab='',
         col=colTypesLin[1],
         sfrac=0.005,cex.axis=1.3,yaxt='s',ylim=c(0,0.8))
  lines(x=c(0,max(get(variable))),y=c(0.66,0.66),col='grey')
  par(new=T)
  plotCI(x=as.factor(get(variable)),y=m.freqFenDove,new=T,
         ui = upIQR.freqFenDove,li=lowIQR.freqFenDove,
         pch=16,xlab='',ylab='',
         col=colTypesLin[2],ylim=c(0,0.8),
         sfrac=0.005,cex.axis=1.3,yaxt='s')
  
})
legend("bottomright",legend=c("Hawk","Dove"),
       lty=c(1,1,1),lwd=2,col=colTypesLin,bty="o",cex=1.15)


par(plt=posPlot(numploty = 2,idploty = 1),xaxt="s",las=1,new=TRUE,xpd=T)
with(evolStats[time==timeChoi],{
  plotCI(x=get(variable),y=m.freqHH,
         ui = upIQR.freqHH,li=lowIQR.freqHH,
         pch=16,xlab=expression(sigma^2~"of the quality distribution")
         ,ylab='',col=colTypesLin[1],
         sfrac=0.005,cex.axis=1.3,yaxt='s',ylim=c(0,0.5))
  par(new=T)
  plotCI(x=get(variable),y=m.freqHD,
         ui = upIQR.freqHD,li=lowIQR.freqHD,
         pch=16,xlab='',ylab='',
         col=colTypesLin[2],
         sfrac=0.005,cex.axis=1.3,yaxt='s',ylim=c(0,0.5))
  par(new=T)
  plotCI(x=get(variable),y=m.freqDD,
         ui = upIQR.freqDD,li=lowIQR.freqDD,
         pch=16,xlab='',ylab='',
         col=colTypesLin[3],
         sfrac=0.005,cex.axis=1.3,yaxt='s',ylim=c(0,0.5))
  lines(x=c(0,max(get(variable))),y=c(0.4356,0.4356),col='grey')
})
legend("bottomright",legend=c("HH","HD","DD"),
       lty=c(1,1,1),lwd=2,col=colTypesLin,bty="o",cex=1.)

# dev.off()


## Box plots -----------------------------------------------------------

pdf(paste0(extSimsDir,"/summ_",nampar,".pdf"))

# Jitter for the replicates
evol[,sort(unique(get(variable)))]

evol[,posX:=match(get(variable),sort(unique(get(variable))))+
       runif(length(get(variable)),
             min = -0.2,max = 0.2)]
cexAxis<-1.3
yLim<-as.numeric(evol[,.(min(c(freqHH,freqHD,freqDD)),
              max(c(freqHH,freqHD,freqDD)))])+c(0,0.02)
par(plt=posPlot(numploty = 1,idploty = 1,numplotx = 3,
                idplotx = 1),xaxt="s",las=1)
bbHH<-boxplot(freqHH~get(variable),data=evol[time==timeChoi],
         pch=16,xlab=variable,
         ylab='',ylim=yLim,
         cex.axis=cexAxis,yaxt='s')
with(evol[time==timeChoi],{
  points(x=posX,y=freqHH,col = 'darkGrey',pch = 20)
  lines(y=rep(0.666^2,2),x=c(0.5,3.5))
  # text(x=posX+0.1,y=freqHH,labels = seed,cex=0.8)
  })
mtext("HH",3,line = -3,cex=2)

par(plt=posPlot(numploty = 1,idploty = 1,numplotx = 3,
                idplotx = 2),xaxt="s",las=1,new=T)
bbHH<-boxplot(freqHD~get(variable),data=evol[time==timeChoi],
              pch=16,xlab=variable,ylim=yLim,
              ylab='',yaxt="n",
              cex.axis=cexAxis)
# axis(2,line = -2)
with(evol[time==timeChoi],{
  points(x=posX,y=freqHD,col = 'darkGrey',pch = 20)
  lines(y=rep(2*0.666*0.333,2),x=c(0.5,3.5))
  # text(x=posX+0.1,y=freqHD,labels = seed)
})
mtext("HD",3,line = -3,cex=2)

par(plt=posPlot(numploty = 1,idploty = 1,numplotx = 3,
                idplotx = 3),xaxt="s",las=1,new=T)
bbHH<-boxplot(freqDD~get(variable),data=evol[time==timeChoi],
              pch=16,xlab=variable,ylim=yLim,
              ylab='',yaxt="n",
              cex.axis=cexAxis)
 # axis(2,line = -2)
with(evol[time==timeChoi],{
  points(x=posX,y=freqDD,col = 'darkGrey',pch = 20)
  lines(y=rep(0.333^2,2),x=c(0.5,3.5))
  # text(x=posX+0.1,y=freqDD,labels = seed)
})
mtext("DD",3,line = -3,cex=2)


par(plt=posPlot(numploty = 1,idploty = 1,numplotx = 2,
                idplotx = 1),xaxt="s",las=1)
bpCue<-boxplot(meanCue~get(variable),data=evol[time==timeChoi],
              pch=16,xlab=variable,
              ylab='',ylim=c(0,1.1),
              cex.axis=cexAxis,yaxt='s')
# axis(2,line = -2)
with(evol[time==timeChoi],{
  points(x=posX,y=meanCue,col = 'darkGrey',pch = 20)
  # text(x=posX+0.1,y=meanCue,labels = seed)
})
mtext("Mean cue size",3,line = -3,cex=1)


par(plt=posPlot(numploty = 1,idploty = 1,numplotx = 2,
                idplotx = 2),xaxt="s",las=1,new=TRUE)
bpCue<-boxplot(sdCue~get(variable),data=evol[time==timeChoi],
               pch=16,xlab=variable,
               ylab='',
               cex.axis=0.8,yaxt='n')
axis(4)
with(evol[time==timeChoi],{
  points(x=posX,y=sdCue,col = 'darkGrey',pch = 20)
  # text(x=posX+0.1,y=sdCue,labels = seed)
})
mtext("Cue sd",3,line = -15,cex=1)


par(plt=posPlot(numploty = 1,idploty = 1,numplotx = 2,
                idplotx = 1),xaxt="s",las=1)
bpCue<-boxplot(meanAlpha~get(variable),data=evol[time==timeChoi],
               pch=16,xlab=variable,
               ylab='',ylim=as.numeric(evol[,.(min(meanAlpha),
                                                     max(meanAlpha))])+c(0,0.4),
               cex.axis=0.8,yaxt='n')
axis(2)
with(evol[time==timeChoi],{
  points(x=posX,y=meanAlpha,col = 'darkGrey',pch = 20)
  # text(x=posX+0.1,y=meanAlpha,labels = seed)
})
mtext("Alpha mean",3,line = -3,cex=1)

par(plt=posPlot(numploty = 1,idploty = 1,numplotx = 2,
                idplotx = 2),xaxt="s",las=1,new=T)
bpCue<-boxplot(meanBeta~get(variable),data=evol[time==timeChoi],
               pch=16,xlab=variable,
               ylab='',ylim=as.numeric(evol[,.(min(meanBeta),
                                               max(meanBeta))])+c(0,0.4),
               cex.axis=0.8,yaxt='n')
axis(4)
with(evol[time==timeChoi],{
  points(x=posX,y=meanBeta,col = 'darkGrey',pch = 20)
  # text(x=posX+0.1,y=meanBeta,labels = seed)
})
mtext("Beta mean",3,line = -3,cex=1)


dev.off()
