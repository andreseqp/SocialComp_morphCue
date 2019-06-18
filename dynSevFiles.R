# Varying parameter of a single scenario ---------------------------------------

# Required libraries -----------------------------------------------------------

library(here)
here()
source(here("AccFunc.R"))

# Scenario to be plotted - corresponds to folders where simulations are stored

scenario<-"QualStDv"

# Load files -------------------------------------------------------------------

(listTest<-list.files(here("Simulations",paste0(scenario,"_"))))
(sdList<-grep("evol",listTest,value=TRUE))

evol<-do.call(rbind,lapply(sdList,filesScenar,scenario))

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
                   lowIQR.freqHH = fivenum(freqHH)[4],
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
                   m.weightCrit_4=mean(WeightCrit_4)),by=.(time,QualStDv)]

# Plot mean and IQRs of the genotypes and phenotypes ----------------------------

png(here("Simulations",scenario,"basidHawkDove.png"),height = 800,width = 800)

timeChoi<-max(evolStats[,time])
par(plt=posPlot(numploty = 2,idploty = 2),xaxt="n",las=1)
with(evolStats[time==timeChoi],{
  plotCI(x=QualStDv,y=m.freqFenHawk,
         ui = upIQR.freqFenHawk,li=lowIQR.freqFenHawk,
         pch=16,xlab='',ylab='',
         col=colTypesLin[1],
         sfrac=0.005,cex.axis=1.3,yaxt='s',ylim=c(0,0.8))
  lines(x=c(0,max(QualStDv)),y=c(0.66,0.66),col='grey')
  par(new=T)
  plotCI(x=QualStDv,y=m.freqFenDove,new=T,
         ui = upIQR.freqFenDove,li=lowIQR.freqFenDove,
         pch=16,xlab='',ylab='',
         col=colTypesLin[2],ylim=c(0,0.8),
         sfrac=0.005,cex.axis=1.3,yaxt='s')
  
})
legend("bottomright",legend=c("Hawk","Dove"),
       lty=c(1,1,1),lwd=2,col=colTypesLin,bty="o",cex=1.15)

par(plt=posPlot(numploty = 2,idploty = 1),xaxt="s",las=1,new=TRUE,xpd=T)
with(evolStats[time==timeChoi],{
  plotCI(x=QualStDv,y=m.freqHH,
         ui = upIQR.freqHH,li=lowIQR.freqHH,
         pch=16,xlab=expression(sigma^2~"of the quality distribution")
         ,ylab='',col=colTypesLin[1],
         sfrac=0.005,cex.axis=1.3,yaxt='s',ylim=c(0,0.5))
  par(new=T)
  plotCI(x=QualStDv,y=m.freqHD,
         ui = upIQR.freqHD,li=lowIQR.freqHD,
         pch=16,xlab='',ylab='',
         col=colTypesLin[2],
         sfrac=0.005,cex.axis=1.3,yaxt='s',ylim=c(0,0.5))
  par(new=T)
  plotCI(x=QualStDv,y=m.freqDD,
         ui = upIQR.freqDD,li=lowIQR.freqDD,
         pch=16,xlab='',ylab='',
         col=colTypesLin[3],
         sfrac=0.005,cex.axis=1.3,yaxt='s',ylim=c(0,0.5))
  lines(x=c(0,max(QualStDv)),y=c(0.4356,0.4356),col='grey')
})
legend("bottomright",legend=c("HH","HD","DD"),
       lty=c(1,1,1),lwd=2,col=colTypesLin,bty="o",cex=1.)

# dev.off()



