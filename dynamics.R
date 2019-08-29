# Evolutionary dynamics of the hawk dove game with an extra strategy that 
# learns a preference for the phenotypic strategy depending on a morphological
# trait

# Required libraries -----------------------------------------------------------

library(here)
here()
source(here("AccFunc.R"))


# Scenario to be plotted - corresponds to folders where simulations are stored

scenario<-"learHonest_/alphaAct"


# Load files -------------------------------------------------------------------

(listTest<-list.files(here("Simulations",paste0(scenario,"_"))))
(sdList<-grep("evol",listTest,value=TRUE))

evol<-fread(here("Simulations",paste0(scenario,"_"),sdList[1]))

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
                     m.weightCrit_4=mean(WeightCrit_4)),by=time]

# Plot mean and IQRs of the genotypes and phenotypes ----------------------------

# png(here("Simulations",scenario,"hawkDoveLearn_0.1.png"),
# height = 800,width = 800)

par(plt=posPlot(numploty = 2,idploty = 2),xaxt="s",las=2)
plot(x=c(0,max(evolStats$time)),y=c(0.5,0.5),type="l",lwd=2,col="grey",
     ylim=c(0,1),xlab="",ylab="",cex.lab=1.5,cex.axis=1.2,xaxt='n')

polygon(x=c(evolStats$time,rev(evolStats$time)),
      y=c(evolStats$upIQR.freqGenHawk,
          rev(evolStats$lowIQR.freqGenHawk)),
        col=colTypesPol[1],border=NA)
polygon(x=c(evolStats$time,rev(evolStats$time)),
        y=c(evolStats$upIQR.freqGenDove,
            rev(evolStats$lowIQR.freqGenDove)),
      col=colTypesPol[2],border=NA)
polygon(x=c(evolStats$time,rev(evolStats$time)),
        y=c(evolStats$upIQR.freqGenEval,
            rev(evolStats$lowIQR.freqGenEval)),
        col=colTypesPol[3],border=NA)
with(evolStats,{
  lines(time,m.freqGenHawk,col=colTypesLin[1],lwd=3)
  lines(time,m.freqGenDove,col=colTypesLin[2],lwd=3)
  lines(time,m.freqEval,col=colTypesLin[3],lwd=3)
  lines(x=c(0,max(time)),y=c(0.66,0.66),col=colTypesLin[1],lwd=2,lty=2)
})


legend("topleft",legend=c("Hawk","Dove"),
       lty=c(1,1),lwd=2,col=colTypesLin[1:2],bty="o",cex=1)


par(plt=posPlot(numploty = 2,idploty = 1),xaxt="n",las=1,new=TRUE,xpd=T)

plot(x=c(0,max(evolStats$time)),y=c(0.4444436,0.4444436),type="l",lwd=2,
     col=colTypesLin[1],ylim=c(0,1),xlab="Generations",ylab="Frequency",
     cex.lab=1.5,cex.axis=1.2,xaxt='s',lty=2)
lines(x=c(0,max(evolStats$time)),y=c(0.4444436,0.4444436)+0.01,type="l",lwd=2,
      col=colTypesLin[2],lty=2)
lines(x=c(0,max(evolStats$time)),y=c(0.1111111,0.1111111),type="l",lwd=2,
      col=colTypesLin[3],lty=2)
polygon(x=c(evolStats$time,rev(evolStats$time)),
        y=c(evolStats$upIQR.freqHH,
            rev(evolStats$lowIQR.freqHH)),
        col=colTypesPol[1],border=NA)
polygon(x=c(evolStats$time,rev(evolStats$time)),
        y=c(evolStats$upIQR.freqHD,
            rev(evolStats$lowIQR.freqHD)),
        col=colTypesPol[2],border=NA)
polygon(x=c(evolStats$time,rev(evolStats$time)),
        y=c(evolStats$upIQR.freqDD,
            rev(evolStats$lowIQR.freqDD)),
        col=colTypesPol[3],border=NA)
with(evolStats,{
  lines(time,m.freqHH,col=colTypesLin[1],lwd=3)
  lines(time,m.freqHD,col=colTypesLin[2],lwd=3)
  lines(time,m.freqDD,col=colTypesLin[3],lwd=3)
})

legend("topright",legend=c("HH","HD","DD"),
       lty=c(1,1,1),lwd=2,col=colTypesLin,bty="o",cex=1.15)

# Phenotypic frequencies 

par(plt=posPlot(numploty = 2,idploty = 1),xaxt="n",las=1,new=TRUE,xpd=T)
plot(x=c(0,max(evolStats$time)),y=c(0.5,0.5),type="l",lwd=2,col="grey",
     ylim=c(0,1),xlab="",ylab="",cex.lab=1.5,cex.axis=1.2,xaxt='s',las=1,
     xaxt="n")

polygon(x=c(evolStats$time,rev(evolStats$time)),
        y=c(evolStats$upIQR.freqFenHawk,
            rev(evolStats$lowIQR.freqFenHawk)),
        col=colTypesPol[1],border=NA)
polygon(x=c(evolStats$time,rev(evolStats$time)),
        y=c(evolStats$upIQR.freqFenDove,
            rev(evolStats$lowIQR.freqFenDove)),
        col=colTypesPol[2],border=NA)
with(evolStats,{
  lines(time,m.freqFenHawk,col=colTypesLin[1],lwd=3)
  lines(time,m.freqFenDove,col=colTypesLin[2],lwd=3)
  lines(x=c(0,max(time)),y=c(0.66,0.66),col="grey",lwd=2)
})
# mtext(side = 2,text = "frequency",line = 3,cex=1.2)

# dev.off()

# Plot the weights of the actor ------------------------------------------------

nCenters<-5
interv<-1/nCenters
centers<-interv*0.5+interv*seq(0,nCenters-1)
weights<-as.double(evolStats[time==max(time),.SD,
                    .SDcols=grep("m.weightAct",names(evolStats),value = TRUE)])
# weights<-rep(-5,nCenters)
rangx<-seq(0,1,length=1000)

par(plt=posPlot())
plot(logist(totRBF(rangx,centers,0.01,weights),alpha = 0,
            beta = 1)~rangx,type='l',col=1,
     xlab="x",ylab="p(Dove)",ylim=c(0,1),lwd=3)
points(y=logist(weights,0,1),x=centers,cex=3)

# Plot the weights of the critic ------------------------------------------------

nCenters<-5
interv<-1/nCenters
centers<-interv*0.5+interv*seq(0,nCenters-1)
weights<-as.double(evolStats[time==max(time),.SD,
                              .SDcols=grep("m.weightCrit",
                                           names(evolStats),value = TRUE)])
# weights<-rep(-5,nCenters)
rangx<-seq(0,1,length=1000)

par(plt=posPlot())
plot(totRBF(rangx,centers,0.01,weights)~rangx,type='l',col=1,
     xlab="x",ylab="Value",ylim=c(0,1),lwd=3)
points(y=weights,x=centers,cex=3)

# Plot mean and IQRs of the reaction norm parameters ---------------------------

par(plt=posPlot(numploty = 2,idploty = 1),xaxt="s",las=1)
plot(x=c(0,max(evolStats$time)),y=c(0,0),type="l",lwd=2,col="grey",
     ylim=fivenum(as.matrix(evol[,.(meanAlpha,meanBeta)]))[c(1,5)],
     xlab="",ylab="",cex.lab=1.5,cex.axis=1.2,xaxt='s')
polygon(x=c(evolStats$time,rev(evolStats$time)),
        y=c(evolStats$upIQR.alpha,rev(evolStats$lowIQR.alpha)),
        col=colGenesPol[1],border = NA)
polygon(x=c(evolStats$time,rev(evolStats$time)),
        y=c(evolStats$upIQR.beta,rev(evolStats$lowIQR.beta)),
        col=colGenesPol[2],border = NA)
with(evolStats,{
  lines(time,m.meanAlpha,col=colGenesLin[1],lwd=3)
  lines(time,m.meanBeta,col=colGenesLin[2],lwd=3)
})
legend("topleft",legend = c(expression(alpha),expression(beta)),
       col=colGenesLin,lwd=2,bty = "n")


hist(evol[time==9600,meanBeta])
hist(evol[time==9600,meanAlpha])
plot(meanAlpha~meanBeta,data=evol[time==9600])
linReg<-lm(meanAlpha~meanBeta,data=evol[time==9600])
linReg$coefficients[2]


# Plot trajectory of the trait evolution for all runs separetely ---------------

traitsTrajs<-dcast(evol,time~seed,value.var = c("meanAlpha","meanBeta"))

par(plt=posPlot(numploty = 2,idploty = 2),xaxt="s",las=1)
matplot(x=traitsTrajs[,time],
        y=traitsTrajs[,.SD,.SDcol=grep("Alpha",names(traitsTrajs),value = T)],
        type="l",lwd=2,col=2:17,lty = 1,
     xlab="",ylab=expression(alpha),cex.lab=1.5,cex.axis=1.2,xaxt='n')
lines(y=c(0,0),x=c(0,traitsTrajs[time==max(time),time]),col="grey")
par(plt=posPlot(numploty = 2,idploty = 1),xaxt="s",las=1,new=T)
matplot(x=traitsTrajs[,time],
        y=traitsTrajs[,.SD,.SDcol=grep("Beta",names(traitsTrajs),value = T)],
        type="l",lwd=2,col=2:17,lty = 1,
        xlab="",ylab=expression(beta),cex.lab=1.5,cex.axis=1.2,xaxt='s')
lines(y=c(0,0),x=c(0,traitsTrajs[time==max(time),time]),col="grey")



# Plot the logistic function of the last generation ---------------------------

rangQual<-seq(0,1,length.out = 100)
par(plt=posPlot(),xaxt="s",las=1)
plot(logist(rangQual,evolStats[time==max(time),m.meanAlpha],
            evolStats[time==max(time),m.meanBeta])~rangQual,
     ylab="Badge size", xlab="Quality",type="l",lwd=3,ylim=c(0,1))


# Frequencies without the colour ribbons ---------------------------------------

par(plt=posPlot(numploty = 2,idploty = 2),xaxt="n")

matplot(x=evolStats[,time],
        y=evolStats[,.(m.freqGenDove,m.freqGenHawk,m.freqEval)],
        pch = 19,ylab="frequency",xlab="",type="l")
lines(x=c(0,max(evol$time)),y=c(0.66,0.66),col="grey")
legend("right",legend=c("Dove","Hawk","Evaluators"),pch = 19,col = c(1,2,3),
       title="genotypes")
par(plt=posPlot(numploty = 2,idploty = 1),xaxt="s",new=TRUE)
matplot(x=evolStats[,time],
        y=evolStats[,.(m.freqFenDove,m.freqFenHawk)],
        pch = 19,ylab="frequency",xlab="",type="l")
lines(x=c(0,max(evol$time)),y=c(0.66,0.66),col="grey")
legend("right",legend=c("Dove","Hawk"),pch = 19,col = c(1,2),
       title="phenotypes")

par(plt=posPlot())

par(plt=posPlot(numploty = 2,idploty = 2),xaxt="n")
matplot(x=evolStats[,time],
        y=evolStats[,.(m.meanAlpha,m.meanBeta)],
        pch = 19,ylab="frequency",xlab="",type="l")
lines(x=c(0,max(evol$time)),y=c(0.66,0.66),col="grey")
legend("right",legend=c("Dove","Hawk","Evaluators"),pch = 19,col = c(1,2,3),
       title="genotypes")

# Dynamics of the reaction norm with the weights -------------------------------

seqYax<-c("s",rep("n",3))
# seqYlabUp<-c("Badge",rep("",3))
seqYlabUp<-c("Value",rep("",3))
seqYlabDown<-c("P(dove)",rep("",3))
seqXlabDown<-c("","Badge size","")

par(plt=posPlot(numploty = 3,numplotx = 1,idploty = 2))
plot(x=c(0,max(evolStats$time)),y=c(0,0),type="l",lwd=2,col="grey",
     ylim=fivenum(as.matrix(evol[,.(meanAlpha,meanBeta)]))[c(1,5)],
     xlab="",ylab="Trait \n value",cex.lab=1.5,cex.axis=1,xaxt='n')
polygon(x=c(evolStats$time,rev(evolStats$time)),
        y=c(evolStats$upIQR.alpha,rev(evolStats$lowIQR.alpha)),
        col=colGenesPol[1],border = NA)
polygon(x=c(evolStats$time,rev(evolStats$time)),
        y=c(evolStats$upIQR.beta,rev(evolStats$lowIQR.beta)),
        col=colGenesPol[2],border = NA)
with(evolStats,{
  lines(time,m.meanAlpha,col=colGenesLin[1],lwd=3)
  lines(time,m.meanBeta,col=colGenesLin[2],lwd=3)
})
legend("topleft",legend = c(expression(alpha),expression(beta)),
       col=colGenesLin,lwd=2,bty = "n")
axis(side=1,padj = -3)
rangQual<-seq(0,1,length.out = 100)
nCenters<-5
interv<-1/nCenters
centers<-interv*0.5+interv*seq(0,nCenters-1)
rangx<-seq(0,1,length=1000)
count<-0
for(genC in round(seq(1,length(unique(evolStats$time)),length.out = 5))[2:5]){
  count<-count+1
  par(plt=posPlot(numplotx = 4,numploty = 3,idplotx = count,idploty = 3),
      xaxt="s",las=1,new=TRUE)
  # plot(logist(rangQual,evolStats[time==unique(time)[genC],m.meanAlpha],
  #             evolStats[time==unique(time)[genC],m.meanBeta])~rangQual,
  #      yaxt=seqYax[count],ylab=seqYlabUp[count], xlab="",type="l",lwd=3,
  #      ylim=c(0,1),xaxt="n")
  weightsCrit<-as.double(evolStats[time==unique(time)[genC],.SD,
                                    .SDcols=grep("m.weightCrit",
                                                 names(evolStats),value = TRUE)])
  plot(totRBF(rangx,centers,0.01,weightsCrit)~rangx,
       yaxt=seqYax[count],ylab=seqYlabUp[count], xlab="",type="l",lwd=3,xaxt="n")
  points(y=weightsCrit,x=centers,cex=1.5,pch=19,col="red")
  text(x=0.5,y=0.8,labels = paste0("time=",unique(evolStats$time)[genC]))
  par(plt=posPlot(numplotx = 4,numploty = 3,idplotx = count,idploty = 1),
      las=1,new=TRUE)
  weightsAct<-as.double(evolStats[time==unique(time)[genC],.SD,
                                .SDcols=grep("m.weightAct",
                                             names(evolStats),value = TRUE)])
  plot(logist(totRBF(rangx,centers,0.01,weightsAct),alpha = 0,
              beta = 1)~rangx,type='l',col=1,
       xlab=seqXlabDown[count],ylab=seqYlabDown[count],ylim=c(0,1),
       lwd=3,yaxt=seqYax[count],xaxt="s")
  points(y=logist(weightsAct,0,1),x=centers,cex=1.5,pch=19,col="red")
}

# Plot the frequency of interaction types --------------------------------------

par(plt=posPlot(numploty = 2,idploty = 2),xaxt="s",las=2)
plot(x=c(0,max(evolStats$time)),y=c(0.4444436,0.4444436),type="l",lwd=2,
     col=colTypesLin[1],ylim=c(0,1),xlab="",ylab="",cex.lab=1.5,
     cex.axis=1.2,xaxt='n',lty=2)
lines(x=c(0,max(evolStats$time)),y=c(0.4444436,0.4444436)+0.01,type="l",lwd=2,
      col=colTypesLin[2],lty=2)
lines(x=c(0,max(evolStats$time)),y=c(0.1111111,0.1111111),type="l",lwd=2,
      col=colTypesLin[3],lty=2)
polygon(x=c(evolStats$time,rev(evolStats$time)),
        y=c(evolStats$upIQR.freqHH,
            rev(evolStats$lowIQR.freqHH)),
        col=colTypesPol[1],border=NA)
polygon(x=c(evolStats$time,rev(evolStats$time)),
        y=c(evolStats$upIQR.freqHD,
            rev(evolStats$lowIQR.freqHD)),
        col=colTypesPol[2],border=NA)
polygon(x=c(evolStats$time,rev(evolStats$time)),
        y=c(evolStats$upIQR.freqDD,
            rev(evolStats$lowIQR.freqDD)),
        col=colTypesPol[3],border=NA)
with(evolStats,{
  lines(time,m.freqHH,col=colTypesLin[1],lwd=3)
  lines(time,m.freqHD,col=colTypesLin[2],lwd=3)
  lines(time,m.freqDD,col=colTypesLin[3],lwd=3)
})

legend("topright",legend=c("HH","HD","DD"),
       lty=c(1,1,1),lwd=2,col=colTypesLin,bty="o",cex=1.15)


par(plt=posPlot(numploty = 2,idploty = 1),xaxt="n",las=2,new=TRUE)
plot(x=c(0,max(evolStats$time)),y=c(0.5,0.5),type="l",lwd=2,col="grey",ylim=c(0,1),
     xlab="",ylab="",cex.lab=1.5,cex.axis=1.2,xaxt='n')

polygon(x=c(evolStats$time,rev(evolStats$time)),
        y=c(evolStats$upIQR.freqFenHawk,
            rev(evolStats$lowIQR.freqFenHawk)),
        col=colTypesPol[1],border=NA)
polygon(x=c(evolStats$time,rev(evolStats$time)),
        y=c(evolStats$upIQR.freqFenDove,
            rev(evolStats$lowIQR.freqFenDove)),
        col=colTypesPol[2],border=NA)
with(evolStats,{
  lines(time,m.freqFenHawk,col=colTypesLin[1],lwd=3)
  lines(time,m.freqFenDove,col=colTypesLin[2],lwd=3)
  lines(x=c(0,max(time)),y=c(0.66,0.66),col="grey",lwd=2)
})

# Quick and dirty plotting -----------------------------------------------------

par(plt=posPlot(numploty = 2,idploty = 2),xaxt="n")
matplot(x=evol[,time],y=evol[,.(freqGenDove,freqGenHawks,freqGenEval)],
        pch = 19,ylab="frequency",xlab="",ylim = c(0,1))
lines(x=c(0,max(evol$time)),y=c(0.66,0.66),col="grey")
legend("right",legend=c("Dove","Hawk","Evaluators"),pch = 19,col = c(1,2,3),
       title="genotypes")

par(plt=posPlot(numploty = 2,idploty = 1),new=T,xaxt="s")
matplot(x=evol[,time],y=evol[,.(freqFenDoves,freqFenHawks)],
        pch = 19,ylab="frequency")
lines(x=c(0,max(evol$time)),y=c(0.66,0.66),col="grey")
legend("right",legend=c("Dove","Hawk"),pch = 19,col = c(1,2),
       title="phenotypes")

par(plt=posPlot(numploty = 2,idploty = 2),xaxt="n")
matplot(x=evol[,time],y=evol[,.(meanAlpha,meanBeta)],
        pch = 19,ylab="trait value")
legend("right",legend=c("alpha","beta"),pch = 19,col = c(1,2),
       title="phenotypes")

par(plt=posPlot(numploty = 2,idploty = 1),xaxt="s",new=TRUE)
matplot(y=evol[,.(meanCue)],x = evol[,.(time)],pch = 19,
        ylab="Cue")



# Effect of SD -----------------------------------------------------------------


sdRaw<-rbindlist(lapply(sdList,FUN = function(x){
  tempDT<-fread(here("Simulations","SdCue_",x))
  sd<-gsub(".txt","",grep("sdCue",strsplit(x,"_")[[1]],value=TRUE))
  sdVal<-as.numeric(gsub("sdCue","",sd))
  tempDT[,sdCuePar:=rep(sdVal,dim(tempDT)[1])]
  return(tempDT)
}))

sdStats<-sdRaw[,.(m.freqGenHawk=mean(freqGenHawks),
                  m.freqGenDove=mean(freqGenDove),
                  m.freqEval=mean(freqGenEval),
                  m.freqFenHawk=mean(freqFenHawks),
                  m.freqFenDove=mean(freqFenDoves),
                  m.meanAlpha=mean(meanAlpha),
                  m.meanBeta=mean(meanBeta)),by=.(time,sdCuePar)]

par(plt=posPlot(numploty = 2,idploty = 2),xaxt="n",las=2)
matplot(x=sdStats[sdCuePar==0.2,time],
        y=sdStats[sdCuePar==0.2,.(m.freqGenDove,m.freqGenHawk,m.freqEval)],
        pch = 19,ylab="frequency",xlab="",type="l",lty=1,ylim=c(0,1))
lines(x=c(0,max(sdStats[sdCuePar==0.2,time])),y=c(0.66,0.66),col="grey")
matlines(x=sdStats[sdCuePar==1,time],
        y=sdStats[sdCuePar==1,.(m.freqGenDove,m.freqGenHawk,m.freqEval)],
        pch = 19,ylab="frequency",xlab="",type="l",lty=2)

legend("right",legend=c("Dove","Hawk","Evaluators"),pch = 19,col = c(1,2,3),
       title="genotypes")
par(plt=posPlot(numploty = 2,idploty = 1),xaxt="s",new=TRUE,las=1)
matplot(x=sdStats[sdCuePar==0.2,time],
        y=sdStats[sdCuePar==0.2,.(m.freqFenDove,m.freqFenHawk)],
        pch = 19,ylab="frequency",xlab="",type="l",lty=1)
matlines(x=sdStats[sdCuePar==1,time],
        y=sdStats[sdCuePar==1,.(m.freqFenDove,m.freqFenHawk)],
        pch = 19,ylab="frequency",xlab="",type="l",lty=2)
lines(x=c(0,max(sdStats[,time])),y=c(0.66,0.66),col="grey")
legend("right",legend=c("Dove","Hawk"),pch = 19,col = c(1,2),
       title="phenotypes")

sdStatsEnd<-sdStats[time>max(time)*0.9,.(m.freqGenHawk=mean(m.freqGenHawk),
                    m.freqGenDove=mean(m.freqGenDove),
                    m.freqEval=mean(m.freqEval),
                    m.freqFenHawk=mean(m.freqFenHawk),
                    m.freqFenDove=mean(m.freqFenDove),
                    m.meanAlpha=mean(m.meanAlpha),
                    m.meanBeta=mean(m.meanBeta)),by=sdCuePar]

par(plt=posPlot())
matplot(x=sdStatsEnd[,sdCuePar],
        y=sdStatsEnd[,.(m.freqGenDove,m.freqGenHawk,m.freqEval)],
        pch=20,col=c(1,2,3))
legend("right",legend=c("Dove","Hawk","Evaluators"),pch = 19,col = c(1,2,3),
       title="genotypes")


par(plt=posPlot())
matplot(x=sdStatsEnd[,sdCuePar],
        y=sdStatsEnd[,.(m.meanAlpha,m.meanBeta)],
        pch=20,col=c(1,2,3))
legend("right",legend=c("Dove","Hawk","Evaluators"),pch = 19,col = c(1,2,3),
       title="genotypes")


# Reaction norms ---------------------------------------------------------------


rangQual<-seq(0,1,length.out = 100)

par(plt=posPlot())
plot(Logist(rangQual,sdStatsEnd[sdCuePar==0.2,m.meanAlpha],
            sdStatsEnd[sdCuePar==0.2,m.meanBeta]),xlab="Badge",ylab="Quality",
     type="l",ylim=c(0.5,1),col=0)
colcount<-1
for (sd in unique(sdStatsEnd$sdCuePar)) {
  lines(Logist(rangQual,sdStatsEnd[sdCuePar==sd,m.meanAlpha],
               sdStatsEnd[sdCuePar==sd,m.meanBeta]),col=colcount+1)
}


