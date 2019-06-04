# Evolutionary dynamics of the hawk dove game with an extra strategy that 
# learns a preference for the phenotypic strategy depending on a morphological
# trait

# Required libraries -----------------------------------------------------------

library(data.table)
library(here)
library('plotrix')
here()
source(here("aesth.R"))
source("C:/Users/a.quinones/Dropbox/R_files/posPlots.R")


# Scenario to be plotted - corresponds to folders where simulations are stored

scenario<-"baselineFit_"


# Load files -------------------------------------------------------------------

(listTest<-list.files(here("Simulations",scenario)))
sdList<-grep(".txt",listTest,value=TRUE)

test1<-fread(here("Simulations",scenario,sdList[1]))

# Quick and dirty plotting -----------------------------------------------------

par(plt=posPlot(numploty = 2,idploty = 2),xaxt="n")
matplot(x=test1[,time],y=test1[,.(freqGenDove,freqGenHawks,freqGenEval)],
        pch = 19,ylab="frequency")
lines(x=c(0,max(test1$time)),y=c(0.66,0.66),col="grey")
legend("right",legend=c("Dove","Hawk","Evaluators"),pch = 19,col = c(1,2,3),
       title="genotypes")

par(plt=posPlot(numploty = 2,idploty = 1),new=T,xaxt="s")
matplot(x=test1[,time],y=test1[,.(freqFenDoves,freqFenHawks)],
        pch = 19,ylab="frequency")
lines(x=c(0,100),y=c(0.66,0.66),col="grey")
legend("right",legend=c("Dove","Hawk"),pch = 19,col = c(1,2),
       title="phenotypes")

par(plt=posPlot(numploty = 2,idploty = 2),xaxt="n")
matplot(x=test1[,time],y=test1[,.(meanAlpha,meanBeta)],
        pch = 19,ylab="trait value")
legend("right",legend=c("alpha","beta"),pch = 19,col = c(1,2),
       title="phenotypes")

par(plt=posPlot(numploty = 2,idploty = 1),xaxt="s",new=TRUE)
matplot(y=test1[,.(meanCue)],x = test1[,.(time)],pch = 19,
     ylab="Cue")


# Extract means and IQR for the dynamic variables ------------------------------

test1Stats<-test1[,.(m.freqGenHawk=mean(freqGenHawks),
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
                     lowIQR.beta=fivenum(meanBeta)[2]),by=time]


# Plot mean and IQRs of the genotypes and phenotypes ----------------------------

par(plt=posPlot(numploty = 2,idploty = 2),xaxt="s",las=2)
plot(x=c(0,max(test1Stats$time)),y=c(0.5,0.5),type="l",lwd=2,col="grey",
     ylim=c(0,1),xlab="",ylab="",cex.lab=1.5,cex.axis=1.2,xaxt='n')

polygon(x=c(test1Stats$time,rev(test1Stats$time)),
      y=c(test1Stats$upIQR.freqGenHawk,
          rev(test1Stats$lowIQR.freqGenHawk)),
        col=colTypesPol[1],border=NA)
polygon(x=c(test1Stats$time,rev(test1Stats$time)),
        y=c(test1Stats$upIQR.freqGenDove,
            rev(test1Stats$lowIQR.freqGenDove)),
      col=colTypesPol[2],border=NA)
polygon(x=c(test1Stats$time,rev(test1Stats$time)),
        y=c(test1Stats$upIQR.freqGenEval,
            rev(test1Stats$lowIQR.freqGenEval)),
        col=colTypesPol[3],border=NA)
with(test1Stats,{
  lines(time,m.freqGenHawk,col=colTypesLin[1],lwd=3)
  lines(time,m.freqGenDove,col=colTypesLin[2],lwd=3)
  lines(time,m.freqEval,col=colTypesLin[3],lwd=3)
})

legend("topright",legend=c("Hawk","Dove","Learner"),
       lty=c(1,1,1),lwd=2,col=colTypesLin,bty="o",cex=1.15)


par(plt=posPlot(numploty = 2,idploty = 1),xaxt="n",las=2,new=TRUE)
plot(x=c(0,max(test1Stats$time)),y=c(0.5,0.5),type="l",lwd=2,col="grey",ylim=c(0,1),
     xlab="",ylab="",cex.lab=1.5,cex.axis=1.2,xaxt='n')

polygon(x=c(test1Stats$time,rev(test1Stats$time)),
        y=c(test1Stats$upIQR.freqFenHawk,
            rev(test1Stats$lowIQR.freqFenHawk)),
        col=colTypesPol[1],border=NA)
polygon(x=c(test1Stats$time,rev(test1Stats$time)),
        y=c(test1Stats$upIQR.freqFenDove,
            rev(test1Stats$lowIQR.freqFenDove)),
        col=colTypesPol[2],border=NA)
with(test1Stats,{
  lines(time,m.freqFenHawk,col=colTypesLin[1],lwd=3)
  lines(time,m.freqFenDove,col=colTypesLin[2],lwd=3)
  lines(x=c(0,max(time)),y=c(0.66,0.66),col="grey",lwd=2)
})


# Plot mean and IQRs of the reaction norm parameters ---------------------------

par(plt=posPlot(numploty = 2,idploty = 1),xaxt="s",las=2,new=TRUE)
plot(x=c(0,max(test1Stats$time)),y=c(0,0),type="l",lwd=2,col="grey",
     ylim=fivenum(as.matrix(test1[,.(meanAlpha,meanBeta)]))[c(1,5)],
     xlab="",ylab="",cex.lab=1.5,cex.axis=1.2,xaxt='s')
polygon(x=c(test1Stats$time,rev(test1Stats$time)),
        y=c(test1Stats$upIQR.alpha,rev(test1Stats$lowIQR.alpha)),
        col=colGenesPol[1],border = NA)
polygon(x=c(test1Stats$time,rev(test1Stats$time)),
        y=c(test1Stats$upIQR.beta,rev(test1Stats$lowIQR.beta)),
        col=colGenesPol[2],border = NA)
with(test1Stats,{
  lines(time,m.meanAlpha,col=colGenesLin[1],lwd=3)
  lines(time,m.meanBeta,col=colGenesLin[2],lwd=3)
})

logist<-function(x,alpha,beta){
  return(1/(1+exp(alpha-beta*x)))
}

rangQual<-seq(0,1,length.out = 100)
par(plt=posPlot(),xaxt="s",las=1)
plot(logist(rangQual,test1Stats[time==max(time),m.meanAlpha],
            test1Stats[time==max(time),m.meanBeta])~rangQual,
     ylab="Badge size", xlab="Quality",type="l",lwd=3,ylim=c(0,1))


# Frequencies without the colour ribbons ---------------------------------------

par(plt=posPlot(numploty = 2,idploty = 2),xaxt="n")

matplot(x=test1Stats[,time],
        y=test1Stats[,.(m.freqGenDove,m.freqGenHawk,m.freqEval)],
        pch = 19,ylab="frequency",xlab="",type="l")
lines(x=c(0,max(test1$time)),y=c(0.66,0.66),col="grey")
legend("right",legend=c("Dove","Hawk","Evaluators"),pch = 19,col = c(1,2,3),
       title="genotypes")
par(plt=posPlot(numploty = 2,idploty = 1),xaxt="s",new=TRUE)
matplot(x=test1Stats[,time],
        y=test1Stats[,.(m.freqFenDove,m.freqFenHawk)],
        pch = 19,ylab="frequency",xlab="",type="l")
lines(x=c(0,max(test1$time)),y=c(0.66,0.66),col="grey")
legend("right",legend=c("Dove","Hawk"),pch = 19,col = c(1,2),
       title="phenotypes")

par(plt=posPlot())

par(plt=posPlot(numploty = 2,idploty = 2),xaxt="n")
matplot(x=test1Stats[,time],
        y=test1Stats[,.(m.meanAlpha,m.meanBeta)],
        pch = 19,ylab="frequency",xlab="",type="l")
lines(x=c(0,max(test1$time)),y=c(0.66,0.66),col="grey")
legend("right",legend=c("Dove","Hawk","Evaluators"),pch = 19,col = c(1,2,3),
       title="genotypes")



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

Logist<-function(x,alpha,beta){
  return(1/(1+exp(alpha-x*beta)))
}

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


