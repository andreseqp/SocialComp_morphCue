library(data.table)
library(here)
here()

source("C:/Users/a.quinones/Dropbox/R_files/posPlots.R")


(listTest<-list.files(here("Simulations","Init_")))
sdList<-grep(".txt",listTest,value=TRUE)

test1<-fread(here("Simulations","Init_",sdList[1]))

par(plt=posPlot(numploty = 2,idploty = 2),xaxt="n")
matplot(y = test1[,.(freqGenDove,freqGenHawks,freqGenEval)],
        x= test1[,time],xlab="",
        pch = 19,ylab="frequency")
lines(x=c(0,max(test1$time)),y=c(0.66,0.66),col="grey")
legend("right",legend=c("Dove","Hawk","Evaluators"),pch = 19,col = c(1,2,3),
       title="genotypes")

par(plt=posPlot(numploty = 2,idploty = 1),new=T,xaxt="s")
matplot(y=test1[,.(freqFenDoves,freqFenHawks)],
        x=test1[,time],
        pch = 19,ylab="frequency")
lines(x=c(0,test1[,max(time)]),y=c(0.66,0.66),col="grey")
legend("right",legend=c("Dove","Hawk"),pch = 19,col = c(1,2),
       title="phenotypes")

par(plt=posPlot(numploty = 2,idploty = 2),xaxt="n")
matplot(test1[,.(meanAlpha,meanBeta)],x = test1[,.(time)],
        pch = 19,ylab="trait value",xlab="")
legend("right",legend=c("Dove","Hawk"),pch = 19,col = c(1,2),
       title="phenotypes")

par(plt=posPlot(numploty = 2,idploty = 1),xaxt="s",new=TRUE)
matplot(y=test1[,.(meanCue)],x = test1[,.(time)],pch = 19,
     ylab="Cue")




test1Stats<-test1[,.(m.freqGenHawk=mean(freqGenHawks),
                     m.freqGenDove=mean(freqGenDove),
                     m.freqEval=mean(freqGenEval),
                     m.freqFenHawk=mean(freqFenHawks),
                     m.freqFenDove=mean(freqFenDoves),
                     m.meanAlpha=mean(meanAlpha),
                     m.meanBeta=mean(meanBeta)),by=time]

par(plt=posPlot(numploty = 2,idploty = 2),xaxt="n",las=1)
matplot(x=test1Stats[,time],
        y=test1Stats[,.(m.freqGenDove,m.freqGenHawk,m.freqEval)],
        pch = 19,ylab="frequency",xlab="",type="l",ylim = c(0,1))
lines(x=c(0,max(test1$time)),y=c(0.66,0.66),col="grey")
legend("right",legend=c("Dove","Hawk","Evaluators"),pch = 19,col = c(1,2,3),
       title="genotypes")
par(plt=posPlot(numploty = 2,idploty = 1),xaxt="s",new=TRUE)
matplot(x=test1Stats[,time],
        y=test1Stats[,.(m.freqFenDove,m.freqFenHawk)],
        pch = 19,ylab="frequency",xlab="",type="l",ylim=c(0,1))
lines(x=c(0,max(test1$time)),y=c(0.66,0.66),col="grey")
legend("right",legend=c("Dove","Hawk"),pch = 19,col = c(1,2),
       title="phenotypes")




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
plot(Logist(rangQual,test1Stats[time==max(time),m.meanAlpha],
            test1Stats[time==max(time),m.meanBeta]),xlab="Badge",ylab="Quality",
     type="l",ylim=c(0.5,1),col=1)
colcount<-1
for (sd in unique(sdStatsEnd$sdCuePar)) {
  lines(Logist(rangQual,sdStatsEnd[sdCuePar==sd,m.meanAlpha],
               sdStatsEnd[sdCuePar==sd,m.meanBeta]),col=colcount+1)
}


