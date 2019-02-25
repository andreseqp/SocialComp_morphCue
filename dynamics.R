library(data.table)

dirAndes<-"C:/Users/a.quinones/"
dirLaptop<-"D:/"

worPlac<-dirAndes

source(paste(worPlac,"Dropbox/R_files/posPlots.R",sep=""))

setwd(paste(worPlac,"Simulations/morphCue/sdCue_/",sep=""))
(listTest<-list.files())
sdList<-grep("sdCue",listTest,value=TRUE)

test1<-fread(listTest[2])

par(plt=posPlot(numploty = 2,idploty = 2),xaxt="n")
matplot(test1[,.(freqGenDove,freqGenHawks,freqGenEval)],
        pch = 19,ylab="frequency")
lines(x=c(0,max(test1$time)),y=c(0.66,0.66),col="grey")
legend("right",legend=c("Dove","Hawk","Evaluators"),pch = 19,col = c(1,2,3),
       title="genotypes")

par(plt=posPlot(numploty = 2,idploty = 1),new=T)
matplot(test1[,.(freqFenDoves,freqFenHawks)],
        pch = 19,ylab="frequency")
lines(x=c(0,100),y=c(0.66,0.66),col="grey")
legend("right",legend=c("Dove","Hawk"),pch = 19,col = c(1,2),
       title="phenotypes")

test1Stats<-test1[,.(m.freqGenHawk=mean(freqGenHawks),
                     m.freqGenDove=mean(freqGenDove),
                     m.freqEval=mean(freqGenEval),
                     m.freqFenHawk=mean(freqFenHawks),
                     m.freqFenDove=mean(freqFenDoves)),by=time]

par(plt=posPlot(numploty = 2,idploty = 2),xaxt="n")
matplot(x=test1Stats[,time],
        y=test1Stats[,.(m.freqGenDove,m.freqGenHawk,m.freqEval)],
        pch = 19,ylab="frequency",xlab="")
lines(x=c(0,max(test1$time)),y=c(0.66,0.66),col="grey")
legend("right",legend=c("Dove","Hawk","Evaluators"),pch = 19,col = c(1,2,3),
       title="genotypes")
par(plt=posPlot(numploty = 2,idploty = 1),xaxt="s",new=TRUE)
matplot(x=test1Stats[,time],
        y=test1Stats[,.(m.freqFenDove,m.freqFenHawk)],
        pch = 19,ylab="frequency",xlab="")
lines(x=c(0,max(test1$time)),y=c(0.66,0.66),col="grey")
legend("right",legend=c("Dove","Hawk"),pch = 19,col = c(1,2),
       title="phenotypes")


# Effect of SD -----------------------------------------------------------------


sdRaw<-rbindlist(lapply(sdList,FUN = function(x){
  tempDT<-fread(x)
  sd<-gsub(".txt","",grep("sdCue",strsplit(x,"_")[[1]],value=TRUE))
  sdVal<-as.numeric(gsub("sdCue","",sd))
  tempDT[,sdCuePar:=rep(sdVal,dim(tempDT)[1])]
  return(tempDT)
}))

sdStats<-sdRaw[,.(m.freqGenHawk=mean(freqGenHawks),
                  m.freqGenDove=mean(freqGenDove),
                  m.freqEval=mean(freqGenEval),
                  m.freqFenHawk=mean(freqFenHawks),
                  m.freqFenDove=mean(freqFenDoves)),by=.(time,sdCuePar)]

par(plt=posPlot(numploty = 2,idploty = 2),xaxt="n",las=2)
matplot(x=sdStats[sdCuePar==0.2,time],
        y=sdStats[sdCuePar==0.2,.(m.freqGenDove,m.freqGenHawk,m.freqEval)],
        pch = 19,ylab="frequency",xlab="",type="l",lty=1,ylim=c(0,1))
lines(x=c(0,max(sdStats[sdCuePar==0.2,time])),y=c(0.66,0.66),col="grey")
matlines(x=sdStats[sdCuePar==0.4,time],
        y=sdStats[sdCuePar==0.4,.(m.freqGenDove,m.freqGenHawk,m.freqEval)],
        pch = 19,ylab="frequency",xlab="",type="l",lty=2)

legend("right",legend=c("Dove","Hawk","Evaluators"),pch = 19,col = c(1,2,3),
       title="genotypes")
par(plt=posPlot(numploty = 2,idploty = 1),xaxt="s",new=TRUE)
matplot(x=test1Stats[,time],
        y=test1Stats[,.(m.freqFenDove,m.freqFenHawk)],
        pch = 19,ylab="frequency",xlab="")
lines(x=c(0,max(test1$time)),y=c(0.66,0.66),col="grey")
legend("right",legend=c("Dove","Hawk"),pch = 19,col = c(1,2),
       title="phenotypes")

