# Varying parameter of a single scenario ---------------------------------------

# Required libraries -----------------------------------------------------------

library(here)
here()
source(here("AccFunc.R"))

# Scenario to be plotted - corresponds to folders where simulations are stored

scenario<-"learHonest_/QualStDv"

# Load files -------------------------------------------------------------------

(listTest<-list.files(here("Simulations",paste0(scenario,"_"))))
(sdList<-grep("evol",listTest,value=TRUE))
(indList<-grep("ind",listTest,value=TRUE))

evol<-do.call(rbind,lapply(sdList,filesScenar,scenario))
inds<-do.call(rbind,lapply(indList,filesScenar,scenario))


# Graphs -----------------------------------------------------------------------

gener<-tail(pop[,unique(time)],1)
lastInt<-tail(pop[,unique(nInteract)],3)
runChoi<-0
nCenters<-5
interv<-1/nCenters
centers<-interv*0.5+interv*seq(0,nCenters-1)
rangx<-seq(0,1,length=1000)
tempPop<-inds[time==gener&nInteract==lastInt[1],.SD[.N],
             .SDcol=c(grep("Weight",
                           names(evol),value = TRUE),"Quality","QualStDv"),
             by=indId]
dataIndAct_0.1<-sapply(as.list(tempPop[QualStDv==0.1,indId]),
                   function(x){x=
                     logist(totRBF(rangx,
                                   centers,0.01,
                                   as.double(
                                     tempPop[indId==x,.SD,
                                             .SDcol=grep("WeightAct",
                                                         names(tempPop),
                                                         value = TRUE)
                                             ])),alpha=0,beta = 1)})

dataIndAct_1.1<-sapply(as.list(tempPop[QualStDv==1.1,indId]),
                       function(x){x=
                         logist(totRBF(rangx,
                                       centers,0.01,
                                       as.double(
                                         tempPop[indId==x,.SD,
                                                 .SDcol=grep("WeightAct",
                                                             names(tempPop),
                                                             value = TRUE)
                                                 ])),alpha=0,beta = 1)})


par(plt=posPlot(numploty = 1,idploty = 1,numplotx = 2,idplotx = 1)
    ,las=1)
matplot(x=rangx,y=dataIndAct_0.1,col = paletteMeans(100)[
  findInterval(tempPop[,Quality],colorbreaksQual)],
  lwd=2,lty = 1,xaxt="s",yaxt="n",ylim = c(0,1),
  xlab="",ylab="",type = "l")
axis(2,cex.axis=1.5)
par(las=0)
mtext("Badge",1,cex = 2,line = 3)
mtext("p(Dove)",2,cex = 2,line = 3)


par(plt=posPlot(numploty = 1,idploty = 1,numplotx = 2,idplotx = 2)
    ,las=1,new=T)
matplot(x=rangx,y=dataIndAct_1.1,col = paletteMeans(100)[
  findInterval(tempPop[,Quality],colorbreaksQual)],
  lwd=2,lty = 1,xaxt="s",yaxt="n",ylim = c(0,1),
  xlab="",ylab="",type = "l")
par(las=0)
mtext("Badge",1,cex = 2,line = 3)
par(las=1)


