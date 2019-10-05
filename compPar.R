# Varying parameter of a single scenario ---------------------------------------

# Required libraries -----------------------------------------------------------

library(here)
here()
source(here("AccFunc.R"))

# Scenario to be plotted - corresponds to folders where simulations are stored

scenario<-"baselineFit"

shortSce<-tail(strsplit(scenario,split = "_/")[[1]],1)

# Load files -------------------------------------------------------------------

(listTest<-list.files(here("Simulations",paste0(scenario,"_"))))
(sdList<-grep("evol",listTest,value=TRUE))
(indList<-grep("ind",listTest,value=TRUE))

inds<-do.call(rbind,lapply(indList[c(3,6,7)],filesScenar,scenario))

inds[,diffActWeight:=abs(WeightAct_0-WeightAct_4)]

# Graphs -----------------------------------------------------------------------

gener<-tail(inds[,unique(time)],1)
lastInt<-tail(inds[,unique(nInteract)],3)
runChoi<-0
nCenters<-6
interv<-1/(nCenters-1)
# centers when extremes are included
centers<-interv*seq(0,nCenters-1) 
# centers when extremes are not included
# centers<-interv*0.5+interv*seq(0,nCenters-1) 
rangx<-seq(0,1,length=1000)
tempPop<-inds[time==gener&nInteract==lastInt[1],.SD[.N],
             .SDcol=c(grep("Weight",
                           names(inds),value = TRUE),"Quality",
                      as.character(shortSce),"diffActWeight"),
             by=indId]
dataIndAct_1<-sapply(as.list(tempPop[get(shortSce)==unique(get(shortSce)[1]),indId]),
                   function(x){x=
                     logist(totRBF(rangx,
                                   centers,0.01,
                                   as.double(
                                     tempPop[indId==x,.SD,
                                             .SDcol=grep("WeightAct",
                                                         names(tempPop),
                                                         value = TRUE)
                                             ])),alpha=0,beta = 1)})
idPAr<-3
dataIndAct_last<-sapply(as.list(tempPop[get(shortSce)==unique(get(shortSce))[idPAr],indId]),
                       function(x){x=
                         logist(totRBF(rangx,
                                       centers,0.01,
                                       as.double(
                                         tempPop[indId==x,.SD,
                                                 .SDcol=grep("WeightAct",
                                                             names(tempPop),
                                                             value = TRUE)
                                                 ])),alpha=0,beta = 1)})

# png(here("Simulations",paste0(scenario,"_"),"compStr1_last.png"),width = 900,height = 600)

par(plt=posPlot(numploty = 1,idploty = 1,numplotx = 2,idplotx = 1)
    ,las=1)
matplot(x=rangx,y=dataIndAct_1,col = paletteMeans(100)[
  findInterval(tempPop[,Quality],colorbreaksQual)],
  lwd=2,lty = 1,xaxt="s",yaxt="n",ylim = c(0,1),
  xlab="",ylab="",type = "l")
text(x = 0.5,y=0.95,labels = bquote(gamma==.(unique(inds[,get(shortSce)][1])))
     ,cex=2)
axis(2,cex.axis=1.5)
par(las=0)
mtext("Badge",1,cex = 2,line = 3)
mtext("p(Dove)",2,cex = 2,line = 3)


par(plt=posPlot(numploty = 1,idploty = 1,numplotx = 2,idplotx = 2)
    ,las=1,new=T)
matplot(x=rangx,y=dataIndAct_last,col = paletteMeans(100)[
  findInterval(tempPop[,Quality],colorbreaksQual)],
  lwd=2,lty = 1,xaxt="s",yaxt="n",ylim = c(0,1),
  xlab="",ylab="",type = "l")
text(x = 0.5,y=0.95,labels = bquote(gamma==.(unique(inds[,get(shortSce)])[idPAr]))
     ,cex=2)
par(las=0)
mtext("Badge",1,cex = 2,line = 3)
par(las=1)

par(new=FALSE)
color.bar.aeqp(paletteMeans(100),min =min(colorbreaksQual),
               max = max(colorbreaksQual),nticks = 3,
               title = "",
               cex.tit = 1,
               numplotx = 15,numploty = 10,idplotx =14,idploty = 1)
title("quality   ", line = 1)

dev.off()


png(here("Simulations",paste0(scenario,"_"),"compStrDiff.png"),
    width = 800,height = 600)
par(plt=posPlot())
plot(diffActWeight~Quality,data=tempPop,
     col=match(get(shortSce),sort(unique(tempPop[,unique(get(shortSce))]))),cex=1.5,pch=20,
     cex.lab=2,ylab="Responsivess",cex.axis=1.5)
legend("topright",legend = sort(tempPop[,unique(get(shortSce))]),pch=20,
       col=1:4,cex = 1.5)
dev.off()


png(here("Simulations",paste0(scenario,"_"),"compStrpDove.png"),
    width = 800,height = 600)

lastInt<-tail(inds[,unique(nInteract)],3)
par(plt=posPlot())
plot(logist(WeightAct_3,alpha = 0,beta = 1)~Quality,
     data=inds[time==gener&nInteract==lastInt[1]],
     xlab="Quality",ylab = "p(Dove)",xaxt="s",
     col=match(get(shortSce),sort(unique(tempPop[,unique(get(shortSce))]))),
     cex=1.5,pch=20,cex.lab=2,cex.axis=1.5)
legend("topright",legend = sort(tempPop[,unique(get(shortSce))]),pch=20,
       col=1:4,cex = 1.5,title = expression(beta))

dev.off()

par(plt=posPlot())
hist(inds[time==gener&get(shortSce)==unique(get(shortSce))[1],Badge],xaxt="s")
hist(inds[time==gener&get(shortSce)==unique(get(shortSce))[3],Badge],xaxt="s")

