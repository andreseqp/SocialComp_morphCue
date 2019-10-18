# Varying parameter of a single scenario ---------------------------------------

# Required libraries -----------------------------------------------------------

library(here)
here()
source(here("AccFunc.R"))

# Scenario to be plotted - corresponds to folders where simulations are stored

scenario<-"QualStDvHonest"

# Load files -------------------------------------------------------------------

(listTest<-list.files(here("Simulations",paste0(scenario,"_"))))
(sdList<-grep("evol",listTest,value=TRUE))
(indList<-grep("ind",listTest,value=TRUE))

inds<-do.call(rbind,lapply(indList,filesScenar,scenario))

inds[,diffActWeight:=abs(WeightAct_0-WeightAct_4)]

shortSce<-gsub("[^[:alpha:]]",gsub(".txt","",tail(strsplit(indList[1],"_")[[1]],1)),
               replacement = "")
  

# Graphs -----------------------------------------------------------------------

inds[,unique(get(shortSce))]

inds[get(shortSce)==unique(get(shortSce))[4]]

gener<-tail(inds[,unique(time)],1)
lastInt<-tail(inds[,unique(nInteract)],3)
runChoi<-0
nCenters<-6
interv<-1/(nCenters-1)
# centers when extremes are included
centers<-interv*seq(0,nCenters-1) 
# centers when extremes are not included
# centers<-interv*0.5+interv*seq(0,nCenters-1) 
yaxtRang<-c("s",rep("n",10))
yaxlabs<-c("p(D)",rep("",10))
tempPop<-inds[time==gener&nInteract==lastInt[1],.SD[.N],
             .SDcol=c(grep("Weight",
                           names(inds),value = TRUE),"Quality",
                      as.character(shortSce),"diffActWeight"),
             by=indId]
tempPop[,unique(get(shortSce))]
plot.new()
for(PAr in tempPop[,unique(get(shortSce))]){
  allWeightsAct.tmp<-tempPop[get(shortSce)==PAr,.SD,
                             .SDcol=grep("WeightAct",
                                         names(tempPop),
                                         value = TRUE)]
  matActors.tmp<-apply(allWeightsAct.tmp,
                        MARGIN=1,FUN = Actor,centers=centers)
  par(plt=posPlot(numploty = 1,idploty = 1,
                  numplotx = length(tempPop[,unique(get(shortSce))]),
                  idplotx = match(PAr,tempPop[,unique(get(shortSce))])),
      las=1,new=TRUE,yaxt=yaxtRang[match(PAr,tempPop[,unique(get(shortSce))])],
      cex.axis=1.5)
  matplot(x=seq(0,1,length.out = 1000),y=matActors.tmp,col = paletteMeans(100)[
    findInterval(tempPop[get(shortSce)==PAr,Quality],colorbreaksQual)],
    lwd=2,lty = 1,xaxt="s",ylim = c(0,1),
    xlab="",ylab="",type = "l")
  text(x = 0.5,y=0.95,labels = bquote(.(shortSce)==
                                        .(PAr)),cex=1)
  par(las=0)
  mtext("Badge",1,cex = 2,line = 3)
  mtext(yaxlabs[match(PAr,tempPop[,unique(get(shortSce))])],2,cex = 2,line = 3)
}
par(new=FALSE)
color.bar.aeqp(paletteMeans(100),min =min(colorbreaksQual),
               max = max(colorbreaksQual),nticks = 3,
               title = "",
               cex.tit = 1,
               numplotx = 15,numploty = 10,idplotx =14,idploty = 1)
title("quality   ", line = 1)

# dev.off()

yaxlabs<-c("Value",rep("",10))
plot.new()
for(PAr in tempPop[,unique(get(shortSce))]){
  allWeightsCrit.tmp<-tempPop[get(shortSce)==PAr,.SD,
                             .SDcol=grep("WeightCrit",
                                         names(tempPop),
                                         value = TRUE)]
  matCritics.tmp<-apply(allWeightsCrit.tmp,
                       MARGIN=1,FUN = Critic,centers=centers)
  par(plt=posPlot(numploty = 1,idploty = 1,
                  numplotx = length(tempPop[,unique(get(shortSce))]),
                  idplotx = match(PAr,tempPop[,unique(get(shortSce))])),
      las=1,new=TRUE,yaxt=yaxtRang[match(PAr,tempPop[,unique(get(shortSce))])],
      cex.axis=1.5)
  matplot(x=seq(0,1,length.out = 1000),y=matCritics.tmp,col = paletteMeans(100)[
    findInterval(tempPop[get(shortSce)==PAr,Quality],colorbreaksQual)],
    lwd=2,lty = 1,xaxt="s",ylim = as.double(tempPop[,.(min(.SD),max(.SD)),
                                          .SDcol=grep("WeightCrit",
                                                      names(tempPop),value = TRUE)])
    +c(0,0.1),
    xlab="",ylab="",type = "l")
  text(x = 0.5,y=0.4,labels = bquote(.(shortSce)==
                                        .(PAr)),cex=1)
  par(las=0)
  mtext("Badge",1,cex = 2,line = 3)
  mtext(yaxlabs[match(PAr,tempPop[,unique(get(shortSce))])],2,cex = 2,line = 3)
}
par(new=FALSE)
color.bar.aeqp(paletteMeans(100),min =min(colorbreaksQual),
               max = max(colorbreaksQual),nticks = 3,
               title = "",
               cex.tit = 1,
               numplotx = 15,numploty = 10,idplotx =14,idploty = 1)
title("quality   ", line = 1)
tempPop[WeightCrit_3>0.4]
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

