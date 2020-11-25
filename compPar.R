# Varying parameter of a single scenario ---------------------------------------

# Required libraries -----------------------------------------------------------

library(here)
here()
source(here("AccFunc.R"))

# Scenario to be plotted - corresponds to folders where simulations are stored

scenario<-"initAct"

extSimsDir<-#here("Simulations",paste0(scenario,"_"))
  paste0("e:/BadgeSims/",scenario,"_")

# Load files -------------------------------------------------------------------

(listTest<-list.files(here("Simulations",paste0(scenario,"_"))))

(listTest<-list.files(extSimsDir,full.names = TRUE))

(sdList<-grep("evol",listTest,value=TRUE))
(indList<-grep("ind",listTest,value=TRUE))

fread(indList[2])

inds<-rbindlist(lapply(indList,filesScenar,scenario,full.name=TRUE),fill = TRUE)

inds[,diffActWeight:=abs(WeightAct_0-WeightAct_4)]

shortSce<-gsub("[^[:alpha:]]",gsub(".txt","",tail(strsplit(indList[1],"_")[[1]],1)),
               replacement = "")
  

# Graphs -----------------------------------------------------------------------

inds[,unique(get(shortSce))]

inds[get(shortSce)==unique(get(shortSce))[3]]

gener<-tail(inds[,unique(time)],1)
lastInt<-tail(inds[,unique(nInteract)],1)
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
                      as.character(shortSce),"diffActWeight",
                      "alpha","beta"),
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

# Reaction norms ---------------------------------------------------------------

yaxlabs<-c("Value",rep("",10))
plot.new()
for(PAr in tempPop[,unique(get(shortSce))]){
  dataIndReact<-sapply(as.list(tempPop[get(shortSce)==PAr,indId]),
                       function(x){x=
                         sapply(rangx, function(y)
                           do.call(logist,
                                   as.list(
                                     c(y,
                                       as.double(tempPop[indId==x,.SD,
                                                         .SDcol=c("alpha","beta")
                                                     ])))))})
  par(plt=posPlot(numploty = 1,idploty = 1,
                  numplotx = length(tempPop[,unique(get(shortSce))]),
                  idplotx = match(PAr,tempPop[,unique(get(shortSce))])),
      las=1,new=TRUE,yaxt=yaxtRang[match(PAr,tempPop[,unique(get(shortSce))])],
      cex.axis=1.5,new=TRUE)
  matplot(x=rangx,y=dataIndReact,col = paletteMeans(100)[
    findInterval(tempPop[,Quality],colorbreaksQual)],lwd=2,lty = 1,xaxt="s")
  
  
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

## Individual variation for different parameters ------------------------------

lastInt<-tail(inds[,unique(nInteract)],2)[1]

popOneInd<-inds[nInteract==lastInt]

png(here("Simulations",paste0(scenario,"_"),
         paste0("indVarScatter","_1",".png")),
    width = 1400,height = 1200)

yaxtAll<-c("s","n","n")
xlabAll<-c("",expression(beta[s]),"")
ylabAll<-c(expression(alpha[s]),"","")
titleAll<-c("Peaceful","Clever","Aggressive")
plot.new()
for(PAr in popOneInd[,sort(unique(get(shortSce)),decreasing = TRUE)]){
  count<-match(PAr,popOneInd[,sort(unique(get(shortSce)),decreasing = TRUE)])
  par(plt=posPlot(numploty = 2,idploty = 2,
                  numplotx = length(popOneInd[,unique(get(shortSce))]),
                  idplotx = count)+c(0,0,-0.05,-0.05),
      las=1,new=TRUE,
      yaxt=yaxtAll[match(PAr,popOneInd[,sort(unique(get(shortSce)),decreasing = TRUE)])],
      cex.axis=1.5,new=TRUE)
  plot(data=popOneInd[(get(shortSce)==PAr&time>max(time)*0.8)&seed %in% 12:15],alpha~beta,ylab="",
       xlab="", pch=20,cex.lab=3,cex.axis=2,las=1,cex=1,xaxt="s",
       ylim=range(popOneInd[,alpha])*c(1.1,1.1),
       xlim=range(popOneInd[,beta]),col=colReps[seed+1])
  lines(x=c(0,0),y=range(popOneInd[,alpha]),col="grey",
        lwd=2)
  lines(y=c(0,0),x=range(popOneInd[,beta]),col="grey",
        lwd=2)
  mtext(text = ylabAll[count],side = 2,line = 3.5,las=1,cex=3.5)
  mtext(text = titleAll[count],side = 3,cex = 3,line = 2)
  # par(plt=posPlot(numploty = 2,idploty = 1,
  #                 numplotx = length(popOneInd[,unique(get(shortSce))]),
  #                 idplotx = count)+c(0,0,-0.05,-0.05),
  #     las=1,new=TRUE,yaxt=yaxtAll[count],
  #     cex.axis=1.5,new=TRUE)
  # plot(data=popOneInd[get(shortSce)==PAr&time>max(time)*0.8],alpha~beta,ylab="",
  #      xlab="", pch=20,cex.lab=3,cex.axis=2,las=1,cex=1,
  #      ylim=range(popOneInd[,alpha])*c(1.1,1.1),
  #      xlim=range(popOneInd[,beta]),col=colReps[seed+1])
  # lines(x=c(0,0),y=range(popOneInd[,alpha]),col="grey",
  #       lwd=2)
  # lines(y=c(0,0),x=range(popOneInd[,beta]),col="grey",
  #       lwd=2)
  mtext(text = xlabAll[count],side = 1,line = 3.7,cex = 3.5)
}
dev.off()






