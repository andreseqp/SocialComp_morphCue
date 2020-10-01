# Learning dynamics for a sample of the population along the
# evolutionary dynamics 

# Required libraries -----------------------------------------------------------

library(here)
here()
source(here("AccFunc.R"))



# Scenario to be plotted - corresponds to folders where simulations are stored

scenario<-"nIntGroupEvol2"
extSimsDir<-paste0("e:/BadgeSims/",scenario,"_")



# Load files -------------------------------------------------------------------

# (listTest<-list.files(here("Simulations",paste0(scenario,"_"))))
(listTest<-list.files(extSimsDir,full.names = TRUE))
(indList<-grep("ind",listTest,value=TRUE))

# parameter values from project folder
paramName<-list.files(here("Simulations",paste0(scenario,"_")))
paramName<-grep(".json",paramName,value=TRUE)
param<-fromJSON(here("Simulations",paste0(scenario,"_"),paramName))


fileId<-1


# indLearn<-fread(here("Simulations",paste0(scenario,"_"),indList[fileId]))
indLearn<-fread(indList[fileId])

# names(indLearn)[29:35]<-c("Quality", "genotype","alpha","beta","Badge","initCrit","initAct")
# names(indLearn)[9:28]<-do.call(rbind,as.list(sapply(0:9,FUN = function(x){
#   return(rbind(paste0("WeightAct_",x),paste0("WeightCrit_",x)))
# })))

Valpar<-gsub("[[:alpha:]]",gsub(".txt","",tail(strsplit(indList[fileId],"_")[[1]],1)),
             replacement = "")
nampar<-gsub("[^[:alpha:]]",gsub(".txt","",tail(strsplit(indList[fileId],"_")[[1]],1)),
             replacement = "")

# new columns ------------------------------------------------------------------


# Changes in learning parameters for several individuals -----------------------

gener<-tail(indLearn[,unique(time)],2)[2]
nCenters<-param$nCenters
sigSquar<-param$sigSq
interv<-1/(nCenters-1)
centers<-interv*seq(0,nCenters-1)
# nCenters<-5
# interv<-1/nCenters
# centers<-interv*0.5+interv*seq(0,nCenters-1)
rangx<-seq(0,1,length=100)
colorbreaksQual<-seq(0,1,length=100)

# Actor 

finReps<-indLearn[time==max(time),unique(seed)]
seedCh<-3
  finReps[round(runif(1,0,length(finReps)))+1]


# Select run and generation to plot
tempPop<-indLearn[time==gener&seed==seedCh]

timePoints<-round(seq(1,length(unique(tempPop[,nInteract]))-1,
                      length.out = 10))

# png(here("Simulations",paste0(scenario,"_"),paste0(nampar,Valpar,"learnDyn.png")),
#     width = 1000,height = 600)

yaxRang<-c("s",rep("n",4))
xaxRang<-c("s","n")
xAxLabs<-rep("",5)
xAxLabs[3]<-"Badge size"
yAxLabs<-c("p(dove)","")
countx<-0
county<-2

par(mfrow=c(1,1))
plot.new()

# for(behavTime in unique(tempPop$nInteract)[1:5]){
  for(behavTime in unique(tempPop$nInteract)[timePoints]){
  if(countx==5)  {countx<-0;county<-county-1}
  countx<-countx+1
  dataIndsAct<-sapply(as.list(tempPop[nInteract==behavTime,indId]),
                       function(x){x=
                         logist(totRBF(rangx,
                                centers,sigSquar,
                                as.double(
                                  tempPop[(nInteract==behavTime&
                                             Quality<0.5)&indId==x,.SD,
                                           .SDcol=grep("WeightAct",
                                                       names(tempPop),
                                                       value = TRUE)
                                           ])),alpha = 0,beta = 1)})
  par(plt=posPlot(numploty = 2,numplotx = 5,idploty = county,idplotx = countx),
      las=1,new=T)
  matplot(x=rangx,y=dataIndsAct,type='l',xlab="",ylab="",
          xaxt=xaxRang[county],yaxt=yaxRang[countx],lty = 1,
          col=paletteMeans(100)[
            findInterval(tempPop[nInteract==behavTime,Quality],colorbreaksQual)],
          # Use this for colour if want to plot each individual 
          # with a different colour
          # 1+match(indLearn[(time==gener&nInteract==behavTime)
          #                  &seed==seedCh,indId],
          #         indLearn[(time==gener)&(seed==seedCh&nInteract==behavTime),
          #                  unique(indId)])
            lwd=0.5,ylim=c(0,1.1),cex.axis=1.3)
  lines(logist(totRBF(rangx,centers,sigSquar,rep(0,nCenters))
              ,alpha = 0,beta = 1)~rangx,
       lwd=1,col=1)
  text(x = 0.5,y=1.06,labels = paste0("t =",behavTime),cex=2)
  if(county==1) mtext(xAxLabs[countx],1,line = 3,cex=2)
  if(countx==1) mtext(yAxLabs[county],2,line = 3,cex=2)
}

# Include if the color scheme relates to quality
par(new=FALSE)
color.bar.aeqp(paletteMeans(100),min =min(colorbreaksQual),
               max = max(colorbreaksQual),nticks = 3,
               title = "Quality",
               cex.tit = 1.1,
               numplotx = 15,numploty = 15,idplotx =2,idploty = 9)

# dev.off()

## Plot learning dynamics of the actor for a single random individual ----------

timePoints<-round(seq(1,13,
                      length.out = 10))
round(seq(1,length(unique(tempPop[,nInteract])),
          length.out = 10))
(randomInd<-tempPop[Quality<0.5,unique(indId)][round(runif(1,0,length(finReps)))+1])

yaxRang<-c("s",rep("n",4))
xaxRang<-c("s","n")
countx<-0
county<-2

plot.new()

# for(behavTime in unique(tempPop$nInteract)[1:5]){
for(behavTime in unique(tempPop$nInteract)[timePoints]){
  if(countx==5)  {countx<-0;county<-county-1}
  countx<-countx+1
  tempActWeight<-as.double(
    tempPop[nInteract==behavTime&indId==randomInd,.SD,
            .SDcol=grep("WeightAct",
                        names(tempPop),
                        value = TRUE)])
  tempDataInd<-data.frame(logist(tempActWeight,alpha = 0,beta = 1),centers)
  names(tempDataInd)<-c("pDove","centers")
  dataIndsAct<-logist(totRBF(rangx,centers,sigSquar,as.double(
                                        tempPop[nInteract==behavTime&indId==randomInd,.SD,
                                                .SDcol=grep("WeightAct",
                                                            names(tempPop),
                                                            value = TRUE)
                                                ])),alpha = 0,beta = 1)
  par(plt=posPlot(numploty = 2,numplotx = 5,idploty = county,idplotx = countx),
      las=1,new=T)
  plot(x=rangx,y=dataIndsAct,type='l',xlab="",ylab="",
          xaxt=xaxRang[county],yaxt=yaxRang[countx],lty = 1,
          col=paletteMeans(100)[
            findInterval(tempPop[nInteract==behavTime&indId==randomInd
                                   ,Quality],colorbreaksQual)],
          # Use this for colour if want to plot each individual 
          # with a different colour
          # 1+match(indLearn[(time==gener&nInteract==behavTime)
          #                  &seed==seedCh,indId],
          #         indLearn[(time==gener)&(seed==seedCh&nInteract==behavTime),
          #                  unique(indId)])
          lwd=2,ylim=c(-0.1,1.1))
  points(x=centers,y=tempDataInd$pDove,pch=20,col="black")
  if(sum(is.na(tempDataInd$pDove))==0){
  linMod1<-lm(data = tempDataInd,pDove~centers)
  linMod2<-lm(data = tempDataInd,pDove~centers+I(centers^2))
  lines(x=rangx,y=sapply(rangx, FUN=function(x){
    linMod2$coefficients[1]+linMod2$coefficients[2]*x+linMod2$coefficients[3]*x^2}),
    col="blue")
  abline(linMod1,col="red")
  text(x = c(0.3,0.6),y=rep(1,2),labels = c(round(linMod1$coefficients[2],2),
                              round(linMod2$coefficients[3],2)))
  }
  lines(logist(totRBF(rangx,centers,sigSquar,rep(0,nCenters))
               ,alpha = 0,beta = 1)~rangx,
        lwd=1,col=1)
  text(x = 0.5,y=0,labels = paste0("nInt=",behavTime))
}


## Learning last generation for quality intervals

# Changes in learning parameters for several individuals -----------------------

gener<-tail(indLearn[,unique(time)],2)[2]
nCenters<-param$nCenters
sigSquar<-param$sigSq
interv<-1/(nCenters-1)
centers<-interv*seq(0,nCenters-1)
# nCenters<-5
# interv<-1/nCenters
# centers<-interv*0.5+interv*seq(0,nCenters-1)
rangx<-seq(0,1,length=500)
colorbreaksQual<-seq(0,1,length=100)

# Actor 

finReps<-indLearn[time==max(time),unique(seed)]
seedCh<-5
finReps[round(runif(1,0,length(finReps)))+1]


# Select run and generation to plot
tempPop<-indLearn[time==gener&seed==seedCh]

behavTime<- tempPop[,tail(unique(nInteract),2)[1],by=indId][,min(V1)]

nIntPlot<-3

qualInt<-seq(0,nIntPlot)/nIntPlot

# png(here("Simulations",paste0(scenario,"_"),paste0(nampar,Valpar,"learnDyn.png")),
#     width = 1000,height = 600)

yaxRang<-c("s",rep("n",4))
xaxRang<-c("s","n")
xAxLabs<-rep("",5)
xAxLabs[3]<-"Badge size"
yAxLabs<-c("p(dove)","")


par(mfrow=c(1,nIntPlot))


for(countInts in 1:nIntPlot){
    
dataIndsAct<-sapply(as.list(tempPop[nInteract==behavTime&
                                      (Quality>qualInt[countInts]&
                                         Quality<qualInt[countInts+1]),
                                    indId]),
                      function(x){x=
                        logist(totRBF(rangx,
                                      centers,sigSquar,
                                      as.double(
                                        tempPop[(nInteract==behavTime)
                                                &indId==x,.SD,
                                                .SDcol=grep("WeightAct",
                                                            names(tempPop),
                                                            value = TRUE)
                                                ])),alpha = 0,beta = 1)})
  # par(plt=posPlot(numploty = 2,numplotx = 5,idploty = county,idplotx = countx),
  #     las=1,new=T)
  matplot(x=rangx,y=dataIndsAct,type='l',xlab="",ylab="",
          xaxt="s",yaxt="s",lty = 1,
          col=paletteMeans(100)[
            findInterval(tempPop[nInteract==behavTime&
                                   (Quality>qualInt[countInts]&
                                      Quality<qualInt[countInts+1]),
                                 Quality],colorbreaksQual)],
          # Use this for colour if want to plot each individual 
          # with a different colour
          # 1+match(indLearn[(time==gener&nInteract==behavTime)
          #                  &seed==seedCh,indId],
          #         indLearn[(time==gener)&(seed==seedCh&nInteract==behavTime),
          #                  unique(indId)])
          lwd=0.5,ylim=c(0,1.1),cex.axis=1.3)
  lines(logist(totRBF(rangx,centers,sigSquar,rep(0,nCenters))
               ,alpha = 0,beta = 1)~rangx,
        lwd=1,col=1)
  # text(x = 0.5,y=1.06,labels = paste0("t =",behavTime),cex=2)
  # if(county==1) mtext(xAxLabs[countx],1,line = 3,cex=2)
  # if(countx==1) mtext(yAxLabs[county],2,line = 3,cex=2)
}

# Include if the color scheme relates to quality
par(new=FALSE)
color.bar.aeqp(paletteMeans(100),min =min(colorbreaksQual),
               max = max(colorbreaksQual),nticks = 3,
               title = "Quality",
               cex.tit = 1.1,
               numplotx = 15,numploty = 15,idplotx =2,idploty = 9)

##

par(mfrow=c(1,1))
plot(x=Quality,y=Badge,pch=20,data=tempop[nInteract==behavTime])


## plots for the change in interaction frequency -------------------------------


par(plt=posPlot(numploty = 2,idploty = 2))

with(tempPop,{
  plot(x=ntotInteract,y=freqHH,pch=20,
          col = match(nInteract,unique(nInteract)))
  lines(x=c(0,1000000),y=c(0.666,0.666)^2,col="grey")
  legend("topleft",legend = unique(nInteract),col=1:5,pch=20,ncol = 15)
})

par(plt=posPlot(numploty = 2,idploty = 1),new=TRUE)

with(indLearn[time==20000&seed==seedCh],{
  plot(x=ntotInteract,y=freqHH,pch=20,
       col = match(nInteract,unique(nInteract)))
  lines(x=c(0,1000000),y=c(0.666,0.666)^2,col="grey")
  legend("topleft",legend = unique(nInteract),col=1:5,pch=20,ncol=15)
})



# Critic 



yaxRang<-c("s",rep("n",4))
xaxRang<-c("s","n")
seedCh<-0
countx<-0
county<-2
plot.new()

ylims<-fivenum(as.matrix(indLearn[,.SD,
                .SDcol=grep("WeightCrit",names(indLearn),
                                     value = TRUE)]))[c(1,5)]
for(behavTime in unique(tempPop$nInteract)){
  if(countx==5)  {countx<-0;county<-county-1}
  countx<-countx+1
  par(plt=posPlot(numploty = 2,numplotx = 5,idploty = county,idplotx = countx),
      las=1,new=T)
  dataIndsCrit<-sapply(as.list(tempPop[nInteract==behavTime,indId]),
                       function(x){x=
                         totRBF(rangx,
                                centers,0.01,
                                as.double(
                                  tempPop[nInteract==behavTime&indId==x,.SD,
                                       .SDcol=grep("WeightCrit",
                                                   names(indLearn),
                                                   value = TRUE)
                                       ]))})
  matplot(x=rangx,y=dataIndsCrit,type='l',xlab="",ylab="",
          xaxt=xaxRang[county],yaxt=yaxRang[countx],lty = 1,
          col=paletteMeans(100)[
            findInterval(tempPop[nInteract==behavTime,Quality],colorbreaksQual)],
          # To color each individual differently
          # col=1+match(indLearn[(time==gener&nInteract==behavTime)
          #                      &seed==seedCh,indId],
          #             indLearn[(time==gener)&(seed==seedCh&nInteract==behavTime),
                                     # unique(indId)]),
  lwd=0.5,ylim = ylims)
  lines(totRBF(rangx,centers,0.01,rep(0,nCenters))~rangx,lwd=1,col=1)
  text(x = 0.5,y=0.38,labels = paste0("nInt=",behavTime))
}
rm(dataIndsCrit)


png(here("Simulations",paste0(scenario,"_"),paste0("learnDyn",gener,".png")),
    height = 800,width = 800)

gener
indLearn[(time==gener),unique(indId)]
learnDyn<-dcast(indLearn[(genotype==2&time==gener)],nInteract~indId,
      value.var=c("WeightAct_3","WeightCrit_3"),fun=function(x){return(x[1])})
cols<-paletteMeans(100)[
  findInterval(indLearn[(genotype==2&time==gener)][
  match(as.numeric(tstrsplit(grep("WeightAct",names(learnDyn),value = T),"_")[[4]]),
        indLearn[(genotype==2&time==gener),indId]),Quality],colorbreaksQual)]

par(plt=posPlot(numploty = 2,idploty = 2))
matplot(x=learnDyn[,nInteract],
        y=learnDyn[,.SD,.SDcols=grep("WeightAct",names(learnDyn),value = T)],
        xlab="",ylab="Weights Actor",xaxt="n",type="l",col = cols,lwd=2,lty=1)
# legend("topright",legend = grep("WeightCrit",names(indLearn),value = T),
#        col = 1:5,lty=1)
title(main = bquote(t==.(gener)),line=-2)
par(plt=posPlot(numploty = 2,idploty = 1),new=TRUE)
matplot(x=learnDyn[,nInteract],
        y=learnDyn[,.SD,.SDcols=grep("WeightCrit",names(learnDyn),value = T)],
        xlab="nInter",ylab="Weights Critic",xaxt="s",type="l",col = cols,lwd=2,
        lty=1)
par(new=FALSE)
color.bar.aeqp(paletteMeans(100),min =min(colorbreaksQual),
               max = max(colorbreaksQual),nticks = 3,
               title = "",
               cex.tit = 1,
               numplotx = 15,numploty = 10,idplotx =15,idploty = 4)
title("quality   ", line = 1)
dev.off()

# Scatterplot relating quality and central weight of the actor -----------------

par(plt=posPlot())
plot(pop[time==min(time),WeightAct_3]~pop[time==min(time),Quality],
     xlab="Quality",ylab = "Central actor weight")

hist(indLearn[,nInteract],xaxt="s")
hist(indLearn[,Badge],xaxt="s")
hist(indLearn[,Quality],xaxt="s")


# Dynamic changes in the responsiveness of the actor  --------------------------

plot.new()

par(plt=posPlot())
plot(diffActWeights~nInteract,data=indLearn[seed==0],col = indId,pch=20,cex=0.5)


diffWeight<-dcast(indLearn[time==0],nInteract~indId+seed,
                 value.var = c("diffActWeights"))


par(plt=posPlot())
matplot(y=diffWeight[,2:dim(diffWeight)[2]],x=diffWeight[,1],
        col = paletteMeans(100)[
  findInterval(indLearn[seed==1&time==0,Quality],colorbreaksQual)],
  type="p",cex=0.8,lty=1,pch=20)
par(new=FALSE)
color.bar.aeqp(paletteMeans(100),min =min(colorbreaksQual),
               max = max(colorbreaksQual),nticks = 3,
               title = "",
               cex.tit = 1,
               numplotx = 15,numploty = 10,idplotx =2,idploty = 9)
title("quality   ", line = 1)
