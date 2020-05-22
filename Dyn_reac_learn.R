# Evolutionary dynamics for individual runs, reaction norms of sampled 
# individuals and their learning profiles

# Required libraries -----------------------------------------------------------

library(here)
here()
source(here("AccFunc.R"))
library("foreach")
library("doParallel")

# Scenario to be plotted - corresponds to folders where simulations are stored

scenario<-"initAct"
extSimsDir<-paste0("e:/BadgeSims/",scenario,"_")


# Load files -------------------------------------------------------------------
# Project folder
# (listTest<-list.files(here("Simulations",paste0(scenario,"_"))))
# External sims folder
(listTest<-list.files(extSimsDir,full.names = TRUE))
(evolList<-grep("evolLearn",listTest,value=TRUE))
(indList<-grep("indLearn",listTest,value=TRUE))

numCores <- 3
registerDoParallel(numCores)

# val<-1

# loop to produce pdfs for parameter values
foreach(val = 1:3,.packages = c("data.table","here")) %dopar% {
source(here("AccFunc.R"))
fileId<-val
# Project folder
# evol<-fread(here("Simulations",paste0(scenario,"_"),evolList[fileId]))
# pop<-fread(here("Simulations",paste0(scenario,"_"),indList[fileId]))
# External sims folder
evol<-fread(evolList[fileId])
pop<-fread(indList[fileId])

Valpar<-gsub("[[:alpha:]]",gsub(".txt","",tail(strsplit(indList[val],"_")[[1]],1)),
     replacement = "")
nampar<-gsub("[^[:alpha:]]",gsub(".txt","",tail(strsplit(indList[val],"_")[[1]],1)),
             replacement = "")



# Get stats from the evolutionary simulations ----------------------------------

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
                   lowIQR.freqHH = fivenum(freqHH)[2],
                   lowIQR.freqHD = fivenum(freqHD)[2],
                   lowIQR.freqDD = fivenum(freqDD)[2],
                   m.weightAct_0=mean(WeightAct_0),
                   m.weightAct_1=mean(WeightAct_1),
                   m.weightAct_2=mean(WeightAct_2),
                   m.weightAct_3=mean(WeightAct_3),
                   m.weightAct_4=mean(WeightAct_4),
                   m.weightAct_5=mean(WeightAct_5),
                   m.weightCrit_0=mean(WeightCrit_0),
                   m.weightCrit_1=mean(WeightCrit_1),
                   m.weightCrit_2=mean(WeightCrit_2),
                   m.weightCrit_3=mean(WeightCrit_3),
                   m.weightCrit_4=mean(WeightCrit_4),
                   m.weightCrit_5=mean(WeightCrit_5)),by=time]


# Plot mean and IQRs among replicates of the genotypes and phenotypes ----------------------------

pdf(paste0(extSimsDir,"/evolDyn_",nampar,Valpar,".pdf"))

# Dynamics of genetypic traits (reaction norm)
par(plt=posPlot(numploty = 3,idploty = 2,numplotx = 2,idplotx = 1,
                lowboundx = 8,upboundx = 93),xaxt="s",las=1)
plot(x=c(0,max(evolStats$time)),y=c(0,0),type="l",lwd=2,col="grey",
     ylim=fivenum(as.matrix(evol[,.(meanAlpha,meanBeta)]))[c(1,5)],
     xlab="",ylab="Trait value",cex.lab=1.2,cex.axis=1,xaxt='n',las=1)
# Variation among replicates
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
axis(side=1,padj = -3.5,cex=0.8,at=axTicks(1),labels = axTicks(1)/100)
# Dynamics of behavioural interactions 

par(plt=posPlot(numploty = 3,idploty = 2,numplotx = 2, idplotx = 2,
                lowboundx = 8,upboundx = 93),
    xaxt="s",las=1,new=TRUE)
plot(x=c(0,max(evolStats$time)),y=c(0,0),type="l",lwd=2,col=0,
     ylim=c(0,0.6),xlab="",ylab="",yaxt="n",
     cex.lab=1.5,cex.axis=1,xaxt='n',las=1)
axis(side=1,padj = -3.5,cex=0.8,at=axTicks(1),labels = axTicks(1)/100)
axis(4,cex=0.8,padj = -0.5)

# Variation among replicates
polygon(x=c(evolStats$time,rev(evolStats$time)),
        y=c(evolStats$upIQR.freqHH,rev(evolStats$lowIQR.freqHH)),
        col=colIntTypesPol[1],border = NA)
polygon(x=c(evolStats$time,rev(evolStats$time)),
        y=c(evolStats$upIQR.freqDD,rev(evolStats$lowIQR.freqDD)),
        col=colIntTypesPol[2],border = NA)
with(evolStats,{
  lines(time,m.freqHH,col=colIntTypesLin[1],lwd=3)
  lines(time,m.freqDD,col=colIntTypesLin[2],lwd=3)
  lines(time,m.freqHD,col=colIntTypesLin[3],lwd=3)
  lines(x=c(0,max(time)),y=c(0.6666,0.6666)^2,col=colIntTypesLin[1],lwd=2,lty=2)
  lines(x=c(0,max(time)),y=c(0.3333,0.3333)^2,col=colIntTypesLin[2],lwd=2,lty=2)
  lines(x=c(0,max(time)),y=c(0.6666*0.33333,0.6666*0.33333)*2+0.01,
        col=colIntTypesLin[3],lwd=2,lty=2)
})

legend("topleft",legend = c("HH","DD","HD"),ncol = 3,
       col=colIntTypesLin,lwd=2,bty = "n",cex=0.8)

# Dynamics of behavioural types
# par(plt=posPlot(numploty = 3,idploty = 3),xaxt="s",las=1,new=TRUE)
# plot(x=c(0,max(evolStats$time)),y=c(0,0),type="l",lwd=2,col=0,
#      ylim=c(0,1),xlab="",ylab="behaviour \n frequency",
#      cex.lab=1.5,cex.axis=1,xaxt='n',las=1)
# 
# # Variation among replicates
# polygon(x=c(evolStats$time,rev(evolStats$time)),
#         y=c(evolStats$upIQR.freqFenHawk,rev(evolStats$lowIQR.freqFenHawk)),
#         col=colIntTypesPol[1],border = NA)
# polygon(x=c(evolStats$time,rev(evolStats$time)),
#         y=c(evolStats$upIQR.freqFenDove,rev(evolStats$lowIQR.freqFenDove)),
#         col=colIntTypesPol[2],border = NA)
# with(evolStats,{
#   lines(time,m.freqFenHawk,col=colIntTypesLin[1],lwd=3)
#   lines(time,m.freqFenDove,col=colIntTypesLin[2],lwd=3)
#   lines(x=c(0,max(time)),y=c(0.6666,0.6666),col=colIntTypesLin[1],lwd=2,lty=2)
#   lines(x=c(0,max(time)),y=c(0.3333,0.3333),col=colIntTypesLin[2],lwd=2,lty=2)
# })
# 
# legend("topleft",legend = c("Hawk","Dove"),
#        col=colIntTypesLin,lwd=2,bty = "n",cex=0.8)


gen2plot<-round(seq(1,length(unique(evolStats$time)),length.out = 5))[2:5]
# Plor the actor
seqYax<-c("s",rep("n",3))
# seqYlabUp<-c("Badge",rep("",3))
seqYlabUp<-c("P(dove)",rep("",3))
seqYlabDown<-c("Badge",rep("",3))
seqXlabDown<-c("","Quality","")
rangQual<-seq(0,1,length.out = 100)
nCenters<-6
interv<-1/(nCenters-1)
centers<-interv*seq(0,nCenters-1)
rangx<-seq(0,1,length=1000)
count<-0
genstoPrint<-round(seq(1,length(unique(evolStats$time)),length.out = 5))[2:5]
# genstoPrint<-round(seq(1,length(unique(evolStats$time))/2,length.out = 5))[2:5]
for(genC in genstoPrint){
  count<-count+1
  par(plt=posPlot(numplotx = 4,numploty = 3,idplotx = count,idploty = 3,
                  lowboundx = 8, upboundx = 93), xaxt="s",las=1,new=TRUE)
  tmpActweights<-evol[(time==unique(time)[genC]),.SD,
                      .SDcols=grep("WeightAct",
                                   names(evol),value = TRUE)]
  weightsAct<-sapply(seq(1,dim(tmpActweights)[1]),function(x){
      logist(totRBF(rangx,
                    centers,0.01,
                    as.double(
                      tmpActweights[x,])),alpha=0,beta = 1)
  })
  matplot(y=weightsAct,x=rangx,
       yaxt=seqYax[count],ylab=seqYlabUp[count],xlab="",type="l",
       lwd=2,xaxt="n",ylim=c(0,1),col=rgb(t(col2rgb(2:10)),
                                          maxColorValue = 255,alpha=80),lty=1)
 
  
  lines(logist(totRBF(rangx,centers,0.01,
                      as.double(evolStats[(time==unique(time)[genC]),.SD,
                                .SDcols=grep("m.weightAct",
                                             names(evolStats),value = TRUE)])),
                      alpha=0,beta=1)~rangx,
        col = "black",lwd=2)
  text(x=0.5,y=0.1,labels = paste0("time=",unique(evolStats$time)[genC]))
  par(plt=posPlot(numplotx = 4,numploty = 3,idplotx = count,idploty = 1,
                  lowboundx = 8, upboundx = 93), las=1,new=TRUE)
  tmpreacNorms<-evol[time==unique(time)[genC],
       .SD,
       .SDcols=c("meanAlpha"
                 ,"meanBeta")]
  matplot(y=sapply(1:dim(tmpreacNorms)[1],FUN=function(x){
                sapply(rangx, function(y){
                  do.call(logist,as.list(c(y,as.double(tmpreacNorms[x,]))))  
                })
              }),
       x=rangx,type='l',xlab=seqXlabDown[count],ylab=seqYlabDown[count],
       ylim=c(0,1),lwd=2,yaxt=seqYax[count],xaxt="s",
       col=rgb(t(col2rgb(2:10)),maxColorValue = 255,alpha=80),lty=1)
  lines(x=rangx,y=sapply(rangx, function(y){
    do.call(logist,as.list(c(y,as.double(evolStats[(time==unique(time)[genC]),.SD,
                                                   .SDcols=c("m.meanAlpha"
                                                             ,"m.meanBeta")]))))  
  }),col = 1,lwd=3)
}
rm(list=grep("temp",ls(),value = T))


## Plot mean and IQRs among individuals for each replicate --------------------

# png(here("Simulations",scenario,"hawkDoveLearn_0.1.png"),
# height = 800,width = 800)

# get the trajectories for individual runs
traitsTrajs<-dcast(evol,time~seed,value.var = c("meanAlpha","meanBeta",
                                                "sdAlpha","sdBeta","freqHH",
                                                "freqHD","freqDD"))
finReps<-evol[time==max(time),seed]


for(runChoi in finReps){
# runChoi<-8

# Average trajectory
par(plt=posPlot(numploty = 3,idploty = 2,numplotx = 2,idplotx = 1,lowboundx = 8, upboundx = 93),
    xaxt="s",las=1)
plot(x=c(0,max(evolStats$time)),y=c(0,0),type="l",lwd=2,col="grey",
     ylim=fivenum(as.matrix(pop[,.(alpha,beta)]))[c(1,5)],
     xlab="",ylab="Trait value",cex.lab=1.2,cex.axis=1,xaxt='n',las=1)
axis(side=1,padj = -3)
legend("topleft",legend = c(expression(alpha),expression(beta)),
       col=colGenesLin,lwd=2,bty = "n")
# Variation among individuals
polygon(x=c(traitsTrajs[,time],rev(traitsTrajs[,time])),
        y=c(traitsTrajs[,apply(.SD,FUN=sum,MARGIN=1),
                        .SDcol=c(paste0("meanAlpha_",runChoi),
                                 paste0("sdAlpha_",runChoi))],
            rev(traitsTrajs[,apply(.SD, MARGIN = 1,FUN = function(x){x[1]-x[2]}),
                            .SDcols=c(paste0("meanAlpha_",runChoi),
                                      paste0("sdAlpha_",runChoi))])),
        col=colGenesPol[1],border = NA)
polygon(x=c(traitsTrajs[,time],rev(traitsTrajs[,time])),
        y=c(traitsTrajs[,apply(.SD,FUN=sum,MARGIN=1),
                        .SDcol=c(paste0("meanBeta_",runChoi),
                                 paste0("sdBeta_",runChoi))],
            rev(traitsTrajs[,apply(.SD, MARGIN = 1,FUN = function(x){x[1]-x[2]}),
                            .SDcols=c(paste0("meanBeta_",runChoi),
                                      paste0("sdBeta_",runChoi))])),
        col=colGenesPol[2],border = NA)

matlines(x=traitsTrajs[,time],
         y=traitsTrajs[,.SD,
                       .SDcol=paste0(c("meanAlpha_","meanBeta_"),runChoi)],
         col=colGenesLin,lty = 2,type="l",lwd=3)


par(plt=posPlot(numploty = 3,idploty = 2,numplotx = 2, idplotx = 2,
                lowboundx = 8, upboundx = 93),
    xaxt="s",las=1,new=TRUE)
plot(x=c(0,max(evol$time)),y=c(0,0),type="l",lwd=2,col=0,
     ylim=c(0,0.6),xlab="",ylab="",yaxt="n",
     cex.lab=1.5,cex.axis=1,xaxt='n',las=1)
axis(side=1,padj = -3.5,cex=0.8,at=axTicks(1),labels = axTicks(1)/100)
axis(4,cex=0.8,padj = -0.5)

matlines(x=traitsTrajs[,time],
         y=traitsTrajs[,.SD,
                       .SDcol=paste0(c("freqHH_","freqDD_","freqHD_"),runChoi)],
         col=colIntTypesLin,lty = 1,type="l",lwd=3)

  lines(x=c(0,max(traitsTrajs[,time])),y=c(0.6666,0.6666)^2,col=colIntTypesLin[1],lwd=2,lty=2)
  lines(x=c(0,max(traitsTrajs[,time])),y=c(0.3333,0.3333)^2,col=colIntTypesLin[2],lwd=2,lty=2)
  lines(x=c(0,max(traitsTrajs[,time])),y=c(0.6666*0.33333,0.6666*0.33333)*2+0.01,
        col=colIntTypesLin[3],lwd=2,lty=2)
  with(evolStats,{
    lines(time,m.freqHH,col="grey",lwd=2,lty=2)
  })
  

legend("topleft",legend = c("HH","DD","HD"),ncol = 3,
       col=colIntTypesLin,lwd=2,bty = "n",cex=0.8)

# Choose which interaction to visualize
lastInt<-2000
tail(pop[seed==runChoi,unique(nInteract)],2)[1]
# to get last interaction: tail(pop[,unique(nInteract)],1)
# Choose time range
gen2plot<-round(seq(1,length(unique(evolStats$time)),length.out = 5))[2:5]
# Plor the actor
seqYax<-c("s",rep("n",3))
# seqYlabUp<-c("Badge",rep("",3))
seqYlabUp<-c("P(dove)",rep("",3))
seqYlabDown<-c("Badge",rep("",3))
seqXlabDown<-c("","Quality",paste0("seed: ",runChoi))
rangQual<-seq(0,1,length.out = 100)
nCenters<-6
interv<-1/(nCenters-1)
centers<-interv*seq(0,nCenters-1)
rangx<-seq(0,1,length=1000)
count<-0
for(genC in genstoPrint){
  count<-count+1
  par(plt=posPlot(numplotx = 4,numploty = 3,idplotx = count,idploty = 3,
                  lowboundx = 8, upboundx = 93),
      xaxt="s",las=1,new=TRUE)
  weightsAct<-as.double(evol[(time==unique(time)[genC])&seed==runChoi,.SD,
                             .SDcols=grep("WeightAct",
                                          names(evol),value = TRUE)])
    tempPop<-pop[(time==unique(time)[genC]&seed==runChoi)&nInteract==lastInt,.SD[.N],
               .SDcol=c(grep("WeightAct",
                             names(evol),value = TRUE),"Quality","alpha","beta"),
               by=indId]
  dataIndAct<-sapply(as.list(tempPop[,indId]),
                     function(x){x=
                       logist(totRBF(rangx,
                                     centers,0.01,
                                     as.double(
                                       tempPop[indId==x,.SD,
                                               .SDcol=grep("WeightAct",
                                                           names(tempPop),
                                                           value = TRUE)
                                               ])),alpha=0,beta = 1)})
  matplot(x=rangx,y=dataIndAct,
       yaxt=seqYax[count],ylab=seqYlabUp[count],xlab="",type="l",
       lwd=2,xaxt="n",ylim=c(0,1),col = paletteMeans(100)[
         findInterval(tempPop[,Quality],colorbreaksQual)],lty = 1)
  lines(logist(totRBF(rangx,centers,0.01,weightsAct),alpha=0,beta=1)~rangx,
           col=1,lwd=3,lty=2)
  text(x=0.5,y=0.1,labels = paste0("time=",unique(evolStats$time)[genC]))
  par(plt=posPlot(numplotx = 4,numploty = 3,idplotx = count,idploty = 1,
                  lowboundx = 8, upboundx = 93), las=1,new=TRUE)
  dataIndReact<-sapply(as.list(tempPop[,indId]),
                       function(x){x=
                         sapply(rangx, 
                                function(y)
                           do.call(logist,
                                   as.list(
                             c(y,as.double(
                               tempPop[indId==x,.SD,
                                                   .SDcol=c("alpha","beta")])))))})
  matplot(x=rangx,y=dataIndReact,col = paletteMeans(100)[
    findInterval(tempPop[,Quality],colorbreaksQual)],type='l',
       
       xlab=seqXlabDown[count],ylab=seqYlabDown[count],ylim=c(0,1),
       lwd=2,yaxt=seqYax[count],xaxt="s")
  lines(sapply(rangx,
               FUN=function(x){
                 do.call(logist,
                         as.list(c(x,
                                   as.double(evol[(time==unique(time)[genC])&
                                                    seed==runChoi,
                                                  .SD,
                                                  .SDcols=c("meanAlpha"
                                                            ,"meanBeta")]))))})
        ~rangx,lwd=3,col=1)
}
rm(list=grep("temp",ls(),value = T))

}

dev.off()

}

par(plt=posPlot())
with(indLearn[nInteract==0,],{
  plot(alpha~time,pch=20,col=match(seed,unique(seed)))
})

# png(here("Simulations",scenario,"hawkDoveLearn_0.1.png"),
# height = 800,width = 800)

# get the trajectories for individual runs
traitsTrajs<-dcast(evol,time~seed,value.var = c("meanAlpha","meanBeta",
                                                "sdAlpha","sdBeta"))
runChoi<-2

# Average trajectory
par(plt=posPlot(numploty = 3,idploty = 2),xaxt="s",las=1)
plot(x=c(0,max(evolStats$time)),y=c(0,0),type="l",lwd=2,col="grey",
     ylim=fivenum(as.matrix(pop[,.(alpha,beta)]))[c(1,5)],
     xlab="",ylab="Trait \n value",cex.lab=1.5,cex.axis=1,xaxt='n',las=1)
# Variation among replicates
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
# Runs' trajectory
# polygon(x=c(traitsTrajs[,time],rev(traitsTrajs[,time])),
#         y=c(traitsTrajs[,apply(.SD,FUN=sum,MARGIN=1),
#                         .SDcol=c(paste0("meanAlpha_",runChoi),
#                                  paste0("sdAlpha_",runChoi))],
#             rev(traitsTrajs[,apply(.SD, MARGIN = 1,FUN = function(x){x[1]-x[2]}),
#                             .SDcols=c(paste0("meanAlpha_",runChoi),
#                                       paste0("sdAlpha_",runChoi))])),
#         col=colGenesPol[1],border = NA)
# polygon(x=c(traitsTrajs[,time],rev(traitsTrajs[,time])),
#         y=c(traitsTrajs[,apply(.SD,FUN=sum,MARGIN=1),
#                         .SDcol=c(paste0("meanBeta_",runChoi),
#                                  paste0("sdBeta_",runChoi))],
#             rev(traitsTrajs[,apply(.SD, MARGIN = 1,FUN = function(x){x[1]-x[2]}),
#                             .SDcols=c(paste0("meanBeta_",runChoi),
#                                       paste0("sdBeta_",runChoi))])),
#         col=colGenesPol[2],border = NA)

matlines(x=traitsTrajs[,time],
        y=traitsTrajs[,.SD,
                      .SDcol=paste0(c("meanAlpha_","meanBeta_"),runChoi)],
        col=colGenesLin,lty = 2,type="l",lwd=3)


# Choose which interaction to visualize
lastInt<-2000
tail(pop[seed==runChoi,unique(nInteract)],2)[1]
# to get last interaction: tail(pop[,unique(nInteract)],1)
# Choose time range
gen2plot<-round(seq(1,length(unique(evolStats$time)),length.out = 5))[2:5]
# Plor the actor
seqYax<-c("s",rep("n",3))
# seqYlabUp<-c("Badge",rep("",3))
seqYlabUp<-c("P(dove)",rep("",3))
seqYlabDown<-c("Badge",rep("",3))
seqXlabDown<-c("","Quality","")
rangQual<-seq(0,1,length.out = 100)
nCenters<-6
interv<-1/(nCenters-1)
centers<-interv*seq(0,nCenters-1)
rangx<-seq(0,1,length=1000)
count<-0
for(genC in round(seq(1,length(unique(evolStats$time)),length.out = 5))[2:5]){
  count<-count+1
  par(plt=posPlot(numplotx = 4,numploty = 3,idplotx = count,idploty = 3),
      xaxt="s",las=1,new=TRUE)
  weightsAct<-as.double(evol[(time==unique(time)[genC])&seed==runChoi,.SD,
                                   .SDcols=grep("WeightAct",
                                                names(evol),value = TRUE)])
  plot(logist(totRBF(rangx,centers,0.01,weightsAct),alpha=0,beta=1)~rangx,
       yaxt=seqYax[count],ylab=seqYlabUp[count],xlab="",type="l",
       lwd=3,xaxt="n",ylim=c(0,1))
  tempPop<-pop[(time==unique(time)[genC]&seed==runChoi)&nInteract==lastInt,.SD[.N],
               .SDcol=c(grep("WeightAct",
                           names(evol),value = TRUE),"Quality","alpha","beta"),
               by=indId]
  dataIndAct<-sapply(as.list(tempPop[,indId]),
                       function(x){x=
                         logist(totRBF(rangx,
                                       centers,0.01,
                                       as.double(
                                         tempPop[indId==x,.SD,
                                                 .SDcol=grep("WeightAct",
                                                             names(tempPop),
                                                             value = TRUE)
                                                          ])),alpha=0,beta = 1)})
  matlines(x=rangx,y=dataIndAct,col = paletteMeans(100)[
    findInterval(tempPop[,Quality],colorbreaksQual)],lwd=2)
  text(x=0.5,y=0.1,labels = paste0("time=",unique(evolStats$time)[genC]))
  par(plt=posPlot(numplotx = 4,numploty = 3,idplotx = count,idploty = 1),
      las=1,new=TRUE)
  plot(sapply(rangx,
               FUN=function(x){
                 do.call(logist,
                         as.list(c(x,
                                   as.double(evol[(time==unique(time)[genC])&
                                                    seed==runChoi,
                                                      .SD,
                                                      .SDcols=c("meanAlpha"
                                                                ,"meanBeta")]))))})
        ~rangx,type='l',col=1,
       xlab=seqXlabDown[count],ylab=seqYlabDown[count],ylim=c(0,1),
       lwd=3,yaxt=seqYax[count],xaxt="s")
  dataIndReact<-sapply(as.list(tempPop[,indId]),
                      function(x){x=
                        sapply(rangx, function(y)
                        do.call(logist,as.list(c(y,as.double(tempPop[indId==x,.SD,
                                                .SDcol=c("alpha","beta")])))))})
  matlines(x=rangx,y=dataIndReact,col = paletteMeans(100)[
    findInterval(tempPop[,Quality],colorbreaksQual)],lwd=2)
}
rm(list=grep("temp",ls(),value = T))

# Plot mean and IQRs of the genotypes and phenotypes ----------------------------

# png(here("Simulations",scenario,"hawkDoveLearn_0.1.png"),
# height = 800,width = 800)

# get the trajectories for individual runs
traitsTrajs<-dcast(evol,time~seed,value.var = c("meanAlpha","meanBeta",
                                                "sdAlpha","sdBeta"))
# Average trajectory
par(plt=posPlot(numploty = 3,idploty = 2),xaxt="s",las=1)
plot(x=c(0,max(evolStats$time)),y=c(0,0),type="l",lwd=2,col="grey",
     ylim=fivenum(as.matrix(evol[,.(meanAlpha,meanBeta)]))[c(1,5)],
     xlab="",ylab="Trait \n value",cex.lab=1.5,cex.axis=1,xaxt='n',las=1)
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
       col=colGenesLin,lwd=2,bty = "n",lty = c(2,1))
axis(side=1,padj = -3)
matlines(x=traitsTrajs[,time],
        y=traitsTrajs[,.SD,
                      .SDcol=grep("meanAlpha_",names(traitsTrajs),value = TRUE)],
        col=colRuns,lty = 2,type="l",lwd=3)
matlines(x=traitsTrajs[,time],
        y=traitsTrajs[,.SD,
                      .SDcol=grep("meanBeta_",names(traitsTrajs),value = TRUE)],
        col=colRuns,lty = 1,type="l",lwd=3)
legend("top",legend = 0:10,col = colRuns,pch=20,bty = "n",ncol = 5)
# Choose time range
gen2plot<-round(seq(1,length(unique(evolStats$time)),length.out = 5))[2:5]
# Plor the actor
seqYax<-c("s",rep("n",3))
# seqYlabUp<-c("Badge",rep("",3))
seqYlabUp<-c("P(dove)",rep("",3))
seqYlabDown<-c("Badge",rep("",3))
seqXlabDown<-c("","Quality","")
rangQual<-seq(0,1,length.out = 100)
nCenters<-6
interv<-1/(nCenters-1)
centers<-interv*seq(0,nCenters-1)
rangx<-seq(0,1,length=1000)
count<-0
for(genC in round(seq(1,length(unique(evolStats$time)),length.out = 5))[2:5]){
  count<-count+1
  tempPop<-pop[time==unique(time)[genC]&nInteract==lastInt,.SD[.N],
               .SDcol=c(grep("WeightAct",
                             names(evol),value = TRUE),"Quality",
                        "alpha","beta","seed"),
               by=indId]
  dataIndAct<-sapply(as.list(tempPop[,indId]),
                     function(x){x=
                       logist(totRBF(rangx,
                                     centers,0.01,
                                     as.double(
                                       tempPop[indId==x,.SD,
                                               .SDcol=grep("WeightAct",
                                                           names(tempPop),
                                                           value = TRUE)
                                               ])),alpha=0,beta = 1)})
  
  par(plt=posPlot(numplotx = 4,numploty = 3,idplotx = count,idploty = 3),
      xaxt="s",las=1,new=TRUE)
  matplot(x=rangx,y=dataIndAct,col = colRuns[tempPop[,seed]],lwd=1,lty = 1,
       yaxt=seqYax[count],ylab=seqYlabUp[count],xlab="",type="l",
       xaxt="n",ylim=c(0,1))
  text(x=0.5,y=0.1,labels = paste0("time=",unique(evolStats$time)[genC]))
  par(plt=posPlot(numplotx = 4,numploty = 3,idplotx = count,idploty = 1),
      las=1,new=TRUE)
  matplot(x=rangx,y=dataIndReact,col = colRuns[tempPop[,seed]],lwd=1,type='l',
       xlab=seqXlabDown[count],ylab=seqYlabDown[count],ylim=c(0,1),
       yaxt=seqYax[count],xaxt="s")
  dataIndReact<-sapply(as.list(tempPop[,indId]),
                       function(x){x=
                         sapply(rangx, function(y)
                           do.call(logist,as.list(c(y,as.double(tempPop[indId==x,.SD,
                                                                        .SDcol=c("alpha","beta")])))))})
  }
rm(list=grep("temp",ls(),value = T))



# Overall variation ------------------------------------------------------------

par(plt=posPlot())
hist(pop[,Badge])
hist(pop[,Quality])
hist(pmin(pmax(rnorm(2000,mean = 0.5,sd = 0.3),0),1))


