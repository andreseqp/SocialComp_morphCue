# Evolutionary dynamics for averages and individual runs, reaction norms of sampled 
# individuals and their learning profiles

# Required libraries -----------------------------------------------------------

require(here)
here()
source(here("AccFunc.R"))
require("foreach")
require("doParallel")
require("jsonlite")

# Scenario to be plotted - corresponds to folders where simulations are stored

scenario<-"betCostEvol2"#"alphaAct"#"nIntGroupNormQual"#

# extSimsDir<-#here("Simulations",paste0(scenario,"_"))
#   paste0("M:/BadgeSims/",scenario,"_")


# Load files -------------------------------------------------------------------
# Project folder
(listTest<-list.files(here("Simulations",paste0(scenario,"_")),full.names = TRUE))
# External sims folder
# (listTest<-list.files(extSimsDir,full.names = TRUE))

(evolList<-grep("evolLearn",listTest,value=TRUE))
(indList<-grep("indLearn",listTest,value=TRUE))
paramName<-list.files(here("Simulations",paste0(scenario,"_")))
# paramName<-list.files(extSimsDir,full.names = TRUE)
paramName<-grep(".json",paramName,value=TRUE)
param<-fromJSON(here("Simulations",paste0(scenario,"_"),paramName[2]))
  # fromJSON(paramName[1])


val<-2
fileId<-val


evolList_runs<-grep(paste0(param$namParam,param$rangParam[val]),
                    evolList,value =TRUE)
indList_runs<-grep(paste0(param$namParam,param$rangParam[val]),
                   indList,value =TRUE)


evol<-do.call(rbind,lapply(evolList_runs,fread))
pop<-do.call(rbind,lapply(indList_runs, fread))
  

  # fileId<-val
  
  # Project folder
  # evol<-fread(here("Simulations",paste0(scenario,"_"),evolList[fileId]))
  # pop<-fread(here("Simulations",paste0(scenario,"_"),indList[fileId]))
  # 
  # External sims folder
  # evol<-fread(evolList[fileId])
  # pop<-fread(indList[fileId])

  # Valpar<-gsub("[[:alpha:]]",gsub(".txt","",tail(strsplit(indList[val],"_")[[1]],1)),
  #              replacement = "")
  
Valpar<-param$rangParam[val]
  
nampar<-param$namParam
  
nCenters<-param$nCenters
sigSquar<-param$sigSq
  
# Aesthetic paremeters ---------------------------------------------------------

cexAxis<-2.2


  ## Calculate clustering for all generations  -------------------
  
runChoi<-18
# Choose which interaction to visualize
lastInt<-tail(pop[,unique(nInteract)],4)[1]
# pop[,max(nInteract),by=.(seed,time)][,min(V1)]
# to get last interaction: tail(pop[,unique(nInteract)],1)
  
popOneInd<-pop[nInteract==lastInt]
vars<-c("alpha","beta","Badge")
# ,"Badge",grep("WeightAct",names(popFinal),value = TRUE))
popOneInd$idClust<-get_clusters(popOneInd,vars,k.max = 5,
                                Bsamples =500,iterMax = 500)


clusSummary<-popOneInd[,.(meanAlph=mean(alpha),meanBet=mean(beta)),
                       by=.(idClust,time,seed)]

clusSummary[,orderClus:=0]
for(i in 1:dim(clusSummary)[1]){
  set(clusSummary,i,6L,match(clusSummary[i,meanAlph],
        sort(clusSummary[time==clusSummary[i,time]&
                           seed==clusSummary[i,seed]]$meanAlph)))
}


popOneInd<-merge(popOneInd,clusSummary,all = TRUE)

popOneInd[,unique(orderClus)]
  
# names(pop)[29:35]<-c("Quality", "genotype","alpha","beta","Badge","initCrit","initAct")
  # names(pop)[9:28]<-do.call(rbind,as.list(sapply(0:9,FUN = function(x){
  #     return(rbind(paste0("WeightAct_",x),paste0("WeightCrit_",x)))
  #     })))
  
# Get stats from the evolutionary simulations ----------------------------------
names(evol)

cols<-c("freqGenHawks","freqGenDove",  "freqGenEval",  #"freqGenLearn",
        "freqFenHawks", "freqFenDoves", "freqHH", "freqHD", "freqDD", "meanCue",
        "meanAlpha", "meanBeta",
        #"meanFit", #"meanInitCrit", "sdInitCrit", "meanInitAct", "sdInitAct",
        "WeightAct_0","WeightCrit_0",
        "WeightAct_1","WeightCrit_1","WeightAct_2",  "WeightCrit_2",
        "WeightAct_3",  "WeightCrit_3", "WeightAct_4",  "WeightCrit_4",
        "WeightAct_5","WeightCrit_5")

my.summary<- function(x) list(mean = mean(x), lowIQR = fivenum(x)[2], 
                              upIQR = fivenum(x)[4])

evolStats<-evol[, as.list(unlist(lapply(.SD, my.summary))), 
                .SDcols = cols,by=time]

## Plot mean and IQRs among individuals for each replicate --------------------


# cexAxis<-1.3

# get the trajectories for individual runs
traitsTrajs<-dcast(evol,time~seed,
                   value.var = c("meanAlpha","meanBeta",
                                 #"meanInitCrit",#"meanFit",
                                 # "meanInitAct","sdInitCrit","sdInitAct",
                                 "sdAlpha","sdBeta","freqHH",
                                 "freqHD","freqDD"))
(finReps<-evol[time==max(time),seed])


genstoPrint<-round(seq(1,length(unique(evolStats$time)),length.out = 5))[2:5]


png(here("Simulations",paste0(scenario,"_"),
         paste0("evolDyn",runChoi,"_",nampar,Valpar,"_1.png")),
    width = 1920,height = 1080)


# Evolutionary dynamics for alpha - intercept of the sender --------------------------------------------

par(plt=posPlot(numploty = 3,idploty = 2,numplotx = 2,idplotx = 1,
                lowboundx = 8, title = TRUE,lowboundy = 10,
                upboundy = 90),
    mfrow=c(1,1), xaxt="s",las=1)

evolDist(indData = pop[seed==runChoi],variable = "alpha",nbins = 10,pal = pal_dist,
         nlevels=10,cexAxis = cexAxis,xlab="",ylab = "",xaxt="n",
         ylim = range(pop[seed==runChoi,alpha])*c(1,1.2))

# plot(x=c(0,max(evolStats$time)),y=c(0,0),type="l",lwd=2,col="grey",
#      ylim=fivenum(as.matrix(pop[,.(alpha,beta)]))[c(1,5)]+c(-0.5,0),
#      xlab="",ylab="",cex.lab=cexAxis,cex.axis=cexAxis,xaxt='n',las=1)
mtext(text = "Trait value",cex = cexAxis,line = 3,side = 2,las=0)
legend("topleft",legend = c(expression(alpha[s])),col=colGenesLin[1],lwd=2,
       bty = "n",cex=2)

# grey lines to show the generations shown in the upper and lower panels

matlines(x=matrix(rep(evolStats[genstoPrint,time],each=2),nrow=2),
         y=matrix(rep(c(-2,3),4),nrow = 2),lty=1,col = "grey",lwd=4)

axis(side=1,padj = -2,at = axTicks(1)[2:(length(axTicks(1))-1)],
     labels = axTicks(1)[2:(length(axTicks(1))-1)]/1000,cex.axis=cexAxis)

# Variation among individuals

# polygon(x=c(traitsTrajs[,time],rev(traitsTrajs[,time])),
#         y=c(traitsTrajs[,apply(.SD,FUN=sum,MARGIN=1),
#                         .SDcol=c(paste0("meanAlpha_",runChoi),
#                                  paste0("sdAlpha_",runChoi))],
#             rev(traitsTrajs[,apply(.SD, MARGIN = 1,FUN = function(x){x[1]-x[2]}),
#                             .SDcols=c(paste0("meanAlpha_",runChoi),
#                                       paste0("sdAlpha_",runChoi))])),
#         col=colGenesPol[1],border = NA)

matlines(x=traitsTrajs[,time],
         y=traitsTrajs[,.SD,
                       .SDcol=paste0(c("meanAlpha_"),runChoi)],
         col=colGenesLin,lty = 2,type="l",lwd=5)

# points(x=popOneInd[seed==runChoi,time],
#        y=popOneInd[seed==runChoi,alpha],pch=20,cex=0.8,
#        col=colRuns[popOneInd[seed==runChoi,orderClus]])

par(plt=posPlot(numploty = 3,idploty = 2,numplotx = 2, idplotx = 2,
                lowboundx = 8, title = TRUE,lowboundy = 10,
                upboundy = 90),
    xaxt="s",las=1,new=TRUE)

evolDist(indData = pop[seed==runChoi],variable = "beta",nbins = 10,pal = pal_dist,
         nlevels=10,cexAxis = 1.5,xlab="",ylab = "",xaxt="n",yaxt = "n",
         ylim = range(pop[seed==runChoi,beta])*c(0.9,1.2))
# plot(x=c(0,max(evolStats$time)),y=c(0,0),type="l",lwd=2,col="grey",
#      ylim=fivenum(as.matrix(pop[,.(alpha,beta)]))[c(1,5)]+c(-0.5,0),
#      xlab="",ylab="",cex.lab=cexAxis,cex.axis=cexAxis,xaxt='n',yaxt="n",las=1)
legend("topleft",legend = c(expression(beta[s])),cex=2,
       col=colGenesLin[2],lwd=2,bty = "n")
# grey lines to show the generations shown in the upper and lower panels

matlines(x=matrix(rep(evolStats[genstoPrint,time],each=2),nrow=2),
         y=matrix(rep(c(-2,6),4),nrow = 2),lty=1,col = "grey",lwd=4)

axis(side=1,padj = -2,at = axTicks(1)[2:(length(axTicks(1))-1)],
     labels = axTicks(1)[2:(length(axTicks(1))-1)]/1000,cex.axis=cexAxis)
axis(4,cex.axis=cexAxis)

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
                       .SDcol=paste0(c("meanBeta_"),runChoi)],
         col=colGenesLin[2],lty = 2,type="l",lwd=5)

# points(x=popOneInd[seed==runChoi,time],
#        y=popOneInd[seed==runChoi,beta],pch=20,cex=0.8,
#        col=colRuns[popOneInd[seed==runChoi,orderClus]])

# Choose time range
gen2plot<-round(seq(1,length(unique(evolStats$time)),length.out = 5))[2:5]
# Plor the actor
seqYax<-c("s",rep("n",3))
# seqYlabUp<-c("Badge",rep("",3))
seqYlabUp<-c("P(dove)",rep("",3))
seqYlabDown<-c("Badge",rep("",3))
seqXlabDown<-c("","Quality","")#paste0("seed: ",runChoi))
seqXlabUp<-c("","Badge","")
rangQual<-seq(0,1,length.out = 50)
interv<-1/(nCenters-1)
centers<-interv*seq(0,nCenters-1)
rangx<-seq(0,1,length=50)
count<-0
for(genC in genstoPrint){
  count<-count+1
  par(plt=posPlot(numplotx = 4,numploty = 3,idplotx = count,idploty = 3,
                  lowboundx = 8, title = TRUE,lowboundy = 10,
                  upboundy = 90),
      xaxt="s",las=1,new=TRUE)
  weightsAct<-as.double(evol[(time==unique(time)[genC])&seed==runChoi,.SD,
                             .SDcols=grep("WeightAct",
                                          names(evol),value = TRUE)])
  tempPop<-popOneInd[(time==unique(time)[genC]&seed==runChoi),.SD[.N],
               .SDcol=c(grep("WeightAct",
                             names(evol),value = TRUE),"Quality","alpha","beta",
                        "orderClus"),
               by=indId]
  
  randInds<-sample(tempPop$indId, length(tempPop$indId), replace = FALSE)
  
  dataIndAct<-sapply(as.list(tempPop[indId %in%randInds,indId]),
                     function(x){x=
                       logist(totRBF(rangx,
                                     centers,sigSquar,
                                     as.double(
                                       tempPop[indId==x,.SD,
                                               .SDcol=grep("WeightAct",
                                                           names(tempPop),
                                                           value = TRUE)
                                               ])),alpha=0,beta = 1)})
  matplot(x=rangx,y=dataIndAct,
          yaxt=seqYax[count],ylab="",xlab="",type="l",cex.lab=cexAxis,
          lwd=2,xaxt="n",ylim=c(-0.06,1),col = paletteMeans(100)[
            findInterval(tempPop[,Quality],colorbreaksQual)],lty = 1,
          cex.axis=cexAxis)
  axis(side = 3,cex.axis=cexAxis,padj = 0.5)
  mtext(text = seqYlabUp[count],cex = cexAxis,line = 4,side = 2,las=0)
  mtext(text = seqXlabUp[count],cex = cexAxis,line = 4,side = 3 ,las=0)
  lines(logist(totRBF(rangx,centers,sigSquar,weightsAct),alpha=0,beta=1)~rangx,
        col=1,lwd=3,lty=2)
  text(x=0.5,y=-0.015,labels = paste0("time=",unique(evolStats$time)[genC]/1000),
       cex=2)
  par(plt=posPlot(numplotx = 4,numploty = 3,idplotx = count,idploty = 1,
                  lowboundx = 8,lowboundy = 10,
                  upboundy = 90), las=1,new=TRUE)
  dataIndReact<-sapply(as.list(tempPop[,indId]),
                       function(x){x=
                         sapply(rangx, 
                                function(y)
                                  do.call(logist,
                                          as.list(
                                            c(y,as.double(
                                              tempPop[indId==x,.SD,
                                                      .SDcol=c("alpha","beta")])))))})
  # matplot(x=rangx,y=dataIndReact,col = paletteMeans(100)[
  #   findInterval(tempPop[,Quality],colorbreaksQual)],type='l',cex.axis=cexAxis,
  #   xlab=seqXlabDown[count],ylab="",ylim=c(0,1),lty=1,
  #   lwd=2,yaxt=seqYax[count],xaxt="s",cex.lab=cexAxis)
  meansClustmp<-tempPop[,.(alphaMean=mean(alpha),betaMean=mean(beta)),by=orderClus]
  reacNormpClust<-sapply(meansClustmp[,orderClus], function(x){
    logist(rangx,alpha = meansClustmp[orderClus==x,alphaMean],
           beta = meansClustmp[orderClus==x,betaMean])})
  matplot(x = rangx,y = reacNormpClust,col=colRuns[meansClustmp$orderClus],
           type='l',cex.axis=cexAxis,
           xlab="",ylab="",ylim=c(0,1),lty=1,
           lwd=2,yaxt=seqYax[count],xaxt="s",cex.lab=cexAxis)
  mtext(text = seqYlabDown[count],cex = cexAxis,line = 4,side = 2,las=0)
  mtext(text = seqXlabDown[count],cex = cexAxis,line = 3.5,side = 1,las=0)
  
  }

rm(list=grep("temp",ls(),value = T))

dev.off()


## Effect of the number of clusters in behavioural interactions ----------

nClustersReps<-popOneInd[,max(orderClus),by=.(seed,time)]


names(popOneInd)
names(evol)

evol<-merge(x=evol,y = nClustersReps,all.x = TRUE,by = c("seed","time"))
setnames(evol,"V1","nClusters")

evol[,posX:=match(nClusters,sort(unique(nClusters)))+
       runif(length(nClusters),
             min = -0.3,max = 0.3)]
# png(here("Simulations",paste0(scenario,"_"),
#          paste0("compClustRep","_",nampar,Valpar,".png")),
#     width = 1400,height = 1000)

cexAxis<-1.5;cexPoints<-1.5
par(mfrow=c(1,2),plt=posPlot())
bbHH<-boxplot(freqHH~nClusters,data=evol,
              pch=16,cex.lab=cexAxis,las=1,
              xlab="cluster",
              ylab='',ylim=fivenum(evol$freqHH)[c(1,5)]+c(0,0.01),
              cex.axis=cexAxis,yaxt='s')

with(evol,{
  points(x=posX,y=freqHH,
         col = colSeeds[match(seed,unique(seed))],pch = 20,cex=cexPoints)
  # lines(y=rep(0.666^2,2),x=c(0.5,3.5))
  text(x=posX+0.2,y=freqHH,labels = seed,cex=0.8)
})
mtext("HH",3,line = -2.5,cex=2)


bbHH<-boxplot(meanBeta~nClusters,data=evol,
              pch=16,cex.lab=2,las=1,
              xlab="cluster",
              ylab='',ylim=fivenum(evol$meanBeta)[c(1,5)]+c(0,0.5),
              cex.axis=cexAxis,yaxt='s')


with(evolFinal,{
  points(x=posX,y=meanBeta,
         col = colSeeds[match(seed,unique(seed))],pch = 20,cex=cexPoints)
  # lines(y=rep(0.666^2,2),x=c(0.5,3.5))
  text(x=posX+0.2,y=freqHH,labels = seed,cex=0.8)
})
mtext(expression(beta[s]),3,line = -2.5,cex=2)
dev.off()
  
## Effect of the alternative equilibria in behavioural interactions ----------

indVarScatter
evolFinal<-evol[time>max(time)*0.8]

names(evolFinal)
selVars<-as.array(names(evolFinal[,8:28]))

clusters<-kmeans(evolFinal[,.SD,.SDcols=selVars],centers = 2,iter.max = 200)


evolFinal[,idClust:=as.factor(clusters$cluster)]
evolFinal[,posX:=match(idClust,sort(unique(idClust)))+
            runif(length(idClust),
                  min = -0.3,max = 0.3)]
png(here("Simulations",paste0(scenario,"_"),
         paste0("compClustRep","_",nampar,Valpar,".png")),
    width = 1400,height = 1000)

cexAxis<-2.2;cexPoints<-2.5
par(mfrow=c(1,2))
bbHH<-boxplot(freqHH~idClust,data=evolFinal,
              pch=16,cex.lab=cexAxis,las=1,
              xlab="cluster",
              ylab='',ylim=fivenum(evolFinal$freqHH)[c(1,5)]+c(0,0.01),
              cex.axis=cexAxis,yaxt='s')


with(evolFinal,{
  points(x=posX,y=freqHH,
         col = colSeeds[match(seed,unique(seed))],pch = 20,cex=cexPoints)
  # lines(y=rep(0.666^2,2),x=c(0.5,3.5))
  # text(x=posX+0.2,y=freqHH,labels = seed,cex=0.8)
})
mtext("HH",3,line = -2.5,cex=2)


bbHH<-boxplot(meanBeta~idClust,data=evolFinal,
              pch=16,cex.lab=cexAxis,las=1,
              xlab="cluster",
              ylab='',ylim=fivenum(evolFinal$meanBeta)[c(1,5)]+c(0,0.5),
              cex.axis=cexAxis,yaxt='s')


with(evolFinal,{
  points(x=posX,y=meanBeta,
         col = colSeeds[match(seed,unique(seed))],pch = 20,cex=cexPoints)
  # lines(y=rep(0.666^2,2),x=c(0.5,3.5))
  # text(x=posX+0.2,y=freqHH,labels = seed,cex=0.8)
})
mtext(expression(beta[s]),3,line = -2.5,cex=2)
dev.off()


# How learning works ---------------------------------------------------------


gener<-0#tail(indLearn[,unique(time)],2)[2]
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
seedCh<-0
finReps[round(runif(1,0,length(finReps)))+1]
indLearn<-pop

# Select run and generation to plot
tempPop<-indLearn[time==gener&seed==seedCh]

timePoints<-round(seq(1,length(unique(tempPop[,nInteract]))-25,
                      length.out = 5))

png(here("Simulations",paste0(scenario,"_"),paste0(nampar,Valpar,"learnDyn.png")),
    width = 1920,height = 1080)

par(mfrow=c(1,1))
plot.new()

par(plt=posPlot(numploty = 2,numplotx = 4,idploty = 2,idplotx = 2,upboundy = 99,
                lowboundy = 20))
rangx<-seq(0,1,by=0.01)
badge<-logist(rangx,alpha = param$alphaBad,beta = param$betaBad)
plot(x=rangx,y=badge,col=1,lwd=4,ylim = c(0,1.1),xlim=c(0,1),xaxt="s",ylab = "",
     xlab="",type="l",cex.axis=cexAxis)
mtext("Badge size",2,cex=2,las=0,line=3)
mtext("Quality",1,line = 3,cex=2)
lines(logist(totRBF(rangx,centers,sigSquar,rep(0,nCenters))
             ,alpha = 0,beta = 1)~rangx,
      lwd=1,col=1)
dataIndsAct<-sapply(as.list(tempPop[nInteract==1700,indId]),
                    function(x){x=
                      logist(totRBF(rangx,
                                    centers,sigSquar,
                                    as.double(
                                      tempPop[(nInteract==1700
                                               &indId==x)
                                              # &(Quality>0.8&Quality<1)
                                              ,.SD,
                                              .SDcol=grep("WeightAct",
                                                          names(tempPop),
                                                          value = TRUE)
                                              ])),alpha = 0,beta = 1)})
par(plt=posPlot(numploty = 2,numplotx = 4,idploty = 2,idplotx = 3,upboundy = 99,
                lowboundy = 20)
    ,new=TRUE)
matplot(x=rangx,y=dataIndsAct,type='l',xlab="",ylab="",
        xaxt="s",yaxt="n",lty = 1,
        col=paletteMeans(100)[
          findInterval(tempPop[nInteract==1700,Quality],colorbreaksQual)],
        lwd=0.5,ylim=c(0,1.1),cex.axis=cexAxis)
axis(4,cex.axis=cexAxis)
mtext(expression(p(dove)),side = 4,cex=2,las=0,line = 3)
mtext("Badge size",1,line = 3,cex=2)

yaxRang<-c("s",rep("n",4))
xaxRang<-c("s","n")
xAxLabs<-rep("",5)
xAxLabs[3]<-"Badge size"
yAxLabs<-c("p(dove)","")
countx<-0
county<-1        
# for(behavTime in unique(tempPop$nInteract)[1:5]){
for(behavTime in unique(tempPop$nInteract)[timePoints]){
  if(countx==5)  {countx<-0;county<-county-1}
  countx<-countx+1
  dataIndsAct<-sapply(as.list(tempPop[nInteract==behavTime,indId]),
                      function(x){x=
                        logist(totRBF(rangx,
                                      centers,sigSquar,
                                      as.double(
                                        tempPop[(nInteract==behavTime
                                                 &indId==x)
                                                # &(Quality>0.8&Quality<1)
                                                ,.SD,
                                                .SDcol=grep("WeightAct",
                                                            names(tempPop),
                                                            value = TRUE)
                                                ])),alpha = 0,beta = 1)})
  par(plt=posPlot(numploty = 2,numplotx = 5,idploty = county,idplotx = countx,
                  upboundy = 95,lowboundy = 10),
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
          lwd=0.5,ylim=c(0,1.1),cex.axis=cexAxis)
  lines(logist(totRBF(rangx,centers,sigSquar,rep(0,nCenters))
               ,alpha = 0,beta = 1)~rangx,
        lwd=1,col=1)
  text(x = 0.5,y=1.06,labels = paste0("t =",behavTime),cex=2)
  if(county==1) mtext(xAxLabs[countx],1,line = 3,cex=2)
  if(countx==1) mtext(yAxLabs[county],2,line = 3,cex=2,las=0)
}


 # Include if the color scheme relates to quality
par(new=FALSE)
color.bar.aeqp(paletteMeans(100),min =min(colorbreaksQual),
               max = max(colorbreaksQual),nticks = 3,
               title = "Quality",
               cex.tit = 1.3,
               numplotx = 13,numploty = 13,idplotx =2,idploty = 10)

dev.off()


  