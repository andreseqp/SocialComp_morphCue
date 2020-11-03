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

scenario<-"nIntGroupEvol4TestSeed"

extSimsDir<-#here("Simulations",paste0(scenario,"_"))
  paste0("e:/BadgeSims/",scenario,"_")


# Load files -------------------------------------------------------------------
# Project folder
# (listTest<-list.files(here("Simulations",paste0(scenario,"_"))))
# External sims folder
(listTest<-list.files(extSimsDir,full.names = TRUE))

(evolList<-grep("evolLearn",listTest,value=TRUE))
(indList<-grep("indLearn",listTest,value=TRUE))
paramName<-list.files(here("Simulations",paste0(scenario,"_")))
paramName<-list.files(extSimsDir,full.names = TRUE)
paramName<-grep(".json",paramName,value=TRUE)
param<-#fromJSON(here("Simulations",paste0(scenario,"_"),paramName))
  fromJSON(paramName[1])


val<-1

# loop to produce pdfs for parameter values
# numCores <- length(indList)
# registerDoParallel(numCores)
# foreach(val = 1:length(indList),.packages = c("data.table","here")) %dopar% {
#   source(here("AccFunc.R"))
  
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
  
  nCenters<-param$nCenters
  sigSquar<-param$sigSq
  
  
  ## Calculate clustering for all generations  but first one -------------------
  
  popOneInd<-pop[nInteract==500]
  vars<-c("alpha","beta")
  # ,"Badge",grep("WeightAct",names(popFinal),value = TRUE))
  popOneInd$idClust<-get_clusters(popOneInd,vars,k.max = 4,
                                  Bsamples =200,iterMax = 500)
  
  
  clusSummary<-popOneInd[,.(meanAlph=mean(alpha),meanBet=mean(beta)),
                         by=.(idClust,time,seed)]
  clusSummary[,orderClus:=0]
  for(i in 1:dim(clusSummary)[1]){
    set(clusSummary,i,6L,match(clusSummary[i,meanAlph],
          sort(clusSummary[time==clusSummary[i,time]&
                             seed==clusSummary[i,seed]]$meanAlph)))
  }
  
  
  popOneInd<-merge(popOneInd,clusSummary,all = TRUE)
  
  
# names(pop)[29:35]<-c("Quality", "genotype","alpha","beta","Badge","initCrit","initAct")
  # names(pop)[9:28]<-do.call(rbind,as.list(sapply(0:9,FUN = function(x){
  #     return(rbind(paste0("WeightAct_",x),paste0("WeightCrit_",x)))
  #     })))
  
  # Get stats from the evolutionary simulations ----------------------------------
  names(evol)
  
  cols<-c("freqGenHawks","freqGenDove",  "freqGenEval",  "freqGenLearn",
          "freqFenHawks", "freqFenDoves", "freqHH", "freqHD", "freqDD", "meanCue",
          "meanAlpha", "meanBeta",
          "meanFit", #"meanInitCrit", "sdInitCrit", "meanInitAct", "sdInitAct",
          "WeightAct_0","WeightCrit_0",
          "WeightAct_1","WeightCrit_1","WeightAct_2",  "WeightCrit_2",
          "WeightAct_3",  "WeightCrit_3", "WeightAct_4",  "WeightCrit_4",
          "WeightAct_5","WeightCrit_5")
  
  my.summary<- function(x) list(mean = mean(x), lowIQR = fivenum(x)[2], 
                                upIQR = fivenum(x)[4])
  
  evolStats<-evol[, as.list(unlist(lapply(.SD, my.summary))), 
                  .SDcols = cols,by=time]
  
  ## Plot mean and IQRs among individuals for each replicate --------------------
  
  
  cexAxis<-1.5
  
  # get the trajectories for individual runs
  traitsTrajs<-dcast(evol,time~seed,
                     value.var = c("meanAlpha","meanBeta",
                                   # "meanInitCrit",
                                   # "meanInitAct","sdInitCrit","sdInitAct",
                                   "sdAlpha","sdBeta","freqHH",
                                   "freqHD","freqDD"))
  (finReps<-evol[time==max(time),seed])
  
  
  # for(runChoi in finReps){
  
  genstoPrint<-round(seq(1,length(unique(evolStats$time)),length.out = 5))[2:5]
  
  runChoi<-0
  
  png(here("Simulations",paste0(scenario,"_"),
           paste0("evolDyn",runChoi,"_",nampar,Valpar,".png")),
      width = 1400,height = 1000)
  
  
  # Evolutionary dynamics for alpha - intercept of the sender --------------------------------------------
  
  par(plt=posPlot(numploty = 3,idploty = 2,numplotx = 2,idplotx = 1,
                  lowboundx = 8, upboundx = 93),mfrow=c(1,1),
      xaxt="s",las=1)
  plot(x=c(0,max(evolStats$time)),y=c(0,0),type="l",lwd=2,col="grey",
       ylim=fivenum(as.matrix(pop[,.(alpha,beta)]))[c(1,5)]+c(-0.5,0),
       xlab="",ylab="",cex.lab=cexAxis,cex.axis=cexAxis,xaxt='n',las=1)
  mtext(text = "Trait value",cex = cexAxis,line = 3,side = 2,las=0)
  axis(side=1,padj = -2,at = axTicks(1),labels = axTicks(1)/1000,cex.axis=cexAxis)
  legend("topleft",legend = c(expression(alpha)),
         col=colGenesLin[1],lwd=2,bty = "n")
  
  # grey lines to show the generations shown in the upper and lower panels
  
  matlines(x=matrix(rep(evolStats[genstoPrint,time],each=2),nrow=2),
           y=matrix(rep(c(-2,4),4),nrow = 2),lty=1,col = "grey",lwd=2)
  
  # Variation among individuals
  
  polygon(x=c(traitsTrajs[,time],rev(traitsTrajs[,time])),
          y=c(traitsTrajs[,apply(.SD,FUN=sum,MARGIN=1),
                          .SDcol=c(paste0("meanAlpha_",runChoi),
                                   paste0("sdAlpha_",runChoi))],
              rev(traitsTrajs[,apply(.SD, MARGIN = 1,FUN = function(x){x[1]-x[2]}),
                              .SDcols=c(paste0("meanAlpha_",runChoi),
                                        paste0("sdAlpha_",runChoi))])),
          col=colGenesPol[1],border = NA)
  
  matlines(x=traitsTrajs[,time],
           y=traitsTrajs[,.SD,
                         .SDcol=paste0(c("meanAlpha_"),runChoi)],
           col=colGenesLin,lty = 2,type="l",lwd=3)
  
  points(x=popOneInd1[seed==runChoi,time],
         y=popOneInd1[seed==runChoi,alpha],pch=20,cex=0.5,
         col=colRuns[popOneInd1[seed==runChoi,orderClus]])
  
  par(plt=posPlot(numploty = 3,idploty = 2,numplotx = 2, idplotx = 2,
                  lowboundx = 8, upboundx = 93),
      xaxt="s",las=1,new=TRUE)
  
  plot(x=c(0,max(evolStats$time)),y=c(0,0),type="l",lwd=2,col="grey",
       ylim=fivenum(as.matrix(pop[,.(alpha,beta)]))[c(1,5)]+c(-0.5,0),
       xlab="",ylab="",cex.lab=cexAxis,cex.axis=cexAxis,xaxt='n',yaxt="n",las=1)
  axis(side=1,padj = -2,at = axTicks(1),labels = axTicks(1)/1000,cex.axis=cexAxis)
  legend("topleft",legend = c(expression(beta)),
         col=colGenesLin[2],lwd=2,bty = "n")
  # grey lines to show the generations shown in the upper and lower panels
  
  matlines(x=matrix(rep(evolStats[genstoPrint,time],each=2),nrow=2),
           y=matrix(rep(c(-2,4),4),nrow = 2),lty=1,col = "grey",lwd=2)
  
  
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
                         .SDcol=paste0(c("meanBeta_"),runChoi)],
           col=colGenesLin[2],lty = 2,type="l",lwd=3)
  
  points(x=popOneInd[seed==runChoi,time],
         y=popOneInd[seed==runChoi,beta],pch=20,cex=0.7,
         col=colRuns[popOneInd[seed==runChoi,orderClus]])
  
  
  
  
  # Choose which interaction to visualize
  lastInt<-500#tail(pop[seed==runChoi,unique(nInteract)],6)[1]
  pop[seed==runChoi,max(nInteract),by=time][,min(V1)]
  # to get last interaction: tail(pop[,unique(nInteract)],1)
  # Choose time range
  gen2plot<-round(seq(1,length(unique(evolStats$time)),length.out = 5))[2:5]
  # Plor the actor
  seqYax<-c("s",rep("n",3))
  # seqYlabUp<-c("Badge",rep("",3))
  seqYlabUp<-c("P(dove)",rep("",3))
  seqYlabDown<-c("Badge",rep("",3))
  seqXlabDown<-c("","Quality","")#paste0("seed: ",runChoi))
  rangQual<-seq(0,1,length.out = 50)
  interv<-1/(nCenters-1)
  centers<-interv*seq(0,nCenters-1)
  rangx<-seq(0,1,length=50)
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
            lwd=2,xaxt="n",ylim=c(-0.05,1),col = paletteMeans(100)[
              findInterval(tempPop[,Quality],colorbreaksQual)],lty = 1,cex.axis=cexAxis)
    mtext(text = seqYlabUp[count],cex = cexAxis,line = 3,side = 2,las=0)
    lines(logist(totRBF(rangx,centers,sigSquar,weightsAct),alpha=0,beta=1)~rangx,
          col=1,lwd=3,lty=2)
    text(x=0.5,y=-0.01,labels = paste0("time=",unique(evolStats$time)[genC]/1000),
         cex=2)
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
      findInterval(tempPop[,Quality],colorbreaksQual)],type='l',cex.axis=cexAxis,
      xlab=seqXlabDown[count],ylab="",ylim=c(0,1),
      lwd=2,yaxt=seqYax[count],xaxt="s",cex.lab=cexAxis)
    mtext(text = seqYlabDown[count],cex = cexAxis,line = 3,side = 2,las=0)
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
  dev.off()
  
  
  ## Effect of the alternative equilibria in behavioural interactions ----------
  
  
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
  
  cexAxis<-1.5;cexPoints<-1.5
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
                pch=16,cex.lab=2,las=1,
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
    