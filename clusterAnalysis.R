require(here)
here()
source(here("AccFunc.R"))
source(here("..","R_files","NbClust.R"))
require("foreach")
require("doParallel")
require("jsonlite")
require("factoextra")
require("cluster")

# Scenario to be plotted - corresponds to folders where simulations are stored

scenario<-"nIntGroupNormQual2"

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
  # evol<-fread(evolList[fileId])
  # pop<-fread(indList[fileId])

  evolList_runs<-grep(paste0(param$namParam,param$rangParam[val]),
                      evolList,value =TRUE)
  indList_runs<-grep(paste0(param$namParam,param$rangParam[val]),
                     indList,value =TRUE)
  
  
  evol<-do.call(rbind,lapply(evolList_runs,fread))
  pop<-do.call(rbind,lapply(indList_runs, fread))
  
    
  Valpar<-param$rangParam[val]
  
  nampar<-param$namParam
  
  nCenters<-param$nCenters
  sigSquar<-param$sigSq
  
  
  
  # Get the optimal number of clusters and assing inds to clusters -------------
  propTime2plot<-0.8
  popFinal<-pop[nInteract==500&time==7000]
  
  popOneInd<-pop[nInteract==500]
  
  vars<-c("alpha","beta")
  # ,"Badge",grep("WeightAct",names(popFinal),value = TRUE))
  names(popFinal)
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
  
  
  popOneInd<-merge(popOneInd,clusSummary,all.x = TRUE)
  
  
  
  
  NbClust::NbClust(popOneInd[seed==9&time==3000,.SD,.SDcols=vars],
                   method = "kmeans",max.nc = 4)
  
  NbClust.AEQP(popOneInd[seed==9&time==3000,.SD,.SDcols=vars],
                   method = "kmeans",max.nc = 4)
  
  focal_rep_time<-popOneInd[seed==15,.(alphaMean=mean(alpha),meanBeta=mean(beta),n=length(indId)),
                            by=orderClus]
  
  popOneInd[orderClus==1&seed==15,.(alpha,beta)]
  
  plot(alpha~beta,col=colRuns[orderClus],data=popOneInd[seed==15])
  
  rangQual<-seq(0,1,by = 0.01)
  reactNorms<-sapply(focal_rep_time$orderClus, function(x){
    logist(rangQual,alpha = focal_rep_time[orderClus==x,alphaMean],
           beta = focal_rep_time[orderClus==x,meanBeta])
  })
  
  par(mfrow=c(1,2))
  matplot(x=rangQual,reactNorms,type="l",lty=1,lwd=3)
  
  dataIndReact<-sapply(as.list(popOneInd[seed==15,indId]),
                       function(x){x=
                         sapply(rangQual, 
                                function(y)
                                  do.call(logist,
                                          as.list(
                                            c(y,as.double(
                                              popOneInd[seed==15&indId==x,.SD,
                                                      .SDcol=c("alpha","beta")])))))})
  matplot(x=rangQual,y=dataIndReact,col = paletteMeans(100)[
    findInterval(popOneInd[seed==15,Quality],colorbreaksQual)],type='l',
    ylim=c(0,1),
    lwd=2)
  
  
  focal_rep_time
  
  dim(popOneInd)
  
  
  
  
  plot(as.matrix(popOneInd[seed==0&time==6000,.SD,.SDcols=vars]))
  
  clusGap(as.matrix(popOneInd[seed==3&time==1000,.SD,.SDcols=vars]),kmeans,
          K.max = 5,B = 200,iter.max=500)
  
  popFinal[seed==1]
  
  popFinal[,nClusters:=popFinal[,length(unique(idClust)),by=seed][
    match(popFinal$seed,popFinal[,length(unique(idClust)),by=seed][,seed]),V1]]
  
  
  
  png(here("Simulations",paste0(scenario,"_"),
  paste0("corrAlphBet_",nampar,Valpar,".png")),
  width = 1400,height = 1000)
  
  nY<-4;nX<-4
  

  par(mfrow=c(1,1))
  seqYax<-c("s",rep("n",nY))
  # seqXax<-c(rep("n",4),"s")
  # seqYlabUp<-c("Badge",rep("",3))
  seqYlabDown<-c("",expression(alpha),rep("",3))
  seqXlabDown<-c("",expression(beta),"","")
  cX<-0;cY<-nY
  plot.new()
  for(cSeed in pop[,unique(seed)]){
    if(cX==nX)  {cX<-0;cY<-cY-1}
    cX<-cX+1
    par(plt=posPlot(numplotx = nX,numploty = nY,idplotx = cX,idploty = cY),
        xaxt="s",las=1,new=TRUE)
    plot(data=popOneInd[seed==cSeed&time==max(time)*0.8],alpha~beta,ylab="",
         xlab="", pch=20,cex.lab=3,cex.axis=2,las=1,cex=2,
         ylim=range(popOneInd[,alpha])+c(0,0.9),
         xlim=range(popOneInd[,beta]),col=colRuns[orderClus],
         yaxt=seqYax[cX],xaxt=seqYax[cY])
    lines(x=c(0,0),y=range(popOneInd[,alpha]),col="grey",
          lwd=2)
    lines(y=c(0,0),x=range(popOneInd[,beta]),col="grey",
          lwd=2)
    text(x = mean(range(popOneInd[,beta])),
         y = range(popOneInd[,alpha])[2]+0.45,
         labels = paste0("seed=",cSeed),cex=1)
    if(cY==1) mtext(seqXlabDown[cX],1,line = 3.5,cex=3)
    if(cX==1) mtext(seqYlabDown[cY],2,line = 3,cex=3,las=1)
    
  }
  
  
  dev.off()
  
  
  plot(data=popOneInd[time==max(time)*0.8],alpha~beta,ylab="",
       xlab="", pch=20,cex.lab=3,cex.axis=2,las=1,cex=2,
       ylim=range(popOneInd[,alpha])+c(0,0.9),
       xlim=range(popOneInd[,beta]),col=colReps[seed],
       yaxt="s",xaxt="s")
  lines(x=c(0,0),y=range(popOneInd[,alpha]),col="grey",
        lwd=2)
  lines(y=c(0,0),x=range(popOneInd[,beta]),col="grey",
        lwd=2)
  mtext(text = expression(alpha),side = 2,line = 3,las=1,cex=3)
  mtext(text = expression(beta),side = 1,line = 2,5,cex = 3)
  
  
  popOneInd[,length(unique(orderClus)),by=.(seed,time)]
  
  evol[,nClusters:=popFinal[,length(unique(idClust)),by=seed][
    match(evol$seed,popFinal[,length(unique(idClust)),by=seed][,seed]),V1]]
  
  
  # png(here("Simulations",paste0(scenario,"_"),
  #          paste0("BehavIntAllClusters_",nampar,Valpar,".png")),
  #     width = 1400,height = 1000)
  
  
  # Effect of clustering on the interactions types 
  
  evol[,posX:=match(nClusters,sort(unique(nClusters)))+
         runif(length(nClusters),
               min = -0.2,max = 0.2)]
  
  cexAxis<-1.5;cexPoints<-1
  
  par(mfrow=c(1,3),xaxt="s",las=1,plt=posPlot())
  
  # frequency of interactions types
  
  bbHH<-boxplot(freqHH~nClusters,data=evol[time>max(time)*propTime2plot],
                pch=16,cex.lab=2,
                xlab="# clusters",
                ylab='',ylim=fivenum(evol$freqHH)[c(1,5)]+c(0,0.005),
                cex.axis=cexAxis,yaxt='s')
  with(evol[time>max(time)*propTime2plot],{
    points(x=posX,y=freqHH,
           col = colSeeds[match(seed,unique(seed))],pch = 20,cex=cexPoints)
    lines(y=rep(0.666^2,2),x=c(0.5,3.5))
    # text(x=posX+0.2,y=freqHH,labels = seed,cex=0.8)
  })
  mtext("HH",3,line = -2.5,cex=2)
  
  par(xaxt="s",las=1)
  bbHH<-boxplot(freqHD~nClusters,data=evol[time>max(time)*propTime2plot],
                pch=16,ylim=fivenum(evol$freqHD)[c(1,5)]+c(0,0.005),
                xlab="# clusters",
                ylab='',cex.lab=2,
                cex.axis=cexAxis)
  # axis(2,line = -2)
  with(evol[time>max(time)*propTime2plot],{
    points(x=posX,y=freqHD,col = colSeeds[match(seed,unique(seed))],pch = 20,
           cex=cexPoints)
    lines(y=rep(2*0.666*0.333,2),x=c(0.5,3.5))
    #text(x=posX+0.1,y=freqHD,labels = seed)
  })
  mtext("HD",3,line = -2.5,cex=2)
  
  bbHH<-boxplot(freqDD~nClusters,data=evol[time>max(time)*propTime2plot],
                pch=16,ylim=fivenum(evol$freqDD)[c(1,5)],#+c(0,0.0001),
                ylab='',cex.lab=2,
                xlab="# clusters",
                cex.axis=cexAxis)
  # axis(2,line = -2)
  with(evol[time>max(time)*propTime2plot],{
    points(x=posX,y=freqDD,col = colSeeds[match(seed,unique(seed))],pch = 20,
           cex=cexPoints)
    lines(y=rep(0.333^2,2),x=c(0.5,3.5))
    #text(x=posX+0.1,y=freqDD,labels = seed)
  })
  mtext("DD",3,line = -2.5,cex=2)
  
  # dev.off()
  
  
  
  