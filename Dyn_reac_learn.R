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

scenario<-"betCostEvol4"

extSimsDir<-here("Simulations",paste0(scenario,"_"))
  # paste0("e:/BadgeSims/",scenario,"_")



# Load files -------------------------------------------------------------------
# when program was run with internal paralellization

# Project folder
(listTest<-list.files(here("Simulations",paste0(scenario,"_")),full.names = TRUE))
# External sims folder
# (listTest<-list.files(extSimsDir,full.names = TRUE))

(evolList<-grep("evolLearn",listTest,value=TRUE))
(indList<-grep("indLearn",listTest,value=TRUE))
paramName<-list.files(here("Simulations",paste0(scenario,"_")))
# paramName<-list.files(extSimsDir,full.names = TRUE)
paramName<-grep(".json",paramName,value=TRUE)
param<-fromJSON(here("Simulations",paste0(scenario,"_"),paramName[1]))
# fromJSON(paramName)

# Choose which parameter to plot

val<-1


# Load files -------------------------------------------------------------------
# when program was run with external paralellization



# loop to produce pdfs for parameter values
numCores <- length(param$rangParam)
registerDoParallel(numCores)

foreach(val = 1:1,#length(param$rangParam),
        .packages = c("data.table","here")) %dopar% {
source(here("AccFunc.R"))

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
  
  # evol<-do.call(rbind,lapply(evolList_runs,function(x){
  #   fread(here("Simulations",paste0(scenario,"_"),x))
  # }))
  # pop<-do.call(rbind,lapply(indList_runs,function(x){
  #   fread(here("Simulations",paste0(scenario,"_"),x))
  # }))
  
  
evol<-do.call(rbind,lapply(evolList_runs,fread))
pop<-do.call(rbind,lapply(indList_runs, fread))

# temp fix to lack of name to the weights

# if(nCenters==8){
# names(evol)[35:38]<-c("WeightAct_6","WeightCrit_6","WeightAct_7",
#                       "WeightCrit_7")
# 
# names(pop)[29:32]<-c("WeightAct_6","WeightCrit_6","WeightAct_7",
#                       "WeightCrit_7")
# }

param<-fromJSON(here("Simulations",paste0(scenario,"_"),paramName[1]))

Valpar<-param$rangParam[val]

nampar<-param$namParam

if(nampar=="nCenters") {nCenters<-Valpar} else nCenters<-param$nCenters

sigSquar<-param$sigSq


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
        do.call(cbind,lapply(0:(nCenters-1), function(x){
          cbind(paste0("WeightAct_",x),paste0("WeightCrit_",x))
        }))
        # "WeightAct_0","WeightCrit_0",
        # "WeightAct_1","WeightCrit_1","WeightAct_2",  "WeightCrit_2",
        # "WeightAct_3",  "WeightCrit_3", "WeightAct_4",  "WeightCrit_4",
        # "WeightAct_5","WeightCrit_5")
)


my.summary<- function(x) list(mean = mean(x), lowIQR = fivenum(x)[2], 
                              upIQR = fivenum(x)[4])

evolStats<-evol[, as.list(unlist(lapply(.SD, my.summary))), 
                .SDcols = cols,by=time]


## Calculate proxy of mean fitness for individuals

# pop[,meanFit:=ifelse(nInteract>0,cumPayoff/nInteract,0)]


# Plot mean and IQRs among replicates of the genotypes and phenotypes ----------------------------

# pdf(paste0(extSimsDir,"/evolDyn_",nampar,Valpar,".pdf"))
pdf(here("Simulations",paste0(scenario,"_"),paste0("evolDyn_",nampar,Valpar,".pdf")))

cexAxis<-1.5

numPlotsDyn<-2

# Dynamics of genetypic traits (reaction norm)
par(plt=posPlot(numploty = 3,idploty = 2,numplotx = numPlotsDyn,idplotx = 1,
                lowboundx = 8,upboundx = 93),xaxt="s",las=1)
plot(x=c(0,max(evolStats$time)),y=c(0,0),type="l",lwd=2,col="grey",
     ylim=fivenum(as.matrix(evol[,.(meanAlpha,meanBeta#,meanInitCrit, meanInitAct
                                    )]))[c(1,5)],
     xlab="",ylab="Trait value",cex.lab=1.2,cex.axis=1,xaxt='n',las=1)
# Variation among replicates
polygon(x=c(evolStats$time,rev(evolStats$time)),
        y=c(evolStats$meanAlpha.upIQR,rev(evolStats$meanAlpha.lowIQR)),
        col=colGenesPol[1],border = NA)
polygon(x=c(evolStats$time,rev(evolStats$time)),
        y=c(evolStats$meanBeta.upIQR,rev(evolStats$meanBeta.lowIQR)),
        col=colGenesPol[2],border = NA)


with(evolStats,{
  lines(time,meanAlpha.mean,col=colGenesLin[1],lwd=3)
  lines(time,meanBeta.mean,col=colGenesLin[2],lwd=3)
  # lines(time,m.meanInCrit,col=colGenesLin[1],lwd=3)
  # lines(time,m.meanInAct,col=colGenesLin[2],lwd=3)
})
legend("topleft",legend = c(expression(alpha),expression(beta)),
       col=colGenesLin,lwd=2,bty = "n")
axis(side=1,padj = -3.5,cex=0.8,at=axTicks(1),labels = axTicks(1)/100)

# Evolutionary Dynamics of learning parameters

# par(plt=posPlot(numploty = 3,idploty = 2,numplotx = 3,idplotx = 2,
#                 lowboundx = 8,upboundx = 93),xaxt="s",las=1,new=TRUE)
# plot(x=c(0,max(evolStats$time)),y=c(0,0),type="l",lwd=2,col="grey",
#      ylim=fivenum(as.matrix(evol[,meanInitCrit,meanInitAct]))[c(1,5)],yaxt="n",
#      xlab="",ylab="",cex.lab=1.2,cex.axis=1,xaxt='n',las=1)
# polygon(x=c(evolStats$time,rev(evolStats$time)),
#         y=c(evolStats$meanFit.upIQR,rev(evolStats$meanFit.lowIQR)),
#         col=colGenesPol[1],border = NA)
# # polygon(x=c(evolStats$time,rev(evolStats$time)),
# #         y=c(evolStats$InAct.upIQR,rev(evolStats$InAct.lowIQR)),
# #         col=colGenesPol[2],border = NA)
# with(evolStats,{
#   lines(time,meanFit.mean,col=colGenesLin[1],lwd=3)
#   # lines(time,meanInAct.mean,col=colGenesLin[2],lwd=3)
# })
# legend("topleft",legend = c("mean Fit"),
#        col=colGenesLin[1],lwd=2,bty = "n")
# axis(side=1,padj = -3.5,cex=0.8,at=axTicks(1),labels = axTicks(1)/100)

# Evolutionary Dynamics of mean fitness

if(numPlotsDyn==3){

par(plt=posPlot(numploty = 3,idploty = 2,numplotx = 3,idplotx = 2,
                lowboundx = 8,upboundx = 93),xaxt="s",las=1,new=TRUE)
plot(x=c(0,max(evolStats$time)),y=c(0,0),type="l",lwd=2,col="grey",
     ylim=fivenum(as.matrix(evol[,meanFit]))[c(1,5)],yaxt="n",
     xlab="",ylab="",cex.lab=1.2,cex.axis=1,xaxt='n',las=1)
polygon(x=c(evolStats$time,rev(evolStats$time)),
        y=c(evolStats$meanFit.upIQR,rev(evolStats$meanFit.lowIQR)),
        col=colGenesPol[1],border = NA)
# polygon(x=c(evolStats$time,rev(evolStats$time)),
#         y=c(evolStats$InAct.upIQR,rev(evolStats$InAct.lowIQR)),
#         col=colGenesPol[2],border = NA)
with(evolStats,{
  lines(time,meanFit.mean,col=colGenesLin[1],lwd=3)
  # lines(time,meanInAct.mean,col=colGenesLin[2],lwd=3)
})
legend("topleft",legend = c("mean Fit"),
       col=colGenesLin[1],lwd=2,bty = "n")
axis(side=1,padj = -3.5,cex=0.8,at=axTicks(1),labels = axTicks(1)/100)
axis(4,cex=0.8,padj = -0.5)

}

# Dynamics of behavioural interactions 
par(plt=posPlot(numploty = 3,idploty = 2,numplotx = numPlotsDyn, 
                idplotx = numPlotsDyn,
                lowboundx = 8,upboundx = 93),
    xaxt="s",las=1,new=TRUE)
plot(x=c(0,max(evolStats$time)),y=c(0,0),type="l",lwd=2,col=0,
     ylim=range(evol$freqHH)+c(-0.1,0.05),xlab="",ylab="",yaxt="n",
     cex.lab=1.5,cex.axis=1,xaxt='n',las=1)
axis(side=1,padj = -3.5,cex=0.8,at=axTicks(1),labels = axTicks(1)/100)
axis(4,cex=0.8,padj = -0.5)

# Variation among replicates
polygon(x=c(evolStats$time,rev(evolStats$time)),
        y=c(evolStats$freqHH.upIQR,rev(evolStats$freqHH.lowIQR)),
        col=colIntTypesPol[1],border = NA)
polygon(x=c(evolStats$time,rev(evolStats$time)),
        y=c(evolStats$freqDD.upIQR,rev(evolStats$freqDD.lowIQR)),
        col=colIntTypesPol[2],border = NA)
with(evolStats,{
  lines(time,freqHH.mean,col=colIntTypesLin[1],lwd=3)
  lines(time,freqDD.mean,col=colIntTypesLin[2],lwd=3)
  lines(time,freqHD.mean,col=colIntTypesLin[3],lwd=3)
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
interv<-1/(nCenters-1)
centers<-interv*seq(0,nCenters-1)
rangx<-seq(0,1,length=50)
count<-0
genstoPrint<-round(seq(1,length(unique(evolStats$time)),length.out = 5))[2:5]
# genstoPrint<-round(seq(1,length(unique(evolStats$time))/2,length.out = 5))[2:5]
for(genC in genstoPrint){
  count<-count+1
  par(plt=posPlot(numplotx = 4,numploty = 3,idplotx = count,idploty = 3,
                  lowboundx = 8, upboundx = 93), xaxt="s",las=1,new=TRUE)
  tmpActweights<-evol[(time==unique(time)[genC]),.SD,
                      .SDcols=grep("WeightAct",
                                   names(evol),value = TRUE)[1:nCenters]]
  weightsAct<-sapply(seq(1,dim(tmpActweights)[1]),function(x){
      logist(totRBF(rangx,
                    centers,sigSquar,
                    as.double(
                      tmpActweights[x,])),alpha=0,beta = 1)
  })
  matplot(y=weightsAct,x=rangx,
       yaxt=seqYax[count],ylab=seqYlabUp[count],xlab="",type="l",
       lwd=2,xaxt="n",ylim=c(0,1),col=rgb(t(col2rgb(2:10)),
                                          maxColorValue = 255,alpha=80),lty=1)
 
  
  lines(logist(totRBF(rangx,centers,sigSquar,
                      as.double(evolStats[(time==unique(time)[genC]),.SD,
                                .SDcols=grep('(?=.*WeightAct)(?=.*mean)',
                                             names(evolStats),value = TRUE,
                                             perl=TRUE)[1:nCenters]])),
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
                                                   .SDcols=c("meanAlpha.mean"
                                                             ,"meanBeta.mean")]))))  
  }),col = 1,lwd=3)
}
rm(list=grep("temp",ls(),value = T))


## Plot mean and IQRs among individuals for each replicate --------------------

# png(here("Simulations",scenario,"hawkDoveLearn_0.1.png"),
# height = 800,width = 800)

# get the trajectories for individual runs
traitsTrajs<-dcast(evol,time~seed,
                   value.var = c("meanAlpha","meanBeta",#"meanFit",
                                 # "meanInitCrit",
                                 # "meanInitAct","sdInitCrit","sdInitAct",
                                  "sdAlpha","sdBeta","freqHH",
                                  "freqHD","freqDD"))
(finReps<-evol[time==max(time),seed])


for(runChoi in finReps){
 # runChoi<-1
 
# Average trajectory
par(plt=posPlot(numploty = 3,idploty = 2,numplotx = numPlotsDyn,idplotx = 1,
                lowboundx = 8, upboundx = 93),
    xaxt="s",las=1)
plot(x=c(0,max(evolStats$time)),y=c(0,0),type="l",lwd=2,col="grey",
     ylim=fivenum(as.matrix(pop[,.(alpha,beta#,initCrit,initAct
                                   )]))[c(1,5)],
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

# Evolutionary trajectory of learning parameters

# par(plt=posPlot(numploty = 3,idploty = 2,numplotx = 3,idplotx = 2,
#                 lowboundx = 8, upboundx = 93),new=TRUE,
#     xaxt="s",las=1)
# plot(x=c(0,max(evolStats$time)),y=c(0,0),type="l",lwd=2,col="grey",
#      ylim=fivenum(as.matrix(pop[seed==seedCh,cumPayoff/nInteract]))[c(1,5)],
#      xlab="",ylab="",cex.lab=1.2,cex.axis=1,xaxt='n',las=1,yaxt="n")
# axis(side=1,padj = -3)
# legend("topleft",legend = c("meanFit"),
#        col=colGenesLin[1],lwd=2,bty = "n")

# Variation among individuals
# polygon(x=c(traitsTrajs[,time],rev(traitsTrajs[,time])),
#         y=c(traitsTrajs[,apply(.SD,FUN=sum,MARGIN=1),
#                         .SDcol=c(paste0("meanInitCrit_",runChoi),
#                                  paste0("sdInitCrit_",runChoi))],
#             rev(traitsTrajs[,apply(.SD, MARGIN = 1,FUN = function(x){x[1]-x[2]}),
#                             .SDcols=c(paste0("meanInitCrit_",runChoi),
#                                       paste0("sdInitCrit_",runChoi))])),
#         col=colGenesPol[1],border = NA)
# polygon(x=c(traitsTrajs[,time],rev(traitsTrajs[,time])),
#         y=c(traitsTrajs[,apply(.SD,FUN=sum,MARGIN=1),
#                         .SDcol=c(paste0("meanInitAct_",runChoi),
#                                  paste0("sdInitAct_",runChoi))],
#             rev(traitsTrajs[,apply(.SD, MARGIN = 1,FUN = function(x){x[1]-x[2]}),
#                             .SDcols=c(paste0("meanInitAct_",runChoi),
#                                       paste0("sdInitAct_",runChoi))])),
#         col=colGenesPol[2],border = NA)

# matlines(x=traitsTrajs[,time],
#          y=traitsTrajs[,.SD,
#                        .SDcol=paste0(c("meanInitCrit_","meanInitAct_"),runChoi)],
#          col=colGenesLin,lty = 2,type="l",lwd=3)

# matlines(x=traitsTrajs[,time],
#          y=traitsTrajs[,.SD,
#                        .SDcol=paste0("meanFit_",runChoi)],
#          col=colGenesLin,lty = 2,type="l",lwd=3)

# Evolutionary trajectory of mean fitness

if(numPlotsDyn==3){

par(plt=posPlot(numploty = 3,idploty = 2,numplotx = 3,idplotx = 2,
                lowboundx = 8, upboundx = 93),new=TRUE,
    xaxt="s",las=1)
plot(x=c(0,max(evolStats$time)),y=c(0,0),type="l",lwd=2,col="grey",
     ylim=fivenum(as.matrix(pop[seed==runChoi,cumPayoff/nInteract]))[c(1,5)],
     xlab="",ylab="",cex.lab=1.2,cex.axis=1,xaxt='n',las=1,yaxt="n")
axis(side=1,padj = -3)
legend("topleft",legend = c("meanFit"),
       col=colGenesLin[1],lwd=2,bty = "n")

matlines(x=traitsTrajs[,time],
         y=traitsTrajs[,.SD,
                       .SDcol=paste0("meanFit_",runChoi)],
         col=colGenesLin,lty = 2,type="l",lwd=3)
axis(side=4)

}
# Evolutionary trajectories of behavioral interactions
par(plt=posPlot(numploty = 3,idploty = 2,numplotx = numPlotsDyn, 
                idplotx = numPlotsDyn,lowboundx = 8, upboundx = 93),
    xaxt="s",las=1,new=TRUE)
plot(x=c(0,max(evol$time)),y=c(0,0),type="l",lwd=2,col=0,
     ylim=fivenum(evol$freqHH)[c(1,5)]+c(-0.1,0.05),xlab="",ylab="",yaxt="n",
     cex.lab=1.5,cex.axis=cexAxis,xaxt='n',las=1)

matlines(x=matrix(rep(evolStats[genstoPrint,time],each=2),nrow=2),
         y=matrix(rep(c(0,1),4),nrow = 2),lty=1,col = "grey",lwd=2)

axis(side=1,padj = -3.5,cex=0.8,at=axTicks(1),labels = axTicks(1)/100,cex.axis=cexAxis)
axis(2,cex=0.8,col = colIntTypesLin[1],hadj = -1.2,tcl=0.3,cex.axis=cexAxis,
     col.axis = colIntTypesLin[1])
lines(x=traitsTrajs[,time],
      y=t(traitsTrajs[,.SD,
                      .SDcol=paste0("freqHH_",runChoi)]),
      col=colIntTypesLin,lty = 1,type="l",lwd=3)
with(evolStats,{
  lines(time,freqHH.mean,col="grey",lwd=2,lty=2)
})
par(new=T)
plot(x=c(0,max(evol$time)),y=c(0,0),type="l",lwd=2,col=0,
     ylim=fivenum(evol$freqHD)[c(1,5)]+c(-0.05,0.1),xlab="",ylab="",yaxt="n",
     cex.lab=1.5,cex.axis=cexAxis,xaxt='n',las=1)
axis(4,cex=0.8,padj = -0.5,col.axis = colIntTypesLin[3],col = colIntTypesLin[3],
     cex.axis=cexAxis)

lines(x=traitsTrajs[,time],
      y=t(traitsTrajs[,.SD,
                      .SDcol=paste0(c("freqHD_"),runChoi)]),
      col=colIntTypesLin[3],lty = 1,type="l",lwd=3)

# lines(x=c(0,max(traitsTrajs[,time])),y=c(0.6666,0.6666)^2,col=colIntTypesLin[1],lwd=2,lty=2)
# lines(x=c(0,max(traitsTrajs[,time])),y=c(0.3333,0.3333)^2,col=colIntTypesLin[2],lwd=2,lty=2)
# lines(x=c(0,max(traitsTrajs[,time])),y=c(0.6666*0.33333,0.6666*0.33333)*2+0.01,
# col=colIntTypesLin[3],lwd=2,lty=2)
legend("top",legend = c("HH","DD","HD"),ncol = 3,bg="white",box.lty="blank",
       col=colIntTypesLin,lwd=2,bty = "o",cex=0.5)
box()


# Choose which interaction to visualize
lastInt<-
pop[ seed==runChoi,max(nInteract),by=.(indId,time)][,min(V1)]
  # tail(pop[seed==runChoi,unique(nInteract)],2)[1]
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
                                          names(evol),value = TRUE)[1:nCenters]])
    tempPop<-pop[(time==unique(time)[genC]&seed==runChoi)&
                   nInteract==lastInt,.SD[.N],
               .SDcol=c(grep("WeightAct",
                             names(evol),value = TRUE)[1:nCenters],
                        "Quality","alpha","beta"),
               by=indId]
  dataIndAct<-sapply(as.list(tempPop[,indId]),
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
       yaxt=seqYax[count],ylab=seqYlabUp[count],xlab="",type="l",
       lwd=2,xaxt="n",ylim=c(0,1),col = paletteMeans(100)[
         findInterval(tempPop[,Quality],colorbreaksQual)],lty = 1)
  lines(logist(totRBF(rangx,centers,sigSquar,weightsAct),alpha=0,beta=1)~rangx,
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



 # no Inits - mean and IQRs among replicates of the genotypes and phenotypes  --------------------------

cexAxis<-1.5


pdf(paste0(extSimsDir,"/evolDyn_",nampar,Valpar,".pdf"))

png(here("Simulations",paste0(scenario,"_"),
         paste0("evolDynALL_",nampar,Valpar,".png")),
    width = 1400,height = 1000)

genstoPrint<-round(seq(1,length(unique(evolStats$time)),length.out = 5))[2:5]

# Dynamics of genetypic traits (reaction norm)
par(plt=posPlot(numploty = 3,idploty = 2,numplotx = 2,idplotx = 1,
                lowboundx = 8,upboundx = 93),xaxt="s",las=1)
plot(x=c(0,max(evolStats$time)),y=c(0,0),type="l",lwd=2,col="grey",
     ylim=fivenum(as.matrix(evol[,.(meanAlpha,meanBeta)]))[c(1,5)]+c(-1,0),
     xlab="",ylab="Trait value",cex.lab=1.3,cex.axis=cexAxis,xaxt='n',las=1)
# Variation among replicates

# grey lines to show the generations shown in the upper and lower panels

matlines(x=matrix(rep(evolStats[genstoPrint,time],each=2),nrow=2),
         y=matrix(rep(c(-2,4),4),nrow = 2),lty=1,col = "grey",lwd=2)

# variation among replicates in reaction norms parameters

polygon(x=c(evolStats$time,rev(evolStats$time)),
        y=c(evolStats$meanAlpha.mean,rev(evolStats$meanAlpha.lowIQR)),
        col=colGenesPol[1],border = NA)
polygon(x=c(evolStats$time,rev(evolStats$time)),
        y=c(evolStats$meanBeta.upIQR,rev(evolStats$meanBeta.lowIQR)),
        col=colGenesPol[2],border = NA)
# Mean value in reaction norm parameters
with(evolStats,{
  lines(time,meanAlpha.mean,col=colGenesLin[1],lwd=3)
  lines(time,meanBeta.mean,col=colGenesLin[2],lwd=3)
  # lines(time,m.meanInCrit,col=colGenesLin[1],lwd=3)
  # lines(time,m.meanInAct,col=colGenesLin[2],lwd=3)
})
legend("topleft",legend = c(expression(alpha),expression(beta)),
       col=colGenesLin,lwd=2,bty = "n",cex=2)
axis(side=1,padj = -3.5,cex=0.8,at=axTicks(1),labels = axTicks(1)/1000,
     cex.axis=cexAxis)

# Dynamics of behavioural interactions 
par(plt=posPlot(numploty = 3,idploty = 2,numplotx = 2, idplotx = 2,
                lowboundx = 8,upboundx = 93),
    xaxt="s",las=1,new=TRUE)
plot(x=c(0,max(evolStats$time)),y=c(0,0),type="l",lwd=2,col=0,
     ylim=c(min(evolStats$freqHH.lowIQR)-0.1,max(evolStats$freqHH.upIQR)+0.05),
     xlab="",ylab="",yaxt="n",
     cex.lab=1.5,cex.axis=cexAxis,xaxt='n',las=1)
axis(side=1,padj = -3.5,cex=0.8,at=axTicks(1),labels = axTicks(1)/1000,cex.axis=cexAxis)
axis(2,cex=0.8,col = colIntTypesLin[1],hadj = -1.2,tcl=0.3,
     col.axis = colIntTypesLin[1],cex.axis=cexAxis)

# Variation among replicates
polygon(x=c(evolStats$time,rev(evolStats$time)),
        y=c(evolStats$freqHH.upIQR,rev(evolStats$freqHH.lowIQR)),
        col=colIntTypesPol[1],border = NA)
with(evolStats,{
  lines(time,freqHH.mean,col=colIntTypesLin[1],lwd=3)
})

par(new=T)
plot(x=c(0,max(evolStats$time)),y=c(0,0),type="l",lwd=2,col=0,
     ylim=c(min(evolStats$freqHD.lowIQR)-0.05,max(evolStats$freqHD.upIQR)+0.1),
     xlab="",ylab="",yaxt="n",cex.lab=1.5,cex.axis=cexAxis,xaxt='n',las=1)

matlines(x=matrix(rep(evolStats[genstoPrint,time],each=2),nrow=2),
         y=matrix(rep(c(0,1),4),nrow = 2),lty=1,col = "grey",lwd=2)

axis(4,cex=0.8,col = colIntTypesLin[3],tcl=0.3,cex.axis=cexAxis,
     col.axis = colIntTypesLin[3],hadj = 0.4)
polygon(x=c(evolStats$time,rev(evolStats$time)),
        y=c(evolStats$freqHD.upIQR,rev(evolStats$freqHD.lowIQR)),
        col=colIntTypesPol[3],border = NA)
with(evolStats,{
  # lines(time,m.freqHH,col=colIntTypesLin[1],lwd=3)
  # lines(time,m.freqDD,col=colIntTypesLin[2],lwd=3)
  lines(time,freqHD.mean,col=colIntTypesLin[3],lwd=3)
  # lines(x=c(0,max(time)),y=c(0.6666,0.6666)^2,col=colIntTypesLin[1],lwd=2,lty=2)
  # lines(x=c(0,max(time)),y=c(0.3333,0.3333)^2,col=colIntTypesLin[2],lwd=2,lty=2)
  # lines(x=c(0,max(time)),y=c(0.6666*0.33333,0.6666*0.33333)*2+0.01,
  #       col=colIntTypesLin[3],lwd=2,lty=2)
})

legend("top",legend = c("HH","DD","HD"),ncol = 3,bg="white",box.lty="blank",
       col=colIntTypesLin,lwd=2,bty = "o",cex=2)
box()

# Dynamics of behavioural types
# par(plt=posPlot(numploty = 3,idploty = 3),xaxt="s",las=1,new=TRUE)
# plot(x=c(0,max(evolStats$time)),y=c(0,0),type="l",lwd=2,col=0,
#      ylim=c(0,1),xlab="",ylab="behaviour \n frequency",
#      cex.lab=1.5,cex.axis=cexAxis,xaxt='n',las=1)
# 
# # Variation among replicates
# polygon(x=c(evolStats$time,rev(evolStats$time)),
#         y=c(evolStats$freqFenHawk.upIQR,rev(evolStats$freqFenHawk.lowIQR)),
#         col=colIntTypesPol[1],border = NA)
# polygon(x=c(evolStats$time,rev(evolStats$time)),
#         y=c(evolStats$freqFenDove.upIQR,rev(evolStats$freqFenDove.lowIQR)),
#         col=colIntTypesPol[2],border = NA)
# with(evolStats,{
#   lines(time,freqFenHawk.mean,col=colIntTypesLin[1],lwd=3)
#   lines(time,freqFenDove.mean,col=colIntTypesLin[2],lwd=3)
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
interv<-1/(nCenters-1)
centers<-interv*seq(0,nCenters-1)
rangx<-seq(0,1,length=100)
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
                  centers,sigSquar,
                  as.double(
                    tmpActweights[x,])),alpha=0,beta = 1)
  })
  matplot(y=weightsAct,x=rangx,
          yaxt=seqYax[count],ylab=seqYlabUp[count],xlab="",type="l",cex.lab=2,
          lwd=2,xaxt="n",ylim=c(-0.05,1),col=rgb(t(col2rgb(2:10)),
                                             maxColorValue = 255,alpha=80),lty=1)
  lines(logist(totRBF(rangx,centers,sigSquar,
                      as.double(evolStats[(time==unique(time)[genC]),.SD,
                                          .SDcols=grep('(?=.*WeightAct)(?=.*mean)',
                                                       names(evolStats),value = TRUE,
                                                       perl=TRUE)])),
               alpha=0,beta=1)~rangx,
        col = "black",lwd=2)
  text(x=0.5,y=-0.01,labels = paste0("time=",unique(evolStats$time)[genC]/1000),
       cex=2)
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
  ylim=c(0,1),lwd=2,yaxt=seqYax[count],xaxt="s",cex.axis=cexAxis,
  col=rgb(t(col2rgb(2:10)),maxColorValue = 255,alpha=80),lty=1,cex.lab=2)
  lines(x=rangx,y=sapply(rangx, function(y){
    do.call(logist,as.list(c(y,as.double(evolStats[(time==unique(time)[genC]),.SD,
                                                   .SDcols=c("meanAlpha.mean"
                                                             ,"meanBeta.mean")]))))  
  }),col = 1,lwd=3)
}
rm(list=grep("temp",ls(),value = T))

dev.off()

## Plot mean and IQRs among individuals for each replicate --------------------

# png(here("Simulations",scenario,"hawkDoveLearn_0.1.png"),
# height = 800,width = 800)


# get the trajectories for individual runs
traitsTrajs<-dcast(evol,time~seed,
                   value.var = c("meanAlpha","meanBeta",
                                 # "meanInitCrit",
                                 # "meanInitAct","sdInitCrit","sdInitAct",
                                 "sdAlpha","sdBeta","freqHH",
                                 "freqHD","freqDD"))
finReps<-evol[time==max(time),seed]


# for(runChoi in finReps){

genstoPrint<-round(seq(1,length(unique(evolStats$time)),length.out = 5))[2:5]

runChoi<-0

png(here("Simulations",paste0(scenario,"_"),
         paste0("evolDyn",runChoi,"_",nampar,Valpar,".png")),
    width = 1400,height = 1000)

  
  # Average trajectory
  par(plt=posPlot(numploty = 3,idploty = 2,numplotx = 2,idplotx = 1,
                  lowboundx = 8, upboundx = 93),
      xaxt="s",las=1)
  plot(x=c(0,max(evolStats$time)),y=c(0,0),type="l",lwd=2,col="grey",
       ylim=fivenum(as.matrix(pop[,.(alpha,beta)]))[c(1,5)]+c(-0.5,0),
       xlab="",ylab="Trait value",cex.lab=1.3,cex.axis=cexAxis,xaxt='n',las=1)
  axis(side=1,padj = -3,at = axTicks(1),labels = axTicks(1)/1000,cex.axis=cexAxis)
  legend("topleft",legend = c(expression(alpha),expression(beta)),
         col=colGenesLin,lwd=2,bty = "n")
  
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
  
  
  # Evolutionary trajectories of behavioral interactions
  par(plt=posPlot(numploty = 3,idploty = 2,numplotx = 2, idplotx = 2,
                  lowboundx = 8, upboundx = 93),
      xaxt="s",las=1,new=TRUE)
  plot(x=c(0,max(evol$time)),y=c(0,0),type="l",lwd=2,col=0,
       ylim=fivenum(evol$freqHH)[c(1,5)]+c(-0.1,0.05),xlab="",ylab="",yaxt="n",
       cex.lab=1.5,cex.axis=cexAxis,xaxt='n',las=1)
  
  matlines(x=matrix(rep(evolStats[genstoPrint,time],each=2),nrow=2),
           y=matrix(rep(c(0,1),4),nrow = 2),lty=1,col = "grey",lwd=2)
  
  axis(side=1,padj = -3.5,cex=0.8,at=axTicks(1),labels = axTicks(1)/100,cex.axis=cexAxis)
  axis(2,cex=0.8,col = colIntTypesLin[1],hadj = -1.2,tcl=0.3,cex.axis=cexAxis,
       col.axis = colIntTypesLin[1])
  lines(x=traitsTrajs[,time],
           y=t(traitsTrajs[,.SD,
                         .SDcol=paste0("freqHH_",runChoi)]),
           col=colIntTypesLin,lty = 1,type="l",lwd=3)
  with(evolStats,{
    lines(time,freqHH.mean,col="grey",lwd=2,lty=2)
  })
  par(new=T)
  plot(x=c(0,max(evol$time)),y=c(0,0),type="l",lwd=2,col=0,
       ylim=fivenum(evol$freqHD)[c(1,5)]+c(-0.05,0.1),xlab="",ylab="",yaxt="n",
       cex.lab=1.5,cex.axis=cexAxis,xaxt='n',las=1)
  axis(4,cex=0.8,padj = -0.5,col.axis = colIntTypesLin[3],col = colIntTypesLin[3],
       cex.axis=cexAxis)
  
  lines(x=traitsTrajs[,time],
           y=t(traitsTrajs[,.SD,
                         .SDcol=paste0(c("freqHD_"),runChoi)]),
           col=colIntTypesLin[3],lty = 1,type="l",lwd=3)
  
  # lines(x=c(0,max(traitsTrajs[,time])),y=c(0.6666,0.6666)^2,col=colIntTypesLin[1],lwd=2,lty=2)
  # lines(x=c(0,max(traitsTrajs[,time])),y=c(0.3333,0.3333)^2,col=colIntTypesLin[2],lwd=2,lty=2)
  # lines(x=c(0,max(traitsTrajs[,time])),y=c(0.6666*0.33333,0.6666*0.33333)*2+0.01,
        # col=colIntTypesLin[3],lwd=2,lty=2)
  legend("top",legend = c("HH","DD","HD"),ncol = 3,bg="white",box.lty="blank",
         col=colIntTypesLin,lwd=2,bty = "o",cex=2)
  box()
  
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
  seqXlabDown<-c("","Quality",paste0("seed: ",runChoi))
  rangQual<-seq(0,1,length.out = 50)
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
                                       centers,sigSquar,
                                       as.double(
                                         tempPop[indId==x,.SD,
                                                 .SDcol=grep("WeightAct",
                                                             names(tempPop),
                                                             value = TRUE)
                                                 ])),alpha=0,beta = 1)})
    matplot(x=rangx,y=dataIndAct,
            yaxt=seqYax[count],ylab=seqYlabUp[count],xlab="",type="l",cex.lab=2,
            lwd=2,xaxt="n",ylim=c(-0.05,1),col = paletteMeans(100)[
              findInterval(tempPop[,Quality],colorbreaksQual)],lty = 1,cex.axis=cexAxis)
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
      xlab=seqXlabDown[count],ylab=seqYlabDown[count],ylim=c(0,1),
      lwd=2,yaxt=seqYax[count],xaxt="s",cex.lab=2)
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

## 



## Plot all the generations/samples of the reaction norms ----------------------

runChoi<-6

# Choose which interaction to visualize
lastInt<-300#tail(pop[seed==runChoi,unique(nInteract)],2)[1]
  pop[seed==runChoi,max(nInteract),by=time][,min(V1)]
# to get last interaction: tail(pop[,unique(nInteract)],1)
# Choose time range
gen2plot<-round(seq(1,length(unique(evolStats$time)),length.out = 20))
# Plot the signaller
png(here("Simulations",paste0(scenario,"_"),
         paste0("reacNorm_snaps",runChoi,"_",nampar,Valpar,".png")),
    width = 1400,height = 1000)

par(mfrow=c(1,1))
seqYax<-c("s",rep("n",4))
# seqXax<-c(rep("n",4),"s")
# seqYlabUp<-c("Badge",rep("",3))
seqYlabDown<-c("","Badge",rep("",3))
seqXlabDown<-c("","Quality",paste0("seed: ",runChoi),"","")
rangQual<-seq(0,1,length.out = 100)
interv<-1/(nCenters-1)
centers<-interv*seq(0,nCenters-1)
rangx<-seq(0,1,length=10)
countX<-0;countY<-4
plot.new()
for(genC in gen2plot){
  if(countX==5)  {countX<-0;countY<-countY-1}
  countX<-countX+1
  par(plt=posPlot(numplotx = 5,numploty = 4,idplotx = countX,idploty = countY,
                  lowboundx = 8, upboundx = 93),
      xaxt="s",las=1,new=TRUE)
  tempPop<-pop[(time==unique(time)[genC]&seed==runChoi)&nInteract==lastInt,.SD[.N],
               .SDcol=c(grep("WeightAct",
                             names(evol),value = TRUE),"Quality","alpha","beta"),
               by=indId]
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
    xlab="",ylab="",ylim=c(0,1.1),lty=1,
    lwd=1,yaxt=seqYax[countX],xaxt=seqYax[countY])
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
  text(x = 0.5,y=1.06,labels = paste0("t =",genC),cex=1)
  if(countY==1) mtext(seqXlabDown[countX],1,line = 3,cex=2)
  if(countX==1) mtext(seqYlabDown[countY],2,line = 2.5,cex=1.5,las=0)
}
rm(list=grep("temp",ls(),value = T))

dev.off()

# frecuency distribution change along evol. time  ------------------------------

for(runChoi in finReps){
runChoi<-10
  
tempop<-pop[(seed==runChoi&nInteract==500)]
 

png(here("Simulations",paste0(scenario,"_"),
         paste0("evolDist",runChoi,"_",nampar,Valpar,".png")),
    width = 1400,height = 1000)



par(plt=posPlot(1,3,1,3),mfrow=c(1,1),xaxt="n",las=1)
betaDist<-evolDist(indData = tempop,variable = "beta",nbins = 20,pal = pal_dist,
         nlevels=10,cexAxis = 1.5,xlab="",ylab ="")
mtext( expression(beta),2,4.5,cex=2.5)
par(plt=posPlot(1,3,1,2),mfrow=c(1,1),xaxt="n",las=1,new=TRUE)
evolDist(indData = tempop,variable = "alpha",nbins = 20,pal = pal_dist,
         nlevels=10,cexAxis = 1.5,xlab="",ylab = "",
         keyTitle = "log(rel. \n freq.)")
mtext( expression(alpha),2,4.5,cex=2.5)
par(plt=posPlot(1,3,1,1),mfrow=c(1,1),xaxt="s",las=1,new=TRUE)
evolDist(indData = tempop,variable = "Badge",nbins = 20,pal = pal_dist,
         nlevels=10,cexAxis = 1.5,xlab="generations",ylab = "")
mtext( "Badge",2,3.5,cex=1.5)

dev.off()


# evolDist(indData = tempop,variable = "Quality",nbins = 20,pal = pal_dist,
#          nlevels=5)

png(here("Simulations",paste0(scenario,"_"),
         paste0("corrAlpBet",runChoi,"_",nampar,Valpar,".png")),
    width = 1400,height = 700)
par(plt=posPlot())
plot(data=pop[seed==runChoi&time>15000],alpha~beta,ylab="",
     xlab="", pch=20,cex.lab=3,cex.axis=3,las=1,cex=4)
mtext(text = expression(alpha),side = 2,line = 3,las=1,cex=3)
mtext(text = expression(beta),side = 1,line = 3,cex = 3)
dev.off()

}

# Overall variation ------------------------------------------------------------ 






par(mfrow=c(1,1))
seqYax<-c("s",rep("n",4))
# seqXax<-c(rep("n",4),"s")
# seqYlabUp<-c("Badge",rep("",3))
seqYlabDown<-c("",expression(Badge),rep("",3))
seqXlabDown<-c("",expression(Quality),"","")
cX<-0;cY<-nY
plot.new()
for(cSeed in pop[,unique(seed)]){
  if(cX==nX)  {cX<-0;cY<-cY-1}
  cX<-cX+1
  par(plt=posPlot(numplotx = nX,numploty = nY,idplotx = cX,idploty = cY),
      xaxt="s",las=1,new=TRUE)
  plot(data=pop[seed==cSeed][time>max(time)/2],Badge~Quality,ylab="",
       xlab="", pch=20,cex.lab=3,cex.axis=3,las=1,cex=2,
       ylim=range(pop[time>max(time)/2,Badge])+c(0,0.9),
       xlim=range(pop[time>max(time)/2,Quality]),
       yaxt=seqYax[cX],xaxt=seqYax[cY])
  lines(x=c(0,0),y=range(pop[time>max(time)/2,Badge]),col="grey",
        lwd=2)
  lines(y=c(0,0),x=range(pop[time>max(time)/2,Quality]),col="grey",
        lwd=2)
  text(x = mean(range(pop[time>max(time)/2,Quality])),
       y = range(pop[time>max(time)/2,Badge])[2]+0.45,
       labels = paste0("seed=",cSeed),cex=1.5)
  if(cY==1) mtext(seqXlabDown[cX],1,line = 3.5,cex=3)
  if(cX==1) mtext(seqYlabDown[cY],2,line = 3,cex=3,las=1)
  # mtext(text = expression(Badge),side = 2,line = 3,las=1,cex=3)
  # mtext(text = expression(Quality),side = 1,line = 2,5,cex = 3)
}


# Visuali classify the  runs in the number of clusters that emerge ------------
seedClass<-data.table(seeds=pop[,unique(seed)],nClusters=c(3,2,3,3,3,2,1,2,3,2))
seedClass<-data.table(seeds=pop[,unique(seed)],nClusters=c(1,2,3,4,2,2,2,2,2,3))
seedClass<-data.table(seeds=pop[,unique(seed)],nClusters=c(3,2,3,1,2,1,2,3))




## Selection -------------------------------------------------------------------

# Choose which interaction to visualize
LastInt<-pop[,max(nInteract),by=.(time,seed,indId)][,min(V1)]
popLastInt<-pop[nInteract==lastInt]

runChoi<-6

gen2plot<-round(seq(1,length(unique(evolStats$time)),length.out = 20))


## on Alpha

# png(here("Simulations",paste0(scenario,"_"),
#          paste0("reacNorm_snaps",runChoi,"_",nampar,Valpar,".png")),
#     width = 1400,height = 1000)

par(mfrow=c(1,1))
seqYax<-c("s",rep("n",4))
# seqXax<-c(rep("n",4),"s")
# seqYlabUp<-c("Badge",rep("",3))
seqYlabDown<-c("","meanFit",rep("",3))
seqXlabDown<-c("","alpha",paste0("seed: ",runChoi),"","")
rangalpha<-seq(range(popLastInt[,alpha])[1],range(popLastInt[,alpha])[2],
               length.out = 50)
countX<-0;countY<-4;plot.new()
for(genC in gen2plot){
  if(countX==5)  {countX<-0;countY<-countY-1}
  countX<-countX+1
  par(plt=posPlot(numplotx = 5,numploty = 4,idplotx = countX,idploty = countY,
                  lowboundx = 8, upboundx = 93),
      xaxt="s",las=1,new=TRUE)
  tempPop<-popLastInt[(time==unique(time)[genC]&seed==runChoi),.SD[.N],
               .SDcol=c("Quality","alpha","beta","meanFit"),
               by=indId]
  plot(x=tempPop[,alpha],y=tempPop[,meanFit],col = paletteMeans(100)[
    findInterval(tempPop[,Quality],colorbreaksQual)],type='p',
    xlab="",ylab="",lty=1,ylim=range(popLastInt[,meanFit])+c(0,0.2),
    xlim=range(popLastInt[,alpha]),
    pch=20,yaxt=seqYax[countX],xaxt=seqYax[countY])
  text(y = range(popLastInt[,meanFit])[2]+0.1 ,x=fivenum(popLastInt[,alpha])[2],
       labels = paste0("t =",genC),cex=1)
  if(var(tempPop$alpha)>0){
    linMod1<-lm(data = tempPop,meanFit~alpha+Quality)
    linMod2<-lm(data = tempPop,meanFit~alpha+Quality+I(alpha^2))
    anoMod<-anova(linMod1,linMod2)
    if(anoMod$`Pr(>F)`[2]<0.05){
      lines(x=rangalpha,y=sapply(rangalpha, FUN=function(x){
        linMod2$coefficients[1]+linMod2$coefficients[2]*x+linMod2$coefficients[3]*0.5+
          linMod2$coefficients[4]*x^2}),
        col="blue")
    }
    else lines(x=rangalpha,y=sapply(rangalpha, FUN=function(x){
      linMod1$coefficients[1]+linMod1$coefficients[2]*x+linMod1$coefficients[3]*0.5}),
      col="red")
    text(x = fivenum(popLastInt[,alpha])[4],y=range(popLastInt[,meanFit])[2]+0.1,
         labels = round(anoMod$`Pr(>F)`[2],2))
  }
  if(countY==1) mtext(seqXlabDown[countX],1,line = 3,cex=2)
  if(countX==1) mtext(seqYlabDown[countY],2,line = 2.5,cex=1.5,las=0)
}
rm(list=grep("temp",ls(),value = T))

# Beta


# Plot the signaller
png(here("Simulations",paste0(scenario,"_"),
         paste0("reacNorm_snaps",runChoi,"_",nampar,Valpar,".png")),
    width = 1400,height = 1000)

par(mfrow=c(1,1))
seqYax<-c("s",rep("n",4))
# seqXax<-c(rep("n",4),"s")
# seqYlabUp<-c("Badge",rep("",3))
seqYlabDown<-c("","meanFit",rep("",3))
seqXlabDown<-c("","beta",paste0("seed: ",runChoi),"","")
rangbeta<-seq(range(popLastInt[,beta])[1],range(popLastInt[,beta])[2],
               length.out = 50)
countX<-0;countY<-4;plot.new()
for(genC in gen2plot){
  if(countX==5)  {countX<-0;countY<-countY-1}
  countX<-countX+1
  par(plt=posPlot(numplotx = 5,numploty = 4,idplotx = countX,idploty = countY,
                  lowboundx = 8, upboundx = 93),
      xaxt="s",las=1,new=TRUE)
  tempPop<-popLastInt[(time==unique(time)[genC]&seed==runChoi),.SD[.N],
                      .SDcol=c("Quality","alpha","beta","meanFit"),
                      by=indId]
  plot(x=tempPop[,beta],y=tempPop[,meanFit],col = paletteMeans(100)[
    findInterval(tempPop[,Quality],colorbreaksQual)],type='p',
    xlab="",ylab="",lty=1,ylim=range(popLastInt[,meanFit])+c(0,0.2),
    xlim=range(popLastInt[,beta]),
    pch=20,yaxt=seqYax[countX],xaxt=seqYax[countY])
  text(y = range(popLastInt[,meanFit])[2]+0.1 ,x=fivenum(popLastInt[,beta])[2],
       labels = paste0("t =",genC),cex=1)
  if(var(tempPop$beta)>0){
    linMod1<-lm(data = tempPop,meanFit~beta+Quality)
    linMod2<-lm(data = tempPop,meanFit~beta+Quality+I(beta^2))
    anoMod<-anova(linMod1,linMod2)
    if(anoMod$`Pr(>F)`[2]<0.05){
      lines(x=rangbeta,y=sapply(rangbeta, FUN=function(x){
        linMod2$coefficients[1]+linMod2$coefficients[2]*x+linMod2$coefficients[3]*0.5+
          linMod2$coefficients[4]*x^2}),
        col="blue")
    }
    else lines(x=rangbeta,y=sapply(rangbeta, FUN=function(x){
      linMod1$coefficients[1]+linMod1$coefficients[2]*x+linMod1$coefficients[3]*0.5}),
      col="red")
    text(x = fivenum(popLastInt[,beta])[4],y=range(popLastInt[,meanFit])[2]+0.1,
         labels = round(anoMod$`Pr(>F)`[2],2))
  }
  if(countY==1) mtext(seqXlabDown[countX],1,line = 3,cex=2)
  if(countX==1) mtext(seqYlabDown[countY],2,line = 2.5,cex=1.5,las=0)
}
rm(list=grep("temp",ls(),value = T))








histBadge<-hist(tempop[time==20000,Badge])
histBeta<-hist(tempop[time==20000,beta],breaks = 30,xlim = c(-3,3))
hist(tempop[,alpha])
hist(tempop[,Quality])


# png(here("Simulations",scenario,"hawkDoveLearn_0.1.png"),
# height = 800,width = 800)

# get the trajectories for individual runs
# traitsTrajs<-dcast(evol,time~seed,value.var = c("meanAlpha","meanBeta",
#                                                 "sdAlpha","sdBeta"))
# runChoi<-2
# 
# # Average trajectory
# par(plt=posPlot(numploty = 3,idploty = 2),xaxt="s",las=1)
# plot(x=c(0,max(evolStats$time)),y=c(0,0),type="l",lwd=2,col="grey",
#      ylim=fivenum(as.matrix(pop[,.(alpha,beta)]))[c(1,5)],
#      xlab="",ylab="Trait \n value",cex.lab=1.5,cex.axis=1,xaxt='n',las=1)
# # Variation among replicates
# polygon(x=c(evolStats$time,rev(evolStats$time)),
#         y=c(evolStats$upIQR.alpha,rev(evolStats$lowIQR.alpha)),
#         col=colGenesPol[1],border = NA)
# polygon(x=c(evolStats$time,rev(evolStats$time)),
#         y=c(evolStats$upIQR.beta,rev(evolStats$lowIQR.beta)),
#         col=colGenesPol[2],border = NA)
# with(evolStats,{
#   lines(time,m.meanAlpha,col=colGenesLin[1],lwd=3)
#   lines(time,m.meanBeta,col=colGenesLin[2],lwd=3)
# })
# legend("topleft",legend = c(expression(alpha),expression(beta)),
#        col=colGenesLin,lwd=2,bty = "n")
# axis(side=1,padj = -3)
# # Runs' trajectory
# # polygon(x=c(traitsTrajs[,time],rev(traitsTrajs[,time])),
# #         y=c(traitsTrajs[,apply(.SD,FUN=sum,MARGIN=1),
# #                         .SDcol=c(paste0("meanAlpha_",runChoi),
# #                                  paste0("sdAlpha_",runChoi))],
# #             rev(traitsTrajs[,apply(.SD, MARGIN = 1,FUN = function(x){x[1]-x[2]}),
# #                             .SDcols=c(paste0("meanAlpha_",runChoi),
# #                                       paste0("sdAlpha_",runChoi))])),
# #         col=colGenesPol[1],border = NA)
# # polygon(x=c(traitsTrajs[,time],rev(traitsTrajs[,time])),
# #         y=c(traitsTrajs[,apply(.SD,FUN=sum,MARGIN=1),
# #                         .SDcol=c(paste0("meanBeta_",runChoi),
# #                                  paste0("sdBeta_",runChoi))],
# #             rev(traitsTrajs[,apply(.SD, MARGIN = 1,FUN = function(x){x[1]-x[2]}),
# #                             .SDcols=c(paste0("meanBeta_",runChoi),
# #                                       paste0("sdBeta_",runChoi))])),
# #         col=colGenesPol[2],border = NA)
# 
# matlines(x=traitsTrajs[,time],
#         y=traitsTrajs[,.SD,
#                       .SDcol=paste0(c("meanAlpha_","meanBeta_"),runChoi)],
#         col=colGenesLin,lty = 2,type="l",lwd=3)
# 
# 
# # Choose which interaction to visualize
# lastInt<-2000
# tail(pop[seed==runChoi,unique(nInteract)],2)[1]
# # to get last interaction: tail(pop[,unique(nInteract)],1)
# # Choose time range
# gen2plot<-round(seq(1,length(unique(evolStats$time)),length.out = 5))[2:5]
# # Plor the actor
# seqYax<-c("s",rep("n",3))
# # seqYlabUp<-c("Badge",rep("",3))
# seqYlabUp<-c("P(dove)",rep("",3))
# seqYlabDown<-c("Badge",rep("",3))
# seqXlabDown<-c("","Quality","")
# rangQual<-seq(0,1,length.out = 100)
# interv<-1/(nCenters-1)
# centers<-interv*seq(0,nCenters-1)
# rangx<-seq(0,1,length=1000)
# count<-0
# for(genC in round(seq(1,length(unique(evolStats$time)),length.out = 5))[2:5]){
#   count<-count+1
#   par(plt=posPlot(numplotx = 4,numploty = 3,idplotx = count,idploty = 3),
#       xaxt="s",las=1,new=TRUE)
#   weightsAct<-as.double(evol[(time==unique(time)[genC])&seed==runChoi,.SD,
#                                    .SDcols=grep("WeightAct",
#                                                 names(evol),value = TRUE)])
#   plot(logist(totRBF(rangx,centers,0.01,weightsAct),alpha=0,beta=1)~rangx,
#        yaxt=seqYax[count],ylab=seqYlabUp[count],xlab="",type="l",
#        lwd=3,xaxt="n",ylim=c(0,1))
#   tempPop<-pop[(time==unique(time)[genC]&seed==runChoi)&nInteract==lastInt,.SD[.N],
#                .SDcol=c(grep("WeightAct",
#                            names(evol),value = TRUE),"Quality","alpha","beta"),
#                by=indId]
#   dataIndAct<-sapply(as.list(tempPop[,indId]),
#                        function(x){x=
#                          logist(totRBF(rangx,
#                                        centers,0.01,
#                                        as.double(
#                                          tempPop[indId==x,.SD,
#                                                  .SDcol=grep("WeightAct",
#                                                              names(tempPop),
#                                                              value = TRUE)
#                                                           ])),alpha=0,beta = 1)})
#   matlines(x=rangx,y=dataIndAct,col = paletteMeans(100)[
#     findInterval(tempPop[,Quality],colorbreaksQual)],lwd=2)
#   text(x=0.5,y=0.1,labels = paste0("time=",unique(evolStats$time)[genC]))
#   par(plt=posPlot(numplotx = 4,numploty = 3,idplotx = count,idploty = 1),
#       las=1,new=TRUE)
#   plot(sapply(rangx,
#                FUN=function(x){
#                  do.call(logist,
#                          as.list(c(x,
#                                    as.double(evol[(time==unique(time)[genC])&
#                                                     seed==runChoi,
#                                                       .SD,
#                                                       .SDcols=c("meanAlpha"
#                                                                 ,"meanBeta")]))))})
#         ~rangx,type='l',col=1,
#        xlab=seqXlabDown[count],ylab=seqYlabDown[count],ylim=c(0,1),
#        lwd=3,yaxt=seqYax[count],xaxt="s")
#   dataIndReact<-sapply(as.list(tempPop[,indId]),
#                       function(x){x=
#                         sapply(rangx, function(y)
#                         do.call(logist,as.list(c(y,as.double(tempPop[indId==x,.SD,
#                                                 .SDcol=c("alpha","beta")])))))})
#   matlines(x=rangx,y=dataIndReact,col = paletteMeans(100)[
#     findInterval(tempPop[,Quality],colorbreaksQual)],lwd=2)
# }
# rm(list=grep("temp",ls(),value = T))
# 
# # Plot mean and IQRs of the genotypes and phenotypes ----------------------------
# 
# # png(here("Simulations",scenario,"hawkDoveLearn_0.1.png"),
# # height = 800,width = 800)
# 
# # get the trajectories for individual runs
# traitsTrajs<-dcast(evol,time~seed,value.var = c("meanAlpha","meanBeta",
#                                                 "sdAlpha","sdBeta"))
# # Average trajectory
# par(plt=posPlot(numploty = 3,idploty = 2),xaxt="s",las=1)
# plot(x=c(0,max(evolStats$time)),y=c(0,0),type="l",lwd=2,col="grey",
#      ylim=fivenum(as.matrix(evol[,.(meanAlpha,meanBeta)]))[c(1,5)],
#      xlab="",ylab="Trait \n value",cex.lab=1.5,cex.axis=1,xaxt='n',las=1)
# polygon(x=c(evolStats$time,rev(evolStats$time)),
#         y=c(evolStats$upIQR.alpha,rev(evolStats$lowIQR.alpha)),
#         col=colGenesPol[1],border = NA)
# polygon(x=c(evolStats$time,rev(evolStats$time)),
#         y=c(evolStats$upIQR.beta,rev(evolStats$lowIQR.beta)),
#         col=colGenesPol[2],border = NA)
# with(evolStats,{
#   lines(time,m.meanAlpha,col=colGenesLin[1],lwd=3)
#   lines(time,m.meanBeta,col=colGenesLin[2],lwd=3)
# })
# legend("topleft",legend = c(expression(alpha),expression(beta)),
#        col=colGenesLin,lwd=2,bty = "n",lty = c(2,1))
# axis(side=1,padj = -3)
# matlines(x=traitsTrajs[,time],
#         y=traitsTrajs[,.SD,
#                       .SDcol=grep("meanAlpha_",names(traitsTrajs),value = TRUE)],
#         col=colRuns,lty = 2,type="l",lwd=3)
# matlines(x=traitsTrajs[,time],
#         y=traitsTrajs[,.SD,
#                       .SDcol=grep("meanBeta_",names(traitsTrajs),value = TRUE)],
#         col=colRuns,lty = 1,type="l",lwd=3)
# legend("top",legend = 0:10,col = colRuns,pch=20,bty = "n",ncol = 5)
# # Choose time range
# gen2plot<-round(seq(1,length(unique(evolStats$time)),length.out = 5))[2:5]
# # Plor the actor
# seqYax<-c("s",rep("n",3))
# # seqYlabUp<-c("Badge",rep("",3))
# seqYlabUp<-c("P(dove)",rep("",3))
# seqYlabDown<-c("Badge",rep("",3))
# seqXlabDown<-c("","Quality","")
# rangQual<-seq(0,1,length.out = 100)
# interv<-1/(nCenters-1)
# centers<-interv*seq(0,nCenters-1)
# rangx<-seq(0,1,length=1000)
# count<-0
# for(genC in round(seq(1,length(unique(evolStats$time)),length.out = 5))[2:5]){
#   count<-count+1
#   tempPop<-pop[time==unique(time)[genC]&nInteract==lastInt,.SD[.N],
#                .SDcol=c(grep("WeightAct",
#                              names(evol),value = TRUE),"Quality",
#                         "alpha","beta","seed"),
#                by=indId]
#   dataIndAct<-sapply(as.list(tempPop[,indId]),
#                      function(x){x=
#                        logist(totRBF(rangx,
#                                      centers,0.01,
#                                      as.double(
#                                        tempPop[indId==x,.SD,
#                                                .SDcol=grep("WeightAct",
#                                                            names(tempPop),
#                                                            value = TRUE)
#                                                ])),alpha=0,beta = 1)})
#   
#   par(plt=posPlot(numplotx = 4,numploty = 3,idplotx = count,idploty = 3),
#       xaxt="s",las=1,new=TRUE)
#   matplot(x=rangx,y=dataIndAct,col = colRuns[tempPop[,seed]],lwd=1,lty = 1,
#        yaxt=seqYax[count],ylab=seqYlabUp[count],xlab="",type="l",
#        xaxt="n",ylim=c(0,1))
#   text(x=0.5,y=0.1,labels = paste0("time=",unique(evolStats$time)[genC]))
#   par(plt=posPlot(numplotx = 4,numploty = 3,idplotx = count,idploty = 1),
#       las=1,new=TRUE)
#   matplot(x=rangx,y=dataIndReact,col = colRuns[tempPop[,seed]],lwd=1,type='l',
#        xlab=seqXlabDown[count],ylab=seqYlabDown[count],ylim=c(0,1),
#        yaxt=seqYax[count],xaxt="s")
#   dataIndReact<-sapply(as.list(tempPop[,indId]),
#                        function(x){x=
#                          sapply(rangx, function(y)
#                            do.call(logist,as.list(c(y,as.double(tempPop[indId==x,.SD,
#                                                                         .SDcol=c("alpha","beta")])))))})
#   }
# rm(list=grep("temp",ls(),value = T))
# 
# 
# 


