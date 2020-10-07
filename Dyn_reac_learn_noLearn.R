# Evolutionary dynamics for averages and individual runs
# Model with reaction norms on both sides of the communication systems

# Required libraries -----------------------------------------------------------

require(here)
here()
source(here("AccFunc.R"))
require("foreach")
require("doParallel")
require("jsonlite")

# Scenario to be plotted - corresponds to folders where simulations are stored

scenario<-"test"

# extSimsDir<-#here("Simulations",paste0(scenario,"_"))
#   paste0("e:/BadgeSims/",scenario,"_")


# Load files -------------------------------------------------------------------
# Project folder
(listTest<-list.files(here("Simulations",paste0(scenario,"_"))))
# External sims folder
# (listTest<-list.files(extSimsDir,full.names = TRUE))

(evolList<-grep("evolLearn",listTest,value=TRUE))

paramName<-list.files(here("Simulations",paste0(scenario,"_")))
paramName<-grep(".json",paramName,value=TRUE)
param<-fromJSON(here("Simulations",paste0(scenario,"_"),paramName[1]))


numCores <- length(indList)
registerDoParallel(numCores)

val<-1

fileId<-val


# Project folder
evol<-fread(here("Simulations",paste0(scenario,"_"),evolList[fileId]))
# pop<-fread(here("Simulations",paste0(scenario,"_"),indList[fileId]))
# External sims folder

# evol<-fread(evolList[fileId])
# pop<-fread(indList[fileId])

Valpar<-gsub("[[:alpha:]]",gsub(".txt","",tail(strsplit(evolList[val],"_")[[1]],1)),
             replacement = "")
nampar<-gsub("[^[:alpha:]]",gsub(".txt","",tail(strsplit(evolList[val],"_")[[1]],1)),
             replacement = "")


# names(pop)[29:35]<-c("Quality", "genotype","alpha","beta","Badge","initCrit","initAct")
# names(pop)[9:28]<-do.call(rbind,as.list(sapply(0:9,FUN = function(x){
#     return(rbind(paste0("WeightAct_",x),paste0("WeightCrit_",x)))
#     })))

# Get stats from the evolutionary simulations ----------------------------------

names(evol)

cols<-c("freqGenHawks","freqGenDove",  "freqGenEval",  "freqGenLearn",
        "freqFenHawks", "freqFenDoves", "freqHH", "freqHD", "freqDD", "meanCue",
        "meanAlpha", "meanBeta", "meanFit", "meanAlphaAtt", "meanBetaAtt",
        "meanGammaAtt")

my.summary<- function(x) list(mean = mean(x), lowIQR = fivenum(x)[2], 
                              upIQR = fivenum(x)[4])

evolStats<-evol[, as.list(unlist(lapply(.SD, my.summary))), .SDcols = cols,by=time]

# Plot mean and IQRs among replicates of the genotypes and phenotypes ----------


cexAxis<-1.5

# Dynamics of genotypic traits (reaction norm) -  signaller
par(plt=posPlot(numploty = 3,idploty = 2,numplotx = 3,idplotx = 1,
                lowboundx = 8,upboundx = 93),xaxt="s",las=1)
plot(x=c(0,max(evolStats$time)),y=c(0,0),type="l",lwd=2,col="grey",
     ylim=fivenum(as.matrix(evol[,.(meanAlpha,meanBeta)]))[c(1,5)],
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
})
legend("topleft",legend = c(expression(alpha[s]),expression(beta[s])),
       col=colGenesLin,lwd=2,bty = "n")
axis(side=1,padj = -3.5,cex=0.8,at=axTicks(1),labels = axTicks(1)/100)

# Reciever

par(plt=posPlot(numploty = 3,idploty = 2,numplotx = 3,idplotx = 2,
                lowboundx = 8,upboundx = 93),xaxt="s",las=1,new=TRUE)
plot(x=c(0,max(evolStats$time)),y=c(0,0),type="l",lwd=2,col="grey",
     ylim=fivenum(as.matrix(evol[,.(meanAlphaAtt,meanBetaAtt,meanGammaAtt)]))[c(1,5)],yaxt="n",
     xlab="",ylab="",cex.lab=1.2,cex.axis=1,xaxt='n',las=1)
# polygon(x=c(evolStats$time,rev(evolStats$time)),
#         y=c(evolStats$meanFit.upIQR,rev(evolStats$meanFit.lowIQR)),
#         col=colGenesPol[1],border = NA)
polygon(x=c(evolStats$time,rev(evolStats$time)),
        y=c(evolStats$meanAlphaAtt.upIQR,rev(evolStats$meanAlphaAtt.lowIQR)),
        col=colGenesPol[1],border = NA)
polygon(x=c(evolStats$time,rev(evolStats$time)),
        y=c(evolStats$meanBetaAtt.upIQR,rev(evolStats$meanBetaAtt.lowIQR)),
        col=colGenesPol[2],border = NA)
polygon(x=c(evolStats$time,rev(evolStats$time)),
        y=c(evolStats$meanGammaAtt.upIQR,rev(evolStats$meanGammaAtt.lowIQR)),
        col=colGenesPol[3],border = NA)
with(evolStats,{
  # lines(time,m.meanFit,col=colGenesLin[1],lwd=3)
  lines(time,meanAlphaAtt.mean,col=colGenesLin[1],lwd=3)
  lines(time,meanBetaAtt.mean,col=colGenesLin[2],lwd=3)
  lines(time,meanGammaAtt.mean,col=colGenesLin[3],lwd=3)
})
# legend("topleft",legend = c("mean Fit"),
#        col=colGenesLin[1],lwd=2,bty = "n")
legend("topleft",legend = c(expression(alpha[r]),expression(beta[r]),
                            expression(gamma[r])),
       col=colGenesLin,lwd=2,bty = "n",ncol = 3)
axis(side=1,padj = -3.5,cex=0.8,at=axTicks(1),labels = axTicks(1)/100)
axis(4,cex=0.8)

# dynamics of behavioural interactions 

par(plt=posPlot(numploty = 3,idploty = 2,numplotx = 3, idplotx = 3,
                lowboundx = 8,upboundx = 93),
    xaxt="s",las=1,new=TRUE)
plot(x=c(0,max(evolStats$time)),y=c(0,0),type="l",lwd=2,col=0,
     ylim=c(0,0.6),xlab="",ylab="",yaxt="n",
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

## Plot reaction norms 


gen2plot<-round(seq(1,length(unique(evolStats$time)),length.out = 5))[2:5]
# Plor the actor
seqYax<-c("s",rep("n",3))
# seqYlabUp<-c("Badge",rep("",3))
seqYlabUp<-c("P(dove)",rep("",3))
seqYlabDown<-c("Badge",rep("",3))
seqXlabDown<-c("","Quality","")
rangQual<-seq(0,1,length.out = 10)
rivalBadge<-seq(0,1,length.out = 10)
rangx<-seq(0,1,length=100)
count<-0
genstoPrint<-round(seq(1,length(unique(evolStats$time)),length.out = 5))[2:5]
# genstoPrint<-round(seq(1,length(unique(evolStats$time))/2,length.out = 5))[2:5]
for(genC in genstoPrint){
  count<-count+1
  par(plt=posPlot(numplotx = 4,numploty = 3,idplotx = count,idploty = 3,
                  lowboundx = 8, upboundx = 93), xaxt="s",las=1,new=TRUE)
  perspTmp<-outer(X=rangQual,Y = rivalBadge,FUN = logist3d, 
                  alpha=evolStats[(time==unique(time)[genC]),meanAlphaAtt.mean],
                  beta=evolStats[(time==unique(time)[genC]),meanBetaAtt.mean],
                  gamma=evolStats[(time==unique(time)[genC]),meanGammaAtt.mean])
  
  persp(x = rangQual,y=rivalBadge,z=perspTmp,phi = 35, theta=40 ,
       xlab="Quality",ylab="Rival's badge",zlim=c(0,1),
        zlab=seqYlabUp[count],col="lightblue")
  
  
  text(x=0.6,y=0.1,labels = paste0("time=",unique(evolStats$time)[genC]))
  
  # Plot the signalers
  
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

