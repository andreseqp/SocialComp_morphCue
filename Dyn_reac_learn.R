# Evolutionary dynamics for individual runs, reaction norms of sampled 
# individuals and their learning profiles

# Required libraries -----------------------------------------------------------

library(here)
here()
source(here("AccFunc.R"))

# Scenario to be plotted - corresponds to folders where simulations are stored

scenario<-"baselineFit"


# Load files -------------------------------------------------------------------

(listTest<-list.files(here("Simulations",paste0(scenario,"_"))))
(evolList<-grep("evol",listTest,value=TRUE))
(indList<-grep("ind",listTest,value=TRUE))

fileId<-2
evol<-fread(here("Simulations",paste0(scenario,"_"),evolList[fileId]))
pop<-fread(here("Simulations",paste0(scenario,"_"),indList[fileId]))

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
                   lowIQR.freqHH = fivenum(freqHH)[4],
                   lowIQR.freqHD = fivenum(freqHD)[2],
                   lowIQR.freqDD = fivenum(freqDD)[2],
                   m.weightAct_0=mean(WeightAct_0),
                   m.weightAct_1=mean(WeightAct_1),
                   m.weightAct_2=mean(WeightAct_2),
                   m.weightAct_3=mean(WeightAct_3),
                   m.weightAct_4=mean(WeightAct_4),
                   m.weightCrit_0=mean(WeightCrit_0),
                   m.weightCrit_1=mean(WeightCrit_1),
                   m.weightCrit_2=mean(WeightCrit_2),
                   m.weightCrit_3=mean(WeightCrit_3),
                   m.weightCrit_4=mean(WeightCrit_4)),by=time]


# Plot mean and IQRs of the genotypes and phenotypes ----------------------------

# png(here("Simulations",scenario,"hawkDoveLearn_0.1.png"),
# height = 800,width = 800)

# get the trajectories for individual runs
traitsTrajs<-dcast(evol,time~seed,value.var = c("meanAlpha","meanBeta"))
runChoi<-5

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
       col=colGenesLin,lwd=2,bty = "n")
axis(side=1,padj = -3)
# Runs' trajectory
par(new=T)
matplot(x=traitsTrajs[,time],
        y=traitsTrajs[,.SD,
                      .SDcol=paste0(c("meanAlpha_","meanBeta_"),runChoi)],
        col=colGenesLin,lty = 2,type="l",lwd=3,
        ylim=fivenum(as.matrix(evol[,.(meanAlpha,meanBeta)]))[c(1,5)],
        ylab = "",xlab="",xaxt="n",yaxt="n")

# Choose time range
gen2plot<-round(seq(1,length(unique(evolStats$time)),length.out = 5))[2:5]
# Plor the actor
seqYax<-c("s",rep("n",3))
# seqYlabUp<-c("Badge",rep("",3))
seqYlabUp<-c("P(dove)",rep("",3))
seqYlabDown<-c("Badge",rep("",3))
seqXlabDown<-c("","Quality","")
rangQual<-seq(0,1,length.out = 100)
nCenters<-5
interv<-1/nCenters
centers<-interv*0.5+interv*seq(0,nCenters-1)
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
  tempPop<-pop[time==unique(time)[genC]&seed==runChoi,.SD[.N],
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


par(plt=posPlot())
hist(pop[,Badge])
hist(pop[,Quality])
hist(pmin(pmax(rnorm(2000,mean = 0.5,sd = 0.3),0),1))


