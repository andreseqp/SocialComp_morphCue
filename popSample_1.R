# Variation in the learning parameters for a sample of the population along the
# evolutionary dynamics 

# Required libraries -----------------------------------------------------------

library(here)
here()
source(here("AccFunc.R"))



# Scenario to be plotted - corresponds to folders where simulations are stored

scenario<-"learHonest_/alphaAct"


# Load files -------------------------------------------------------------------

(listTest<-list.files(here("Simulations",paste0(scenario,"_"))))
(evolList<-grep("evol",listTest,value=TRUE))
(indList<-grep("ind",listTest,value=TRUE))

fileId<-3
evol<-fread(here("Simulations",paste0(scenario,"_"),evolList[fileId]))
pop<-fread(here("Simulations",paste0(scenario,"_"),indList[fileId]))



# Extract means and IQR for the dynamic variables ------------------------------

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

# Extract means and IQR for the dynamic variables ------------------------------

popStats<-pop[, as.list(unlist(lapply(.SD, 
                                      function(x) list(m = mean(x),
                                                       upIQR = fivenum(x)[4],
                                                       downIQR = fivenum(x)[2]
                                      )))),
              by = time, 
              .SDcols=c("Quality","alpha","beta","Badge","nInteract","WeightAct_0",
                        "WeightAct_1","WeightAct_2","WeightAct_3","WeightAct_4",
                        "WeightCrit_0","WeightCrit_1","WeightCrit_2",
                        "WeightCrit_3","WeightCrit_4")]

# Extract means and IQR for the dynamic variables for each replicate -----------
# separately
Runmeans<-pop[, as.list(unlist(lapply(.SD, 
                                          function(x) list(m = mean(x),
                                                           upIQR = fivenum(x)[4],
                                                           downIQR = fivenum(x)[2]
                                          )))),
                  by = .(time,seed), 
                  .SDcols=c("Quality","alpha","beta","Badge","nInteract","WeightAct_0",
                            "WeightAct_1","WeightAct_2","WeightAct_3","WeightAct_4",
                            "WeightCrit_0","WeightCrit_1","WeightCrit_2",
                            "WeightCrit_3","WeightCrit_4")]


# Plot variation of the weights ------------------------------------------------

# png(here("Simulations",paste0(scenario,"_"),"weightsVarQualSt.png"),
#     width = 1000,height = 800)

gener<-tail(pop[,unique(time)],1)
runChoi<-0
nCenters<-5
interv<-1/nCenters
centers<-interv*0.5+interv*seq(0,nCenters-1)
rangx<-seq(0,1,length=1000)
tempPop<-pop[time==gener,.SD[.N],
             .SDcol=c(grep("Weight",
                           names(evol),value = TRUE),"Quality"),
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
dataRunsAct<-sapply(as.list(evol[time==gener,seed]),
                   function(x){x=
                     logist(totRBF(rangx,
                                   centers,0.01,
                                   as.double(
                                     evol[time==gener&seed==x,.SD,
                                             .SDcol=grep("WeightAct",
                                                         names(evol),
                                                         value = TRUE)
                                             ])),alpha=0,beta = 1)})


# yaxs<-c("s","n","n")
# ylabsUP<-c("p(Dove)","","")
# ylabsDO<-c("Value","","")
# Actor 
plot.new()
# Average of the evolutionary replicates
weightsActMean<-as.double(evolStats[time==gener,.SD,
                                  .SDcols=grep("m.weightAct",names(evolStats),
                                               value = TRUE)])
# One particular run
# weightsAct<-as.double(evol[time==gener&seed==runChoi,.SD,
#                            .SDcols=grep("WeightAct",names(evol),
#                                         value = TRUE)])

par(plt=posPlot(numploty = 2,idploty = 2,numplotx = 2,idplotx = 2)
    -c(0.05,0.05,0,0),las=1,new=T)
matplot(x=rangx,y=dataIndAct,col = paletteMeans(100)[
  findInterval(tempPop[,Quality],colorbreaksQual)],
lwd=2,lty = 1,xaxt="n",yaxt="n",
  xlab="",ylab="",type = "l")
axis(4,cex.axis=1.5)
par(las=0)
mtext("p(Dove)",4,cex = 2,line = 3)
par(las=1)
lines(logist(totRBF(rangx,centers,0.01,weightsActMean),alpha = 0,
            beta = 1)~rangx,lwd=3,col="black")
# average of all the replicates from the pop data
lines(logist(totRBF(rangx,centers,0.01,
                    as.double(popStats[time==gener,.SD,
                                       .SDcols=paste0("WeightAct_",0:4,".m")])),
             alpha=0,beta=1)~rangx,col="green",lwd=3)
matlines(x=rangx,y=dataRunsAct,col="red",lwd=0.8)

# average of individuals from one run
# lines(logist(totRBF(rangx,centers,0.01,
#                     as.double(Runmeans[time==gener&seed==runChoi,.SD,
#                                        .SDcols=paste0("WeightAct_",0:4,".m")])),
#              alpha=0,beta=1)~rangx,col="red")


# Critic 

# get data structure ready for plotting
dataIndCrit<-sapply(as.list(tempPop[,indId]),
                    function(x){x=
                      totRBF(rangx,
                                    centers,0.01,
                                    as.double(
                                      tempPop[indId==x,.SD,
                                              .SDcol=grep("WeightCrit",
                                                          names(tempPop),
                                                          value = TRUE)
                                              ]))})
dataRunsCrit<-sapply(as.list(evol[time==gener,seed]),
                    function(x){x=
                      totRBF(rangx,
                                    centers,0.01,
                                    as.double(
                                      evol[time==gener&seed==x,.SD,
                                           .SDcol=grep("WeightCrit",
                                                       names(evol),
                                                       value = TRUE)
                                           ]))})


# average of all replicates
weightsCritAll<-as.double(evolStats[time==gener,.SD,
                                 .SDcols=grep("m.weightCrit",
                                              names(evolStats),value = TRUE)])
# One particular run
# weightsCritOne<-as.double(evol[time==gener&seed==runChoi,.SD,
#                                .SDcols=grep("WeightCrit",names(evol),
#                                             value = TRUE)])

par(plt=posPlot(numploty = 2,idploty = 1,numplotx = 2,idplotx = 2)
    -c(0.05,0.05,0,0),new=TRUE)
matplot(x=rangx,y=dataIndCrit,col = paletteMeans(100)[
  findInterval(tempPop[,Quality],colorbreaksQual)],lwd=2,lty = 1,
  type = "l",xaxt="s",yaxt="n",xlab="Badge",ylab="",cex.lab=2)
axis(4,cex.axis=1.5)
par(las=0)
mtext("Value",4,cex = 2,line = 3)
par(las=1)

lines(totRBF(rangx,centers,0.01,weightsCritAll)~rangx,
     lwd=3,col="black")

# Average from all replicates from sample
lines(totRBF(rangx,centers,0.01,
             as.double(popStats[time==gener,.SD,
                                .SDcols=paste0("WeightCrit_",0:4,".m")]))
      ~rangx,col="red")

matlines(x=rangx,y=dataRunsCrit,col="red",lwd=0.8)


# average of individuals from one run
# lines(totRBF(rangx,centers,0.01,
#                     as.double(Runmeans[time==gener&seed==runChoi,.SD,
#                                        .SDcols=paste0("WeightCrit_",0:4,".m")]))
#              ~rangx,col="red")

# par(new=FALSE)
# color.bar.aeqp(paletteMeans(100),min =min(colorbreaksQual),
#                max = max(colorbreaksQual),nticks = 3,
#                title = "",
#                cex.tit = 1.2,
#                numplotx = 15,numploty = 10,idplotx =15,idploty = 9)

par(new=FALSE)
color.bar.aeqp(paletteMeans(100),min =min(colorbreaksQual),
               max = max(colorbreaksQual),nticks = 3,
               title = "",
               cex.tit = 1,
               numplotx = 15,numploty = 10,idplotx =14,idploty = 4)
title("quality   ", line = 1)
# dev.off()


# par(plt=posPlot())
# plot(totRBF(rangx,centers,0.01,weightsCrit)~rangx,type="l",xaxt="s",yaxt="s",
#       lwd=3)
# Average from all replicates
# lines(totRBF(rangx,centers,0.01,
#              as.double(popStats[time==gener,.SD,
#                                 .SDcols=paste0("WeightCrit_",0:4,".m")]))
#       ~rangx,col="green")
# average of individuals from one run
# lines(totRBF(rangx,centers,0.01,
#              as.double(Runmeans[time==gener&seed==runChoi,.SD,
#                                 .SDcols=paste0("WeightCrit_",0:4,".m")]))
#       ~rangx,col="red")
# matlines(x=rangx,y=dataIndCrit,col = paletteMeans(100)[
#   findInterval(tempPop[,Quality],colorbreaksQual)])

# Show the reaction norms of the sampled individuals ---------------------------

# png(here("Simulations",paste0(scenario,"_"),"react_norm.png"),
# width = 1000,height = 800)


gener<-pop[,unique(time)][9]
rangx<-seq(0,1,length=1000)
# yaxs<-c("s","n","n")
# ylabsUP<-c("p(Dove)","","")
# ylabsDO<-c("Value","","")
# Actor 
# plot.new()
par(plt=posPlot(numploty = 1,idploty = 1,numplotx = 2,idplotx = 1)
    -c(0.05,0.05,0,0),las=1,new=T)
plot(rep(0.5,1000)~rangx,type='l',col="grey",xaxt="s",
     yaxt="s", xlab="Quality",ylab="Badge",ylim=c(0,1),lwd=1.5,cex.axis=1.5,
     cex.lab=2)
lines(sapply(rangx,
             FUN=function(x){
               do.call(logist,
                       as.list(c(x,
                                 as.double(popStats[time==gener,
                                                    .SD,
                                                    .SDcols=c("alpha.m"
                                                              ,"beta.m")]))))})
              ~rangx,
      col="black",lwd=3)
dataIndReact<-sapply(as.list(tempPop[,indId]),
                     function(x){x=
                       sapply(rangx, function(y)
                         do.call(logist,
                                 as.list(
                                   c(y,
                                     as.double(tempPop[indId==x,.SD,
                                                       .SDcol=c("alpha","beta")
                                                       ])))))})
matlines(x=rangx,y=dataIndReact,col = paletteMeans(100)[
  findInterval(tempPop[,Quality],colorbreaksQual)],lwd=2)




# Scatterplot relating quality and central weight of the actor -----------------

png(here("Simulations",paste0(scenario,"_"),"weightVSQualiy.png"),
    width = 1000,height = 800)
par(plt=posPlot())
plot(logist(WeightAct_3,alpha = 0,beta = 1)~Quality,data=pop[time==gener],
     xlab="Quality",ylab = "p(Dove)",xaxt="s",
     col=genotype+1,
     pch=19)
legend("topright",legend = unique(pop$QualStDv)[order(unique(pop$QualStDv))],
       col = colboxes[1:3],pch = 19,title = expression(sigma^2))
dev.off()

hist(pop[time==gener,nInteract],xaxt="s")
hist(pop[time==gener,Badge],xaxt="s")


pop[WeightAct_3==0,unique(genotype)]
