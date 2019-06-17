# Learning dynamics for a sample of the population along the
# evolutionary dynamics 

# Required libraries -----------------------------------------------------------

library(here)
here()
source(here("AccFunc.R"))



# Scenario to be plotted - corresponds to folders where simulations are stored

scenario<-"baselineFit_"


# Load files -------------------------------------------------------------------

(listTest<-list.files(here("Simulations",scenario)))
(List<-grep("ind",listTest,value=TRUE))

indLearn<-fread(here("Simulations",scenario,List[1]))


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

# Changes in learning parameters for several individuals -----------------------

gener<-indLearn[,unique(time)][3]
nCenters<-5
interv<-1/nCenters
centers<-interv*0.5+interv*seq(0,nCenters-1)
rangx<-seq(0,1,length=1000)
colorbreaksQual<-seq(0,1,length=100)

# Actor 

yaxRang<-c("s",rep("n",4))
xaxRang<-c("s","n")
seedCh<-0
countx<-0
county<-2
plot.new()
for(behavTime in unique(indLearn$nInteract)){
  if(countx==5)  {countx<-0;county<-county-1}
  countx<-countx+1
  par(plt=posPlot(numploty = 2,numplotx = 5,idploty = county,idplotx = countx),
      las=1,new=T)
  plot(logist(totRBF(rangx,centers,0.01,rep(0,5))
              ,alpha = 0,beta = 1)~rangx,type='l',xlab="",ylab="",ylim=c(0.4,0.6),
       lwd=3,col=1,xaxt=xaxRang[county],yaxt=yaxRang[countx])
  text(x = 0.5,y=0.58,labels = paste0("nInt=",behavTime))
  for(idInd in indLearn[(time==gener&nInteract==behavTime)&seed==seedCh,
                      unique(indId)]){
    lines(logist(totRBF(rangx,centers,0.01,
                        as.double(indLearn[(time==gener&indId==idInd)&
                                        (seed==seedCh&nInteract==behavTime),.SD,
                                      .SDcols=grep("WeightAct",names(indLearn),
                                                   value=TRUE)][1])),
                 alpha=0,beta=1)~rangx,
          col=1+match(idInd,indLearn[(time==gener)&
                                       (seed==seedCh&nInteract==behavTime),
                                     unique(indId)]),lwd=0.5)
  }
}


# par(new=FALSE)
# color.bar.aeqp(paletteMeans(100),min =min(colorbreaksQual),
#                max = max(colorbreaksQual),nticks = 3,
#                title = "",
#                cex.tit = 1.2,
#                numplotx = 15,numploty = 10,idplotx =15,idploty = 9)

# Critic 

yaxRang<-c("s",rep("n",4))
xaxRang<-c("s","n")
seedCh<-0
countx<-0
county<-2
plot.new()
for(behavTime in unique(indLearn$nInteract)){
  if(countx==5)  {countx<-0;county<-county-1}
  countx<-countx+1
  par(plt=posPlot(numploty = 2,numplotx = 5,idploty = county,idplotx = countx),
      las=1,new=T)
  plot(totRBF(rangx,centers,0.01,rep(0,5))~rangx,type='l',xlab="",ylab="",
       ylim=c(0,0.4), lwd=3,col=1,xaxt=xaxRang[county],yaxt=yaxRang[countx])
  text(x = 0.5,y=0.38,labels = paste0("nInt=",behavTime))
  for(idInd in indLearn[(time==gener&nInteract==behavTime)&seed==seedCh,
                        unique(indId)]){
  lines(totRBF(rangx,centers,0.01,
                      as.double(indLearn[(time==gener&indId==idInd)&
                                           (seed==seedCh&nInteract==behavTime),.SD,
                                         .SDcols=grep("WeightCrit",names(indLearn),
                                                      value=TRUE)][1]))~rangx,
        col=1+match(idInd,indLearn[(time==gener)&
                                         (seed==seedCh&nInteract==behavTime),
                                       unique(indId)]),lwd=0.5)
  }
}


indCh<-80
par(plt=posPlot(numploty = 2,idploty = 2))
matplot(x=indLearn[(time==gener&seed==seedCh)&indId==indCh,nInteract],
        y=indLearn[(time==gener&seed==seedCh)&indId==indCh,.SD,
                   .SDcols=grep("WeightAct",names(indLearn),value = T)],
        xlab="",ylab="Weights Actor",xaxt="n",type="l")
legend("topright",legend = grep("WeightCrit",names(indLearn),value = T),
       col = 1:5,lty=1)
par(plt=posPlot(numploty = 2,idploty = 1),new=TRUE)
matplot(x=indLearn[(time==gener&seed==seedCh)&indId==indCh,nInteract],
        y=indLearn[(time==gener&seed==seedCh)&indId==indCh,.SD,
                   .SDcols=grep("WeightCrit",names(indLearn),value = T)],
        xlab="nInter",ylab="Weights Actor",xaxt="s",type="l")


# par(new=FALSE)
# color.bar.aeqp(paletteMeans(100),min =min(colorbreaksQual),
#                max = max(colorbreaksQual),nticks = 3,
#                title = "",
#                cex.tit = 1.2,
#                numplotx = 15,numploty = 10,idplotx =15,idploty = 4)



# Scatterplot relating quality and central weight of the actor -----------------

par(plt=posPlot())
plot(pop[time==min(time),WeightAct_3]~pop[time==min(time),Quality],
     xlab="Quality",ylab = "Central actor weight")

hist(pop[,nInteract],xaxt="s")
hist(pop[,Badge],xaxt="s")

