# Learning dynamics for a sample of the population along the
# evolutionary dynamics 

# Required libraries -----------------------------------------------------------

library(here)
here()
source(here("AccFunc.R"))



# Scenario to be plotted - corresponds to folders where simulations are stored

scenario<-"learHonest_/strQual1"



# Load files -------------------------------------------------------------------

(listTest<-list.files(here("Simulations",paste0(scenario,"_"))))
(List<-grep("ind",listTest,value=TRUE))

fileId<-1
indLearn<-fread(here("Simulations",paste0(scenario,"_"),List[fileId]))

# new columns ------------------------------------------------------------------

indLearn[,diffActWeights:=abs(WeightAct_0-WeightAct_4)]


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
seedCh<-1
countx<-0
county<-2

plot.new()

timePoints<-round(seq(1,length(unique(indLearn[time==gener,nInteract])),length.out = 10))

for(behavTime in unique(indLearn$nInteract)[timePoints]){
  if(countx==5)  {countx<-0;county<-county-1}
  countx<-countx+1
  dataIndsAct<-sapply(as.list(indLearn[(time==gener&nInteract==behavTime)
                                        &seed==seedCh,indId]),
                       function(x){x=
                         logist(totRBF(rangx,
                                centers,0.01,
                                as.double(
                                  indLearn[(time==gener&seed==seedCh)&
                                             (indId==x&nInteract==behavTime),.SD,
                                           .SDcol=grep("WeightAct",
                                                       names(indLearn),
                                                       value = TRUE)
                                           ])),alpha = 0,beta = 1)})
  par(plt=posPlot(numploty = 2,numplotx = 5,idploty = county,idplotx = countx),
      las=1,new=T)
  matplot(x=rangx,y=dataIndsAct,type='l',xlab="",ylab="",
          xaxt=xaxRang[county],yaxt=yaxRang[countx],lty = 1,
          col=1+match(indLearn[(time==gener&nInteract==behavTime)
                               &seed==seedCh,indId],
                      indLearn[(time==gener)&(seed==seedCh&nInteract==behavTime),
                               unique(indId)]),lwd=0.5,ylim=c(0,1))
  lines(logist(totRBF(rangx,centers,0.01,rep(0,5))
              ,alpha = 0,beta = 1)~rangx,
       lwd=1,col=1)
  text(x = 0.5,y=0.58,labels = paste0("nInt=",behavTime))
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

ylims<-fivenum(as.matrix(indLearn[,.SD,
                .SDcol=grep("WeightCrit",names(indLearn),
                                     value = TRUE)]))[c(1,5)]
for(behavTime in unique(indLearn$nInteract)[timePoints]){
  if(countx==5)  {countx<-0;county<-county-1}
  countx<-countx+1
  par(plt=posPlot(numploty = 2,numplotx = 5,idploty = county,idplotx = countx),
      las=1,new=T)
  dataIndsCrit<-sapply(as.list(indLearn[(time==gener&nInteract==behavTime)
                                        &seed==seedCh,indId]),
                       function(x){x=
                         totRBF(rangx,
                                centers,0.01,
                                as.double(
                                  indLearn[(time==gener&seed==seedCh)&
                                             (indId==x&nInteract==behavTime),.SD,
                                       .SDcol=grep("WeightCrit",
                                                   names(indLearn),
                                                   value = TRUE)
                                       ]))})
  matplot(x=rangx,y=dataIndsCrit,type='l',xlab="",ylab="",
          xaxt=xaxRang[county],yaxt=yaxRang[countx],lty = 1,
          col=1+match(indLearn[(time==gener&nInteract==behavTime)
                               &seed==seedCh,indId],
                      indLearn[(time==gener)&(seed==seedCh&nInteract==behavTime),
                                     unique(indId)]),lwd=0.5,ylim = ylims)
  lines(totRBF(rangx,centers,0.01,rep(0,5))~rangx,lwd=1,col=1)
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

hist(pop[,nInteract],xaxt="s")
hist(pop[,Badge],xaxt="s")
hist(pop[,Quality],xaxt="s")


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
