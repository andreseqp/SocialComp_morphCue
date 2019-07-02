# Variation in the learning parameters for a sample of the population along the
# evolutionary dynamics 

# Required libraries -----------------------------------------------------------

library(here)
here()
source(here("AccFunc.R"))



# Scenario to be plotted - corresponds to folders where simulations are stored

scenario<-"baselineFit"


# Load files -------------------------------------------------------------------

(listTest<-list.files(here("Simulations",paste0(scenario,"_"))))
(List<-grep("indLearn",listTest,value=TRUE))

pop<-fread(here("Simulations",paste0(scenario,"_"),List[1]))

pop<-pop[,.SD[nInteract==max(nInteract)],by=.(seed,time,indId)]


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

# Plot variation of the weights ------------------------------------------------

# png(here("Simulations",paste0(scenario,"_"),"weightsVarQualSt.png"),
#     width = 1000,height = 800)

gener<-pop[,unique(time)][25]
nCenters<-5
interv<-1/nCenters
centers<-interv*0.5+interv*seq(0,nCenters-1)
rangx<-seq(0,1,length=1000)
colorbreaksQual<-seq(0,1,length=100)
# yaxs<-c("s","n","n")
# ylabsUP<-c("p(Dove)","","")
# ylabsDO<-c("Value","","")
# Actor 
plot.new()
weightsAct<-as.double(evolStats[time==gener,.SD,
                                  .SDcols=grep("m.weightAct",names(evolStats),
                                               value = TRUE)])
par(plt=posPlot(numploty = 2,idploty = 2,numplotx = 1),las=1,new=T)
plot(logist(totRBF(rangx,centers,0.01,weightsAct),alpha = 0,
            beta = 1)~rangx,type='l',col=1,xaxt="n",
     yaxt="s",
     xlab="",ylab="p(Dove)",ylim=c(0,1),lwd=3)
lines(logist(totRBF(rangx,centers,0.01,
                    as.double(popStats[time==gener,.SD,
                                       .SDcols=paste0("WeightAct_",0:4,".m")])),
             alpha=0,beta=1)~rangx,col="red")
for(ind in pop[time==gener&genotype==2,unique(indId)]){
  lines(logist(totRBF(rangx,centers,0.01,
                      as.double(pop[(time==gener&indId==ind),.SD,
                                    .SDcols=grep("WeightAct",names(pop),
                                                 value=TRUE)][1])),
               alpha=0,beta=1)~rangx,col=paletteMeans(100)[
                 findInterval(pop[(time==gener&indId==ind),
                                  Quality],colorbreaksQual)],lwd=0.5)
}
# Critic 
weightsCrit<-as.double(evolStats[time==gener,.SD,
                                 .SDcols=grep("m.weightCrit",
                                              names(evolStats),value = TRUE)])
par(plt=posPlot(numploty = 2,idploty = 1),new=TRUE)
plot(totRBF(rangx,centers,0.01,weightsCrit)~rangx,type='l',col=1,xaxt="s",
     yaxt="s",xlab="Badge",ylab="value",ylim=c(-1,1.5),lwd=3)
lines(totRBF(rangx,centers,0.01,
             as.double(popStats[time==gener,.SD,
                                .SDcols=paste0("WeightCrit_",0:4,".m")]))
      ~rangx,col="red")
for(ind in pop[time==gener,unique(indId)]){
  lines(totRBF(rangx,centers,0.01,
               as.double(pop[(time==gener&indId==ind),.SD,
                             .SDcols=grep("WeightCrit",names(pop),
                                          value=TRUE)][1]))
        ~rangx,col=paletteMeans(100)[
          findInterval(pop[(time==gener&indId==ind),
                           Quality],colorbreaksQual)],lwd=0.5)
}
  

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
               numplotx = 15,numploty = 10,idplotx =15,idploty = 4)
title("quality   ", line = 1)
# dev.off()

# Show the reaction norms of the sampled individuals ---------------------------

gener<-pop[,unique(time)][25]
rangx<-seq(0,1,length=1000)
# yaxs<-c("s","n","n")
# ylabsUP<-c("p(Dove)","","")
# ylabsDO<-c("Value","","")
# Actor 
plot.new()
par(plt=posPlot(numploty = 1,idploty = 1,numplotx = 1),las=1,new=T)
plot(rep(0.5,1000)~rangx,type='l',col="grey",xaxt="s",
     yaxt="s", xlab="Quality",ylab="Badge",ylim=c(0,1),lwd=1.5)
lines(sapply(rangx,
             FUN=function(x){
               do.call(logist,
                       as.list(c(x,
                                 as.double(popStats[time==gener,
                                                    .SD,
                                                    .SDcols=c("alpha.m"
                                                              ,"beta.m")]))))})
              ~rangx,
      col="black")

for(ind in pop[time==gener&genotype==2,unique(indId)]){
  lines(sapply(rangx,
               FUN=function(x){
                 do.call(logist,
                         as.list(c(x,
                                   as.double(pop[(time==gener&indId==ind),
                                                      .SD,
                                                      .SDcols=c("alpha"
                                                                ,"beta")][1]
                                             ))))})
        ~rangx,col=pop[(time==gener&indId==ind),seed]+2,lwd=0.5)
}



pop[(time==gener&indId==ind)]



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
