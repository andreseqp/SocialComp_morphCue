# Variation in the learning parameters for a sample of the population along the
# evolutionary dynamics 

# Required libraries -----------------------------------------------------------

library(here)
here()
source(here("AccFunc.R"))



# Scenario to be plotted - corresponds to folders where simulations are stored

scenario<-"mutType_"


# Load files -------------------------------------------------------------------

(listTest<-list.files(here("Simulations",scenario)))
(List<-grep("pop",listTest,value=TRUE))

pop<-fread(here("Simulations",scenario,List[1]))

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

gener<-pop[,unique(time)][5]
nCenters<-5
interv<-1/nCenters
centers<-interv*0.5+interv*seq(0,nCenters-1)
rangx<-seq(0,1,length=1000)
colorbreaksQual<-seq(0,1,length=100)
# Actor 

weightsAct<-as.double(evolStats[time==gener,.SD,
                             .SDcols=grep("m.weightAct",names(evolStats),
                                          value = TRUE)])

par(plt=posPlot(numploty = 2,idploty = 2),las=1)
plot(logist(totRBF(rangx,centers,0.01,weightsAct),alpha = 0,
            beta = 1)~rangx,type='l',col=1,xaxt="n",
     xlab="",ylab="p(dove)",ylim=c(0,1),lwd=3)
lines(logist(totRBF(rangx,centers,0.01,
                    as.double(popStats[time==gener,.SD,
                                       .SDcols=paste0("WeightAct_",0:4,".m")])),
                    alpha=0,beta=1)~rangx,col="red")
for(ind in pop[time==gener,unique(idInd)]){
  lines(logist(totRBF(rangx,centers,0.01,
                     as.double(pop[time==gener&idInd==ind,.SD,
                                        .SDcols=grep("WeightAct",names(pop),
                                                     value=TRUE)][1])),
              alpha=0,beta=1)~rangx,col=paletteMeans(100)[
                findInterval(pop[time==gener&idInd==ind,
                                 Quality],colorbreaksQual)],lwd=0.5)
}

par(new=FALSE)
color.bar.aeqp(paletteMeans(100),min =min(colorbreaksQual),
               max = max(colorbreaksQual),nticks = 3,
               title = "",
               cex.tit = 1.2,
               numplotx = 15,numploty = 10,idplotx =15,idploty = 9)

# Critic ------------------------------------------------

weightsCrit<-as.double(evolStats[time==min(time),.SD,
                             .SDcols=grep("m.weightCrit",
                                          names(evolStats),value = TRUE)])
par(plt=posPlot(numploty = 2,idploty = 1),new=TRUE)
plot(totRBF(rangx,centers,0.01,weightsCrit)~rangx,type='l',col=1,
     xlab="x",ylab="Value",ylim=c(0,1),lwd=3)
# points(y=weights,x=centers,cex=3)
lines(totRBF(rangx,centers,0.01,
                    as.double(popStats[time==gener,.SD,
                                       .SDcols=paste0("WeightCrit_",0:4,".m")]))
      ~rangx,col="red")
for(ind in pop[time==gener,unique(idInd)]){
  lines(totRBF(rangx,centers,0.01,
                      as.double(pop[time==gener&idInd==ind,.SD,
                                    .SDcols=grep("WeightCrit",names(pop),
                                                 value=TRUE)][1]))
        ~rangx,col=paletteMeans(100)[
                 findInterval(pop[time==gener&idInd==ind,
                                  Quality],colorbreaksQual)],lwd=0.5)
}

par(new=FALSE)
color.bar.aeqp(paletteMeans(100),min =min(colorbreaksQual),
               max = max(colorbreaksQual),nticks = 3,
               title = "",
               cex.tit = 1.2,
               numplotx = 15,numploty = 10,idplotx =15,idploty = 4)



# Scatterplot relating quality and central weight of the actor -----------------

par(plt=posPlot())
plot(pop[time==min(time),WeightAct_3]~pop[time==min(time),Quality],
     xlab="Quality",ylab = "Central actor weight")

hist(pop[,nInteract],xaxt="s")
hist(pop[,Badge],xaxt="s")

