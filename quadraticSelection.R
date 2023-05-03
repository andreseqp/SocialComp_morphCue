# Playing around to visualize frequancy- dependent selection
# 

# Required libraries -----------------------------------------------------------

require(here)
library(ggplot2)
source(here("AccFunc.R"))
require("jsonlite")

# Scenario to be plotted - corresponds to folders where simulations are stored

scenario<-"betCostEvol5"#"alphaAct"#"nIntGroupNormQual"#

# extSimsDir<-#here("Simulations",paste0(scenario,"_"))
#   paste0("M:/BadgeSims/",scenario,"_")


# Load files -------------------------------------------------------------------
# Project folder
(listTest<-list.files(here("Simulations",paste0(scenario,"_")),full.names = TRUE))
# External sims folder
# (listTest<-list.files(extSimsDir,full.names = TRUE))

(evolList<-grep("evolLearn",listTest,value=TRUE))
(indList<-grep("indLearn",listTest,value=TRUE))
paramName<-list.files(here("Simulations",paste0(scenario,"_")))
# paramName<-list.files(extSimsDir,full.names = TRUE)
paramName<-grep(".json",paramName,value=TRUE)
param<-fromJSON(here("Simulations",paste0(scenario,"_"),paramName[2]))
# fromJSON(paramName[1])


val<-1
fileId<-val


evolList_runs<-grep(paste0(param$namParam,param$rangParam[val]),
                    evolList,value =TRUE)
indList_runs<-grep(paste0(param$namParam,param$rangParam[val]),
                   indList,value =TRUE)


evol<-do.call(rbind,lapply(evolList_runs,fread))
pop<-do.call(rbind,lapply(indList_runs, fread))


lastInt<-tail(pop[,unique(nInteract)],1)

popOneInd<-pop[nInteract==lastInt]

pop[,unique(cumPayoff)]

popOneInd[,unique(time)]

popOneInd.gen<-popOneInd

popOneInd.gen[,dist:=sqrt((Badge-mean(Badge))^2)]

str(popOneInd)

ggplot(popOneInd.gen,aes(y=cumPayoff,x=Quality))+
  geom_point(aes(color=as.factor(seed)))+
  geom_smooth()+
  theme_classic()

ggplot(popOneInd.gen,aes(y=cumPayoff,x=Badge))+
  geom_point(aes(color=as.factor(seed)))+
  geom_vline(xintercept = median(popOneInd.gen$Badge))+
  geom_smooth()+
  theme_classic()

ggplot(popOneInd.gen,aes(y=cumPayoff,x=dist))+
  geom_point(aes(color=as.factor(seed)))+
  geom_smooth()+
  theme_classic()

nbins<-30

ggplot(popOneInd.gen,aes(x=Badge))+
  geom_histogram(bins = nbins)+
  theme_classic()

par(mfrow=c(1,1),plt=posPlot())

hist.badge<-hist(popOneInd.gen$Badge,breaks = nbins,freq = TRUE)

popOneInd.gen[,binFreq:=sapply(Badge,function(x){
  hist.badge$density[which.min(abs(hist.badge$mids-x))]
})]

str(popOneInd.gen)

ggplot(popOneInd.gen,aes(y=cumPayoff,x=binFreq))+
  geom_point(aes(color=as.factor(seed)))+
  geom_smooth()+
  theme_classic()


model1 <-lm(data = popOneInd.gen,cumPayoff~Quality)

anova(model1)

model.loess <-loess(data = popOneInd.gen,cumPayoff~Quality)

predict.loess<-predict(model.loess, data.frame(Quality = rep(0.5,1000),
                                binFreq = seq(0,4,length=1000)), se = TRUE)

str(predict.loess)

dim(popOneInd.gen)

popOneInd.gen$resid.model1<-model1$residuals

popOneInd.gen$resid.loess<-model.loess$residuals

ggplot(popOneInd.gen,aes(y=resid.model1,x=binFreq))+
  geom_point(aes(color=as.factor(seed)))+
  geom_smooth()+
  theme_classic()

ggplot(popOneInd.gen,aes(y=resid.loess,x=binFreq))+
  geom_point(aes(color=as.factor(seed)))+
  geom_smooth()+
  theme_classic()

ggplot(popOneInd.gen,aes(y=binFreq,x=Quality))+
  geom_point(aes(color=as.factor(seed)))+
  geom_smooth()+
  theme_classic()

multReg.gen <- lm(data = popOneInd.gen,resid.loess~binFreq)

anova(lm(data=popOneInd.gen,binFreq~Quality+I(Quality^2)))

anova(multReg.gen)







