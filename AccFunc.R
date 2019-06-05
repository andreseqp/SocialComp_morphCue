############   Accesory functions ##############################################

# Libraries
source(here("aesth.R"))
source("C:/Users/a.quinones/Dropbox/R_files/posPlots.R")
library('plotrix')
library(data.table)

# Calculate response triggered by one RBF --------------------------------------

RBF<-function(x,xCenter,sigSq){
  return(exp(-(x-xCenter)^2/(2*sigSq)))
}

# Calculate the total response triggered by one input --------------------------

totRBF<-function(x,xCenter,sigSq,featWeig){
  tmp<-sapply(xCenter, RBF,x=x,sigSq=sigSq)
  tmp<-apply(tmp, 1, FUN=crossprod,y=featWeig)
  #tmp<-apply(tmp,1,FUN = sum)
  return(tmp)
}

# Logistic function ------------------------------------------------------------

logist<-function(x,alpha,beta){
  return(1/(1+exp(alpha-beta*x)))
}