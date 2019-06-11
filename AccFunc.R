############   Accesory functions ##############################################

# Libraries
source(here("aesth.R"))
source("C:/Users/a.quinones/Dropbox/R_files/posPlots.R")
library('plotrix')
library(data.table)

# Calculate response triggered by one RBF --------------------------------------

RBF<-function(x,xCenter,sigSq){
  return(exp(-(abs(x-xCenter))^2/(2*sigSq)))
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

# Create output folders --------------------------------------------------------

check_create.dir<-function(folder,param,values){
  setwd(folder)
  listfolders<-paste(param,values,"_",sep = "")  
  currFolders<-lapply(listfolders,dir.exists)
  if(sum(currFolders>0)){
    warning("At least one of the folders already exists \n Please check",
            immediate. = TRUE)
    print(cbind(listfolders,currFolders))
    ans<-readline("Want to continue?")
    if(substr(ans, 1, 1) == "y"){
      lapply(listfolders,dir.create)
      return(listfolders)
    }
    else{
      return(listfolders)
    }
  }else{
    lapply(listfolders,dir.create)
    return(listfolders)
  }
}

# Colour bar -------------------------------------------------------------------

color.bar.aeqp <- function(lut, min, max=-min, nticks=11, ticks=seq(min, max, len=nticks), title='',
                           cex.tit=1,numplotx,numploty,idplotx,idploty) {
  scale = (length(lut)-1)/(max-min)
  
  # dev.new(width=1.75, height=5)
  par(plt=posPlot(numplotx,numploty,idplotx,idploty),new=TRUE,cex.main=cex.tit,
      yaxt='s')
  plot(c(0,10), c(min,max), type='n', bty='n', xaxt='n', xlab='', yaxt='n', 
       ylab='', main=title)
  axis(2, ticks, las=1)
  for (i in 1:(length(lut)-1)) {
    y = (i-1)/scale + min
    rect(0,y,10,y+1/scale, col=lut[i], border=NA)
  }	
}
