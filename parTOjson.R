# ------------------------ generate json files  ------------------------------------------ #


library("jsonlite")
library("here")
source(here("AccFunc.R"))


fileName<-"parameters.json"


param<-list(totGen   = 10,   nRep    = 30,
            printGen = 1,   printLearn = 1,
            printLearnInt = 100,
            popSize  = 1000,  baselineFit = 2,   
            MutSd    = 0.0,
            nInt        = 2000,  init     = c(0,0,1),
            mutRate  = 0.02,  mutType  = 0,
            sampleSize = 20,   strQual  = 10,
            alphaBad	 = 5,    betaBad	 = 10,
            alphaCrit  = 0.2,  alphaAct = 0.2,
            sigSq   	 = 0.01, nCenters = 5,
            QualStDv   = 0.3,  
            payoff_matrix = c(1.5,1,0,0.5),
            namParam = "alphaAct",
            rangParam = c(0.1,0.01,0.001),
            folder=paste(here("Simulations"),"/",sep=""))



rang<-1

check_create.dir(here("Simulations"),param = "learHonest",
                 values = c(""))

check_create.dir(here("Simulations","learHonest_"),param = param$namParam,
                 values = c(""))

listfolders<-check_create.dir(here("Simulations","alphaAC_"),
                                    param = rep("SdCue",5),
                              values = rang)


for (i in 1:1) {
  param$folder<-paste0(here("Simulations","learHonest_",param$namParam),"_/")
  outParam<-toJSON(param,auto_unbox = TRUE,pretty = TRUE)
  if(file.exists(paste(param$folder,fileName,sep = ''))){
    currFile<-fromJSON(paste(param$folder,fileName,sep = ''))
    if(sum(unlist(currFile)!=unlist(param))>0){
      warning("You are erasing old files!! n\ Check first!!!",immediate. = TRUE)
      print("OLD value")
      print(unlist(currFile)[unlist(currFile)!=unlist(param)])
      print("NEW value")
      print(unlist(param)[unlist(currFile)!=unlist(param)])
      ans<-readline("Want to continue?")
      if(substr(ans, 1, 1) == "y"){
        write(outParam,paste(param$folder,fileName,sep = ""))
      }
    }
  }
  else{
    write(outParam,paste(param$folder,fileName,sep = ""))
  }
  # system(paste(exedir,
  #   gsub("\\","/",paste(simsdir,listfolders[i],fileName,sep="\\"),fixed=TRUE)
  #   ,sep = " "))
}
gsub(pattern = "\\",replacement = "/",simsdir,fixed=TRUE)

# system(paste(exedir,
#              gsub("\\","/",paste(simsdir,listfolders[1],fileName,sep="\\"),fixed=TRUE)
#              ,sep = " "))



testParam$folder<-paste(simsDir,"test_/",sep = "")
outParam<-toJSON(testParam,auto_unbox = TRUE,pretty = TRUE)
write(outParam,paste(visualDir,"test.json",sep="/"))



