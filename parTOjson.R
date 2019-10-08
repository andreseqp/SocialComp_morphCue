# ------------------------ generate json files  ------------------------------------------ #


library("jsonlite")
library("here")
source(here("AccFunc.R"))


fileName<-"parameters.json"
here()


param<-list(totGen   = 5,   nRep    = 30,
            printGen = 1,   printLearn = 1,
            printLearnInt = 100,
            popSize  = 1000,  baselineFit = 2,   
            MutSd    = 0,
            nInt        = 5000,  init     = c(0,0,1),
            mutRate  = 0.02,  mutType  = 0,
            sampleSize = 20,   strQual  = 10,
            alphaBad	 = 0,    betaBad	 = 0,
            alphaCrit  = 0.01,  alphaAct = 0.01,
            sigSq   	 = 0.01, nCenters = 6,
            initCrit = 0,      initAct=0,
            QualStDv   = 0.0,  
            payoff_matrix = c(1.5,1,0,0.5),
            namParam = "initAct",
            rangParam = invertLogist(c(0.85,0.666,0.333,0.15)),
            folderL=paste(here("Simulations"),"/",sep=""))

apendScenar<-""

param$folder=paste0("/hpcfs/home/a.quinones/BadgeStatus/",
                    param$namParam,apendScenar)

# read and edit json 
# oldJson<-fromJSON(here("Simulations","learHonest_","strQual1_",fileName))
# diffJsons(oldJson,param)
# param<-oldJson
# param$alphaAct<-0.01
# param$alphaCrit<-0.01
# param$folderL<-param$folder
# param$folder<-paste0("/hpcfs/home/a.quinones/BadgeStatus/",param$namParam,"1_/")
# param$baselineFit<-5


rang<-1


check_create.dir(here("Simulations"),param = "Centers6",
                 values = c(""))

check_create.dir(here("Simulations"),param = paste0(param$namParam,apendScenar),
                 values = c(""))

listfolders<-check_create.dir(here("Simulations","alphaAC_"),
                                    param = rep("SdCue",5),
                              values = rang)


for (i in 1:1) {
  param$folderL<-paste0(here("Simulations",
                            param$namParam),apendScenar,"_/")
  outParam<-toJSON(param,auto_unbox = TRUE,pretty = TRUE)
  if(file.exists(paste(param$folderL,fileName,sep = ''))){
    currFile<-fromJSON(paste(param$folderL,fileName,sep = ''))
    if(sum(unlist(currFile)!=unlist(param))>0){
      warning("You are erasing old files!! n\ Check first!!!",immediate. = TRUE)
      print("OLD value")
      print(unlist(currFile)[unlist(currFile)!=unlist(param)])
      print("NEW value")
      print(unlist(param)[unlist(currFile)!=unlist(param)])
      ans<-readline("Want to continue?")
      if(substr(ans, 1, 1) == "y"){
        write(outParam,paste(param$folderL,fileName,sep = ""))
        jobfile(param$folderL,paste0(param$namParam,apendScenar))
      }
    }
    else{
      jobfile(param$folderL,paste0(param$namParam,apendScenar))
    }
  }
  else{
    write(outParam,paste(param$folderL,fileName,sep = ""))
    jobfile(param$folderL,paste0(param$namParam,apendScenar))
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



