# ------------------------ generate json files  ------------------------------------------ #


library("jsonlite")
library("here")
source(here("AccFunc.R"))


fileName<-"parameters.json"
here()


param<-list(totGen   =10,   nRep    = 5,
            printGen = 1,   printLearn = 1,
            printLearnInt = 500,
            popSize  = 1000,  baselineFit = 2,   
            MutSd    = 0.2,
            nInt        = 30000,  init     = c(0,0,1),
            mutRate  = 0.0,  mutType  = 0,
            sampleSize = 30,   strQual  = 10,
            alphaBad	 = 3,    betaBad	 = 6,
            alphaCrit  = 0.02,  alphaAct = 0.02,
            sigSq   	 = 0.01, nCenters = 6,
            initCrit = 0,      initAct=0,
            QualStDv   = 1.1, betCost = 0,
            alphCost	 = 3,
            nIntGroup  = 1000,
            payoff_matrix = c(1.5,1,0,0.5),
            namParam = "alphaAct",
            rangParam = c(0.005,0.01,0.02,0.05,0.1),
            folderL=paste(here("Simulations"),"/",sep=""))

apendScenar<-"Learn1"
param$folderL<-paste0(param$folderL,
       param$namParam,apendScenar,"_/")
# runTime<-"360:00:00"# "10:00:00"# 
# 
# partition<-"long"#"short"#
# 
# memor<- "16GB"#"4096"
# 
# nodes<-10

## For the cluster Uniandes
param$folder<-paste0("/hpcfs/home/a.quinones/BadgeStatus/",
                    param$namParam,apendScenar,"_/")
## for the cluster unine
param$folder<-paste0("/home/ubuntu/BadgeStatus/",
                     param$namParam,apendScenar,"_/")
## For the local pc
param$folder<-param$folderL
# read and edit json 
# oldJson<-fromJSON(here("Simulations","strQualEvol_",fileName))
# diffJsons(oldJson,param)
# param<-oldJson
# param$alphaAct<-0.01
# param$alphaCrit<-0.01
# param$folderL<-param$folder
# param$folder<-paste0("/hpcfs/home/a.quinones/BadgeStatus/",param$namParam,"1_/")
# param$baselineFit<-5


# rang<-1

## Old ways of arranging folders
# check_create.dir(here("Simulations"),param = "Centers6",
#                  values = c(""))
# listfolders<-check_create.dir(here("Simulations","alphaAC_"),
#                               param = rep("SdCue",5),
#                               values = rang)


check_create.dir(here("Simulations"),param = paste0(param$namParam,apendScenar),
                 values = c(""))



for (i in 1:1) {
  param$folderL<-paste0(here("Simulations",
                            param$namParam),apendScenar,"_/")
  # param$folder<-param$folderL
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
        # jobfile(param$folderL,paste0(param$namParam,apendScenar),timelim = runTime,
        #         partition,nodes = nodes,mem = memor)
      }
    }
    else{
      # jobfile(param$folderL,paste0(param$namParam,apendScenar),timelim = runTime,
      #         partition,nodes = nodes,mem = memor)
    }
  }
  else{
    write(outParam,paste(param$folderL,fileName,sep = ""))
    # jobfile(param$folderL,paste0(param$namParam,apendScenar),timelim = runTime,
    #         partition,nodes = nodes,mem = memor)
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



