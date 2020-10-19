# ------------------------ generate json files  ------------------------------------------ #


library("jsonlite")
library("here")
source(here("AccFunc.R"))


fileName<-"parameters"
here()

param1<-#fromJSON(paste0("E:/BadgeSims","/betCostNoLearn_","/parameters.json"))
   fromJSON(here("Simulations","betCostNoLearn_","parameters.json"))

param<-list(totGen   =20000,   nRep    = 8,
            printGen = 1000,   printLearn = 1000,
            printLearnInt = 50,
            popSize  = 2000,  baselineFit = 2,   
            MutSd    = 0.2,  nInt        = 500,  init     = c(0,0,0,1),
            mutRate  = 0.001,  mutType  = 0,
            sampleSize = 100,   strQual  = 10,
            errorQual = 0,    alphaBad	 = 0,    betaBad	 = 0,
            alphaRes =0,betaRes=0, gammaRes=0,
            alphaCrit  = 0.5,  alphaAct = 0.5,
            sigSq   	 = 0.01, nCenters = 6,
            initCrit = 0,      initAct=0,   gamma = 0,
            QualStDv   = 1.1, betCost = 0,
            alphCost	 = 3, mutLearn = FALSE,
            nIntGroup  = 2000, 
            payoff_matrix = c(1.5,1,0,0.5),
            namParam = "nIntGroup",
            rangParam = c(8,2000),
            typeAgent = 3,
            folderL=paste(here("Simulations"),"/",sep="")) # comment for debug
# folderL=paste(here("Simulations","test_"),"/",sep="")) # comment for realease

param$folderL<-paste(here("Simulations"),"/",sep="")

param$rangParam<-c(0,2,5)

apendScenar<-"NoLearn"
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
# param$folder<-paste0("/hpcfs/home/a.quinones/BadgeStatus/",
#                     param$namParam,apendScenar,"_/")
## for the cluster unine
param$folder<-paste0("/home/ubuntu/BadgeStatus/",
                     param$namParam,apendScenar,"_/")
## For the local pc
param$folder<-paste0("e:/BadgeSims/",param$namParam,apendScenar,"_/")
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
check_create.dir("e:/BadgeSims",param = paste0(param$namParam,apendScenar),
                 values = c(""))

rangparam<-param$rangParam

for (i in 1:1) {
  param$folderL<-paste0(here("Simulations",
                            param$namParam),apendScenar,"_/") # comment for debug
  # param$folder<-param$folderL # for debug
  # param$rangParam<-c(rangparam[i])
  outParam<-toJSON(param,auto_unbox = TRUE,pretty = TRUE)
  # filenameL<-paste0(fileName,i,".json")
  filenameL<-paste0(fileName,".json")
  if(file.exists(paste(param$folderL,filenameL,sep = ''))){
    currFile<-fromJSON(paste(param$folderL,filenameL,sep = ''))
    if(sum(unlist(currFile)!=unlist(param))>0){
      warning("You are erasing old files!! n\ Check first!!!",immediate. = TRUE)
      print("OLD value")
      print(unlist(currFile)[unlist(currFile)!=unlist(param)])
      print("NEW value")
      print(unlist(param)[unlist(currFile)!=unlist(param)])
      ans<-readline("Want to continue?")
      if(substr(ans, 1, 1) == "y"){
        write(outParam,paste(param$folderL,filenameL,sep = ""))
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
    write(outParam,paste(param$folderL,filenameL,sep = ""))
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



