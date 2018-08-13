# ------------------------ generate json files  ------------------------------------------ #

library("jsonlite")

projDir<-"D:/quinonesa/SocialCompetence/"

simsDir<-"S:/quinonesa/Simulations/Comp_cue/"

visualDir<-"D:\\quinonesa/VisualStudio/Compet_cue/"

exedir<-paste(projDir,'/./cue_compt.exe',sep='')

fileName<-"parameters.json"


param<-list(totGen   =50000,   nRep        = 10,
            printGen = 500,    popSize  = 1000, 
            meanCue     = 20,   baselineFit = 1,
            sdCue    = 0.2,     nInt        = 50,
            mutRate  = 0.001, 
            payoff_matrix = c(1.5,1,0,0.5),
            namParam = "sdCue",
            rangParam = c(0.2,0.4,0.6,0.8,1 ),
            folder=simsDir)

setwd(simsDir)

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

rang<-c(0,1)
rangSD<-seq(0,1,length=5)

check_create.dir(simsDir,param = rep("sdCue",1),
                 values = c(""))

listfolders<-check_create.dir(paste(simsDir,"SdCue1_/",sep=""),
                                    param = rep("SdCue",5),
                              values = rang)


for (i in 1:1) {
  param$folder<-paste(simsDir,"sdCue_/",sep="")
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



