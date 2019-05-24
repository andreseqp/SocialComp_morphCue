# ------------------------ generate json files  ------------------------------------------ #


library("jsonlite")
library("here")


fileName<-"parameters.json"


param<-list(totGen   =10000,   nRep        = 10,
            printGen = 500,    popSize  = 5000, 
            meanCue     = 20,   baselineFit = 1,
            sdCue    = 0.2,     nInt        = 50,
            mutRate  = 0.001, 
            payoff_matrix = c(1.5,1,0,0.5),
            namParam = "sdCue",
            rangParam = c(0.2,0.4,0.6,0.8,1 ),
            folder=paste(here("Simulations"),"/",sep=""))

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

check_create.dir(here("Simulations"),param = rep("sdCue",1),
                 values = c(""))

listfolders<-check_create.dir(here("Simulations","sdCue_"),
                                    param = rep("SdCue",5),
                              values = rang)


for (i in 1:1) {
  param$folder<-paste(here("Simulations","sdCue_"),"/",sep="")
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



