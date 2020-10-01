# Comparing results from different scenarios -----------------------------------

# Required libraries -----------------------------------------------------------

library(here)
here()
source(here("AccFunc.R"))


# Choove scenarios to be plotted - corresponds to folders where simulations are stored

scenarios<-c("nIntGroupEvol1","betCostEvol1")

variable<-
  lapply(scenarios,FUN=gsub,pattern="[^[:alpha:]]",replacement = '')
variable1<-gsub("Evol",variable1,replacement = '')
extSimsDir<-paste0("e:/BadgeSims/",scenario1,"_")

variable2<-gsub("[^[:alpha:]]",scenario2,replacement = '')
variable2<-gsub("Evol",variable2,replacement = '')
extSimsDir<-paste0("e:/BadgeSims/",scenario2,"_")


# Load files -------------------------------------------------------------------

(listTest<-list.files(extSimsDir,full.names = TRUE))
(evolList<-grep("evolLearn",listTest,value=TRUE))
(indList<-grep("indLearn",listTest,value=TRUE))

nampar<-gsub("[^[:alpha:]]",gsub(".txt","",tail(strsplit(indList[1],"_")[[1]],1)),
             replacement = "")

# (listTest<-list.files(here("Simulations",paste0(scenario,"_"))))
# (sdList<-grep("evol",listTest,value=TRUE))

evol<-do.call(rbind,lapply(evolList,filesScenar,scenario,full.name=TRUE))

pop<-do.call(rbind,lapply(indList, filesScenar,scenario,full.name=TRUE))
