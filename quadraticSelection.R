# Playing around to visualize frequancy- dependent selection
# 

# Required libraries -----------------------------------------------------------

require(here)
here()
source(here("AccFunc.R"))
require("jsonlite")

# Scenario to be plotted - corresponds to folders where simulations are stored

scenario<-"betCostEvol4"#"alphaAct"#"nIntGroupNormQual"#

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

popOneInd.gen<-popOneInd[time==3000]

str(popOneInd)



multReg.gen <- lm(data = popOneInd.1000,cumPayoff/ntotInteract~Quality+Badge+I(Badge^2))

anova(multReg.gen)




