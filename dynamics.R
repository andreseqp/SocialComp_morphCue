library(data.table)
source("D:/quinonesa/Dropbox/R_files/posPlots.R")

setwd("S:/quinonesa/Simulations/Comp_cue/test_/")
(listTest<-list.files())

test1<-fread(listTest[1])

par(plt=posPlot(numploty = 2,idploty = 2),xaxt="n")
matplot(test1[,.(freqFenDoves,freqGenHawks,freqGenEval)],
        pch = 19,ylab="frequency")
lines(x=c(0,max(test1$time)),y=c(0.66,0.66),col="grey")
legend("right",legend=c("Dove","Hawk","Evaluators"),pch = 19,col = c(1,2,3),
       title="genotypes")

par(plt=posPlot(numploty = 2,idploty = 1),new=T)
matplot(test1[,.(freqFenDoves,freqFenHawks)],
        pch = 19,ylab="frequency")
lines(x=c(0,100),y=c(0.66,0.66),col="grey")
legend("right",legend=c("Dove","Hawk"),pch = 19,col = c(1,2),
       title="phenotypes")




