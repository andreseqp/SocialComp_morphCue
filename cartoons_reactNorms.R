########## Cartoon plot of reaction norms #####################################

# Load libraries and external functions -----------------------------------
library(here)
# directory where source files are saved

source('C:/Users/a.quinones/Dropbox/R_files/posPlots.R')


## Reaction norms ------------------------------------------------------------

# output file

png(here("reacNorm.png"),width = 800,height = 800)


qualRange<-seq(0,1,length=100)
alpha<-5
beta<--10
badgeFunc<-function(x,alpha,beta,maxBadge) {d<-maxBadge/(1+exp(alpha+beta*x))}
par(plt=posPlot())
plot(badgeFunc(qualRange,alpha,beta,1)~qualRange,type="l",cex.lab=1.5,
     cex.axis=2,lwd=3,
     col="red",ylim=c(0,1.1),xaxt="s",yaxt="s",ylab="",xlab="") #,col=i
# lines(badgeFunc(qualRange,3,-60,1.05)~qualRange,cex.lab=1.5,cex.axis=2,lwd=3,
#       col="blue")
mtext(side=1,text="Quality",line=2,cex=5)
mtext(side=2,text="Badge",line=2,cex=5)
# text(0.01,1.17,"A", cex=2)
dev.off()

