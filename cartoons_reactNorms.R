########## Cartoon plot of reaction norms #####################################

# Load libraries and external functions -----------------------------------
library(here)
# directory where source files are saved

source(here("..","R_files","posPlots.R"))


## Reaction norms ------------------------------------------------------------

# output file

# png(here("reacNorm.png"),width = 800,height = 800)


qualRange<-seq(0,1,length=100)
alpha<-3
beta<-6
badgeFunc<-function(x,alpha,beta,maxBadge) {d<-maxBadge/(1+exp(alpha-beta*x))}
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

# Receiver 3d reaction norm ----------------------------------------------------

rivalBadge<-qualRange



perspTmp<-outer(X=qualRange,Y = rivalBadge,FUN = logist3d, 
                alpha=-10,beta=-20,gamma=0)

persp(x = qualRange,y=rivalBadge,z=perspTmp,phi = 35, theta=40 ,
      xlab="Quality",ylab="Rival's badge",zlim=c(0,1),
      zlab="p(Dove)",col="lightblue")

## Survival function -----------------------------------------------------------

# png(here("reacNorm.png"),width = 800,height = 800)

badQualRang<-seq(0,1,length=1000)
alpha<-1
beta<-6
costFunc<-function(beta,alpha,x) {d<-1/(1+exp(-alpha-beta*x))}
par(plt=posPlot())
betRang<-c(20)
matplot(y=sapply(betRang,FUN = costFunc,alpha=alpha,x=badQualRang),x=badQualRang,
        type="l",cex.lab=1.5, cex.axis=2,lwd=3,lty=1,las=1,
     col=c(2,3),ylim=c(0.5,1.1),xaxt="s",yaxt="s",ylab="",xlab="") #,col=i
# lines(badgeFunc(qualRange,3,-60,1.05)~qualRange,cex.lab=1.5,cex.axis=2,lwd=3,
#       col="blue")
# legend("bottomright",legend = betRang,title = expression(beta),col=1:3,
       # lty=1)
mtext(side=1,text="Quality",line=2.8,cex=3)
mtext(side=2,text="Cost",line=3,cex=3)
# text(0.01,1.17,"A", cex=2)
dev.off()

