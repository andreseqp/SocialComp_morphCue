############   Accesory functions ##############################################

# Libraries
source(here("aesth.R"))
library("jsonlite")
library('rmarkdown')
require("NbClust")
source(here("..","R_files","NbClust.R"))
source(here("..","R_files","posPlots.R"))
source(here("..","R_files","Filled.contour3.R"))
# library('plotrix')
library(data.table)
require(lattice)

# Calculate response triggered by one RBF --------------------------------------

RBF<-function(x,xCenter,sigSq){
  return(exp(-(abs(x-xCenter))^2/(2*sigSq)))
}

# Calculate the total response triggered by one input --------------------------

totRBF<-function(x,xCenter,sigSq,featWeig){
  tmp<-sapply(xCenter, RBF,x=x,sigSq=sigSq)
  tmp<-apply(tmp, 1, FUN=crossprod,y=featWeig)
  #tmp<-apply(tmp,1,FUN = sum)
  return(tmp)
}

# Logistic function ------------------------------------------------------------

logist<-function(x,alpha=0,beta=1){
  return(1/(1+exp(alpha-beta*x)))
}

logist3d<-function(qual,rivalBadge,alpha=0,beta=1, gamma=0){
  return(1/(1+exp(alpha-beta*qual-gamma*rivalBadge)))
}

# inverted Logistic function ------------------------------------------------------------

invertLogist<-function(x,alpha=0,beta=1){
  return((-log(-1+(1/x))-alpha)/beta)
}

# Create output folders --------------------------------------------------------

check_create.dir<-function(folder,param,values){
  listfolders<-paste0(folder,"/",param,values,"_")  
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

# Colour bar -------------------------------------------------------------------

color.bar.aeqp <- function(lut, min, max=-min, nticks=11, ticks=seq(min, max, len=nticks), title='',
                           cex.tit=1,numplotx,numploty,idplotx,idploty) {
  scale = (length(lut)-1)/(max-min)
  
  # dev.new(width=1.75, height=5)
  par(plt=posPlot(numplotx,numploty,idplotx,idploty),new=TRUE,cex.main=cex.tit,
      yaxt='s')
  plot(c(0,10), c(min,max), type='n', bty='n', xaxt='n', xlab='', yaxt='n', 
       ylab='', main=title)
  axis(2, ticks, las=1)
  for (i in 1:(length(lut)-1)) {
    y = (i-1)/scale + min
    rect(0,y,10,y+1/scale, col=lut[i], border=NA)
  }	
}

# Load all the files for a scenario varying one parameter ----------------------
# not robust yet to filename variation CHECK!!! --------------------------------

filesScenar<-function(filename,scenario,full.name=FALSE){
  par<-gsub(".txt","",tail(strsplit(filename,"_")[[1]],1))
  parVal<-as.numeric(gsub("[[:alpha:]]",par,replacement = ''))
  parNam<-gsub("[^[:alpha:]]",par,replacement = '')
  if(full.name){
    tmp<-fread(filename)
  }
  else{
    tmp<-fread(here("Simulations",paste0(scenario,"_"),filename)) 
  }
  tmp[,eval(parNam):=parVal]
  return(tmp)
}

# Load all the files for different scenarios  varying one parameter ----------------------


filesCompScenar<-function(filename,scenario,full.name=FALSE){
  par<-gsub(".txt","",tail(strsplit(filename,"_")[[1]],1))
  parVal<-as.numeric(gsub("[[:alpha:]]",par,replacement = ''))
  parNam<-gsub("[^[:alpha:]]",par,replacement = '')
  if(full.name){
    tmp<-fread(filename)
  }
  else{
    tmp<-fread(here("Simulations",paste0(scenario,"_"),filename)) 
  }
  tmp[,eval(parNam):=parVal]
  return(tmp)
}


# Visualize difference in parameters between 2 JSON files ----------------------

diffJsons<-function(json1,json2){
  print("JSON.1")
  print(unlist(json1)[unlist(json1)!=unlist(json2)])
  print("JSON.2")
  print(unlist(json2)[unlist(json1)!=unlist(json2)])
}

## Automatically produce job files ---------------------------------------------

jobfile<-function(folder,jobname,timelim="10:00:00",
                  part="short",nodes=1,mem=4096){
  bashafile<-list(line0="#!/bin/bash",
                  jobname="#SBATCH --job-name=",
                  partit="#SBATCH -p ",nodes="#SBATCH -N ",
                  cpus="#SBATCH --cpus-per-task=1", 
                  mem="#SBATCH --mem=",
                  time="#SBATCH --time=",
                  mailu="#SBATCH --mail-user=a.quinones@uniandes.edu.co",
                  mailt="#SBATCH --mail-type=END",
                  outp= paste0("#SBATCH -o ","/hpcfs/home/a.quinones/BadgeStatus/",
                               jobname,"_/TEST_job.o%j"),
                  gethost="host=`/bin/hostname`",
                  getdate="date=/bin/date",
                  exec=paste0("/hpcfs/home/a.quinones/BadgeStatus/./Morph_cue ",
                              "/hpcfs/home/a.quinones/BadgeStatus/",
                              jobname,"_/parameters.json"),
                  printhost="echo \"Run  at: \"$host",
                  printdate="echo  \"Run  on: \"$date")
  
  bashafile$jobname<-paste0(bashafile$jobname,jobname)
  bashafile$time<-paste0(bashafile$time,timelim)
  bashafile$partit<-paste0(bashafile$partit,part)
  bashafile$mem<-paste0(bashafile$mem,mem)
  bashafile$nodes<-paste0(bashafile$nodes,nodes)
  if(file.exists(paste0(folder,"jobfile.sh"))){
    unlink(paste0(folder,"jobfile.sh"))
  }
  conJobfile<-lapply(bashafile, write, file=file(paste0(folder,"jobfile.sh"),"wb"), append=T)
  showConnections()
}


# function to generate the actor or critic with respect to badge ---------------

Actor<-function(weights,centers,sigSq=0.01,nx=1000){
  logist(totRBF(seq(0,1,length.out = nx),
                centers,sigSq,as.double(weights)),alpha = 0,beta = 1)
}

Critic<-function(weights,centers,sigSq=0.01,nx=1000){
  totRBF(seq(0,1,length.out = nx),
                centers,sigSq,as.double(weights))
}


# Functio to generate evolutionary dynamics in the form of frecuency distributions

evolDist<-function(indData,variable,nbins,range=NULL,pal, nlevels =10, 
                   numx=1,numy=1,idx=1,idy=1,NewP=FALSE,
                   ylim=range(indData[,get(variable)]),
                   cexAxis=2,xlab="x",ylab=variable, keyTitle="", xaxt="s",
                   yaxt="s",...){
  if(is.null(range)){
    maxV<-indData[,max(get(variable))];minV<-indData[,min(get(variable))]
  }
  else {maxV<-range[1];minV<-range[2]}
  Interv<-seq(minV,maxV,length.out = nbins+1)
  ints<-(Interv[2]-Interv[1])/2
  bins<-seq(minV+ints,maxV-ints,length.out = nbins)
  timeseq<-indData[,unique(time)]
  indData[,Int:=findInterval(get(variable),Interv)]
  indData[,bin:=bins[Int]]
  totalCounts<-indData[,length(bin),by=.(time)]
  # contour2<-as.data.frame(matrix(0,ncol = 3,nrow = nbins*length(timeseq)))
  contourData<-dcast(indData[,length(bin)/totalCounts$V1[match(unique(time),time)],
                             by=.(time,Int)],Int~time,
                     value.var="V1")
  contour1<-matrix(0,nrow = nbins,ncol = length(timeseq))
  # count<-0
  for(i in 1:nbins){
    for(j in 1:length(timeseq)){
      # count<-count+1
      contour1[i,j]<-as.numeric(contourData[Int==i,j+1,with=FALSE])
      # contour2[count,]<-c(bins[i],
      #                     timeseq[j],
      #                     as.numeric(contourData[Int==i,j+1,with=FALSE]))
    }
  }
  # contour2[is.na(contour2)] <- 0
  contour1[is.na(contour1)] <- 0
  # names(contour2)<-c(variable,"time","counts")
  # levelplot(data = contour2,
  #           log(0.01+t(counts))~time*get(variable),col.regions =  pal)
  # # par(plt=posPlot(numplotx = numx,numploty = numy,idplotx = idx,idploty = idy),
  #                 new=NewP)
  
 
  filled.contour3(x=as.numeric(timeseq),y = as.numeric(bins),z = log(0.01+t(contour1)),
                 color.palette = pal, nlevels = nlevels,ylim=ylim,
                 plot.axes={
                   axis(1,cex.axis=cexAxis,xaxt=xaxt)
                   axis(2,cex.axis=cexAxis,yaxt=yaxt)
                 },
                 plot.title={
                   title(xlab=xlab,cex.lab=2)
                   mtext(ylab,2,cex=2,line=3,las=1)},
                 key.title = {par(cex.main=cexAxis);title(main=keyTitle)}
                 )
  # return(list(x=timeseq,y=bins,z=contour1))
}


# Function to generate the clustering for different runs -----------------------

get_clusters<-function(DT,vars,k.max=5,Bsamples=500,iterMax=100){
  listTimeSeed<-DT[,length(indId),by=.(seed,time)][,.(seed,time)]
  # Method to decide the optimal number of clusters
  listTimeSeed<-split(t(as.matrix(listTimeSeed)), 
        rep(1:ncol(t(as.matrix(listTimeSeed))), 
            each = nrow(t(as.matrix(listTimeSeed)))))
  totClusters<-do.call(rbind,sapply(listTimeSeed, function(x){
    if(x[1]==1&x[2]==20000){
      print("STOPPP!!!")
    }
      print(x)
      error<-FALSE
      error<-tryCatch(expr = 
        { clus_gap<-cluster::clusGap(as.matrix(
          DT[seed==x[1]&time==x[2],.SD,.SDcols=vars]),kmeans,
          K.max = k.max,B = Bsamples,iter.max=iterMax)
          nclustersGap<-cluster::maxSE(f = clus_gap$Tab[, "gap"], 
                                      SE.f = clus_gap$Tab[, "SE.sim"])
          FALSE
        },error = function(e) {TRUE}
      )
      if(error) {
        clustersMatch<-cbind(which(DT[,seed] %in% x[1] & DT[,time] %in% x[2]),
                             rep(1,dim(DT[seed==x[1]&time==x[2],.SD,.SDcols=vars])[1]))
      }
      else if (nclustersGap==1){
        clusters<-kmeans(as.matrix(
        DT[seed==x[1]&time==x[2],.SD,.SDcols=vars]),
        centers = nclustersGap,iter.max = 100)
        clustersMatch<-cbind(which(DT[,seed] %in% x[1] & DT[,time] %in% x[2]),
                           clusters$cluster)
      }
      else{
        print(x)
        if(x[1]==1&x[2]==20000){
          print("STOPPP!!!")
        }
        tryCatch(expr = invisible(capture.output(clus_Nb<-NbClust.AEQP(as.matrix(
          DT[seed==x[1]&time==x[2],.SD,.SDcols=vars]),
          min.nc = 2,max.nc = k.max,
          method = "kmeans"))),
          error = function(e) invisible(capture.output(clus_Nb<-NbClust.AEQP(as.matrix(
            DT[seed==x[1]&time==x[2],.SD,.SDcols=vars]),
            min.nc = 2,max.nc = k.max-1,
            method = "kmeans")))
        )
        clusters<-clus_Nb$Best.partition
        clustersMatch<-cbind(which(DT[,seed] %in% x[1] & DT[,time] %in% x[2]),
                               clusters)
        print("wait")
        }
        return(clustersMatch)
      }
    )
  )
  # else {errorCondition("invalid method")}
  totClusters<-as.data.frame(totClusters)
  totClusters<-totClusters[order(totClusters$V1),]
  return(totClusters[,2])
}

## Function to calculate ESS in the classic gam from the pay-off matrix --------

ESS<-function(pay_offsM){
  # /*  (This are IDs to the entries of the matrix, NOT payoffs)    
  # Hawk      Dove     
  # Hawk   0         1      
  # Dove   2         3
  # */
  V<-pay_offsM[2]; C<-pay_offsM[2]
  hawk<-V/C; dove<-1-hawk
  HH<-hawk**2 ; HD<-hawk*dove; DD<-dove**2
  if(HH+HD+DD!=1)  warning("proportions do not add up to 1")
  return(list(hawk=hawk,dove=dove,HH=HH,HD=HD,DD=DD))
}



# clus_silou<-c(0,clus_silou)
# nclustersSil<-which.max(clus_silou)


# DT[,idClust:=as.numeric(idClust)]

# for(i in which(DT[,seed] %in% runChoi)){
#   set(popFinal,i,match( "idClust",names(DT)),
#       clustersMatch[clustersMatch[,2]==i,1])
# }


# #SBATCH --job-name=TestJOB		#Nombre del job
# #SBATCH -p short			#Cola a usar, Default=short (Ver colas y límites en /hpcfs/shared/README/partitions.txt)
# #SBATCH -N 1				#Nodos requeridos, Default=1
# #SBATCH -n 1				#Tasks paralelos, recomendado para MPI, Default=1
# #SBATCH --cpus-per-task=1		#Cores requeridos por task, recomendado para multi-thread, Default=1
# #SBATCH --mem=2000		#Memoria en Mb por CPU, Default=2048
# #SBATCH --time=00:10:00			#Tiempo máximo de corrida, Default=2 horas
# #SBATCH --mail-user=USER@uniandes.edu.co
# #SBATCH --mail-type=ALL			
# #SBATCH -o TEST_job.o%j			#Nombre de archivo de salida
# 
# host=`/bin/hostname`
# date=`/bin/date`
# echo "Soy un JOB de prueba"
# echo "Corri en la maquina: "$host
# echo "Corri el: "$date




