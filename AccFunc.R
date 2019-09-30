############   Accesory functions ##############################################

# Libraries
source(here("aesth.R"))
library('rmarkdown')
source(here("..","R_files","posPlots.R"))
# library('plotrix')
library(data.table)

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

logist<-function(x,alpha,beta){
  return(1/(1+exp(alpha-beta*x)))
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

filesScenar<-function(filename,scenario){
  splitScen<-tail(strsplit(scenario,"_")[[1]],1)
  splitScen<-gsub("/",splitScen,replacement = '')
  extPar<-strsplit(filename,splitScen)[[1]][2]
  parVal<-as.numeric(substr(gsub("[[:alpha:]]",extPar,replacement = ''),1,3))
  tmp<-fread(here("Simulations",paste0(scenario,"_"),filename))
  tmp[,eval(splitScen):=parVal]
  return(tmp)
}


## Automatically produce job files ---------------------------------------------

jobfile<-function(folder,jobname){
  
  bashafile<-list(line0="#!/bin/bash",jobname="#SBATCH --job-name=",
                  partit="#SBATCH -p short",nodes="#SBATCH -N 1",
                  cpus="#SBATCH --cpus-per-task=1", mem="#SBATCH --mem=2000",
                  time="#SBATCH --time=00:10:00",
                  mailu="#SBATCH --mail-user=a.quinones@uniandes.edu.co",
                  mailt="#SBATCH --mail-type=END",
                  outp="#SBATCH -o TEST_job.o%j",
                  gethost="host=`/bin/hostname`",
                  getdate="date=/bin/date",
                  exec=".././Morph_cue parameters.json",
                  printhost="echo \"Run  at: \"$host",
                  printdate="echo  \"Run  on: \"$date")
  
  bashafile$jobname<-paste0(bashafile$jobname,"jobname")
  if(file.exists(here(scenario,"jobfile.sh"))){
    unlink(here(scenario,"jobfile.sh"))
  }
  lapply(bashafile, write, file=file(here(scenario,"jobfile.sh"),"wb"), append=T)
}

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

