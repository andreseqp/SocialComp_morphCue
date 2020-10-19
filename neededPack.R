# Funtion to quickly install all the necesary packages -------------------------

installProj.Packages<-function(){
  packlist<-c("doParallel",  "parallel", "iterators","foreach","lattice",
              "data.table", "rmarkdown","jsonlite","RColorBrewer", "here",
              "stats","graphics", "grDevices")
  install.packages(packlist)
}