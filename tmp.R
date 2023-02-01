tmp<-function(x){
  count<-match(x,popOneInd[,sort(unique(get(shortSce)),decreasing = TRUE)])
  tempPop<-popOneInd[get(shortSce)==x&(seed==repsScen[count]&time==max(time))]
  tempPop<-tempPop[,.SD[.N],
                   .SDcol=c(grep("WeightAct",
                                 names(popOneInd),value = TRUE),"Quality","alpha","beta"
                            # ,
                            # "orderClus"),
                   ),
                   by=indId]
  dataIndReact<-sapply(as.list(tempPop[,indId]),
                       function(x){x=
                         sapply(rangx, 
                                function(y)
                                  do.call(logist,
                                          as.list(
                                            c(y,as.double(
                                              tempPop[indId==x,.SD,
                                                      .SDcol=c("alpha","beta")])))))})
  return(dataIndReact)
}