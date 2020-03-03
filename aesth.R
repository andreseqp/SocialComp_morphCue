# Aesthetic parameters of the project ------------------------------------------

colTypesPol<-c(rgb(239/255,138/255,98/255,alpha = 0.5),
            rgb(103/255,169/255,207/255,alpha = 0.5),
            rgb(153/255,142/255,195/255,alpha = 0.5))


colTypesLin<-c(rgb(239/255,138/255,98/255,alpha = 1),
               rgb(103/255,169/255,207/255,alpha = 1),
               rgb(153/255,142/255,195/255,alpha = 1))

colGenesPol<-c(rgb(241/255,163/255,64/255,alpha = 0.5),
               rgb(153/255,142/255,195/255,alpha = 0.5))

colGenesLin<-c(rgb(241/255,163/255,64/255),
               rgb(153/255,142/255,195/255))

paletteMeans <- colorRampPalette(c('#d73027','#fc8d59','#fee090',
                                   '#e0f3f8','#91bfdb','#4575b4')[6:1],
                                 alpha=TRUE)
colboxes<- c('#31a354','#d7191c','#fdae61','#2b83ba',"black")

colRuns<-c('#e41a1c','#377eb8','#4daf4a', '#984ea3','#ff7f00')

colorbreaksQual<-seq(0,1,length=100)
