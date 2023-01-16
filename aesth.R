# Aesthetic parameters of the project ------------------------------------------

library("RColorBrewer")


colIntTypesLin<-brewer.pal(3,name = "Set1")
colIntTypesPol<-rgb(t(col2rgb(colIntTypesLin)),alpha = 100,maxColorValue = 255)

colTypesPol<-c(rgb(239/255,138/255,98/255,alpha = 0.5),
            rgb(103/255,169/255,207/255,alpha = 0.5),
            rgb(153/255,142/255,195/255,alpha = 0.5))


colTypesLin<-c(rgb(239/255,138/255,98/255,alpha = 1),
               rgb(103/255,169/255,207/255,alpha = 1),
               rgb(153/255,142/255,195/255,alpha = 1))

colGenesPol<-c(rgb(241/255,163/255,64/255,alpha = 0.5),
               rgb(153/255,142/255,195/255,alpha = 0.5),
               rgb(127/255,191/255,123/255,alpha = 0.5))

colGenesLin<-c(rgb(241/255,163/255,64/255),
               rgb(153/255,142/255,195/255),
               rgb(127/255,191/255,123/255))

paletteMeans <- colorRampPalette(c('#d73027','#fc8d59','#fdcb44',
                                   '#a2dbea','#91bfdb','#4559b4')[6:1])
colboxes<- c('#31a354','#d7191c','#fdae61','#2b83ba',"black")

colRuns<-c('#e41a1c','#377eb8','#4daf4a', '#984ea3','#ff7f00')

colSeeds<-brewer.pal(10,name = "Paired")

pal_dist<-colorRampPalette(brewer.pal(9,"Blues"))

colReps<-c(brewer.pal(12,"Paired"),1:3)

colorbreaksQual<-seq(0,1,length=100)


multDiscrPallet <- c(
  "#FF7F00", # orange
  "black", "gold1",
  "skyblue2", "#FB9A99", # lt pink
  "palegreen2",
  "#CAB2D6", # lt purple
  "#FDBF6F", # lt orange
  "gray70", "khaki2",
  "maroon", "orchid1", 
  "dodgerblue2", "#E31A1C", # red
  "green4",
  "#6A3D9A", # purple
  "deeppink1", "blue1", "steelblue4",
  "darkturquoise", "green1", "yellow4", "yellow3",
  "darkorange4", "brown"
)
