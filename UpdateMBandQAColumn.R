library(dplyr)

options(digits=12)
setwd("E:/HYPACK_Projects/2020_DNR_Lakes/2020-06_Mozingo/GIS")
xyz_new=read.table("Mozingo_xyz_thinned_Map4_withTPU.txt",header=TRUE,sep="\t",stringsAsFactors = FALSE)
xyz_table=read.table("E:/HYPACK_Projects/2020_DNR_Lakes/2020-06_Mozingo/RTextFiles/Mozingo_xyz_uncert_source.csv",header=TRUE,sep=",",stringsAsFactors = FALSE)
#xyz_table=read.table("E:/HYPACK_Projects/2020_DNR_Lakes/2020-06_Mozingo/GIS/Mozingo_xyz_uncert_source.csv",header=TRUE,sep=",",stringsAsFactors = FALSE)


xyz_new[,1]<-NULL
#xyz_new[,c(1,6,7)]<-NULL
#######################################
#colnames(xyz_new)=c("Z","Map","X","Y")
#xyz_new[,c("X","Y")]<-round(xyz_new[,c("X","Y")],2)
#xyz_new["Map"]<-1



xyz<-left_join(xyz_table,xyz_new,by=c("X","Y"))
xyz$Map.x<-ifelse(xyz$Map.y %in% c(1),1,0)

#xyz$Map.x<-ifelse(xyz$Map.y==1,1,0)
length(which(xyz$Map.x==1))
names(xyz)[names(xyz)=="Map.x"]<-"Map"
names(xyz)[names(xyz)=="Z.x"]<-"Z"
names(xyz)[names(xyz)=="Source.x"]<-"Source"
names(xyz)[names(xyz)=="TPU.x"]<-"CUBEuncert"
names(xyz)[names(xyz)=="QA.x"]<-"QA"
names(xyz)[names(xyz)=="RAND.x"]<-"RAND"

xyz$Map.y<-NULL
xyz$Z.y<-NULL
xyz$Source.y<-NULL
xyz$QA.y<-NULL
xyz$RAND.y<-NULL
xyz$TPU.y<-NULL

#only use na.omit if no ADCP data
#xyz<-na.omit(xyz)
set.seed=(1996)
xyz["RAND"]=sample(1:24,nrow(xyz),replace=TRUE)

#Removing potential Map Points and QA Points duplicates and assigning QA points based on random number
xyz$QA=ifelse(xyz$RAND==6 & xyz$Map!=1,1,0)
#testing, removing QA==1 if ADCP==1
length(which(xyz$Source=="ADCP" & xyz$QA==1))
xyz$QA=ifelse(xyz$Source=="ADCP" & xyz$RAND==6,0,xyz$QA)
length(which(xyz$Source=="ADCP" & xyz$QA==1))

#Removing the RAND column for export
xyz2=xyz[,-7]

#Outputting a final Map/QA file for metadata
write.table(xyz2,file="Mozingo_xyz_fulltable.txt",row.names=FALSE,sep=",")



meta<-matrix(nrow=2,ncol=4)

meta[1,1]<-max(xyz2$X)
meta[1,2]<-max(xyz2$Y)
meta[1,3]<-max(xyz2$Z)
meta[2,1]<-min(xyz2$X)
meta[2,2]<-min(xyz2$Y)
meta[2,3]<-min(xyz2$Z)

#No ADCP
meta[1,4]<-max(xyz2$TPU)
meta[2,4]<-min(xyz2$TPU)

#ADCP
temp=xyz2[!(xyz2$Source=="ADCP"),]
#temp2=xyz2[!(xyz2$Source=="GPS"),]
meta[1,4]<-max(temp$TPU)
meta[2,4]<-min(temp$TPU)


#################
colnames(meta)<-c("X","Y","Z","TPU")
write.table(meta, file="E:/HYPACK_Projects/2020_DNR_Lakes/Provisional-Final/Mozingo_minmax.csv",row.names=FALSE,sep=",")

