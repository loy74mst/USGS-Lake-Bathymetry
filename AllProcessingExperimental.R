library(lidR)
library(rgdal)
library(raster)
library(stringr)
library(dplyr)
#################################################
#Reading in the table full of goodies

goodies=read.table("E:/HYPACK_Projects/2019_DNR_Lakes/QA_MapPointsCounts.csv",header=TRUE,sep=",",stringsAsFactors=FALSE)

for(m in 1:nrow(goodies)){

outpath=


###############################################################
#Combining CUBE .xyz and CUBE Uncertainty .xyz into one file and removing overlappling/non-matching points. More specifically,
#The list of fiio95444les is read in from the directory. Regardless of the number of CUBE files in the directory, it will remove
#the cross check file and read in the uncertainty file separately while combining all of the xyz files into one data frame.
#This also binds the xyz and uncertainty columns while removing duplicates and creates two new columns for QA and Map.
setwd("E:/HYPACK_Projects/2019_DNR_Lakes/NewBethany_2020/Sort")
ls=list.files(pattern=".xyz$")
ls=ls[!str_detect(ls,pattern="XCheck")]
ls=ls[!str_detect(ls,pattern="uncert")]

##############FILL IN XYZ STUFF HERE
i=1
for(i in 1:length(ls)){
  xyz=read.table(ls[i],sep=" ")
  xyz=as.data.frame(xyz,stringsAsFactors=FALSE)
  
  if(i==1){
    colnames(xyz)=c("X","Y","Z")
    all=xyz
    temp=NULL
    xyz_all=plyr::rbind.fill(all,temp)
    
  }else{
    colnames(xyz)=c("X","Y","Z")
    temp=xyz
    xyz_all=plyr::rbind.fill(all,temp)
  }
  
}

xyz_all[c("Source")]="MB"

############UNCERTAINTY LOOP
i=1
ls=list.files(pattern=".xyz$")
ls=ls[!str_detect(ls,pattern="XCheck")]
ls=ls[!str_detect(ls,pattern="CUBE.xyz$")]
for(i in 1:length(ls)){
  uncert=read.table(ls[i],sep=" ")
  uncert=as.data.frame(uncert,stringsAsFactors=FALSE)
  
  if(i==1){
    colnames(uncert)=c("X",'Y','TPU')
    all=uncert
    temp=NULL
    uncert_all=plyr::rbind.fill(all,temp)
    
  }else{
    colnames(uncert)=c('X','Y','TPU')
    temp=uncert
    uncert_all=plyr::rbind.fill(all,temp)
  }
}  
#########ADCP LOOP
setwd('E:/HYPACK_Projects/2019_DNR_Lakes/NewBethany_2020/NewBethanyADCP')
i=1
ls=list.files(pattern="ADCP.txt$")
for(i in 1:length(ls)){
  adcp=read.table(ls[i],sep="\t",header=TRUE)
  adcp=as.data.frame(adcp,stringsAsFactors=FALSE)
  
  if(i==1){
    adcp=subset(adcp,select=c("UTM_X","UTM_Y","Bed_elev"))
    colnames(adcp)=c("X","Y","Z")
    all=adcp
    temp=NULL
    adcp_all=plyr::rbind.fill(all,temp)
    
  }else{
    adcp=subset(adcp,select=c("UTM_X","UTM_Y","Bed_elev"))
    colnames(adcp)=c("X","Y","Z")
    temp=adcp
    adcp_all=plyr::rbind.fill(all,temp)
  }
}  

adcp_all[c("Source")]="ADCP"

###########################################################################
#Load in LiDAR and clip to shapefile
#Setting a variable shortcut for the working directory
#Concatenating LiDAR files. If there is more than one .xyz file in the LiDAR folder, removes overlapping points.
#This section will read both in and combine them into one file and export it with only X, Y, and Z data.

setwd("E:/HYPACK_Projects/2019_DNR_Lakes/NewBethany_2020/LIDAR")
ls=list.files(pattern=".las$")

i=1
for(i in 1:length(ls)){
  las=readLAS(ls[i])
  las=lasfilterduplicates(las)
  las=lasfiltersurfacepoints(las,res=0.5)
  
  las_sp=as.spatial(las)
  las_df=as.data.frame(las_sp,stringsAsFactors=FALSE)
  las_df=filter(las_df,Classification==2)
  
  
  if(length(ls)==1){
    las_df=subset(las_df,select=c("X","Y","Z"))
    las_all=las_df
    break
  }
  
  if(i==1){
    las_df=subset(las_df,select=c("X","Y","Z"))
    las_all=las_df
  }else{
    las_df=subset(las_df,select=c("X","Y","Z"))
    las_all=rbind(las_all,las_df)
    
  }
}

#Cuts out LiDAR points below water surface elevation and above dam elevation. Change this value of elev for each new lake!
#Refer to Excel document for corrected WSE and the WSL Projections Proposal for top of dam/spillway
elev=las_all[,3]
head(elev)
clip_las=subset(las_all,elev>=280.708 & elev<=288)
clip_las$Z=clip_las$Z*3.28
clip_xyz=subset(xyz_all,xyz_all$Z <280.708)
clip_uncert=subset(uncert_all,uncert_all$TPU<6)

#################DATA FRAME COMBINING, UNIT CONVERSION, REARRANGING, EXPORTING
df=plyr::rbind.fill(clip_xyz,clip_uncert)
df=merge(clip_xyz,clip_uncert,by=(c("X","Y")))
df=plyr::rbind.fill(df,adcp_all)
df=unique(df)
df$Z=df$Z*3.28
df$TPU=df$TPU*3.28

set.seed=(1996)
df["RAND"]=sample(1:25,nrow(df),replace=TRUE)
df[c("QA","Map")]=0
df=df[c(1,2,3,5,8,7,6,4)]
sep=subset(df,select=c(X,Y,Z))

dir.create(outPath)

write.table(sep,file="E:/HYPACK_Projects/2019_DNR_Lakes/NewBethany_2020/RTextFiles/NewBethany_xyz.csv",sep=",",row.names = FALSE)
write.table(sep,file="E:/HYPACK_Projects/2019_DNR_Lakes/NewBethany_2020/RTextFiles/NewBethany_xyz.txt",sep=",",row.names = FALSE)
write.table(df,file="E:/HYPACK_Projects/2019_DNR_Lakes/NewBethany_2020/RTextFiles/NewBethany_xyz_uncert_source.csv",sep=",",row.names = FALSE)
write.table(df,file="E:/HYPACK_Projects/2019_DNR_Lakes/NewBethany_2020/RTextFiles/NewBethany_xyz_uncert_source.txt",sep=",",row.names = FALSE)
write.table(clip_las,file="E:/HYPACK_Projects/2019_DNR_Lakes/NewBethany_2020/RTextFiles/NewBethany_las.csv",sep=",",row.names = FALSE)
write.table(clip_las,file="E:/HYPACK_Projects/2019_DNR_Lakes/NewBethany_2020/RTextFiles/NewBethany_las.txt",sep=",",row.names = FALSE)

}