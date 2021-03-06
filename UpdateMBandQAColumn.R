#---
#  title: "UpdateMBandQAColumn"
#author: "Lindi Oyler"
#date: "9/27/2021"
#output: html_document
#---

# 1. Function Definition:
#This code uses the name of the lake, the home directory, and a list of alternative data sources to update the Map=1 and QA=1 points from the original bathy points table that was output in the RTextFiles folder. It also records and exports the min/max values for the X, Y, Z, and CUBE Uncert columns for metadata production. Note: alternative data sources are not included in finding the min/max values because of their CUBE_Uncert=-9999 assignments.The outputs for this code can be found in the lake's Provisional-Final folder.


library(dplyr)
options(digits=12)
updateMBQAcols = function(lake_name, home_dir,alt_data_sources){

  #Reading in the new xyz_map_thinned attribute table
  ls = list.files(path=paste(home_dir,'/GIS',sep=""),pattern=".csv$",full.names = TRUE)
  xyz_new=read.table(ls[1],header=TRUE,sep=",",stringsAsFactors = FALSE)
  xyz_new[,1]<-NULL #Removing the OID column that is automatically put in the table by Arc upon export
  
  #Reading in the full XYZ table
  ls2 = list.files(path=paste(home_dir,'/RTextFiles',sep=""),pattern="uncert_source.csv$",full.names = TRUE)
  xyz_table=read.table(ls2[1],header=TRUE,sep=",",stringsAsFactors = FALSE)
  #Changing the column of CUBE.Uncert to CUBE_Uncert (there's a space in the original column name that gets translated as a . Dumb, I know.)
  colnames(xyz_table)[4] <- "CUBE_Uncert"

  #Joining the two tables side by side by XY location
  xyz<-left_join(xyz_table,xyz_new,by=c("X","Y"))
  
  #Setting the Map column from the original XYZ table as 1 if the new XYZ table Map column is 1
  xyz$Map.x<-ifelse(xyz$Map.y %in% c(1),1,0)
  
  #Making sure the number of Map=1 points are equal in both tables. This should be equal to the entire length of the new XYZ table.
  
  if(length(which(xyz$Map.x==1)) != nrow(xyz_new)){
    
    print(paste("Length of Map=1 points in the original table: ",length(which(xyz$Map.x==1)),sep=""))
    print(paste("Length of Map=1 points in the new table: ",nrow(xyz_new),sep=""))
    stop("Something isn't matching up quite right with the table joining. Check this out with manual inspection.")
    print(paste("Length of Map=1 points in the original table: ",length(which(xyz$Map.x==1)),sep=""))
    print(paste("Length of Map=1 points in the new table: ",nrow(xyz_new),sep=""))
  }else{
    print("The table lengths look good to go. Let's continue!")
  }
  
  #Editing the columns of the big original XYZ table to be
  names(xyz)[names(xyz)=="Map.x"]<-"Map"
  names(xyz)[names(xyz)=="Z.x"]<-"Z"
  names(xyz)[names(xyz)=="Source.x"]<-"Source"
  names(xyz)[names(xyz)=="QA.x"]<-"QA"
  names(xyz)[names(xyz)=="Rand.x"]<-"Rand"
  names(xyz)[names(xyz)=="CUBE_Uncert.x"] <- "CUBE_Uncert"
  
  #Deleting unnecessary columns
  xyz$Map.y<-NULL
  xyz$Z.y<-NULL
  xyz$Source.y<-NULL
  xyz$QA.y<-NULL
  xyz$Rand.y<-NULL
  xyz$TPU.y<-NULL
  xyz$CUBE_Uncert.y <- NULL
  
  #We'll see if this code works.
  i=1
  for(i in 1:length(alt_data_sources)){
    xyz$CUBE_Uncert[xyz$Source == alt_data_sources[i]] <- -9999
  }
  
  #Assigning QA points
  xyz$QA = ifelse(xyz$Rand==6,1,0)
  paste("Number of QA points selected: ",length(which(xyz$QA==1)))
  prefilter_qa_length = length(which(xyz$QA==1))
  
  #Removing potential Map Points and QA Points duplicates and assigning QA points based on random number
  xyz$QA=ifelse(xyz$Rand==6 & xyz$Map!=1,1,0)
  #testing, removing QA==1 in alternative data sources
  if(length(alt_data_sources)!=0){
    i=1
    for(i in 1:length(alt_data_sources)){
      paste("Number of QA points from alternative data sources: ",length(which(xyz$Source==alt_data_sources[i] & xyz$QA==1)))
      xyz$QA=ifelse(xyz$Source==alt_data_sources[i] & xyz$Rand==6,0,xyz$QA)
      length(which(xyz$Source==alt_data_sources[i] & xyz$QA==1))
      qa_remaining = prefilter_qa_length - length(which(xyz$Source==alt_data_sources[i] & xyz$QA==1))
      paste("Number of QA points left after filtering from alternative data sources: ",qa_remaining)
      
    }
  }
  
  
  #Removing the RAND column for export
  xyz2=xyz[,-7]
  
  #Outputting a final Map/QA file for metadata
  out_fulltable = paste(home_dir,"/","Provisional-Final/",lake_name,"_xyz_fulltable.csv",sep="")
  print("Writing out full bathy points table...")
  write.table(xyz2,out_fulltable,row.names=FALSE,sep=",")
  
  #Writing the metadata
  
  meta<-matrix(nrow=2,ncol=4)
  
  meta[1,1]<-max(xyz2$X)
  meta[1,2]<-max(xyz2$Y)
  meta[1,3]<-max(xyz2$Z)
  meta[2,1]<-min(xyz2$X)
  meta[2,2]<-min(xyz2$Y)
  meta[2,3]<-min(xyz2$Z)
  
  #If alternative data sources are present
  
  if(length(alt_data_sources)!=0){
    i=1
    for(i in 1:length(alt_data_sources)){
      temp=xyz2[!(xyz2$Source==alt_data_sources[i]),]
    }
    meta[1,4]<-max(temp$CUBE_Uncert)
    meta[2,4]<-min(temp$CUBE_Uncert)
  }else{
    #No alternative data sources
    meta[1,4]<-max(xyz2$CUBE_Uncert)
    meta[2,4]<-min(xyz2$CUBE_Uncert)
  }
  
  #################
  colnames(meta)<-c("X","Y","Z","CUBE Uncert")
  out_meta = paste(home_dir,"/","Provisional-Final/",lake_name,"_meta_minmax.csv",sep="")
  print("Writing out metadata table...")
  write.table(meta, file=out_meta,row.names=FALSE,sep=",")
  print("All done!")
}


updateMBQAcols(lake_name = 'GardenCityNew',
               home_dir = 'E:/HYPACK_Projects/2020_DNR_Lakes/2020-07_GardenCityNew',
               alt_data_sources = c())
