library(lidR)
library(rgdal)
library(raster)
library(stringr)
library(dplyr)

###############################################################
#Combining CUBE .xyz and CUBE Uncertainty .xyz into one file and removing overlappling/non-matching points. More specifically,
#The list of files is read in from the directory. Reg==ardless of the number of CUBE files in the directory, it will remove
#the cross check file and read in the uncertainty file separately while combining all of the xyz files into one data frame.
#This also binds the xyz and uncertainty columns while removing duplicates and creates two new columns for QA and Map.


###############################################################################################
XYZtoTextFilesandLidarProcessing <-function(home_dir, lake_name, WSEL, dam_elev){
  #Sit back, relax, and watch the magic happen.
  ##Establishing Sort, Lidar, ADCP, GPS, and Output directories
  sort_path = paste(home_dir,"/Sort",sep="")
  lidar_path = paste(home_dir,"/Lidar",sep="")
  adcp_path = paste(home_dir,"/ADCP",sep="")
  gps_path = paste(home_dir,"/GPS",sep="")
  out_path = paste(home_dir,"/RTextFiles",sep="")
  
  #Concatenating XYZ files
  setwd(sort_path)
  ls=list.files(pattern=".xyz$")
  ls=ls[!str_detect(ls,pattern="XCheck")]
  ls=ls[!str_detect(ls,pattern="Xcheck")]
  ls=ls[!str_detect(ls,pattern="uncert")]
  print(ls)
  options(digits=12)
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
  ls=ls[!str_detect(ls,pattern="Xcheck")]
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
  if(dir.exists(adcp_path)){
    setwd(adcp_path)
    i=1
    ls=list.files(pattern="ADCP.txt$")
    for(i in 1:length(ls)){
      adcp=read.table(ls[i],sep=",",header=TRUE)
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
    
  }
  
  
  #########GPS LOOP
  if(dir.exists(gps_path)){
    setwd(gps_path)
    i=1
    ls=list.files(pattern="GPS.csv$")
    for(i in 1:length(ls)){
      gps=read.table(ls[i],sep=",",header=TRUE)
      gps=as.data.frame(gps,stringsAsFactors=FALSE)
      if(i==1){
        gps=subset(gps,select=c("X","Y","Z"))
        colnames(gps)=c("X","Y","Z")
        all=gps
        temp=NULL
        gps_all=plyr::rbind.fill(all,temp)
        
      }else{
        gps=subset(gps,select=c("UTM_X","UTM_Y","Bed_elev"))
        colnames(gps)=c("X","Y","Z")
        temp=gps
        gps_all=plyr::rbind.fill(all,temp)
      }
    }  
    
    gps_all[c("Source")]="GPS"
  }
  
  ###########################################################################
  #Load in LiDAR and clip to shapefile
  #Setting a variable shortcut for the working directory
  #Concatenating LiDAR files. If there is more than one .xyz file in the LiDAR folder, removes overlapping points.
  #This section will read both in and combine them into one file and export it with only X, Y, and Z data.
  
  setwd(lidar_path)
  ls=list.files(pattern=".laz$")
  
  i=1
  for(i in 1:length(ls)){
    las=readLAS(ls[i])
    las=lasfilterduplicates(las)
    las=lasfiltersurfacepoints(las,res=1)
    
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
  clip_las=subset(las_all,elev>=(dam_elev + 2))
  clip_las$Z=clip_las$Z*3.28
  clip_xyz=subset(xyz_all,xyz_all$Z < (WSEL+2))
  clip_uncert=subset(uncert_all,uncert_all$TPU<6)
  
  #################DATA FRAME COMBINING, UNIT CONVERSION, REARRANGING, EXPORTING
  #df=plyr::rbind.fill(clip_xyz,clip_uncert)
  df=merge(clip_xyz,clip_uncert,by=(c("X","Y")))
  
  if(dir.exists(adcp_path)){
    df=plyr::rbind.fill(df,adcp_all)
  }
  if(dir.exists(gps_path)){
    df=plyr::rbind.fill(df, gps_all)
  }
  df=unique(df)
  df$Z=df$Z*3.28
  df$TPU=df$TPU*3.28
  
  #Setting the number of QA points and randomization groups
  total_points = nrow(xyz_all)
  map_pt_count = round(total_points*0.25)
  lower_qa_pt_bound = round(0.1*map_pt_count)
  upper_qa_pt_bound = round(0.15*map_pt_count)
  qa_pt_count = median(c(lower_qa_pt_bound, upper_qa_pt_bound))
  approx_QAgroupsize = round(qa_pt_count/0.75)
  num_groups = round(total_points / approx_QAgroupsize)
  FifteenPercContourQAsize = round(qa_pt_count*0.15)
  FivePercContourQAsize = round(qa_pt_count*0.05)
  
  cat(
    "Lake: ",lake_name,"\n",
    "Total points: ",total_points,"\n",
    "Map point count: ",map_pt_count,"\n",
    "QA point count: ",qa_pt_count,"\n",
    "Approx. QA group size: ",approx_QAgroupsize,"\n",
    "Number of random groups: ",num_groups,"\n",
    "15% Contour QA size: ",FifteenPercContourQAsize,"\n",
    "5% Contour QA size: ",FivePercContourQAsize,"\n"
  )
  
  set.seed=(1996)
  df["RAND"]=sample(1:num_groups,nrow(df),replace=TRUE)
  df[("QA")]=0
  df[("Map")]=1
  df=df[c(1,2,3,5,8,7,6,4)]
  
  
  write.table(df,file=paste(out_path,"/",lake_name,"_xyz_uncert_source.csv",sep=""),sep=",",row.names = FALSE)
  write.table(df,file=paste(out_path,"/",lake_name,"_xyz_uncert_source.txt",sep=""),sep=",",row.names = FALSE)
  write.table(clip_las,file=paste(out_path,"/",lake_name,"_las.csv",sep=""),sep=",",row.names = FALSE)
  write.table(clip_las,file=paste(out_path,"/",lake_name,"_las.txt",sep=""),sep=",",row.names = FALSE)
  
  
}


####INITIAL INPUTS. THESE NEED TO BE SPECIFIED BY THE USER

XYZtoTextFilesandLidarProcessing(home_dir = "E:/HYPACK_Projects/2020_DNR_Lakes/2020-07_MilanCityLake",
                                 lake_name = "MilanCityLake",
                                 WSEL = 261.828,
                                 dam_elev = 261.84)




