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
XYZToTextFiles<-function(home_dir, lake_name, WSEL, dam_elev,alt_source_types){
  gc()
  memory.size(max=TRUE)
  #Required libraries
  start.time <- Sys.time() #Timing how long the program takes to run
  options(digits=12)
  
  list.of.packages <- c("lidR", "rgdal", "raster","stringr","dplyr","sf")
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)
  options("rgdal_show_exportToProj4_warnings"="none")
  
  library(lidR)
  library(rgdal)
  library(raster)
  library(stringr)
  library(dplyr)
  library(sf)
  
  print("Working on XYZ files...")
  ##Establishing Sort, Lidar, ADCP, GPS, and Output directories
  sort_path = paste(home_dir,"/Sort",sep="")
  lidar_path = paste(home_dir,"/Lidar",sep="")
  out_path = paste(home_dir,"/RTextFiles",sep="")
  
  #Concatenating XYZ files only
  setwd(sort_path)
  ls=list.files(pattern=".xyz$")
  ls=ls[!str_detect(ls,pattern="XCheck")]
  ls=ls[!str_detect(ls,pattern="Xcheck")]
  ls=ls[!str_detect(ls,pattern="uncert")]
  print(ls)
  
  ##############
  #Looping through the various XYZ files and concatenating them
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
  
  ############
  #Looping through the various CUBE uncertainty files and concatenating them
  print("Working on CUBE Uncertainty files...")
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
  #########
  #Looping through the alternative source files (if they exist) and concatenating them. This loop is skipped if they do not exist
  
  i=1
  if(length(alt_source_types!=0)){
    for(i in 1:length(alt_source_types)){
      type = alt_source_types[[i]]
      paste("Working on",type,"files...")
      alt_path = paste(home_dir,"/",type,sep="")
      if(dir.exists(alt_path)){
        setwd(alt_path)
        j=1
        ls=list.files(pattern=".csv$")
        for(j in 1:length(ls)){
          alt=read.table(ls[j],sep=",",header=TRUE)
          alt=as.data.frame(alt,stringsAsFactors=FALSE)
          if(j==1){
            colnames(alt)=c("X","Y","Z")
            all=alt
            temp=NULL
            alt_all=plyr::rbind.fill(all,temp)
            
          }else{
            colnames(alt)=c("X","Y","Z")
            temp=alt
            alt_all=plyr::rbind.fill(all,temp)
          }
        }  
        
        alt_all[c("Source")]=type
        
        if(i==1){
          full_alt_table = plyr::rbind.fill(alt_all)
        }else{
          full_alt_table = plyr::rbind.fill(full_alt_table,alt_all)
        }
        
      }
      
    }
  }
  
  
  clip_xyz=subset(xyz_all,xyz_all$Z < (WSEL+0.5)) #Selects MB points that are 0.5 meter above the WSEL and less
  clip_uncert=subset(uncert_all,uncert_all$TPU<=1.52) #Removing points with a CUBE Uncertainty >1.52 meters (~5 feet)
  
  #################DATA FRAME COMBINING, UNIT CONVERSION, REARRANGING, EXPORTING
  #df=plyr::rbind.fill(clip_xyz,clip_uncert) #For emergency use only in case the number of Uncertainty points doesn't match the number                                                    of XYZ points
  print("Merging, combining, and moving some stuff around!")
  df=merge(clip_xyz,clip_uncert,by=(c("X","Y"))) #Merging the MB and CUBE uncertainty points by XY location
  
  if(length(alt_source_types!=0)){
    df=plyr::rbind.fill(df,full_alt_table)
    df$TPU <- ifelse(df$Source!="MB",-9999,df$TPU)
  } ##Adding alternative source points into the overall dataframe if they exist
  
  
  df=unique(df) #Removing duplicates that may have occurred during joining 
  df$Z=df$Z*3.2808 #Converting the Z column to feet
  df$TPU <- ifelse(df$Source=="MB",df$TPU*3.2808,df$TPU)
  
  #df$TPU=df$TPU*3.28 #Converting the CUBE Uncert column
  
  #Setting the number of QA points and randomization groups
  total_points = nrow(xyz_all)
  map_pt_count = round(total_points*0.25)
  lower_qa_pt_bound = round(0.1*map_pt_count)
  upper_qa_pt_bound = round(0.15*map_pt_count)
  qa_pt_count = round(median(c(lower_qa_pt_bound, upper_qa_pt_bound)))
  approx_QAgroupsize = round(qa_pt_count/0.75)
  num_groups = round(total_points / approx_QAgroupsize)
  FifteenPercContourQAsize = round(qa_pt_count*0.15)
  FivePercContourQAsize = round(qa_pt_count*0.05)
  
  #Printing the results
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
  colnames(df) = c("X","Y","Z","CUBE_Uncert","Map","QA","Rand","Source")
  
  print('Writing out tables...')
  write.table(df,file=paste(out_path,"/",lake_name,"_xyz_uncert_source.csv",sep=""),sep=",",row.names = FALSE)
  write.table(df,file=paste(out_path,"/",lake_name,"_xyz_uncert_source.txt",sep=""),sep=",",row.names = FALSE)
  
  #Stopping the clock to record how long the program took to run
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  print(time.taken)
}


LidarToTextFiles <- function(home_dir, lake_name, WSEL, dam_elev){
  memory.size(max=TRUE)
  #Required libraries
  start.time <- Sys.time() #Timing how long the program takes to run
  
  list.of.packages <- c("lidR", "rgdal", "raster","stringr","dplyr","sf")
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)
  
  gc()
  
  library(lidR)
  library(rgdal)
  library(raster)
  library(stringr)
  library(dplyr)
  library(sf)
  
  lidar_path = paste(home_dir,"/Lidar",sep="")
  out_path = paste(home_dir,"/RTextFiles",sep="")
  ###########################################################################
  #Setting a variable shortcut for the working directory
  #Concatenating LiDAR files. If there is more than one .xyz file in the LiDAR folder, removes overlapping points.
  #This section will read both in and combine them into one file and export it with only X, Y, and Z data.
  print("Working on Lidar files...")
  setwd(lidar_path)
  ls=list.files(lidar_path,pattern=(".las"))
  
  #Reading in the Lidar files in a loop
  if(length(ls)!=0){
    i=1
    for(i in 1:length(ls)){
      las=readLAS(paste(lidar_path,"/",ls[i],sep=""))
      las=filter_duplicates(las) #Removing duplicate points
      #las=lasfiltersurfacepoints(las,res=1) #Filtering to get ground returns only and setting the resolution
      
      las_sp=as.spatial(las) #converting from .LAZ to a spatial points dataframe to a regular dataframe
      las_df=as.data.frame(las_sp,stringsAsFactors=FALSE)
      las_df=filter(las_df,Classification==2)
      
      
      if(length(ls)==1){
        las_df=subset(las_df,select=c("X","Y","Z")) #Filtering out all of the extra information and retaining XYZ if there is only 1 file
        las_all=las_df
        break
      }
      
      if(i==1){
        las_df=subset(las_df,select=c("X","Y","Z")) #Filtering out all of the extra information and retaining XYZ for multiple files
        las_all=las_df
      }else{
        las_df=subset(las_df,select=c("X","Y","Z")) #Filtering out all of the extra information and retaining XYZ for the last file if                                                          there are multiple
        las_all=rbind(las_all,las_df)
        
      }
    }
    
    rm(las)
    rm(las_sp)
    las_all$Source <- "Lidar"
    #The National Map DEM-->points conversion and concatenation with lidar
    print("Adding in the NED DEM...")
    ls = list.files(pattern="proj.tif$")
    dem_proj = raster(ls[1],xy=TRUE)
    
    temp_las <- st_as_sf(las_all,coords=c("X","Y"),crs="EPSG:6344")
    las_bbox <- st_bbox(temp_las,xmin=min(las_all$X), xmax=max(las_all$X),
                        ymin=min(las_all$Y), ymax=max(las_all$Y),y)
    las_bbox_poly=st_as_sfc(las_bbox)
    las_bbox_buffer=st_buffer(las_bbox_poly,1000)
    las_buffer_extent <- st_bbox(las_bbox_buffer)
    dem_clipped <- crop(x=dem_proj, y=las_buffer_extent)
    dem_df=as.data.frame(dem_clipped,xy=TRUE)
    dem_df$Source <- "NED"
    colnames(dem_df)=c("X","Y","Z","Source")
    dem_df=na.omit(dem_df)
    las_all <- rbind(las_all,dem_df)
  }else{
    print("Ope! No Lidar files here. Adding in the NED DEM...")
    ls = list.files(pattern="proj.tif$")
    dem_proj = raster(ls[1],xy=TRUE)
    dem_df = as.data.frame(dem_proj,xy=TRUE)
    las_all=dem_df
    dem_df$Source <- "NED"
    colnames(dem_df)=c("X","Y","Z","Source")
    dem_df=na.omit(dem_df)
    las_all <- dem_df
  }
  
  las_all=unique(las_all)
  #Cuts out LiDAR points below water surface elevation and above dam elevation.
  #Refer to Excel document for corrected WSE and the WSL Projections Proposal for top of dam/spillway
  print("Merging and moving stuff around...")
  elev=las_all[,3]
  clip_las=subset(las_all,elev<=(dam_elev + 5)) #Selects Lidar points that are less than 5 meters above the dam/spillway
  clip_las$Z=clip_las$Z*3.28 #Converting to feet
  
  print("Writing out the best-looking tables you've ever seen...")
  write.table(clip_las,file=paste(out_path,"/",lake_name,"_las.csv",sep=""),sep=",",row.names = FALSE)
  write.table(clip_las,file=paste(out_path,"/",lake_name,"_las.txt",sep=""),sep=",",row.names = FALSE)
  
  
  #Stopping the clock to record how long the program took to run
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  print(time.taken)
  
}

XCheckToTextFiles <- function(home_dir,lake_name){
  sort_path = paste(home_dir,"/Sort",sep="")
  out_path = paste(home_dir,"/RTextFiles",sep="")
  
  #Concatenating XYZ files only
  setwd(sort_path)
  ls=list.files(pattern="k.xyz$")
  print(ls)
  
  
  i=1
  for(i in 1:length(ls)){
    xcheck=read.table(ls[i],sep=" ")
    xcheck=as.data.frame(xcheck,stringsAsFactors=FALSE)
    
    if(i==1){
      colnames(xcheck)=c("X","Y","Zxcheck")
      all=xcheck
      temp=NULL
      xcheck_all=plyr::rbind.fill(all,temp)
      
    }else{
      colnames(xcheck)=c("X","Y","Zxcheck")
      temp=xcheck
      xcheck_all=plyr::rbind.fill(all,temp)
    }
    
  }
  xcheck_all$Zxcheck <- xcheck_all$Zxcheck*3.2808
  write.table(xcheck_all,file=paste(out_path,"/",lake_name,"_xcheck.csv",sep=""),sep=",",row.names = FALSE)
  write.table(xcheck_all,file=paste(out_path,"/",lake_name,"_xcheck.txt",sep=""),sep=",",row.names = FALSE)
  
  
}



####INITIAL INPUTS. THESE NEED TO BE SPECIFIED BY THE USER

XYZToTextFiles(home_dir = "E:/HYPACK_Projects/2020_DNR_Lakes/GardenCityOldDEMO/2020-07_GardenCityOld",
               lake_name = "GardenCityOld",
               WSEL =271.74,
               dam_elev =271.874,
               alt_source_types=c("ADCP","SB","GPS"))

LidarToTextFiles(home_dir = "E:/HYPACK_Projects/2020_DNR_Lakes/GardenCityOldDEMO/2020-07_GardenCityOld",
                 lake_name = "GardenCityOld",
                 WSEL =271.74,
                 dam_elev =271.874)

XCheckToTextFiles(home_dir = "E:/HYPACK_Projects/2020_DNR_Lakes/GardenCityOldDEMO/2020-07_GardenCityOld",
                  lake_name = "GardenCityOld")
