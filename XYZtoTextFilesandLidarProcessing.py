# This code uses ### R SCRIPT ### and combines all CUBE .xyz and CUBE Uncertainty .xyz for each lake survey into one file and removing overlappling/non-matching points. More specifically, the list of files is read in from the directory. Regardless of the number of CUBE files in the directory, it will remove
# the cross check file and read in the uncertainty file separately while combining all of the xyz files into one data frame.
# This also binds the xyz and uncertainty columns while removing duplicates and creates two new columns for QA and Map.


import numpy as np
import pandas as pd
import laspy
import os
import glob
import statistics
import random
import rasterio
from shapely.geometry import box
import geopandas as gpd
from rasterio.mask import mask
import fiona
from rasterio.warp import calculate_default_transform, reproject, Resampling
from rastertodataframe import raster_to_dataframe


sort_path = home_dir + "/Sort"
lidar_path = home_dir + "/Lidar"
adcp_path = home_dir + "/ADCP"
gps_path = home_dir + "/GPS"
out_path = home_dir + "/RTextFiles"


def XYZToTextFiles(home_dir, lake_name, WSEL, dam_elev):

    ##Iterating through the Sort folder XYZ files
    #Looping through the various XYZ files and concatenating them.
    fileList = glob.glob(sort_path + '/*.xyz')
    fileList = [x for x in fileList if not x.endswith('XCheck.xyz') and not x.endswith('Xcheck.xyz') and not x.endswith('uncert.xyz')
                and not x.endswith('xcheck.xyz') and not x.endswith('xCheck.xyz')]
    i=0
    for i in range(len(fileList)):
        xyz = pd.read_table(fileList[i],sep=" ")
        if i==0:
            xyz.columns = ['X', 'Y', 'Z']
            all = xyz
            temp = pd.DataFrame()
            xyz_all = all.append(pd.DataFrame(data=temp), ignore_index=True)
        else:
            xyz = xyz.set_axis({"X", "Y", "Z"}, axis=1)
            temp = xyz
            xyz_all = all.append(pd.DataFrame(data=temp), ignore_index=True)
    xyz_all = xyz_all.assign(Source = "MB")

    ##Iterating through the Sort folder Uncertainty files
    #Looping through the various CUBE Uncertainty files and concatenating them.
    fileList = glob.glob(sort_path + '/*uncert.xyz')

    i = 0
    for i in range(len(fileList)):
        uncert = pd.read_table(fileList[i],sep=" ")
        if i==0:
            uncert.columns = ['X', 'Y', 'CUBE Uncert']
            all = uncert
            temp = pd.DataFrame()
            uncert_all = all.append(pd.DataFrame(data=temp), ignore_index=True)
        else:
            uncert = uncert.set_axis({"X", "Y", "CUBE Uncert"}, axis=1)
            temp = uncert
            uncert_all = all.append(pd.DataFrame(data=temp), ignore_index=True)
    uncert_all = uncert_all[uncert_all['CUBE Uncert'] < 5.0]

    ##Iterating through the ADCP folder csv files
    #Looping through the various ADCP csv files and concatenating them.
    fileList = glob.glob(adcp_path + '/*ADCP.csv')
    if os.path.exists(adcp_path):
        i = 0
        for i in range(len(fileList)):
            adcp = pd.read_table(fileList[i],sep=",")
            if i==0:
                adcp.columns = ['X', 'Y', 'Z']
                all = adcp
                temp = pd.DataFrame()
                adcp_all = all.append(pd.DataFrame(data=temp), ignore_index=True)
            else:
                adcp = adcp.set_axis({"X", "Y", "Z"}, axis=1)
                temp = adcp
                adcp_all = all.append(pd.DataFrame(data=temp), ignore_index=True)
        adcp_all = adcp_all.assign(Source="ADCP")

    ##Iterating through the GPS folder csv files if
    # Looping through the various ADCP csv files and concatenating them.
    fileList = glob.glob(gps_path + '/*GPS.csv')
    if os.path.exists(gps_path):
        i = 0
        for i in range(len(fileList)):
            gps = pd.read_table(fileList[i], sep=",")
            if i == 0:
                gps.columns = ['X', 'Y', 'Z']
                all = gps
                temp = pd.DataFrame()
                gps_all = all.append(pd.DataFrame(data=temp), ignore_index=True)
            else:
                gps = gps.set_axis({"X", "Y", "Z"}, axis=1)
                temp = gps
                gps_all = all.append(pd.DataFrame(data=temp), ignore_index=True)
        gps_all = gps_all.assign(Source="GPS")


    ###Data frame combining, unit conversion, rearranging, exporting
    print("Merging, combining, and moving some stuff around!")
    clip_xyz = xyz_all[xyz_all['Z'] < (WSEL + 0.5)]
    df = pd.merge(clip_xyz, uncert_all, on = ['X','Y'])
    if os.path.exists(adcp_path):
        df = df.append(adcp_all,ignore_index=True)
    if os.path.exists(gps_path):
        df = df.append(gps_all, ignore_index=True)

    #Removing any duplicates that may have occurred
    df = df.drop_duplicates()
    df['Z'] = df['Z']*3.2808
    df['CUBE Uncert'] = df['CUBE Uncert']*3.2808

    print("Total points: ",len(xyz_all))
    print("Map Point Count: ",round(len(xyz_all)*0.25))

    lower_qa_pt_bound = round(0.1*round(len(xyz_all)*0.25))
    upper_qa_pt_bound = round(0.15*round(len(xyz_all)*0.25))
    qa_point_count = round(statistics.median([lower_qa_pt_bound,upper_qa_pt_bound]))
    print("QA Point Count: ",qa_point_count)
    print("Approx QA Group Size: ",(qa_point_count/0.75))
    num_groups = round(len(xyz_all)) /(qa_point_count/0.75)
    print("Number of random groupings: ",num_groups)
    print("15% Contour QA size: ",round(qa_point_count*0.15))
    print("5% Contour QA Size: ",round(qa_point_count*0.05))

    random.seed(1996)
    df["Rand"] = np.random.randint(1,num_groups,size = len(df))
    df["QA"] = 0
    df["Map"] = 1
    df = df.reindex(columns=['X', 'Y', 'Z', 'CUBE Uncert', 'Map', 'QA', 'Rand','Source'])

    print("Writing out tables...")
    out_dir = out_path +"/"+ lake_name + "_xyz.csv"
    df.to_csv(out_dir, sep=",",index=False,header=True)

def LidarToTextFiles(home_dir, lake_Name, WSEL, dam_elev):
    fileList = glob.glob(lidar_path + "/*.las")

    #Reading in LiDAR files
    i=0
    for i in range(len(fileList)):
        las = laspy.read(fileList[i])
        las_x = pd.DataFrame(las.X/100)
        las_y = pd.DataFrame(las.Y/100)
        las_z = pd.DataFrame((las.Z/100)*3.2808)
        las_df = pd.concat([las_x, las_y, las_z], axis=1)
        las_df.columns = ['X', "Y", "Z"]

        if i==0:
            las_all = las_df
        else:
            las_all = las_all.append(las_df,ignore_index=True)

    minx, miny = min(las_all['X']), min(las_all['Y'])
    maxx, maxy = max(las_all['X']), max(las_all['Y'])

    #Reading in the NED and combining the points with the LiDAR
    fileList = glob.glob(lidar_path + "/*.tif")
    ned = rasterio.open(fileList[0])
    bbox = box(minx,miny,maxx,maxy)


    geo = gpd.GeoDataFrame({'geometry':bbox},index=[0],crs=ned.crs)

    ############STILL WORKING ON THIS PART. NEXT STEP: CREATE A POINT AT EACH PIXEL, EXTRACT Z, CONVERT TO DATAFRAME
    def getFeatures(gdf):
        """Function to parse features from GeoDataFrame in such a manner that rasterio wants them"""
        import json
        return [json.loads(gdf.to_json())['features'][0]['geometry']]
    coords = getFeatures(geo)
    out_ned, out_transform = mask(ned, coords)
    with rasterio.open(fileList[0]) as src:
        ned, out_transform = mask(src, coords, crop=True)
        out_meta = src.meta

    # Save clipped imagery
    out_meta.update({"driver": "GTiff",
                     "height": ned.shape[1],
                     "width": ned.shape[1],
                     "transform": out_transform})

    with rasterio.open(lidar_path + "/NED_clip.tif", "w", **out_meta) as dest:
        dest.write(out_ned)



    #ned_df = raster_to_dataframe('NED_clip.tif')

    import georasters as gr
    ned = rasterio.open(fileList[0])
    ned_clipped = rasterio.open(lidar_path + "/NED_clip.tif")
    test_gr = gr.GeoRaster(ned_clipped,geot =ned_clipped.transform,projection=ned_clipped.crs)

    test_pd = gr.to_pandas(test_gr)

