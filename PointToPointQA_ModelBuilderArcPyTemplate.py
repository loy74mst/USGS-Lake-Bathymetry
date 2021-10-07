# -*- coding: utf-8 -*-
# ---------------------------------------------------------------------------
# PointToPointQA_ModelBuilderArcPyTemplate.py
# Created on: 2021-09-29 14:18:56.00000
#   (generated by ArcGIS/ModelBuilder)
# Usage: PointToPointQA_ModelBuilderArcPyTemplate <Xcheck_CSV_File> <Lake_Name> <Output_QA_Geodatabase> <Full_Bathy_Points_Dataset> <Buffer_Distance> <QA_Folder> 
# Description: 
# ---------------------------------------------------------------------------

# Import arcpy module
import arcpy

# Script arguments
Xcheck_CSV_File = arcpy.GetParameterAsText(0)
if Xcheck_CSV_File == '#' or not Xcheck_CSV_File:
    Xcheck_CSV_File = "E:\\HYPACK_Projects\\2020_DNR_Lakes\\2020-07_MilanCityLake\\RTextFiles\\MilanCityLake_xcheck.csv" # provide a default value if unspecified

Lake_Name = arcpy.GetParameterAsText(1)
if Lake_Name == '#' or not Lake_Name:
    Lake_Name = "MilanCityLake" # provide a default value if unspecified

Output_QA_Geodatabase = arcpy.GetParameterAsText(2)
if Output_QA_Geodatabase == '#' or not Output_QA_Geodatabase:
    Output_QA_Geodatabase = "E:\\HYPACK_Projects\\2020_DNR_Lakes\\2020-07_MilanCityLake\\GIS\\QA\\MilanCityLake_QA.gdb" # provide a default value if unspecified

Full_Bathy_Points_Dataset = arcpy.GetParameterAsText(3)
if Full_Bathy_Points_Dataset == '#' or not Full_Bathy_Points_Dataset:
    Full_Bathy_Points_Dataset = "E:\\HYPACK_Projects\\2020_DNR_Lakes\\2020-07_MilanCityLake\\Provisional-Final\\MilanCityLake2020_bathy_pts.shp" # provide a default value if unspecified

Buffer_Distance = arcpy.GetParameterAsText(4)
if Buffer_Distance == '#' or not Buffer_Distance:
    Buffer_Distance = "0.05 Meters" # provide a default value if unspecified

QA_Folder = arcpy.GetParameterAsText(5)
if QA_Folder == '#' or not QA_Folder:
    QA_Folder = "E:\\HYPACK_Projects\\2020_DNR_Lakes\\2020-07_MilanCityLake\\GIS\\QA" # provide a default value if unspecified

# Local variables:
MilanCityLake_xcheck_Layer = "MilanCityLake_xcheck_Layer"
Output_Feature_Class = "E:\\HYPACK_Projects\\2020_DNR_Lakes\\2020-07_MilanCityLake\\GIS\\QA\\MilanCityLake_QA.gdb\\MilanCityLake_xcheck"
v_Lake_Name__p2pQA_extent = "%Output QA Geodatabase%\\%Lake Name%_p2pQA_extent"
v_Lake_Name__p2pQA_MBclip = "%Output QA Geodatabase%\\%Lake Name%_p2pQA_MBclip"
v_LakeName__p2pQA_MBclip_Laye = "%LakeName%_p2pQA_MBclip_Laye"
v_LakeName__p2pQA_MBclip_Laye__3_ = v_LakeName__p2pQA_MBclip_Laye
v_Lake_Name__p2pQA_MBbuffer = "%Output QA Geodatabase%\\%Lake Name%_p2pQA_MBbuffer"
v_Lake_Name__p2pQA_select = "%Output QA Geodatabase%\\%Lake Name%_p2pQA_select"
v_Lake_Name__p2pQA = "%Output QA Geodatabase%\\%Lake Name%_p2pQA"
v_Lake_Name__p2pQAtable = v_Lake_Name__p2pQA
v_Lake_Name__p2pQA_xls = "%QA Folder%\\%Lake Name%_p2pQA.xls"

# Process: Make XY Event Layer
arcpy.MakeXYEventLayer_management(Xcheck_CSV_File, "x", "y", MilanCityLake_xcheck_Layer, "PROJCS['NAD_1983_2011_UTM_Zone_15N',GEOGCS['GCS_NAD_1983_2011',DATUM['D_NAD_1983_2011',SPHEROID['GRS_1980',6378137.0,298.257222101]],PRIMEM['Greenwich',0.0],UNIT['Degree',0.0174532925199433]],PROJECTION['Transverse_Mercator'],PARAMETER['False_Easting',500000.0],PARAMETER['False_Northing',0.0],PARAMETER['Central_Meridian',-93.0],PARAMETER['Scale_Factor',0.9996],PARAMETER['Latitude_Of_Origin',0.0],UNIT['Meter',1.0]];-5120900 -9998100 10000;-100000 10000;-100000 10000;0.001;0.001;0.001;IsHighPrecision", "Z")

# Process: Feature Class to Feature Class
arcpy.FeatureClassToFeatureClass_conversion(MilanCityLake_xcheck_Layer, Output_QA_Geodatabase, "%Lake Name%_xcheck", "", "X \"X\" true true false 8 Double 0 0 ,First,#,MilanCityLake_xcheck_Layer,X,-1,-1;Y \"Y\" true true false 8 Double 0 0 ,First,#,MilanCityLake_xcheck_Layer,Y,-1,-1;Z \"Z\" true true false 8 Double 0 0 ,First,#,MilanCityLake_xcheck_Layer,Z,-1,-1", "")

# Process: Minimum Bounding Geometry
arcpy.MinimumBoundingGeometry_management(Output_Feature_Class, v_Lake_Name__p2pQA_extent, "RECTANGLE_BY_AREA", "ALL", "", "NO_MBG_FIELDS")

# Process: Clip
arcpy.Clip_analysis(Full_Bathy_Points_Dataset, v_Lake_Name__p2pQA_extent, v_Lake_Name__p2pQA_MBclip, "")

# Process: Make Feature Layer
arcpy.MakeFeatureLayer_management(v_Lake_Name__p2pQA_MBclip, v_LakeName__p2pQA_MBclip_Laye, "", "", "OBJECTID OBJECTID VISIBLE NONE;Shape Shape VISIBLE NONE;X X VISIBLE NONE;Y Y VISIBLE NONE;Z Z VISIBLE NONE;CUBE_Uncer CUBE_Uncer VISIBLE NONE;Map Map VISIBLE NONE;QA QA VISIBLE NONE;Source Source VISIBLE NONE")

# Process: Buffer
arcpy.Buffer_analysis(v_Lake_Name__p2pQA_MBclip, v_Lake_Name__p2pQA_MBbuffer, Buffer_Distance, "FULL", "ROUND", "NONE", "", "PLANAR")

# Process: Clip (2)
arcpy.Clip_analysis(Output_Feature_Class, v_Lake_Name__p2pQA_MBbuffer, v_Lake_Name__p2pQA_select, "")

# Process: Select Layer By Location
arcpy.SelectLayerByLocation_management(v_LakeName__p2pQA_MBclip_Laye, "WITHIN_A_DISTANCE", v_Lake_Name__p2pQA_select, "0.05 Meters", "NEW_SELECTION", "NOT_INVERT")

# Process: Spatial Join
arcpy.SpatialJoin_analysis(v_LakeName__p2pQA_MBclip_Laye__3_, v_Lake_Name__p2pQA_select, v_Lake_Name__p2pQA, "JOIN_ONE_TO_ONE", "KEEP_ALL", "X \"X\" true true false 8 Double 0 0 ,First,#,%LakeName%_p2pQA_MBclip_Laye,X,-1,-1;Y \"Y\" true true false 8 Double 0 0 ,First,#,%LakeName%_p2pQA_MBclip_Laye,Y,-1,-1;Z \"Z\" true true false 8 Double 0 0 ,First,#,%LakeName%_p2pQA_MBclip_Laye,Z,-1,-1;CUBE_Uncer \"CUBE_Uncer\" true true false 8 Double 0 0 ,First,#,%LakeName%_p2pQA_MBclip_Laye,CUBE_Uncer,-1,-1;Map \"Map\" true true false 4 Long 0 0 ,First,#,%LakeName%_p2pQA_MBclip_Laye,Map,-1,-1;QA \"QA\" true true false 4 Long 0 0 ,First,#,%LakeName%_p2pQA_MBclip_Laye,QA,-1,-1;Source \"Source\" true true false 254 Text 0 0 ,First,#,%LakeName%_p2pQA_MBclip_Laye,Source,-1,-1;X_1 \"X\" true true false 8 Double 0 0 ,First,#,%Output QA Geodatabase%\\%Lake Name%_p2pQA_select,X,-1,-1;Y_1 \"Y\" true true false 8 Double 0 0 ,First,#,%Output QA Geodatabase%\\%Lake Name%_p2pQA_select,Y,-1,-1;Z_1 \"Z\" true true false 8 Double 0 0 ,First,#,%Output QA Geodatabase%\\%Lake Name%_p2pQA_select,Z,-1,-1", "CLOSEST", "", "")

# Process: Table to Table
arcpy.TableToTable_conversion(v_Lake_Name__p2pQA, Output_QA_Geodatabase, "%Lake Name%_p2pQAtable", "", "X \"X\" true true false 8 Double 0 0 ,First,#,%Output QA Geodatabase%\\%Lake Name%_p2pQA,X,-1,-1;Y \"Y\" true true false 8 Double 0 0 ,First,#,%Output QA Geodatabase%\\%Lake Name%_p2pQA,Y,-1,-1;Z \"Z\" true true false 8 Double 0 0 ,First,#,%Output QA Geodatabase%\\%Lake Name%_p2pQA,Z,-1,-1;CUBE_Uncer \"CUBE_Uncer\" true true false 8 Double 0 0 ,First,#,%Output QA Geodatabase%\\%Lake Name%_p2pQA,CUBE_Uncer,-1,-1;Map \"Map\" true true false 4 Long 0 0 ,First,#,%Output QA Geodatabase%\\%Lake Name%_p2pQA,Map,-1,-1;QA \"QA\" true true false 4 Long 0 0 ,First,#,%Output QA Geodatabase%\\%Lake Name%_p2pQA,QA,-1,-1;Source \"Source\" true true false 254 Text 0 0 ,First,#,%Output QA Geodatabase%\\%Lake Name%_p2pQA,Source,-1,-1;X_1 \"X\" true true false 8 Double 0 0 ,First,#,%Output QA Geodatabase%\\%Lake Name%_p2pQA,X_1,-1,-1;Y_1 \"Y\" true true false 8 Double 0 0 ,First,#,%Output QA Geodatabase%\\%Lake Name%_p2pQA,Y_1,-1,-1;Z_1 \"Z\" true true false 8 Double 0 0 ,First,#,%Output QA Geodatabase%\\%Lake Name%_p2pQA,Z_1,-1,-1", "")

# Process: Table To Excel
arcpy.TableToExcel_conversion(v_Lake_Name__p2pQAtable, v_Lake_Name__p2pQA_xls, "ALIAS", "CODE")

