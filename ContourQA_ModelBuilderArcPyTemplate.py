# -*- coding: utf-8 -*-
# ---------------------------------------------------------------------------
# ContourQA_ModelBuilderArcPyTemplate.py
# Created on: 2021-09-29 14:27:20.00000
#   (generated by ArcGIS/ModelBuilder)
# Usage: ContourQA_ModelBuilderArcPyTemplate <Output_QA_Geodatabase> <Final_xyz_map_thinned_Points> <Lake_Name> <Buffer_Distance> <QA_Folder> <Final_Contours> 
# Description: 
# ---------------------------------------------------------------------------

# Import arcpy module
import arcpy

# Script arguments
Output_QA_Geodatabase = arcpy.GetParameterAsText(0)
if Output_QA_Geodatabase == '#' or not Output_QA_Geodatabase:
    Output_QA_Geodatabase = "E:\\HYPACK_Projects\\2020_DNR_Lakes\\2020-07_MilanCityLake\\GIS\\QA\\MilanCityLake_QA.gdb" # provide a default value if unspecified

Final_xyz_map_thinned_Points = arcpy.GetParameterAsText(1)
if Final_xyz_map_thinned_Points == '#' or not Final_xyz_map_thinned_Points:
    Final_xyz_map_thinned_Points = "E:\\HYPACK_Projects\\2020_DNR_Lakes\\2020-07_MilanCityLake\\GIS\\MilanCityLake.gdb\\MilanCityLake_xyz_map_thinned3" # provide a default value if unspecified

Lake_Name = arcpy.GetParameterAsText(2)
if Lake_Name == '#' or not Lake_Name:
    Lake_Name = "MilanCityLake" # provide a default value if unspecified

Buffer_Distance = arcpy.GetParameterAsText(3)
if Buffer_Distance == '#' or not Buffer_Distance:
    Buffer_Distance = "0.1 Meters" # provide a default value if unspecified

QA_Folder = arcpy.GetParameterAsText(4)
if QA_Folder == '#' or not QA_Folder:
    QA_Folder = "E:\\HYPACK_Projects\\2020_DNR_Lakes\\2020-07_MilanCityLake\\GIS\\QA" # provide a default value if unspecified

Final_Contours = arcpy.GetParameterAsText(5)
if Final_Contours == '#' or not Final_Contours:
    Final_Contours = "E:\\HYPACK_Projects\\2020_DNR_Lakes\\2020-07_MilanCityLake\\GIS\\MilanCityLake.gdb\\MilanCityLake_allcontoursSmoothed" # provide a default value if unspecified

# Local variables:
v_Lake_Name__contourBuffer = "%Output QA Geodatabase%\\%Lake Name%_contourBuffer"
v_Lake_Name__contourMapClip = "%Output QA Geodatabase%\\%Lake Name%_contourMapClip"
v_Lake_Name__contourQAjoin = "%Output QA Geodatabase%\\%Lake Name%_contourQAjoin"
v_Lake_Name__contourQA = v_Lake_Name__contourQAjoin
v_Lake_Name__contourQA_xls = "%QA Folder%\\%Lake Name%_contourQA.xls"

# Process: Buffer
arcpy.Buffer_analysis(Final_Contours, v_Lake_Name__contourBuffer, Buffer_Distance, "FULL", "ROUND", "NONE", "", "PLANAR")

# Process: Clip
arcpy.Clip_analysis(Final_xyz_map_thinned_Points, v_Lake_Name__contourBuffer, v_Lake_Name__contourMapClip, "")

# Process: Spatial Join
arcpy.SpatialJoin_analysis(v_Lake_Name__contourMapClip, Final_Contours, v_Lake_Name__contourQAjoin, "JOIN_ONE_TO_ONE", "KEEP_ALL", "", "CLOSEST", "", "")

# Process: Table to Table
arcpy.TableToTable_conversion(v_Lake_Name__contourQAjoin, Output_QA_Geodatabase, "%Lake Name%_contourQAtable", "", "Id \"Id\" true true false 4 Long 0 0 ,First,#,%Output QA Geodatabase%\\%Lake Name%_contourQAjoin,Id,-1,-1;Contour \"Contour\" true true false 8 Double 0 0 ,First,#,%Output QA Geodatabase%\\%Lake Name%_contourQAjoin,Contour,-1,-1;InLine_FID \"InLine_FID\" true true false 4 Long 0 0 ,First,#,%Output QA Geodatabase%\\%Lake Name%_contourQAjoin,InLine_FID,-1,-1;X \"X\" true true false 8 Double 0 0 ,First,#,%Output QA Geodatabase%\\%Lake Name%_contourQAjoin,X,-1,-1;Y \"Y\" true true false 8 Double 0 0 ,First,#,%Output QA Geodatabase%\\%Lake Name%_contourQAjoin,Y,-1,-1;Z \"Z\" true true false 8 Double 0 0 ,First,#,%Output QA Geodatabase%\\%Lake Name%_contourQAjoin,Z,-1,-1;CUBE_Uncert \"CUBE Uncert\" true true false 8 Double 0 0 ,First,#,%Output QA Geodatabase%\\%Lake Name%_contourQAjoin,CUBE_Uncert,-1,-1;Map \"Map\" true true false 4 Long 0 0 ,First,#,%Output QA Geodatabase%\\%Lake Name%_contourQAjoin,Map,-1,-1;QA \"QA\" true true false 4 Long 0 0 ,First,#,%Output QA Geodatabase%\\%Lake Name%_contourQAjoin,QA,-1,-1;Rand \"Rand\" true true false 4 Long 0 0 ,First,#,%Output QA Geodatabase%\\%Lake Name%_contourQAjoin,Rand,-1,-1;Source \"Source\" true true false 8000 Text 0 0 ,First,#,%Output QA Geodatabase%\\%Lake Name%_contourQAjoin,Source,-1,-1;Shape_Length \"Shape_Length\" false true true 8 Double 0 0 ,First,#,%Output QA Geodatabase%\\%Lake Name%_contourQAjoin,Shape_Length,-1,-1", "")

# Process: Table To Excel
arcpy.TableToExcel_conversion(v_Lake_Name__contourQA, v_Lake_Name__contourQA_xls, "ALIAS", "CODE")

