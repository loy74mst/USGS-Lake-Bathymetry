# -*- coding: utf-8 -*-
# ---------------------------------------------------------------------------
# CreateVerticalAccuracyMap_ModelBuilderArcPyTemplate.py
# Created on: 2021-09-30 13:27:54.00000
#   (generated by ArcGIS/ModelBuilder)
# Usage: CreateVerticalAccuracyMap_ModelBuilderArcPyTemplate <Lake_Name> <Output_QA_Geodatabase> <QA_Folder> <MB_Extent_Polygon> <Map_Resolution__meters_> <TINQAselect_Feature_Class> 
# Description: 
# ---------------------------------------------------------------------------

# Import arcpy module
import arcpy

# Script arguments
Lake_Name = arcpy.GetParameterAsText(0)
if Lake_Name == '#' or not Lake_Name:
    Lake_Name = "MilanCityLake" # provide a default value if unspecified

Output_QA_Geodatabase = arcpy.GetParameterAsText(1)
if Output_QA_Geodatabase == '#' or not Output_QA_Geodatabase:
    Output_QA_Geodatabase = "E:\\HYPACK_Projects\\2020_DNR_Lakes\\2020-07_MilanCityLake\\GIS\\QA\\MilanCityLake_QA.gdb" # provide a default value if unspecified

QA_Folder = arcpy.GetParameterAsText(2)
if QA_Folder == '#' or not QA_Folder:
    QA_Folder = "E:\\HYPACK_Projects\\2020_DNR_Lakes\\2020-07_MilanCityLake\\GIS\\QA" # provide a default value if unspecified

MB_Extent_Polygon = arcpy.GetParameterAsText(3)
if MB_Extent_Polygon == '#' or not MB_Extent_Polygon:
    MB_Extent_Polygon = "E:\\HYPACK_Projects\\2020_DNR_Lakes\\2020-07_MilanCityLake\\GIS\\MilanCityLake.gdb\\MilanCityLake_MBpoly" # provide a default value if unspecified

Map_Resolution__meters_ = arcpy.GetParameterAsText(4)
if Map_Resolution__meters_ == '#' or not Map_Resolution__meters_:
    Map_Resolution__meters_ = "0.5" # provide a default value if unspecified

TINQAselect_Feature_Class = arcpy.GetParameterAsText(5)
if TINQAselect_Feature_Class == '#' or not TINQAselect_Feature_Class:
    TINQAselect_Feature_Class = "E:\\HYPACK_Projects\\2020_DNR_Lakes\\2020-07_MilanCityLake\\GIS\\QA\\MilanCityLake_QA.gdb\\MilanCityLake_TINQAselect" # provide a default value if unspecified

# Local variables:
String2 = "%Output QA Geodatabase%\\%Lake Name%_MBextentRaster"
v_String2_ = "%String2%"
MilanCityLake_TINQAselect = TINQAselect_Feature_Class
MilanCityLake_TINQAselect__2_ = MilanCityLake_TINQAselect
v_String1_ = "%String1%"
v_Lake_Name__VAraster_tif = "%QA Folder%\\%Lake Name%_VAraster.tif"
String1 = "%Output QA Geodatabase%\\%Lake Name%_VAInterp"

# Process: Polygon to Raster
arcpy.PolygonToRaster_conversion(MB_Extent_Polygon, "Coffee", v_String2_, "CELL_CENTER", "NONE", "0.5")

# Process: Add Field
arcpy.AddField_management(TINQAselect_Feature_Class, "Difference", "DOUBLE", "", "", "", "", "NULLABLE", "NON_REQUIRED", "")

# Process: Calculate Field
arcpy.CalculateField_management(MilanCityLake_TINQAselect, "Difference", "[Z_MBelev] - [Z]", "VB", "")

# Process: Natural Neighbor
arcpy.NaturalNeighbor_3d(MilanCityLake_TINQAselect__2_, "Difference", v_String1_, Map_Resolution__meters_)

# Process: Con
arcpy.gp.Con_sa(v_String2_, v_String1_, v_Lake_Name__VAraster_tif, "", "\"VALUE\"=0")

