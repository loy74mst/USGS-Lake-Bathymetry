# -*- coding: utf-8 -*-
# ---------------------------------------------------------------------------
# SedimentQA_ModelBuilderArcPyTemplate.py
# Created on: 2021-10-21 11:43:55.00000
#   (generated by ArcGIS/ModelBuilder)
# Usage: SedimentQA_ModelBuilderArcPyTemplate <Final_sedGrid_Raster> <Output_QA_Folder> <Datum_Adjustment_Amount> <Lake_Name> <Old_Survey_Obliques_Feature_Class> <Buffer_Distance> <MBonly_Feature_Class> 
# Description: 
# ---------------------------------------------------------------------------


# Script arguments
import arcpy.analysis

Lake_Name = arcpy.GetParameterAsText(0)
Output_QA_Folder = arcpy.GetParameterAsText(1)
Old_Survey_xcheck = arcpy.GetParameterAsText(2)
Final_sedGrid_Raster = arcpy.GetParameterAsText(3)
MBonly_pts = arcpy.GetParameterAsText(4)
Buffer_Distance = arcpy.GetParameterAsText(5)
if Buffer_Distance == '#' or not Buffer_Distance:
    Buffer_Distance = "0.25 Meters" # provide a default value if unspecified

# Local variables:
QA_Geodatabase = Output_QA_Folder + "\\" + Lake_Name + "_QA.gdb"
sedQA_obBuffer = QA_Geodatabase + "\\" + Lake_Name + "_sedQA_obBuffer"
SedThicknessRaster = Output_QA_Folder + "\\" + Lake_Name + "_SedThicknessRaster.tif"
sedQA_MBclip = QA_Geodatabase + "\\" + Lake_Name + "_sedQA_MBclip"
sedQA_SPjoin = QA_Geodatabase + "\\" + Lake_Name + "_sedQA_SPjoin"
sedQA = QA_Geodatabase + "\\" + Lake_Name + "_sedQA"
sedQA_xls = Output_QA_Folder + "\\" + Lake_Name + "_sedQA.xls"
sedQA_Layer = "sedQA_Layer"

# Process: Buffering the old survey xcheck points
arcpy.AddMessage("Buffering the old survey xcheck points...")
arcpy.Buffer_analysis(Old_Survey_xcheck, sedQA_obBuffer, Buffer_Distance, "FULL", "ROUND", "NONE", "", "PLANAR")

# Process: Clip
arcpy.AddMessage("Clipping the new survey points to the old survey xcheck buffer...")
arcpy.Clip_analysis(MBonly_pts, sedQA_obBuffer, sedQA_MBclip, "")

# Process: Spatial Join
arcpy.AddMessage("Spatial joining the new survey elevations to the old survey xcheck points...")
arcpy.SpatialJoin_analysis(Old_Survey_xcheck, sedQA_MBclip, sedQA_SPjoin, "JOIN_ONE_TO_ONE", "KEEP_ALL","","CLOSEST", "", "")

# Process: Add Field (2)
arcpy.AddMessage("Adding additional fields and calculating values...")
arcpy.AddField_management(sedQA_SPjoin, "Difference", "DOUBLE", "", "", "", "", "NULLABLE", "NON_REQUIRED", "")

# Process: Calculate Field (2)
arcpy.CalculateField_management(sedQA_SPjoin, "Difference", "[Z] - [ELEV_adj]", "VB", "")

# Process: Add Field (3)
arcpy.AddField_management(sedQA_SPjoin, "Z_Elev", "DOUBLE", "", "", "", "", "NULLABLE", "NON_REQUIRED", "")

# Process: Calculate Field (3)
arcpy.CalculateField_management(sedQA_SPjoin, "Z_Elev", "[Z]", "VB", "")

# Process: Copy Raster
arcpy.AddMessage("Copying over the sediment thickness raster...")
arcpy.CopyRaster_management(Final_sedGrid_Raster, SedThicknessRaster, "", "", "-3.402823e+38", "NONE", "NONE", "", "NONE", "NONE", "TIFF", "NONE")

# Process: Add Surface Information
arcpy.AddMessage("Extracting sediment thickness raster values...")
arcpy.AddSurfaceInformation_3d(sedQA_SPjoin, SedThicknessRaster, "Z", "BILINEAR", "", "1", "0", "NO_FILTER")

# Process: Make Feature Layer
arcpy.AddMessage("Selecting out only the xcheck points that are within the buffer distance of a new survey point to be compared...")
arcpy.MakeFeatureLayer_management(sedQA_SPjoin, sedQA_Layer, "", "", "OBJECTID OBJECTID VISIBLE NONE;Shape Shape VISIBLE NONE;Join_Count Join_Count VISIBLE NONE;BufferDist BufferDist VISIBLE NONE;TARGET_FID TARGET_FID VISIBLE NONE;AREA AREA VISIBLE NONE;PERIMETER PERIMETER VISIBLE NONE;OBLIQUES_ OBLIQUES_ VISIBLE NONE;OBLIQUES_ID OBLIQUES_ID VISIBLE NONE;XSECT XSECT VISIBLE NONE;DEPTH DEPTH VISIBLE NONE;ELEV ELEV VISIBLE NONE;TINELEV TINELEV VISIBLE NONE;F_POLYGONID F_POLYGONID VISIBLE NONE;F_SCALE F_SCALE VISIBLE NONE;F_ANGLE F_ANGLE VISIBLE NONE;ELEV_adj ELEV_adj VISIBLE NONE;Join_Count Join_Count VISIBLE NONE;TARGET_FID TARGET_FID VISIBLE NONE;CID CID VISIBLE NONE;X X VISIBLE NONE;Y Y VISIBLE NONE;Z Z VISIBLE NONE;CUBE_Uncert CUBE_Uncert VISIBLE NONE;Map Map VISIBLE NONE;QA QA VISIBLE NONE;Rand Rand VISIBLE NONE;Source Source VISIBLE NONE;Difference Difference VISIBLE NONE;Z_Elev Z_Elev VISIBLE NONE")

# Process: Select Layer By Location
arcpy.SelectLayerByLocation_management(sedQA_Layer, "WITHIN_A_DISTANCE", sedQA_MBclip, Buffer_Distance, "NEW_SELECTION", "NOT_INVERT")

# Process: Copy Features
arcpy.Select_analysis(sedQA_Layer, sedQA, where_clause="Z <10")

# Process: Table To Excel
arcpy.AddMessage("Exporting the sediment QA table to an excel document...")
arcpy.TableToExcel_conversion(sedQA, sedQA_xls, "ALIAS", "CODE")

