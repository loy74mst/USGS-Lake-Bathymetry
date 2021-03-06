# -*- coding: utf-8 -*-
# ---------------------------------------------------------------------------
# CreateContours_ModelBuilderTemplate.py
# Created on: 2021-09-15 13:45:47.00000
#   (generated by ArcGIS/ModelBuilder)
# Usage: CreateContours_ModelBuilderTemplate <Lake_Name> <TIN_Surface> <Workspace> <Contour_interval> <Base_contour> <Max_Spillway_Elevation> <Water_surface_Elevation> <Map_Resolution> <Smoothing_Tolerance>
# Description:
# ---------------------------------------------------------------------------


# Script arguments
Lake_Name = arcpy.GetParameterAsText(0)
TIN_Surface = arcpy.GetParameterAsText(1)
Workspace = arcpy.GetParameterAsText(2)
if Workspace == '#' or not Workspace:
    Workspace = "C:\Users\loyler\OneDrive - DOI\Documents\ArcGIS\Default.gdb" # provide a default value if unspecified
Contour_interval = arcpy.GetParameterAsText(3)
Base_contour = arcpy.GetParameterAsText(4)
Max_Spillway_Elevation = arcpy.GetParameterAsText(5)
Water_surface_Elevation = arcpy.GetParameterAsText(6)
Map_Resolution = arcpy.GetParameterAsText(7)
if Map_Resolution == '#' or not Map_Resolution:
    Map_Resolution = "CELLSIZE 0.5" # provide a default value if unspecified
Smoothing_Tolerance = arcpy.GetParameterAsText(8)
if Smoothing_Tolerance == '#' or not Smoothing_Tolerance:
    Smoothing_Tolerance = "1.5 Meters" # provide a default value if unspecified

arcpy.env.overwriteOutput = True
arcpy.env.workspace = Workspace

# Local variables:
wsel = Water_surface_Elevation
dam_elev = Max_Spillway_Elevation

wsel_contour = Workspace + "\\" + Lake_Name + "_wsel"
dam_elev_contour = Workspace + "\\" + Lake_Name + "_damelev"
raster = Workspace + "\\" + Lake_Name + "_raster"
contoursfromraster01 = Workspace + "\\" +Lake_Name + "_contoursfromraster01"
contoursfromraster = Workspace + "\\" + Lake_Name + "_contoursfromraster"
selected = Workspace + "\\" + Lake_Name + "_selected"
allcontoursUnsmoothed = Workspace + "\\" + Lake_Name + "_allcontoursUnsmoothed"
smoothed = Workspace + "\\" + Lake_Name + "_allcontoursSmoothed"

# Process: TIN to Raster
arcpy.AddMessage("Creating the bathy raster from the TIN...")

cell_size = "CELLSIZE " + Map_Resolution
tempEnvironment0 = arcpy.env.outputCoordinateSystem
arcpy.env.outputCoordinateSystem = "PROJCS['NAD_1983_2011_UTM_Zone_15N',GEOGCS['GCS_NAD_1983_2011',DATUM['D_NAD_1983_2011',SPHEROID['GRS_1980',6378137.0,298.257222101]],PRIMEM['Greenwich',0.0],UNIT['Degree',0.0174532925199433]],PROJECTION['Transverse_Mercator'],PARAMETER['False_Easting',500000.0],PARAMETER['False_Northing',0.0],PARAMETER['Central_Meridian',-93.0],PARAMETER['Scale_Factor',0.9996],PARAMETER['Latitude_Of_Origin',0.0],UNIT['Meter',1.0]]"
arcpy.TinRaster_3d(TIN_Surface, raster, "FLOAT", "NATURAL_NEIGHBORS", cell_size, "1")


# Process: Contour (2)
arcpy.AddMessage('Creating the water-surface elevation contour...')
arcpy.Contour_3d(raster, contoursfromraster01, "0.1", wsel, "1", "CONTOUR", "")

# Process: Select (2)
arcpy.AddMessage('Creating the spillway elevation contour...')
select_criteria =  "( Shape_Length >25) AND (Contour = " + dam_elev + ")"
arcpy.Select_analysis(contoursfromraster01, dam_elev_contour, select_criteria)

# Process: Select
select_criteria2 = "( Shape_Length >25) AND (Contour = " + wsel + ")"
arcpy.Select_analysis(contoursfromraster01, wsel_contour, select_criteria2)


# Process: Contour
arcpy.AddMessage('Creating the main lake body contours...')
arcpy.Contour_3d(raster, contoursfromraster, Contour_interval, Base_contour, "1", "CONTOUR", "")

# Process: Select (3)
select_criteria3 = "( Shape_Length >25) AND ( Contour < " + dam_elev + " + " + Contour_interval + ")"
arcpy.Select_analysis(contoursfromraster, selected, select_criteria3)

# Process: Merge
arcpy.AddMessage('Merging all contours together...')
arcpy.Merge_management([dam_elev_contour,wsel_contour,selected], allcontoursUnsmoothed, "Shape_Length \"Shape_Length\" true true true 0 Double 0 0 ,First,#,%Workspace%/%Lake Name%_spillwayelev,Shape_Length,-1,-1,%Workspace%/%Lake Name%_spillwayelev,Shape_length,-1,-1,%Workspace%/%Lake Name%_wsel,Shape_Length,-1,-1,%Workspace%/%Lake Name%_wsel,Shape_length,-1,-1,%Workspace%/%Lake Name%_selected,Shape_Length,-1,-1,%Workspace%/%Lake Name%_selected,Shape_length,-1,-1;CONTOUR \"CONTOUR\" true true false 0 Double 0 0 ,First,#,%Workspace%/%Lake Name%_spillwayelev,CONTOUR,-1,-1,%Workspace%/%Lake Name%_wsel,CONTOUR,-1,-1,%Workspace%/%Lake Name%_selected,CONTOUR,-1,-1")

# Process: Smooth Line
arcpy.AddMessage('Smoothing contours...')
arcpy.SmoothLine_cartography(allcontoursUnsmoothed, smoothed, "PAEK", Smoothing_Tolerance, "FIXED_CLOSED_ENDPOINT", "NO_CHECK", "")

arcpy.AddMessage('All done!')

