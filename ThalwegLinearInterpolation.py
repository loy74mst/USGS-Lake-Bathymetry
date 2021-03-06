"""
XYZtoTIN
==============

Script:   ThalwegLinearInterpolation.py

Author:   loyler@usgs.gov

Modified: 2021-09-07

Purpose:  Creating linearly interpolated points between multibeam xyz_thinned and las_thinned points in coves and areas
            where connection points are sparse.

Useage: This is the local script version of this tool. There is another ArcMap-based too that works as well.

References:

:---------------------------------------------------------------------:
"""
# -*- coding: utf-8 -*-
# ---------------------------------------------------------------------------
# ThalwegLinearInterpolation_ModelBuilderArcpyTemplate.py
# Created on: 2021-08-09 10:14:00.00000
#   (generated by ArcGIS/ModelBuilder)
#
# Description: This script creates the linear interpolated thalweg points that connect the multibeam data and the Lidar in coves and areas where points
# are sparse. Inputs required are the xyz_thinned.shp, las_thinned.shp, and the polylines that will be used for the interpolation. Naming convention
# should be as follows: [LakeName]_LR1 for the first line of linear interpolation, [LakeName]_LR2 for the second, and so forth. TINs that are generated
# afterwards should follow a similar convention so that the linear interpolation points correspond to the TIN they were used to generate. LR1 was used
# to create TIN1 and so forth. The prelimTIN has no interpolation points. Use this tool well! A lot of time and effort went into creating it.
# ---------------------------------------------------------------------------

# Import arcpy module
import arcpy
#Checking to make sure the appropriate extensions are activated
class LicenseError(Exception):
    pass

try:
    if arcpy.CheckExtension("3D") == "Available":
        arcpy.CheckOutExtension("3D")
    else:
        # raise a custom exception
        raise LicenseError
except LicenseError:
    print("3D Analyst license is unavailable")
except arcpy.ExecuteError:
    print(arcpy.GetMessages(2))

#Timing how long it takes to run each function
from datetime import datetime
start_time = datetime.now()

#Function definition:
def linear_interp(LakeName,Home_GDB, LR_Shapefile, Map_Resolution):
    arcpy.env.overwriteOutput = True
    arcpy.env.workspace = Home_GDB


    # Local variables:
    lr_shapefile = LR_Shapefile
    distance = Map_Resolution
    xyz_pts = LakeName + "_xyz_thinned"
    las_pts = LakeName + "_las_thinned"
    lr_pts = LR_Shapefile + "points"
    spjoin1=Home_GDB + "\\"+ "spjoin1"
    spjoin2 = Home_GDB + "\\" + "spjoin2"

    #Setting backup defaults
    if distance == '#' or not distance:
        distance = "0.5 Meters" # provide a default value if unspecified

    if Home_GDB == '#' or not Home_GDB:
        Output_GDB = "C:\\Users\\loyler\\Documents\\ArcGIS\\Default.gdb"  # provide a default value if unspecified


    # Process: Spatial Joining the XYZ point value to the polyline to get the low evaluation value
    arcpy.SpatialJoin_analysis(lr_shapefile, xyz_pts,spjoin1)

    # Process: Spatial Joining the Lidar point value to the polyline to get the high evaluation value
    arcpy.SpatialJoin_analysis(spjoin1, las_pts,spjoin2)

    # Process: Generate Points Along Lines
    arcpy.GeneratePointsAlongLines_management(spjoin2, lr_pts, "DISTANCE", distance, "", "")

    # Process: Add XY Coordinates for computing Euclidian distance
    arcpy.AddXY_management(lr_pts)

    #Adding all of the necessary fields: LineLength, Euclidian Distance, Gradient, Interpolated Z value
    arcpy.AddField_management(lr_pts, "LineLength", "DOUBLE", "", "", "", "", "NULLABLE", "NON_REQUIRED", "")
    arcpy.AddField_management(lr_pts, "Euc_Dist", "DOUBLE", "", "", "", "", "NULLABLE", "NON_REQUIRED", "")
    arcpy.AddField_management(lr_pts, "Gradient", "DOUBLE", "", "", "", "", "NULLABLE", "NON_REQUIRED", "")
    arcpy.AddField_management(lr_pts, "Z_interp", "DOUBLE", "", "", "", "", "NULLABLE", "NON_REQUIRED", "")

    #Calculating LineLength in feet (SHAPE_Length is currently in meters)
    arcpy.CalculateField_management(lr_pts, "LineLength", "!SHAPE_Length!*3.2808", "PYTHON_9.3")


    #Calculating the Euclidian Distance between each generated point and the original XYZ point of interest
    arcpy.CalculateField_management(lr_pts, "Euc_Dist", "(math.sqrt((!POINT_X! - !X!)**2 + (!POINT_Y! - !Y!)**2))*3.2808", "PYTHON_9.3")

    #Calculating the Gradient --> how quickly the elevation increases with respect to the length of the line segment
    arcpy.CalculateField_management(lr_pts, "Gradient", "( !Z_1! - !Z!) / !LineLength!", "PYTHON_9.3")

    #Calculating the interpolated Z value for each point
    arcpy.CalculateField_management(lr_pts, "Z_interp", "!Z! +( !Gradient! * !Euc_Dist! )", "PYTHON_9.3")

    #Deleting unneccesary fields
    arcpy.ImportToolbox("C:/Program Files (x86)/XTools/XTools Pro/Toolbox/XTools Pro.tbx")
    arcpy.XToolsGP_DeleteMultipleFields_xtp(lr_pts,
                                        "ORIG_FID;Join_Count;TARGET_FID;Join_Count_1;TARGET_FID_1;Join_Count_12;"
                                        "TARGET_FID_12;CID;CUBE_Uncert;Map;QA;"
                                        "Rand;Source;Join_Count_12_13;TARGET_FID_12_13;CID_1")

#Calling the function
linear_interp(LakeName="MilanGolfCourse",
              Home_GDB="E:\\HYPACK_Projects\\2020_DNR_Lakes\\2020-07_MilanGolfCourse\\GIS\\MilanGolfCourse.gdb",
              LR_Shapefile="E:\\HYPACK_Projects\\2020_DNR_Lakes\\2020-07_MilanGolfCourse\\GIS\\MilanGolfCourse.gdb\\MilanGolfCourse_LR1",
              Map_Resolution=1)
end_time = datetime.now()
print('Duration: {}'.format(end_time - start_time))
