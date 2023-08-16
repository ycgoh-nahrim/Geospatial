## standalone script
##==================================
##Mosaic To New Raster
##Usage: MosaicToNewRaster_management inputs;inputs... output_location raster_dataset_name_with_extension 
##                                    {coordinate_system_for_the_raster} 8_BIT_UNSIGNED | 1_BIT | 2_BIT | 4_BIT 
##                                    | 8_BIT_SIGNED | 16_BIT_UNSIGNED | 16_BIT_SIGNED | 32_BIT_FLOAT | 32_BIT_UNSIGNED 
##                                    | 32_BIT_SIGNED | | 64_BIT {cellsize} number_of_bands {LAST | FIRST | BLEND  | MEAN 
##                                    | MINIMUM | MAXIMUM} {FIRST | REJECT | LAST | MATCH}       
import arcpy
##input (tiles) folder
arcpy.env.workspace = r"F:\tif_PM"
##output folder
output_loc="F:/tif_PM_all"

#for writing raster layer list string	
lyr_names=[]
for lyr in arcpy.ListRasters("IF" + "*", ""): 
	#make raster layer list string
	lyr_names.append(lyr)
	lyr_names_all=";".join(lyr_names)

##Mosaic several TIFF images to a new TIFF image
##if convert to TIF by adding .tif here, DEM will be wrong. 
##convert to tif in ArcGIS after this mosaic
arcpy.MosaicToNewRaster_management(lyr_names_all,output_loc, "IFSAR5m_PM", "", "", "", "1", "","")
print "done"
