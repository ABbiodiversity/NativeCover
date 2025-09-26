#
# Title: Data merging
# Created: April 3rd, 2024
# Last Updated: May 14th, 2025
# Author: Brandon Allen
# Objectives: Merge the watersheds into a single provincial layer for reporting.
# Keywords: Notes, Initialization, Merging
#

#########
# Notes #
#########~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##################
# Initialization # 
##################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Clear memory
rm(list=ls())
gc()

# Load libraries
library(reticulate)
library(sf)

####################
# Initialize arcpy #
####################

# Set python 
use_python(python = "C:/Program Files/ArcGIS/Pro/bin/Python/envs/arcgispro-py3/python.exe")

# Load arcpy
arcpy <- import('arcpy') 
numpy <- import("numpy", convert = FALSE)

# Define parallel processing factor
arcpy$env$parallelProcessingFactor <- "100%"

###########
# Merging #  
###########~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Define year
hfi.years <- c(2010, 2018, 2019, 2020, 2021, 2022)

# For each year, combine the tiles into a single dataset
for (year in hfi.years) {
        
        # Create geodatabases
        arcpy$CreateFileGDB_management(out_folder_path = paste0(getwd(), "/results/gis/"), 
                                       out_name = paste0("Terrestrial_Current_", year, ".gdb"))
        
        arcpy$CreateFileGDB_management(out_folder_path = paste0(getwd(), "/results/gis/"), 
                                       out_name = paste0("Terrestrial_Reference_", year, ".gdb"))
        
        arcpy$CreateFileGDB_management(out_folder_path = paste0(getwd(), "/results/gis/"), 
                                       out_name = paste0("Aquatic_Current_", year, ".gdb"))
        
        arcpy$CreateFileGDB_management(out_folder_path = paste0(getwd(), "/results/gis/"), 
                                       out_name = paste0("Aquatic_Reference_", year, ".gdb"))
        
        # Define the list of geodatabases that are required for the merge
        tile.path <- list.files(paste0(getwd(), "/data/processed/", year, "/gis/"), full.names = TRUE)
        tile.list <- list(Terrestrial_Current = NULL,
                          Terrestrial_Reference = NULL,
                          Aquatic_Current = NULL,
                          Aquatic_Reference = NULL)
        
        for (tile in tile.path) {
                
                arcpy$env$workspace <- tile
                layer.id <- arcpy$ListFeatureClasses()
                
                tile.list$Terrestrial_Current <- c(tile.list$Terrestrial_Current, paste0(tile, "/", layer.id[grep("terrestrial_current_complete", layer.id)]))
                tile.list$Terrestrial_Reference <- c(tile.list$Terrestrial_Reference, paste0(tile, "/", layer.id[grep("terrestrial_reference", layer.id)]))
                tile.list$Aquatic_Current <- c(tile.list$Aquatic_Current, paste0(tile, "/", layer.id[grep("aquatic_current_complete", layer.id)]))
                tile.list$Aquatic_Reference <- c(tile.list$Aquatic_Reference, paste0(tile, "/", layer.id[grep("aquatic_reference", layer.id)]))
                
        }
        
        # For each layer, loop through the process
        for(layer in names(tile.list)) {
                
                # Create the merged layer
                arcpy$Merge_management(inputs = paste(tile.list[[layer]], collapse = ";"), 
                                       output = paste0(getwd(), "/results/gis/", layer, "_", year, ".gdb/native_cover"))
                
                # Recalculate areas
                arcpy$CalculateGeometryAttributes_management(in_features = paste0(getwd(), "/results/gis/", layer, "_", year, ".gdb/native_cover"), 
                                                             geometry_property = list(c("TotalArea", "AREA_GEODESIC")), 
                                                             area_unit = "SQUARE_METERS")
                
                # Identify fields within the layer to be removed
                field.list <- arcpy$ListFields(dataset = paste0(getwd(), "/results/gis/", layer, "_", year, ".gdb/native_cover"),
                                               field_type = "All")
                field.list <- unlist(lapply(field.list, function(x) x$name))
                field.list <- field.list[!field.list %in% c("OBJECTID", "Shape", "Recovery", "TotalArea", "Shape_Area", "Shape_Length")] # Excluding the FID and shape fields
                
                # Add catch so the field management only applies if the length of the field list is great than 0
                if(length(field.list) != 0) {
                        
                        arcpy$DeleteField_management(in_table = paste0(getwd(), "/results/gis/", layer, "_", year, ".gdb/native_cover"), 
                                                     drop_field = field.list)   
                        
                }

                
        }
        
        print(year)
        
}

rm(list=ls())
gc()

