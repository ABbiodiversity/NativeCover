# ---
# title: "Data merging for the Native Cover indicator"
# author: "Brandon Allen"
# created: "2025-05-13"
# inputs: ["2_pipeline/year - native cover indicator per watershed"]
# outputs: ["3_output/processed provincial indicator status"]
# notes: 
#   "This script merges the individual HUC-8 watershed summaries into a single provincial geodatabase"
# ---

# 1.0 Initializing environment ----
# This section handles defining the input data 
# and required functions.

# Clear memory
rm(list=ls())
gc()

# Load libraries
library(reticulate)
library(sf)

# Initialize ArcPy
# Set python 
use_python(python = "C:/Program Files/ArcGIS/Pro/bin/Python/envs/arcgispro-py3/python.exe")
arcpy <- import('arcpy') 
numpy <- import("numpy", convert = FALSE)

# Define parallel processing factor
arcpy$env$parallelProcessingFactor <- "100%"

# 2.0 Merging ----
# This section merges the individual watershed units into singular geodatabases

# Define year
hfi.years <- c(2010, 2018, 2019, 2020, 2021, 2022, 2023)

# For each year, combine the tiles into a single dataset
for (year in hfi.years) {
        
        # Create geodatabases
        arcpy$CreateFileGDB_management(out_folder_path = paste0(getwd(), "/3_output/gis/"), 
                                       out_name = paste0("Terrestrial_", year, ".gdb"))
        
        arcpy$CreateFileGDB_management(out_folder_path = paste0(getwd(), "/3_output/gis/"), 
                                       out_name = paste0("Aquatic_", year, ".gdb"))
        
        
        # Define the list of geodatabases that are required for the merge
        tile.path <- list.files(paste0(getwd(), "/2_pipeline/", year, "/"), full.names = TRUE)
        tile.list <- list(Terrestrial = paste0(tile.path, "/terrestrial_native_cover"),
                          Aquatic = paste0(tile.path, "/aquatic_native_cover"))
        
        # For each layer, recalculate the total area (TotalArea) of polygons and the recovery metric (Recovery)
        for(layer in names(tile.list)) {
                
                # Create the merged layer
                arcpy$Merge_management(inputs = paste(tile.list[[layer]], collapse = ";"), 
                                       output = paste0(getwd(), "/3_output/gis/", layer, "_", year, ".gdb/native_cover"))
                
    
        }
        
        print(year)
        
}

rm(list=ls())
gc()

