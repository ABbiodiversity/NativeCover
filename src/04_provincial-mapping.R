#
# Title: Provincial Reporting
# Created: April 3rd, 2024
# Last Updated: April 10th, 2024
# Author: Brandon Allen
# Objectives: Calculate the terrestrial and aquatic native cover indicators for provincial reporting
# Keywords: Notes, Native Cover, Merging
#

#########
# Notes #
#########~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# 1) This analysis is run for each HUC 8 across the province.
# 2) We are using the ABMI Alberta Wetland Inventory. It is assumed areas not delineated with "aquatic" habitats are upland
#
################
# Native Cover # 
################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Clear memory
rm(list=ls())
gc()

# Load libraries
library(reticulate)
library(sf)

# Source scripts
source("src/native-cover_functions.R")

# Define HUC units and HFI inventories
watershed.layer<- read_sf("data/base/boundaries/HUC_8_EPSG3400.shp")
watershed.ids <- watershed.layer$HUC_8

# Load recovery curves
load("data/lookup/harvest-recovery-curves_80years.Rdata")
load("data/lookup/harvest-stand-areas_2010HFI_v7.Rdata")

####################
# Initialize arcpy #
####################

py_discover_config() # We need version 3.7
py_config() # Double check it is version 3.7

# Set python 
use_python(python = "C:/Users/ballen/AppData/Local/r-miniconda/envs/r-reticulate/python.exe")

# Load arcpy
arcpy <- import('arcpy') 
numpy <- import("numpy", convert = FALSE)

# Define parallel processing factor
arcpy$env$parallelProcessingFactor <- "100%"

############################
# Native Cover Calculation # 
############################

for (HUC.id in watershed.ids) { 
        
        # 2010 HFI 
        watershed.layer.2010 <- native_cover_mapping(landcover = "data/base/landcover/ABMIwetlandInventory.gdb/ABMIwetlandInventory",
                                                     riparian = "data/base/landcover/LoticRiparianDigitalElevationModelDerived/Data/Shapefile/10TM_Offset/LoticRiparianDigitalElevationModelDerived.shp",
                                                     hfi.inventory = "data/base/footprint/HFI_2010_Updated2023.gdb/HFI2010_Integrated",
                                                     harvest.areas = harvest.areas,
                                                     recovery.curve = recovery.curve,
                                                     boundaries = "data/base/boundaries/HUC_8_NSR.shp",
                                                     huc.id = HUC.id,
                                                     hfi.year = 2010,
                                                     arcpy = arcpy,
                                                     numpy = numpy)
        
        # 2021 HFI
        watershed.layer.2021 <- native_cover_mapping(landcover = "data/base/landcover/ABMIwetlandInventory.gdb/ABMIwetlandInventory", 
                                                     riparian = "data/base/landcover/LoticRiparianDigitalElevationModelDerived/Data/Shapefile/10TM_Offset/LoticRiparianDigitalElevationModelDerived.shp", 
                                                     hfi.inventory = "data/base/footprint/HFI2021.gdb/HFI2021", 
                                                     harvest.areas = harvest.areas,
                                                     recovery.curve = recovery.curve,
                                                     boundaries = "data/base/boundaries/HUC_8_NSR.shp", 
                                                     huc.id = HUC.id, 
                                                     hfi.year = 2021,
                                                     arcpy = arcpy, 
                                                     numpy = numpy)
        
        
        print(HUC.id)
        
}

rm(list=ls())
gc()

###########
# Merging #  
###########~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Clear memory
rm(list=ls())
gc()

# Load libraries
library(reticulate)
library(sf)
library(raster)

# Define year
hfi.years <- c(2010, 2021)

# Initialize arcpy
py_discover_config() # We need version 3.9
py_config() # Double check it is version 3.9

# Set python 
use_python(python = "C:/Users/ballen/AppData/Local/r-miniconda/envs/r-reticulate/python.exe")

# Load arcpy
arcpy <- import('arcpy') 

# Define parallel processing factor
arcpy$env$parallelProcessingFactor <- "100%"

# For each year and each of the the three buffers, combine the tiles into a single dataset

for (year in hfi.years) {
        
        # Create geodatabases
        arcpy$CreateFileGDB_management(out_folder_path = paste0(getwd(), "/results/gis/custom-reporting/"), 
                                       out_name = paste0("Terrestrial_Current_", year, ".gdb"))
        
        arcpy$CreateFileGDB_management(out_folder_path = paste0(getwd(), "/results/gis/custom-reporting/"), 
                                       out_name = paste0("Terrestrial_Reference_", year, ".gdb"))
        
        arcpy$CreateFileGDB_management(out_folder_path = paste0(getwd(), "/results/gis/custom-reporting/"), 
                                       out_name = paste0("Aquatic_Current_", year, ".gdb"))
        
        arcpy$CreateFileGDB_management(out_folder_path = paste0(getwd(), "/results/gis/custom-reporting/"), 
                                       out_name = paste0("Aquatic_Reference_", year, ".gdb"))
        
        # Define the list of geodatabases that are required for the merge
        tile.path <- list.files(paste0(getwd(), "/data/processed/mapping/", year, "/gis/"), full.names = TRUE)
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
                                       output = paste0(getwd(), "/results/gis/custom-reporting/", layer, "_", year, ".gdb/native_cover"))
                
                # Recalculate areas
                arcpy$CalculateGeometryAttributes_management(in_features = paste0(getwd(), "/results/gis/custom-reporting/", layer, "_", year, ".gdb/native_cover"), 
                                                             geometry_property = list(c("TotalArea", "AREA_GEODESIC")), 
                                                             area_unit = "SQUARE_METERS")
                
                # Identify fields within the layer to be removed
                field.list <- arcpy$ListFields(dataset = paste0(getwd(), "/results/gis/custom-reporting/", layer, "_", year, ".gdb/native_cover"),
                                               field_type = "All")
                field.list <- unlist(lapply(field.list, function(x) x$name))
                field.list <- field.list[!field.list %in% c("OBJECTID", "Shape", "Recovery", "TotalArea", "Shape_Area", "Shape_Length")] # Excluding the FID and shape fields
                
                arcpy$DeleteField_management(in_table = paste0(getwd(), "/results/gis/custom-reporting/", layer, "_", year, ".gdb/native_cover"), 
                                             drop_field = field.list)
                
        }
        
        print(year)
        
}

rm(list=ls())
gc()

