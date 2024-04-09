#
# Title: Provincial Reporting
# Created: April 3rd, 2024
# Last Updated: April 8th, 2024
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
        
        # Create geodatabase
        arcpy$CreateFileGDB_management(out_folder_path = paste0(getwd(), "/results/gis"), 
                                       out_name = paste0("InteriorHabitat_", year, ".gdb"))
        
        # Define the list of geodatabases that are required for the merge
        tile.path <- list.files(paste0(getwd(), "/data/processed/", year), full.names = TRUE)
        tile.list <- list(IntHab_50 = NULL,
                          IntHab_200 = NULL,
                          IntHab_500 = NULL)
        
        for (tile in tile.path) {
                
                arcpy$env$workspace <- tile
                layer.id <- arcpy$ListFeatureClasses()
                
                tile.list$IntHab_50 <- c(tile.list$IntHab_50, paste0(tile, "/", layer.id[grep("IntHab_50_", layer.id)]))
                tile.list$IntHab_200 <- c(tile.list$IntHab_200, paste0(tile, "/", layer.id[grep("IntHab_200_", layer.id)]))
                tile.list$IntHab_500 <- c(tile.list$IntHab_500, paste0(tile, "/", layer.id[grep("IntHab_500_", layer.id)]))
                
        }
        
        # Create the merged layer
        arcpy$Merge_management(inputs = paste(tile.list$IntHab_50, collapse = ";"), 
                               output = paste0(getwd(), "/results/gis/InteriorHabitat_", year, ".gdb/IntHab_50"))
        
        arcpy$Merge_management(inputs = paste(tile.list$IntHab_200, collapse = ";"), 
                               output = paste0(getwd(), "/results/gis/InteriorHabitat_", year, ".gdb/IntHab_200"))
        
        arcpy$Merge_management(inputs = paste(tile.list$IntHab_500, collapse = ";"), 
                               output = paste0(getwd(), "/results/gis/InteriorHabitat_", year, ".gdb/IntHab_500"))
        
        # Clip to the backfill layer
        arcpy$PairwiseClip_analysis(in_features = paste0("D:/backfill/Version7.0/gdb_veghf_reference_condition_", year, ".gdb/veghf_", year), 
                                    clip_features = paste0(getwd(), "/results/gis/InteriorHabitat_", year, ".gdb/IntHab_50"), 
                                    out_feature_class = paste0(getwd(), "/results/gis/InteriorHabitat_", year, ".gdb/IntHab_50_backfill"))
        
        arcpy$PairwiseClip_analysis(in_features = paste0("D:/backfill/Version7.0/gdb_veghf_reference_condition_", year, ".gdb/veghf_", year), 
                                    clip_features = paste0(getwd(), "/results/gis/InteriorHabitat_", year, ".gdb/IntHab_200"), 
                                    out_feature_class = paste0(getwd(), "/results/gis/InteriorHabitat_", year, ".gdb/IntHab_200_backfill"))
        
        arcpy$PairwiseClip_analysis(in_features = paste0("D:/backfill/Version7.0/gdb_veghf_reference_condition_", year, ".gdb/veghf_", year), 
                                    clip_features = paste0(getwd(), "/results/gis/InteriorHabitat_", year, ".gdb/IntHab_500"), 
                                    out_feature_class = paste0(getwd(), "/results/gis/InteriorHabitat_", year, ".gdb/IntHab_500_backfill"))
        
        # Export table as a csv for processing
        arcpy$ExportTable_conversion(in_table = paste0(getwd(), "/results/gis/InteriorHabitat_", year, ".gdb/IntHab_50_backfill"),
                                     out_table = paste0(getwd(), "/results/gis/InteriorHabitat_", year, "_50.csv"))
        
        arcpy$ExportTable_conversion(in_table = paste0(getwd(), "/results/gis/InteriorHabitat_", year, ".gdb/IntHab_200_backfill"),
                                     out_table = paste0(getwd(), "/results/gis/InteriorHabitat_", year, "_200.csv"))
        
        arcpy$ExportTable_conversion(in_table = paste0(getwd(), "/results/gis/InteriorHabitat_", year, ".gdb/IntHab_500_backfill"),
                                     out_table = paste0(getwd(), "/results/gis/InteriorHabitat_", year, "_500.csv"))
        
        # Clean up the files
        arcpy$Delete_management(paste0(getwd(), "/results/tables/InteriorHabitat_", year, ".gdb/IntHab_50_backfill"))
        arcpy$Delete_management(paste0(getwd(), "/results/tables/InteriorHabitat_", year, ".gdb/IntHab_200_backfill"))
        arcpy$Delete_management(paste0(getwd(), "/results/tables/InteriorHabitat_", year, ".gdb/IntHab_500_backfill"))
        
        print(year)
        
}

rm(list=ls())
gc()

