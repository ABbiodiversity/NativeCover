#
# Title: Calculating of "terrestrial" and "aquatic" native cover
# Created: June 3rd, 2022
# Last Updated: June 6th, 2022
# Author: Brandon Allen
# Objectives: Calculate percent native cover for both habitat classes
# Keywords: Notes, Native Cover
#

#########
# Notes #
#########~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# 1) This analysis is run for each HUC 8 across the province.
# 2) We are using the ABNI Alberta Wetland Inventory. It is assumed areas not delineated with "aquatic" habitats are upland
#
################
# Native Cover # 
################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Clear memory, source scripts
rm(list=ls())
gc()

# Load libraries
library(reticulate)
library(sf)

# Define HUC units and HFI inventories
HFI <- 2018
watershed.layer<- read_sf("data/base/boundaries/HUC_8_EPSG3400.shp")
watershed.ids <- watershed.layer$HUC_8

# Define the spatial file for storing results
watershed.layer$LowRef <- NA
watershed.layer$LowCur <- NA
watershed.layer$UpRef <- NA
watershed.layer$UpCur <- NA

####################
# Initialize arcpy #
####################

py_discover_config() # We need version 3.7
py_config() # Double check it is version 3.7

# Set python 
use_python(python = "C:/Users/ballen/miniconda3/envs/r-reticulate/python.exe")

# Load arcpy
arcpy <- import('arcpy') 

# Define the scratch space otherwise functions without defined outputs will fail
scratch.space <- "C:/Users/ballen/Desktop/LandscapeConnectivity/data/processed/scratch/"
arcpy$env$scratchWorkspace <- scratch.space

# Define parallel processing factor
arcpy$env$parallelProcessingFactor <- "100%"

############################
# Native Cover Calculation #
############################

for (HUC.id in watershed.ids) { 
        
        # Select watershed
        arcpy$Select_analysis(in_features = "data/base/boundaries/HUC_8_EPSG3400.shp", 
                              out_feature_class = "data/processed/scratch/boundary.shp", 
                              where_clause =  paste0('"HUC_8" = ', "'", paste(HUC.id), "'"))
        
        # Clip footprint to boundary of interest
        arcpy$Clip_analysis(in_features = "data/base/footprint/HFI_2018_v1.gdb/HFI_2018", 
                            clip_features = "data/processed/scratch/boundary.shp", 
                            out_feature_class = "data/processed/scratch/hfi.shp")
        
        # Clip wetland layer to boundary of interest
        arcpy$Clip_analysis(in_features = "data/base/landcover/ABMIwetlandInventory.gdb/ABMIwetlandInventory", 
                            clip_features = "data/processed/scratch/boundary.shp", 
                            out_feature_class = "data/processed/scratch/wetland_inventory.shp")
        
        # Dissolve the wetland layer
        arcpy$PairwiseDissolve_analysis(in_features = "data/processed/scratch/wetland_inventory.shp", 
                                        out_feature_class = "data/processed/scratch/lowland_reference.shp")
        
        # Calculate upland layer (inverse of lowland)
        arcpy$SymDiff_analysis(in_features = "data/processed/scratch/lowland_reference.shp", 
                               update_features = "data/processed/scratch/boundary.shp", 
                               out_feature_class = "data/processed/scratch/upland_reference.shp")
        
        # Remove footprint from upland layer
        arcpy$Erase_analysis(in_features = "data/processed/scratch/upland_reference.shp", 
                             erase_features = "data/processed/scratch/hfi.shp", 
                             out_feature_class = "data/processed/scratch/upland_current.shp")

        
        # Remove footprint from lowland layer
        arcpy$Erase_analysis(in_features = "data/processed/scratch/lowland_reference.shp", 
                             erase_features = "data/processed/scratch/hfi.shp", 
                             out_feature_class = "data/processed/scratch/lowland_current.shp")
        
        # Calculate areas and store
        watershed.layer[watershed.layer$HUC_8 == HUC.id, "LowRef"] <- sum(st_area(read_sf("data/processed/scratch/lowland_reference.shp")))
        watershed.layer[watershed.layer$HUC_8 == HUC.id, "LowCur"] <- sum(st_area(read_sf("data/processed/scratch/lowland_current.shp")))
        watershed.layer[watershed.layer$HUC_8 == HUC.id, "UpRef"] <- sum(st_area(read_sf("data/processed/scratch/upland_reference.shp")))
        watershed.layer[watershed.layer$HUC_8 == HUC.id, "UpCur"] <- sum(st_area(read_sf("data/processed/scratch/upland_current.shp")))
        
        # Remove all layers
        arcpy$Delete_management(in_data = "data/processed/scratch/lowland_current.shp")
        arcpy$Delete_management(in_data = "data/processed/scratch/lowland_reference.shp")
        arcpy$Delete_management(in_data = "data/processed/scratch/upland_current.shp")
        arcpy$Delete_management(in_data = "data/processed/scratch/upland_reference.shp")
        arcpy$Delete_management(in_data = "data/processed/scratch/boundary.shp")
        arcpy$Delete_management(in_data = "data/processed/scratch/hfi.shp")
        arcpy$Delete_management(in_data = "data/processed/scratch/wetland_inventory.shp")

        print(HUC.id)
        
        }

# Calculate index and Format results
watershed.layer$UpCov <- (watershed.layer$UpCur / watershed.layer$UpRef) * 100
watershed.layer$LowCov <- (watershed.layer$LowCur / watershed.layer$LowRef) * 100
watershed.layer <- watershed.layer[, c("HUC_8", "UpCur", "UpRef", "UpCov", "LowCur", "LowRef", "LowCov", "geometry")]

# Save layer
write_sf(watershed.layer, file = "data/processed/2018/native_cover_2018.shp")