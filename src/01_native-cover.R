#
# Title: Calculating the terrestrial and aquatic native cover indicator
# Created: June 3rd, 2022
# Last Updated: July 24th, 2023
# Author: Brandon Allen
# Objectives: Calculate percent native cover for both habitat classes
# Keywords: Notes, Native Cover
#

#########
# Notes #
#########~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# 1) This analysis is run for each HUC 8 across the province.
# 2) We are using the ABMI Alberta Wetland Inventory. It is assumed areas not delineated with "aquatic" habitats are upland
#
################
# Native Cover # 
################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


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

# Define the spatial file for storing results
watershed.layer$LowRef <- NA
watershed.layer$LowCur <- NA
watershed.layer$UpRef <- NA
watershed.layer$UpCur <- NA
watershed.layer$UpCov <- NA
watershed.layer$LowCov <- NA

# Create a version for 2010 and 2018
watershed.layer.2010 <- watershed.layer
watershed.layer.2018 <- watershed.layer
watershed.layer.2019 <- watershed.layer
watershed.layer.2020 <- watershed.layer

# Load recovery curves
load("data/lookup/harvest-recovery-curves_80years.Rdata")
load("data/lookup/harvest-stand-areas_2010HFI_v7.Rdata")

####################
# Initialize arcpy #
####################

py_discover_config() # We need version 3.7
py_config() # Double check it is version 3.7

# Set python 
use_python(python = "C:/Users/ballen/miniconda3/envs/r-reticulate/python.exe")

# Load arcpy
arcpy <- import('arcpy') 

# Define parallel processing factor
arcpy$env$parallelProcessingFactor <- "100%"

############################
# Native Cover Calculation # 
############################

for (HUC.id in watershed.ids) { 
        
        # 2010 HFI 
        watershed.layer.2010 <- native_cover(landcover = "data/base/landcover/ABMIwetlandInventory.gdb/ABMIwetlandInventory",
                                             riparian = "data/base/landcover/LoticRiparianDigitalElevationModelDerived/Data/Shapefile/10TM_Offset/LoticRiparianDigitalElevationModelDerived.shp",
                                             hfi.inventory = "data/base/footprint/HFI_2010_Updated2023.gdb/HFI2010_Integrated",
                                             harvest.areas = harvest.areas,
                                             recovery.curve = recovery.curve,
                                             boundaries = "data/base/boundaries/HUC_8_NSR.shp",
                                             huc.id = HUC.id,
                                             hfi.year = 2010,
                                             results = watershed.layer.2010,
                                             arcpy = arcpy)
        
        # 2018 HFI
        watershed.layer.2018 <- native_cover(landcover = "data/base/landcover/ABMIwetlandInventory.gdb/ABMIwetlandInventory", 
                                             riparian = "data/base/landcover/LoticRiparianDigitalElevationModelDerived/Data/Shapefile/10TM_Offset/LoticRiparianDigitalElevationModelDerived.shp", 
                                             hfi.inventory = "data/base/footprint/HFI_2018_v1.gdb/HFI_2018", 
                                             harvest.areas = harvest.areas,
                                             recovery.curve = recovery.curve,
                                             boundaries = "data/base/boundaries/HUC_8_NSR.shp", 
                                             huc.id = HUC.id, 
                                             hfi.year = 2018, 
                                             results = watershed.layer.2018, 
                                             arcpy = arcpy)
        
        # 2019 HFI
        watershed.layer.2019 <- native_cover(landcover = "data/base/landcover/ABMIwetlandInventory.gdb/ABMIwetlandInventory", 
                                             riparian = "data/base/landcover/LoticRiparianDigitalElevationModelDerived/Data/Shapefile/10TM_Offset/LoticRiparianDigitalElevationModelDerived.shp", 
                                             hfi.inventory = "data/base/footprint/HFI_2019_v2.gdb/HFI_2019_v2", 
                                             harvest.areas = harvest.areas,
                                             recovery.curve = recovery.curve,
                                             boundaries = "data/base/boundaries/HUC_8_NSR.shp", 
                                             huc.id = HUC.id, 
                                             hfi.year = 2019, 
                                             results = watershed.layer.2019, 
                                             arcpy = arcpy)
        
        # 2020 HFI
        watershed.layer.2020 <- native_cover(landcover = "data/base/landcover/ABMIwetlandInventory.gdb/ABMIwetlandInventory", 
                                             riparian = "data/base/landcover/LoticRiparianDigitalElevationModelDerived/Data/Shapefile/10TM_Offset/LoticRiparianDigitalElevationModelDerived.shp", 
                                             hfi.inventory = "data/base/footprint/HFI_2020.gdb/HFI2020_Integrated", 
                                             harvest.areas = harvest.areas,
                                             recovery.curve = recovery.curve,
                                             boundaries = "data/base/boundaries/HUC_8_NSR.shp", 
                                             huc.id = HUC.id, 
                                             hfi.year = 2020, 
                                             results = watershed.layer.2020, 
                                             arcpy = arcpy)
        
        
        print(HUC.id)
        
}

# Save layers
write_sf(watershed.layer.2010, dsn = "results/gis/2010/native_cover_HFI2010.shp")
write_sf(watershed.layer.2018, dsn = "results/gis/2018/native_cover_HFI2018.shp")
write_sf(watershed.layer.2019, dsn = "results/gis/2019/native_cover_HFI2019.shp")
write_sf(watershed.layer.2020, dsn = "results/gis/2020/native_cover_HFI2020.shp")

rm(list=ls())
gc()
