#
# Title: Calculating of "terrestrial" and "aquatic" native cover
# Created: June 3rd, 2022
# Last Updated: October 7th, 2022
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

# Create a version for 2010 and 2018
watershed.layer.2010 <- watershed.layer
watershed.layer.2018 <- watershed.layer

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

# # Create a merged 2010 HFI layer (Created on October 7, 2022)
# # Define workspace
# arcpy$env$workspace <- paste0(getwd(), "/data/base/footprint/HFI_2010_v1.gdb")
# 
# # Create a single merged 2010 HFI
# arcpy$Merge_management(inputs = "HFI_2010_Sublayers/o01_Reservoirs_HFI2010; HFI_2010_Sublayers/o02_BPSDL_HFI2010; HFI_2010_Sublayers/o03_Roads_HFI2010; HFI_2010_Sublayers/o04_Railways_HFI2010; HFI_2010_Sublayers/o05_Canals_HFI2010; HFI_2010_Sublayers/o06_Verge_HFI2010; HFI_2010_Sublayers/o07_Mine_Sites_HFI2010; HFI_2010_Sublayers/o08_Industrial_Sites_HFI2010; HFI_2010_Sublayers/o09_WellSites_Active_HFI2010; HFI_2010_Sublayers/o10_Landfill_HFI2010; HFI_2010_Sublayers/o11_OtherVegSurfacesRecreation_HFI2010; HFI_2010_Sublayers/o12_Wind_Gen_Facilities_HFI2010; HFI_2010_Sublayers/o13_TransmissionLines_HFI2010; HFI_2010_Sublayers/o14_CFO_HFI2010; HFI_2010_Sublayers/o15_Residential_Areas_HFI2010; HFI_2010_Sublayers/o16_WellSites_Abandoned_HFI2010; HFI_2010_Sublayers/o17_Cultivation_HFI2010; HFI_2010_Sublayers/o18_HarvestAreas_HFI2010; HFI_2010_Sublayers/o20_SeismicLines_HFI2010; HFI_2010_Sublayers/o21_DisturbedVegetation_HFI2010", 
#                        output = "HFI_2010_merged")

############################
# Native Cover Calculation # 
############################

for (HUC.id in watershed.ids) { 
        
        # 2010 HFI
        watershed.layer.2010 <- native_cover(landcover = "data/base/landcover/ABMIwetlandInventory.gdb/ABMIwetlandInventory", 
                                             riparian = "data/base/landcover/LoticRiparianDigitalElevationModelDerived/Data/Shapefile/10TM_Offset/LoticRiparianDigitalElevationModelDerived.shp", 
                                             boundaries = "data/base/boundaries/HUC_8_EPSG3400.shp", 
                                             hfi.boundary = "data/base/footprint/HFI_2010_v1.gdb/HFI_2010_merged", 
                                             huc.id = HUC.id, 
                                             hfi.year = 2010, 
                                             results = watershed.layer.2010, 
                                             arcpy = arcpy)
        
        # 2018 HFI
        watershed.layer.2018 <- native_cover(landcover = "data/base/landcover/ABMIwetlandInventory.gdb/ABMIwetlandInventory", 
                                             riparian = "data/base/landcover/LoticRiparianDigitalElevationModelDerived/Data/Shapefile/10TM_Offset/LoticRiparianDigitalElevationModelDerived.shp", 
                                             boundaries = "data/base/boundaries/HUC_8_EPSG3400.shp", 
                                             hfi.boundary = "data/base/footprint/HFI_2018_v1.gdb/HFI_2018", 
                                             huc.id = HUC.id, 
                                             hfi.year = 2018, 
                                             results = watershed.layer.2018, 
                                             arcpy = arcpy)
        
        
        print(HUC.id)

}

# Calculate the % native cover index
watershed.layer.2010$UpCov <- (watershed.layer.2010$UpCur / watershed.layer.2010$UpRef) * 100
watershed.layer.2010$LowCov <- (watershed.layer.2010$LowCur / watershed.layer.2010$LowRef) * 100
watershed.layer.2018$UpCov <- (watershed.layer.2018$UpCur / watershed.layer.2018$UpRef) * 100
watershed.layer.2018$LowCov <- (watershed.layer.2018$LowCur / watershed.layer.2018$LowRef) * 100

# Save layers
write_sf(watershed.layer.2010, dsn = "data/processed/2010/native_cover_HFI2010.shp")
write_sf(watershed.layer.2018, dsn = "data/processed/2018/native_cover_HFI2018.shp")

rm(list=ls())
gc()
