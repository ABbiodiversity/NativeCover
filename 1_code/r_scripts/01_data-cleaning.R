#
# Title: Data cleaning for the Native Cover indicator
# Created: June 3rd, 2022
# Last Updated: May 13, 2025
# Author: Brandon Allen
# Objectives: Calculate percent native cover for both habitat classes
# Keywords: Notes, Initialization, Footprint Cleaning, Native Cover Cleaning
#

##################
# Initialization # 
##################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Clear memory
rm(list=ls())
gc()

# Load libraries
library(foreach)
library(parallel)
library(reticulate)
library(sf)

# Source scripts
source("src/native-cover_functions.R")

# Load recovery curves
load("data/lookup/harvest-recovery-curves_80years.Rdata")
load("data/lookup/harvest-stand-areas_2010HFI_backfillv7_v2.0.Rdata")

# Load HFI paths
hfi.paths <- read.csv("data/lookup/hfi-path-lookup.csv")

# Define watershed IDs
watershed.layer<- read_sf("data/base/boundaries/HUC_8_EPSG3400.shp")
watershed.ids <- watershed.layer$HUC_8

##################################
# Initialize parallel processing #
##################################

# Define the cores for parallel processing
n.clusters <- 10
core.input <- makeCluster(n.clusters)
clusterExport(core.input, c("harvest.areas", "recovery.curve", "hfi.paths",
                            "native_cover_cleaning", "footprint_cleaning"))
clusterEvalQ(core.input, {
        
        # Load libraries
        library(reticulate)
        library(sf)
        
        # Set python 
        use_python(python = "C:/Program Files/ArcGIS/Pro/bin/Python/envs/arcgispro-py3/python.exe")
        
        # Load arcpy
        arcpy <- import('arcpy') 
        numpy <- import("numpy", convert = FALSE)
        
        # Define parallel processing factor
        # This needs to be set to 0 when performing parallel processing on workers.
        # If not set to 0, processes get jumbled and may fail.
        arcpy$env$parallelProcessingFactor <- "0%"
        
})

######################
# Footprint Cleaning # Function has a catch to only update the names in the 2022 hfi
######################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Standardize the harvest area polygons
hfi.series <- c(2010, 2018, 2019, 2020, 2021, 2022)
parLapply(core.input, 
          as.list(hfi.series), 
          fun = function(hfi) tryCatch(footprint_cleaning(hfi.inventory = hfi.paths,
                                                          hfi.year = hfi,
                                                          arcpy = arcpy), error = function(e) e)
)

#########################
# Native Cover Cleaning # 
#########################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Process the individual watershed units
hfi.series <- c(2010, 2018, 2019, 2020, 2021, 2022)
foreach(hfi = hfi.series) %dopar% 
        
        parLapply(core.input, 
                  as.list(watershed.ids), 
                  fun = function(HUC.id) tryCatch(native_cover_cleaning(landcover = "data/base/landcover/ABMIwetlandInventory.gdb/ABMIwetlandInventory",
                                                                        riparian = "data/base/landcover/LoticRiparianDigitalElevationModelDerived/Data/Shapefile/10TM_Offset/LoticRiparianDigitalElevationModelDerived.shp",
                                                                        hfi.inventory = paste0("data/processed/hfi/hfi_", hfi, ".gdb/hfi_", hfi),
                                                                        boundaries = "data/base/boundaries/HUC_8_NSR.shp", 
                                                                        harvest.areas = harvest.areas,
                                                                        recovery.curve = recovery.curve,
                                                                        huc.id = HUC.id,
                                                                        hfi.year = hfi,
                                                                        arcpy = arcpy,
                                                                        numpy = numpy), error = function(e) e)
        )

stopCluster(core.input)

rm(list=ls())
gc()
