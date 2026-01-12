# ---
# title: "Data cleaning for the Native Cover indicator"
# author: "Brandon Allen"
# created: "2025-05-13"
# inputs: ["0_data/external/lookup/harvest-recovery-curves_80years.Rdata"; 
#          "0_data/internal/lookup/harvest-stand-areas_2010HFI_backfillv7_v2.0.Rdata"; 
#          "0_data/external/boundaries/HUC_8_EPSG3400.shp";
#          "0_data/external/landcover/ABMIwetlandInventory.gdb/ABMIwetlandInventory"; 
#          "0_data/external/landcover/LoticRiparianDigitalElevationModelDerived/Data/Shapefile/10TM_Offset/LoticRiparianDigitalElevationModelDerived.shp";
#          "0_data/external/boundaries/HUC_8_NSR.shp"; 
#          "Each available HFI"]
# outputs: ["2_pipeline/hfi  - standardized data";
#           "2_pipeline/year - native cover indicator per watershed"]]
# notes: 
#   "This script processes the landcover and footprint inventories required for calculating
#    the Native Cover indicator across Alberta."
# ---

# 1.0 Initializing environment ----
# This section handles defining the input data 
# and required functions.

# 1.1 Clear memory ----
rm(list=ls())
gc()

# 1.2 Load libraries ----
library(foreach)
library(parallel)
library(reticulate)
library(sf)

# 1.3 Source custom functions ----
source("1_code/r_scripts/footprint-cleaning_functions.R")
source("1_code/r_scripts/native-cover_functions.R")

# 1.4 Load recovery curves ----
load("0_data/external/lookup/harvest-recovery-curves_80years.Rdata")
load("0_data/internal/lookup/harvest-stand-areas_2010HFI_backfillv7_v2.0.Rdata")

# 1.5 Define HFI paths ----
hfi.paths <- read.csv("0_data/external/lookup/hfi-path-lookup.csv")

# 1.6 Define watershed IDs ----
watershed.layer<- read_sf("0_data/external/boundaries/HUC_8_EPSG3400.shp")
watershed.ids <- watershed.layer$HUC_8

# 1.7 Initialize parallel processing ----

# Define the cores for parallel processing
n.clusters <- 10
core.input <- makeCluster(n.clusters)
clusterExport(core.input, c("harvest.areas", "recovery.curve", "hfi.paths",
                            "native_cover_provincial", "footprint_cleaning"))
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

# 2.0 Indicator Calculation ----
# This section standardizes the human footprint inventories based on the latest standard
# and calculates the native cover indicator. The polygons can then be aggregated to 
# various spatial scales (see 06_reporting for an example).

# 2.1 Footprint Cleaning ----
# Function has a catch to only update the names in the 2022 hfi

# Standardize the harvest area polygons
hfi.series <- c(2010, 2018, 2019, 2020, 2021, 2022)
parLapply(core.input, 
          as.list(hfi.series), 
          fun = function(hfi) tryCatch(footprint_cleaning(hfi.inventory = hfi.paths,
                                                          geodatabase.path = "2_pipeline/",
                                                          hfi.year = hfi,
                                                          arcpy = arcpy), error = function(e) e)
)

# 2.2 Native Cover Cleaning ----
# Process the individual watershed units
hfi.series <- c(2010, 2018, 2019, 2020, 2021, 2022)
foreach(hfi = hfi.series) %dopar% 
        
        parLapply(core.input, 
                  as.list(watershed.ids), 
                  fun = function(HUC.id) tryCatch(native_cover_provincial(landcover = "0_data/external/landcover/ABMIwetlandInventory.gdb/ABMIwetlandInventory",
                                                                        riparian = "0_data/external/landcover/LoticRiparianDigitalElevationModelDerived/Data/Shapefile/10TM_Offset/LoticRiparianDigitalElevationModelDerived.shp",
                                                                        hfi.inventory = paste0("2_pipeline/hfi_", hfi, ".gdb/hfi_", hfi),
                                                                        geodatabase.path = paste0("2_pipeline/", hfi), 
                                                                        boundaries = "0_data/external/boundaries/HUC_8_NSR.shp", 
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
