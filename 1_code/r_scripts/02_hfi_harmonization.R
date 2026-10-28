# ---
# title: "HFI Harmonization"
# author: "Brandon Allen"
# created: "2025-05-13"
# inputs: ["0_data/external/boundaries/HUC_8_EPSG3400.shp";
#          "Each available HFI"]
# outputs: ["2_pipeline/hfi  - standardized data"]
# notes: 
#   "This script harmonizes the footprint inventories required for calculating
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
source("1_code/r_scripts/functions/footprint-cleaning.R")

# 1.4 Define HFI paths ----
hfi.paths <- read.csv("0_data/external/lookup/hfi-path-lookup.csv")

# 1.5 Define watershed IDs ----
watershed.layer<- read_sf("0_data/external/boundaries/HUC_8_EPSG3400.shp")
watershed.ids <- watershed.layer$HUC_8

# 1.6 Define the HFI years
hfi.series <- c(2010, 2018, 2019, 2020, 2021, 2022, 2023)

# 1.7 Initialize parallel processing ----

# Define the cores for parallel processing
n.clusters <- length(hfi.series)
core.input <- makeCluster(n.clusters)
clusterExport(core.input, c("footprint_cleaning"))
clusterEvalQ(core.input, {
        
        # Load libraries
        library(reticulate)
        library(sf)
        
        # Set python 
        use_python(python = "C:/Program Files/ArcGIS/Pro/bin/Python/envs/arcgispro-py3/python.exe")
        
        # Load arcpy
        arcpy <- import('arcpy') 
        
        # Define parallel processing factor
        # This needs to be set to 0 when performing parallel processing on workers.
        # If not set to 0, processes get jumbled and may fail.
        arcpy$env$parallelProcessingFactor <- "0%"
        
})

# 2.0 HFI Standardization  ----
# This section standardizes the human footprint inventories based on the latest standard
# and calculates the native cover indicator. The polygons can then be aggregated to 
# various spatial scales.
#
# As regeneration curves are applied to other feature types, additional standardization
# of feature types within the human footprint inventory will be required.

# 2.1 Footprint Cleaning ----
# Function has a catch to correct each year as appropriate

# Standardize the harvest area polygons
parLapply(core.input, 
          as.list(hfi.series), 
          fun = function(hfi) tryCatch(footprint_cleaning(hfi.inventory = hfi.paths,
                                                          geodatabase.path = "2_pipeline/",
                                                          hfi.year = hfi,
                                                          arcpy = arcpy), error = function(e) e)
)

stopCluster(core.input)

rm(list=ls())
gc()
