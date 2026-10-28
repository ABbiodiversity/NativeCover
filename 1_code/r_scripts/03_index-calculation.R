# ---
# title: "Native Cover calculation"
# author: "Brandon Allen"
# created: "2025-05-13"
# inputs: ["0_data/external/lookup/harvest-recovery-curves_80years.Rdata"; 
#          "0_data/internal/lookup/harvest-stand-areas_2010HFI_backfillv7_v2.0.Rdata"; 
#          "0_data/external/landcover/ABMIwetlandInventory.gdb/ABMIwetlandInventory"; 
#          "0_data/external/landcover/LoticRiparianDigitalElevationModelDerived/Data/Shapefile/10TM_Offset/LoticRiparianDigitalElevationModelDerived.shp";
#          "0_data/external/boundaries/HUC_8_NSR.shp"; 
#          "2_pipeline/hfi  - standardized data"]
# outputs: ["2_pipeline/year - native cover indicator per watershed"]]
# notes: 
#   "This script uses the harmonized footprint inventories to calcualte
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
source("1_code/r_scripts/functions/native-cover-index.R")
source("1_code/r_scripts/functions/recovery-index-helper.R")
source("1_code/r_scripts/functions/drop-unwanted-fields-helper.R")

# 1.4 Load recovery curves ----
load("0_data/external/lookup/harvest-recovery-curves_80years.Rdata")
load("0_data/internal/lookup/harvest-stand-areas_2010HFI_backfillv7_v2.0.Rdata")
harvest_areas <- harvest.areas
recovery_curve <- recovery.curve

# 1.5 Define watershed IDs ----
watershed_layer<- read_sf("0_data/external/boundaries/HUC_8_EPSG3400.shp")
watershed_ids <- watershed_layer$HUC_8

# 1.6 Define the HFI for processing ----
hfi_series <- c(2010, 2018, 2019, 2020, 2021, 2022, 2023)

# 1.7 Initialize parallel processing ----
# Define the cores for parallel processing
n.clusters <- 16
core.input <- makeCluster(n.clusters)
clusterExport(core.input, c("harvest_areas", "recovery_curve", "drop_unwanted_fields",
                            "init_python_helpers", "native_cover_index"))
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

# 2.0 Indicator Calculation ----
# This section uses the harmonized human footprint inventories 
# and calculates the native cover indicator. The polygons can then be aggregated to 
# various spatial scales.
#
# As regeneration curves are applied to other feature types, additional standardization
# of feature types within the human footprint inventory will be required.

# 2.1 Native Cover Index ----
# Process the individual watershed units
foreach(hfi = hfi_series) %dopar% 
        
        parLapply(core.input, 
                  as.list(watershed_ids), 
                  fun = function(HUC_id) tryCatch(native_cover_index(landcover = "0_data/external/landcover/ABMIwetlandInventory.gdb/ABMIwetlandInventory",
                                                                     riparian = "0_data/external/landcover/LoticRiparianDigitalElevationModelDerived/Data/Shapefile/10TM_Offset/LoticRiparianDigitalElevationModelDerived.shp",
                                                                     hfi_inventory = paste0("2_pipeline/hfi_", hfi, ".gdb/hfi_", hfi),
                                                                     geodatabase_path = paste0("2_pipeline/", hfi), 
                                                                     boundaries = "0_data/external/boundaries/HUC_8_NSR.shp", 
                                                                     harvest_areas = harvest_areas,
                                                                     recovery_curve = recovery_curve,
                                                                     huc_id = HUC_id,
                                                                     hfi_year = hfi,
                                                                     arcpy = arcpy), error = function(e) e)
        )

stopCluster(core.input)

rm(list=ls())
gc()
