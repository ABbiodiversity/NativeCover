# ---
# title: "Data Packaging for the Native Cover indicator"
# author: "Brandon Allen"
# created: "2025-05-13"
# inputs: ["2_pipeline/year - native cover indicator per watershed"]
# outputs: ["3_output/processed provincial indicator status"]
# notes: 
#   "This script merges the individual HUC-8 watershed summaries into a single provincial shapefile for BSC reporting"
# ---

# 1.0 Initializing environment ----
# This section handles defining the input data 
# and required functions.

# Clear memory
rm(list=ls())
gc()

# Load libraries and functions
library(reticulate)
library(sf)
source("1_code/r_scripts/functions/native-cover-summary.R")

# Initialize ArcPy
# Set python 
use_python(python = "C:/Program Files/ArcGIS/Pro/bin/Python/envs/arcgispro-py3/python.exe")
arcpy <- import('arcpy') 

# Define parallel processing factor
arcpy$env$parallelProcessingFactor <- "100%"

# Define watershed template
watershed_template <- read_sf("0_data/external/boundaries/HUC_8_EPSG3400.shp")

# 2.0 Generate Watershed Summaries ----
# This section generates the watershed summaries

# Define year
hfi_years <- c(2010, 2018, 2019, 2020, 2021, 2022, 2023)

# For each year, combine the tiles into a single dataset
for (year in hfi_years) {
        
        # Define the geodatabase for our scratch workspace
        arcpy$CreateFileGDB_management(out_folder_path = paste0(getwd(), "/2_pipeline/scratch/"), 
                                       out_name = paste0("native_cover_", year, ".gdb"))
        
        # Define workspace
        arcpy$env$workspace <- paste0(getwd(), "/2_pipeline/scratch/native_cover_", year, ".gdb")
        
        # Create the results lookup table
        native_cover_results <- watershed_template
        native_cover_results$Year <- year
        native_cover_results$AWNC <- NA
        native_cover_results$TNC <- NA
        
        ############################
        # Native Cover Calculation # 
        ############################
        
        for (region in native_cover_results$HUC_8) { 
                
                # Create the regional boundary
                arcpy$Select_analysis(in_features = "0_data/external/boundaries/HUC_8_EPSG3400.shp",
                                      out_feature_class = "boundary",
                                      where_clause = paste0("HUC_8 = '", region, "'"))
                
                # Define the region path
                region_path <- paste0(arcpy$env$workspace, "/boundary")
                
                # Define the gdb workspace
                gdb_workspace <- arcpy$env$workspace
                
                # Aquatic and Wetland Native Cover
                native_cover_results[native_cover_results$HUC_8 == region, "AWNC"] <- native_cover_summary(gdb_path = paste0(getwd(), "/3_output/gis/Aquatic_", year, ".gdb/native_cover"), 
                                                                                                           region_id = region_path, 
                                                                                                           index = "Index",
                                                                                                           geodatabase = gdb_workspace, 
                                                                                                           arcpy = arcpy)
                # Terrestrial Native Cover
                native_cover_results[native_cover_results$HUC_8 == region, "TNC"] <- native_cover_summary(gdb_path = paste0(getwd(), "/3_output/gis/Terrestrial_", year, ".gdb/native_cover"), 
                                                                                                          region_id = region_path, 
                                                                                                          index = "Index",
                                                                                                          geodatabase = gdb_workspace, 
                                                                                                          arcpy = arcpy)
                
                # Delete the temporary file
                arcpy$Delete_management(in_data = paste0(arcpy$env$workspace, "/boundary"))
                
                print(region)
                
        }
        
        
        # Save the results
        write_sf(obj = native_cover_results, 
                 dsn = paste0("3_output/reporting/bsc/native-cover_HFI", year, ".shp"))
        
        # Delete the temporary file
        arcpy$Delete_management(in_data = arcpy$env$workspace)
        
        print(year)
        
}

# 3.0 Generate the final object ----
# This section merges the generates shapefiles into a single object for reporting on Alberta Geodiscover

# Load the results
native_cover_2010 <- read_sf("3_output/reporting/bsc/native-cover_HFI2010.shp")
native_cover_2018 <- read_sf("3_output/reporting/bsc/native-cover_HFI2018.shp")
native_cover_2019 <- read_sf("3_output/reporting/bsc/native-cover_HFI2019.shp")
native_cover_2020 <- read_sf("3_output/reporting/bsc/native-cover_HFI2020.shp")
native_cover_2021 <- read_sf("3_output/reporting/bsc/native-cover_HFI2021.shp")
native_cover_2022 <- read_sf("3_output/reporting/bsc/native-cover_HFI2022.shp")
native_cover_2023 <- read_sf("3_output/reporting/bsc/native-cover_HFI2023.shp")

# Standardize the names
native_cover <- native_cover_2010[, c("HUC_2", "HUC_4", "HUC_6", "HUC_8")]

# Add TNC
native_cover$TNC2010 <- native_cover_2010$TNC
native_cover$TNC2018 <- native_cover_2018$TNC
native_cover$TNC2019 <- native_cover_2019$TNC
native_cover$TNC2020 <- native_cover_2020$TNC
native_cover$TNC2021 <- native_cover_2021$TNC
native_cover$TNC2022 <- native_cover_2022$TNC
native_cover$TNC2023 <- native_cover_2023$TNC

# Add AWNC
native_cover$AWNC2010 <- native_cover_2010$AWNC
native_cover$AWNC2018 <- native_cover_2018$AWNC
native_cover$AWNC2019 <- native_cover_2019$AWNC
native_cover$AWNC2020 <- native_cover_2020$AWNC
native_cover$AWNC2021 <- native_cover_2021$AWNC
native_cover$AWNC2022 <- native_cover_2022$AWNC
native_cover$AWNC2023 <- native_cover_2023$AWNC

# Save
write_sf(obj = native_cover, dsn = "3_output/reporting/bsc/native_cover_2010_2023.shp")

rm(list=ls())
gc()
