# ---
# title: "Defining proportion of harvest areas (Backfill Version 7.0)"
# author: "Brandon Allen"
# created: "2025-05-113"
# inputs: ["D:/backfill/Version7.0/gdb_veghf_reference_condition_2010.gdb/veghf_2010"; 
#          "0_data/external/footprint/HFI2022_v1_1.gdb/HFI2022"; 
#          "0_data/external/boundaries/Natural_Regions_Subregions_of_Alberta.shp"]

# outputs: ["0_data/internal/lookup/harvest-stand-areas_2010HFI.Rdata"]
# notes: 
#   "This script calculate the reference area proportions (2010) for coniferous and 
#    deciduous stands in each natural subregion. This is based on the backfill version 7.0 and
#    supersedes 01a_forest-harvest-areas-v6.R."
# ---

# 1.0 Initializing environment ----
# This section handles defining the input data 
# and required functions.

# 1.1 Clear memory ----
rm(list=ls())
gc()

# 1.2 Load libraries ----
library(reticulate)
library(sf)

# 1.2 Initialize arcpy  ----
# Set python 
use_python(python = "C:/Program Files/ArcGIS/Pro/bin/Python/envs/arcgispro-py3/python.exe")

# Load arcpy
arcpy <- import('arcpy') 

# Define parallel processing factor
arcpy$env$parallelProcessingFactor <- "100%"

# Define the geodatabase for storing results
arcpy$CreateFileGDB_management(out_folder_path = paste0(getwd(), "/2_pipeline/scratch/"), 
                               out_name = "forest-areas.gdb")

# Define workspace
arcpy$env$workspace <- paste0(getwd(), "/2_pipeline/scratch/forest-areas.gdb")

# 2.0 Forest proportions ----
# This section handles calculating the proportion of coniferous and deciduous stands
# in each natural subregion of Alberta.

# 2.1 Define list of valid subregions ----
subregion.path <- "0_data/external/boundaries/Natural_Regions_Subregions_of_Alberta.shp"
natural.subregions <- read_sf(subregion.path)
natural.subregions <- unique(natural.subregions$NSRNAME)

# 2.2 Select all of the forested areas from the backfill layer (Version 7.0, 2010 HFI) ----
arcpy$Select_analysis(in_features = "D:/backfill/Version7.0/gdb_veghf_reference_condition_2010.gdb/veghf_2010", 
                      out_feature_class = "forested", 
                      where_clause =  paste0('"Combined_ChgByCWCS" IN ', "('AlpineLarch', 'Fir', 'Pine', 'Spruce', 'Conif', 'Decid', 'Mixedwood')"))

# 2.3 Select the timber harvest polygons based on the 2022 HFI ----
arcpy$Select_analysis(in_features = paste0(getwd(), "/0_data/external/footprint/HFI2022_v1_1.gdb/HFI2022"), 
                      out_feature_class = "harvest", 
                      where_clause =  paste0('"FEATURE_TY" IN ', "('TIMBER-HARVEST-GREEN-AREA', 'TIMBER-HARVEST-WHITE-AREA')"))

# 2.4 Subset the the vegetation information for harvest polygons ----
arcpy$PairwiseClip_analysis(in_features = "forested", 
                            clip_features = "harvest",
                            out_feature_class = "forest_harvest")

# 2.5 Loop through each natural subregion to calculation proportions ----
harvest.areas <- data.frame(NSR = natural.subregions,
                            Coniferous = NA,
                            Deciduous = NA)

for (nsr in natural.subregions) {
        
        # Select the natrual subregion
        arcpy$Select_analysis(in_features = subregion.path, 
                              out_feature_class = "boundary", 
                              where_clause =  paste0('"NSRNAME" = ', "'", nsr, "'"))
        
        # Clip the footprint to the boundary of interest
        arcpy$PairwiseClip_analysis(in_features = "forested", 
                                    clip_features = "boundary", 
                                    out_feature_class = "veg_hf")
        
        # Load layer
        veg.in <- read_sf(dsn = arcpy$env$workspace,
                          layer = "veg_hf")
        
        # Subset to harvested areas
        veg.in <- veg.in[veg.in$FEATURE_TY %in% c("HARVEST-AREA", "HARVEST-AREA-WHITE-ZONE"), ]
        
        # Area calculate
        total.area <- sum(as.numeric(st_area(veg.in)))
        harvest.areas[harvest.areas$NSR == nsr, "Coniferous"] <- (sum(as.numeric(st_area(veg.in[veg.in$Combined_ChgByCWCS %in% c("AlpineLarch", "Fir", "Pine", "Spruce", "Conif"), ]))) / total.area) * 100
        harvest.areas[harvest.areas$NSR == nsr, "Deciduous"] <- (sum(as.numeric(st_area(veg.in[veg.in$Combined_ChgByCWCS %in% c("Decid", "Mixedwood"), ]))) / total.area) * 100
        
        # Remove layers that are no longer required
        arcpy$Delete_management(in_data = "boundary")
        arcpy$Delete_management(in_data = "veg_hf")
        rm(veg.in)
        
        print(nsr)
        
}

# 2.6 Clean results and save ----
# There there is no harvest within a natural subregion, fix the proportions to 50% so they are evently weighted
harvest.areas$Coniferous[is.na(harvest.areas$Coniferous)] <- 50
harvest.areas$Deciduous[is.na(harvest.areas$Deciduous)] <- 50

comment(harvest.areas) <- c("Proportions of coniferous and deciduous stands based on 2010 harvested distribution.",
                          "This was chosen because 2010 is the reference condition for the BMF indicator.",
                          "Includes both Timber Harvest Green Area and White Areas as defined by the 2022 HFI.",
                          "Backfill version 7, HFI 2010.",
                          "Calculated May 13th, 2025.")
save(harvest.areas, file = "0_data/internal/harvest-stand-areas_2010HFI_backfillv7_v2.0.Rdata")

# Remove temporary files
arcpy$Delete_management(in_data = arcpy$env$workspace)

rm(list=ls())
gc()

