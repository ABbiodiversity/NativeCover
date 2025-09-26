# ---
# title: "Defining proportion of harvest areas (Backfill Version 6.0)"
# author: "Brandon Allen"
# created: "2025-10-16"
# inputs: ["D:/backfill/Veg61_2010_HFI_2010v2_Grid_1KM.gdb/Veg61_2010_HFI_2010v2_Grid_1KM_GenHabTY"; 
#          "0_data/external/boundaries/Natural_Regions_Subregions_of_Alberta.shp"]
# outputs: ["0_data/internal/lookup/harvest-stand-areas_2010HFI.Rdata"]
# notes: 
#   "This script calculate the reference area proportions (2010) for coniferous and 
#    deciduous stands in each natural subregion. This is based on the backfill version 6.0 and
#    has been superseded by 01b_forest-harvest-areas-v7.R."
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
arcpy$CreateFileGDB_management(out_folder_path = paste0(getwd(), "/0_data/internal/landcover/"), 
                               out_name = "forest-areas.gdb")

# Define workspace
arcpy$env$workspace <- paste0(getwd(), "/0_data/internal/landcover/forest-areas.gdb")

# 2.0 Forest proportions ----
# This section handles calculating the proportion of coniferous and deciduous stands
# in each natural subregion of Alberta.

# 2.1 Define list of valid subregions ----
natural.subregions <- read_sf("0_data/external/boundaries/Natural_Regions_Subregions_of_Alberta.shp")
natural.subregions <- unique(natural.subregions$NSRNAME)

# 2.2 Select all of the forested areas from the backfill layer ----
arcpy$Select_analysis(in_features = "D:/backfill/Veg61_2010_HFI_2010v2_Grid_1KM.gdb/Veg61_2010_HFI_2010v2_Grid_1KM_GenHabTY", 
                      out_feature_class = "forested", 
                      where_clause =  paste0('"Combined_ChgByCWCS" IN ', "('AlpineLarch', 'Fir', 'Pine', 'Spruce', 'Conif', 'Decid', 'Mixedwood')"))

# 2.3 Loop through each natural subregion to calculation proportions ----
harvest.areas <- data.frame(NSR = natural.subregions,
                            Coniferous = NA,
                            Deciduous = NA)

for (nsr in natural.subregions) {
        
        # Select watershed
        arcpy$Select_analysis(in_features = "0_data/external/boundaries/Natural_Regions_Subregions_of_Alberta.shp", 
                              out_feature_class = "boundary", 
                              where_clause =  paste0('"NSRNAME" = ', "'", nsr, "'"))
        
        # Clip the footprint to the boundary of interest
        arcpy$PairwiseClip_analysis(in_features = "forested", 
                                    clip_features = "boundary", 
                                    out_feature_class = "veg_hf")
        
        # Load layer
        veg.in <- read_sf(dsn = "0_data/internal/landcover/forest-areas.gdb",
                          layer = "veg_hf")
        
        # Subset to harvested areas
        veg.in <- veg.in[veg.in$FEATURE_TY == "HARVEST-AREA", ]
        
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

# 2.4 Clean results and save ----
# There there is no harvest within a natural subregion, fix the proportions to 0
harvest.areas$Coniferous[is.na(harvest.areas$Coniferous)] <- 0
harvest.areas$Deciduous[is.na(harvest.areas$Deciduous)] <- 0

comment(harvest.areas) <- c("Proportions of coniferous and deciduous stands based on 2010 harvested distribution.",
                          "This was chosen because 2010 is the reference condition for the BMF indicator.",
                          "Backfill version 6.1, HFI 2010",
                          "Calculated October 28th, 2022.")
save(harvest.areas, file = "0_data/internal/lookup/harvest-stand-areas_2010HFI.Rdata")

rm(list=ls())
gc()