#
# Title: Calculating the Reference harvest area proportions using version 7 of the backfill layer
# Created: July 13th, 2023
# Last Updated: July 20th, 2023
# Author: Brandon Allen
# Objectives: Calculate the reference area proportions (2010) for coniferous and deciduous stands in each natural subregion using version 7 of the backfill.
# Keywords: Notes, Forest proportions 

######################
# Forest proportions # 
######################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Clear memory
rm(list=ls())
gc()

# Load libraries
library(reticulate)
library(sf)

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

# Define the geodatabase for storing results
arcpy$CreateFileGDB_management(out_folder_path = paste0(getwd(), "/data/base/landcover/"), 
                               out_name = "forest-areas-v7.gdb")

# Define workspace
arcpy$env$workspace <- paste0(getwd(), "/data/base/landcover/forest-areas-v7.gdb")

# Define the natural subregions
natural.subregions <- read_sf("data/base/boundaries/Natural_Regions_Subregions_of_Alberta.shp")
region.list <- unique(natural.subregions$NSRNAME)

# Select all of the forested areas from the backfill layer
arcpy$Select_analysis(in_features = "D:/backfill/Veg7HF2010/gdb_SC_veg7hf_2010.gdb/vegv7hf2010", 
                      out_feature_class = "forested", 
                      where_clause =  paste0('"Combined_ChgByCWCS" IN ', "('AlpineLarch', 'Fir', 'Pine', 'Spruce', 'Conif', 'Decid', 'Mixedwood')"))

harvest.areas <- data.frame(NSR = region.list,
                            Coniferous = NA,
                            Deciduous = NA)

for (nsr in region.list) {
        
        # Select watershed
        arcpy$Select_analysis(in_features = "data/base/boundaries/Natural_Regions_Subregions_of_Alberta.shp", 
                              out_feature_class = "boundary", 
                              where_clause =  paste0('"NSRNAME" = ', "'", nsr, "'"))
        
        # Clip the footprint to the boundary of interest
        arcpy$PairwiseClip_analysis(in_features = "forested", 
                                    clip_features = "boundary", 
                                    out_feature_class = "veg_hf")
        
        # Load layer
        veg.in <- read_sf(dsn = "data/base/landcover/forest-areas-v7.gdb",
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

# There there is no harvest within a natural subregion, fix the proportions to 50% so they are evently weighted
harvest.areas$Coniferous[is.na(harvest.areas$Coniferous)] <- 50
harvest.areas$Deciduous[is.na(harvest.areas$Deciduous)] <- 50

comment(harvest.areas) <- c("Proportions of coniferous and deciduous stands based on 2010 harvested distribution.",
                          "This was chosen because 2010 is the reference condition for the BMF indicator.",
                          "Includes only HARVEST-AREAS, includes HARVEST-AREA-WHITE-ZONE.",
                          "HARVEST-AREAS-WHITE-ZONE is not in the 2018HFI.",
                          "Therefore, we are looking for all 'potentially harvested' areas.",
                          "Backfill version 7, HFI 2010.",
                          "Calculated July 14th, 2023.")
save(harvest.areas, file = "data/lookup/harvest-stand-areas_2010HFI_v7.Rdata")

rm(list=ls())
gc()

