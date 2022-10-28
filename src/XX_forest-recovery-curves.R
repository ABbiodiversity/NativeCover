#
# Title: Standardize forest recovery curves
# Created: October 26th, 2022
# Last Updated: October 28th, 2022
# Author: Brandon Allen
# Objectives: Aggregate the standardized forest recovery curve for each NSR
# Keywords: Notes, Forest recovery

###################
# Forest recovery # 
###################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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
                               out_name = "forest-areas.gdb")

# Define workspace
arcpy$env$workspace <- paste0(getwd(), "/data/base/landcover/forest-areas.gdb")

# Define the natural subregions
natural.subregions <- read_sf("data/base/boundaries/Natural_Regions_Subregions_of_Alberta.shp")

region.list <- unique(natural.subregions$NSRNAME)

# Select all of the forested areas from the backfill layer
arcpy$Select_analysis(in_features = "D:/backfill/veg61hf2018_bdqt.gdb/veg61hf2018_BDQT_mtos", 
                      out_feature_class = "forested", 
                      where_clause =  paste0('"Combined_ChgByCWCS" IN ', "('AlpineLarch', 'Fir', 'Pine', 'Spruce', 'Conif', 'Decid', 'Mixedwood')"))

nsr.results <- data.frame(NSR = region.list,
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
        veg.in <- read_sf(dsn = "data/base/landcover/forest-areas.gdb",
                          layer = "veg_hf")
        
        # Subset to harvested areas
        veg.in <- veg.in[veg.in$FEATURE_TY == "HARVEST-AREA", ]
        
        # Area calculate
        total.area <- sum(as.numeric(st_area(veg.in)))
        nsr.results[nsr.results$NSR == nsr, "Coniferous"] <- (sum(as.numeric(st_area(veg.in[veg.in$Combined_ChgByCWCS %in% c("AlpineLarch", "Fir", "Pine", "Spruce", "Conif"), ]))) / total.area) * 100
        nsr.results[nsr.results$NSR == nsr, "Deciduous"] <- (sum(as.numeric(st_area(veg.in[veg.in$Combined_ChgByCWCS %in% c("Decid", "Mixedwood"), ]))) / total.area) * 100
        
        # Remove layers that are no longer required
        arcpy$Delete_management(in_data = "boundary")
        arcpy$Delete_management(in_data = "veg_hf")
        rm(veg.in)
        
        print(nsr)
        
}

# There there is no harvest within a natural subregion, fix the proportions to 0
nsr.results$Coniferous[is.na(nsr.results$Coniferous)] <- 0
nsr.results$Deciduous[is.na(nsr.results$Deciduous)] <- 0

comment(nsr.results) <- c("Proportions of coniferous and deciduous stands based on 2018 harvested distribution.",
                          "Backfill version 6.1, HFI 2018",
                          "Calculated October 28th, 2022.")
save(nsr.results, file = "data/lookup/forest-areas_2018.Rdata")

rm(list=ls())
gc()