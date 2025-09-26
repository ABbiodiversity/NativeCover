#
# Title: ABMI Reporting
# Created: May 27th, 2024
# Last Updated: May 16th, 2025
# Author: Brandon Allen
# Objectives: Calculate percent native cover for the COSIA and AlPac regions
# Keywords: Notes, Native Cover
#

#########
# Notes #
#########~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# 1) This analysis is run for the five COSIA regions and the AlPac FMA
# 2) We are using the ABMI Alberta Wetland Inventory. It is assumed areas not delineated with "aquatic" habitats are upland
#
################
# Native Cover # 
################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Clear memory
rm(list=ls())
gc()

# Load libraries
library(reticulate)
library(sf)

################################
# Define native cover function #
################################

native_cover <- function(current, reference, region) {
        
        # Clip to the region of interest
        arcpy$PairwiseClip_analysis(in_features = current, 
                                    clip_features = region, 
                                    out_feature_class = "current_region")
        
        arcpy$PairwiseClip_analysis(in_features = reference, 
                                    clip_features = region, 
                                    out_feature_class = "reference_region")
        
        # Load polygon into R to calculate the final score
        current.condition <- read_sf(dsn = paste0(getwd(), "/beta/scratch/native_cover.gdb"),
                                     layer = "current_region")
        
        reference.condition <- read_sf(dsn = paste0(getwd(), "/beta/scratch/native_cover.gdb"),
                                     layer = "reference_region")
        
        area.km <- sum(current.condition$Recovery * current.condition$Shape_Area) / 1000000
        area.condition <- (sum(current.condition$Recovery * current.condition$Shape_Area) /
                                   sum(reference.condition$Recovery * reference.condition$Shape_Area)) * 100
                
        # Delete the temporary files
        arcpy$Delete_management(in_data = "current_region")
        arcpy$Delete_management(in_data = "reference_region")
        
        return(c(area.condition, area.km))
        
}

####################
# Initialize arcpy #
####################

# Set python 
use_python(python = "C:/Program Files/ArcGIS/Pro/bin/Python/envs/arcgispro-py3/python.exe")

# Load arcpy
arcpy <- import('arcpy') 

# Define parallel processing factor
arcpy$env$parallelProcessingFactor <- "100%"

# Define the geodatabase for our scratch workspace
arcpy$CreateFileGDB_management(out_folder_path = paste0(getwd(), "/beta/scratch/"), 
                               out_name = "native_cover.gdb")

# Define workspace
arcpy$env$workspace <- paste0(getwd(), "/beta/scratch/native_cover.gdb")

# Define the five reporting regions
reporting.regions <- c("aoi_69.shp", "aoi_67.shp", "aoi_66.shp", "aoi_68.shp", "aoi_65.shp", "aoi_64.shp")
names(reporting.regions) <- c("alpac", "minable", "coldlake", "peaceriver", "athabasca", "osmregion")

# Create the results lookup table
native.cover.results <- data.frame(Region = c("alpac", "minable", "coldlake", "peaceriver", 
                                              "athabasca", "osmregion"),
                                   Aquatic2010 = NA,
                                   Aquatic2010km = NA,
                                   Aquatic2023 = NA,
                                   Aquatic2023km = NA,
                                   Terrestrial2010 = NA,
                                   Terrestrial2010km = NA,
                                   Terrestrial2023 = NA,
                                   Terrestrial2023km = NA)
rownames(native.cover.results) <- native.cover.results$Region

############################
# Native Cover Calculation # 
############################

for (region in names(reporting.regions)) { 
        
        # Define the region path
        region.path <- paste0("D:/spatial/Land_Cover_analysis/export_shapefile/",
                              reporting.regions[region])
        
        # Aquatic 2010
        native.cover.results[region, 
                             c("Aquatic2010", "Aquatic2010km")] <- native_cover(current = paste0(getwd(), "/results/gis/Aquatic_Current_2010.gdb/native_cover"), 
                                                            reference = paste0(getwd(), "/results/gis/Aquatic_Reference_2010.gdb/native_cover"), 
                                                            region = region.path)
        # Aquatic 2023
        native.cover.results[region, 
                             c("Aquatic2023", "Aquatic2023km")] <- native_cover(current = paste0(getwd(), "/results/gis/Aquatic_Current_2023.gdb/native_cover"), 
                                                            reference = paste0(getwd(), "/results/gis/Aquatic_Reference_2023.gdb/native_cover"), 
                                                            region = region.path)
        
        # Terrestrial 2010
        native.cover.results[region, 
                             c("Terrestrial2010", "Terrestrial2010km")] <- native_cover(current = paste0(getwd(), "/results/gis/Terrestrial_Current_2010.gdb/native_cover"), 
                                                            reference = paste0(getwd(), "/results/gis/Terrestrial_Reference_2010.gdb/native_cover"), 
                                                            region = region.path)
        
        # Terrestrial 2023
        native.cover.results[region, 
                             c("Terrestrial2023", "Terrestrial2023km")] <- native_cover(current = paste0(getwd(), "/results/gis/Terrestrial_Current_2023.gdb/native_cover"), 
                                                            reference = paste0(getwd(), "/results/gis/Terrestrial_Reference_2023.gdb/native_cover"), 
                                                            region = region.path)
        
        print(region)
        
}

# Save the results
write.csv(native.cover.results, file = "results/reporting/alpac-cosia-native-cover_2023.csv", row.names = FALSE)

# Delete the temporary file
arcpy$Delete_management(in_data = paste0(getwd(), "/beta/scratch/native_cover.gdb"))

rm(list=ls())
gc()
