#
# Title: Standardize forest harvest areas
# Created: October 19th, 2022
# Last Updated: October 19th, 2022
# Author: Brandon Allen
# Objectives: Delineate the forest harvest areas for the two footprint inventories
# Keywords: Notes, Forest harvest

#########
# Notes #
#########~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# 1) This analysis is run for each HUC 8 across the province.
# 2) Extracting the forest harvest areas from the 2010 and 2018 backfill layers
#
##################
# Forest harvest # 
##################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Clear memory
rm(list=ls())
gc()

# Load libraries
library(reticulate)

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
arcpy$CreateFileGDB_management(out_folder_path = paste0(getwd(), "/data/base/footprint/"), 
                               out_name = "harvest_areas.gdb")

# Define workspace
arcpy$env$workspace <- paste0(getwd(), "/data/base/footprint/harvest_areas.gdb")

# Extract the 2010 harvest areas
arcpy$Select_analysis(in_features = "D:/backfill/Veg61_2010_HFI_2010v2_Grid_1KM.gdb/Veg61_2010_HFI_2010v2_Grid_1KM_GenHabTY", 
                      out_feature_class = "harvested_2010HFI", 
                      where_clause =  paste0('"FEATURE_TY" = ', "'HARVEST-AREA'"))

# Extract the 2018 harvest areas
arcpy$Select_analysis(in_features = "D:/backfill/veg61hf2018_bdqt.gdb/veg61hf2018_BDQT_mtos", 
                      out_feature_class = "harvested_2018HFI", 
                      where_clause =  paste0('"FEATURE_TY" = ', "'HARVEST-AREA'"))

# Clear memory
rm(list=ls())
gc()

