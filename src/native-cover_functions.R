#
# Title: Functions for calculating "terrestrial" and "aquatic" native cover
# Created: October 7, 2022
# Last Updated: October 7, 2022
# Author: Brandon Allen
# Objectives: Define functions for calculating percent native cover
# Keywords: Native Cover
#

################
# Native Cover # 
################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

native_cover <- function(landcover, riparian, boundaries, hfi.boundary, huc.id, hfi.year, results, arcpy) {
        
        # Define the geodatabase for storing results
        arcpy$CreateFileGDB_management(out_folder_path = paste0(getwd(), "/data/processed/", hfi.year, "/gis/"), 
                                       out_name = paste0(huc.id, ".gdb"))
        
        # Define workspace
        arcpy$env$workspace <- paste0(getwd(), "/data/processed/", hfi.year, "/gis/", huc.id, ".gdb")
        
        # Select watershed
        arcpy$Select_analysis(in_features = boundaries, 
                              out_feature_class = "boundary", 
                              where_clause =  paste0('"HUC_8" = ', "'", paste(huc.id), "'"))
        
        # Clip footprint to boundary of interest
        arcpy$PairwiseClip_analysis(in_features = hfi.boundary, 
                                    clip_features = "boundary", 
                                    out_feature_class = "footprint")
        
        # Clip wetland layer to boundary of interest
        arcpy$PairwiseClip_analysis(in_features = landcover, 
                                    clip_features = "boundary", 
                                    out_feature_class = "wetland_inventory")
        
        # Clip riparian layer to boundary of interest
        arcpy$PairwiseClip_analysis(in_features = riparian, 
                                    clip_features = "boundary", 
                                    out_feature_class = "riparian_inventory")
        
        # Merge the wetland and riparian areas together
        arcpy$Merge_management(inputs = "wetland_inventory; riparian_inventory", 
                               output = "lowland_complex")
        
        # Dissolve the wetland layer
        arcpy$PairwiseDissolve_analysis(in_features = "lowland_complex", 
                                        out_feature_class = "lowland_reference")
        
        # Calculate upland layer (inverse of lowland)
        arcpy$SymDiff_analysis(in_features = "lowland_reference", 
                               update_features = "boundary", 
                               out_feature_class = "upland_reference")
        
        # Remove footprint from upland layer
        arcpy$Erase_analysis(in_features = "upland_reference", 
                             erase_features = "footprint", 
                             out_feature_class = "upland_current")
        
        # Remove footprint from lowland layer
        arcpy$Erase_analysis(in_features = "lowland_reference", 
                             erase_features = "footprint", 
                             out_feature_class = "lowland_current")
        
        # Calculate areas and store
        results[results$HUC_8 == huc.id, "LowRef"] <- sum(st_area(read_sf(dsn = arcpy$env$workspace, 
                                                                          layer =  "lowland_reference")))
        results[results$HUC_8 == huc.id, "LowCur"] <- sum(st_area(read_sf(dsn = arcpy$env$workspace, 
                                                                          layer =  "lowland_current")))
        results[results$HUC_8 == huc.id, "UpRef"] <- sum(st_area(read_sf(dsn = arcpy$env$workspace, 
                                                                         layer =  "upland_reference")))
        results[results$HUC_8 == huc.id, "UpCur"] <- sum(st_area(read_sf(dsn = arcpy$env$workspace, 
                                                                         layer =  "upland_current")))
        # Remove all layers
        arcpy$Delete_management(in_data = "boundary")
        arcpy$Delete_management(in_data = "footprint")
        arcpy$Delete_management(in_data = "wetland_inventory")
        arcpy$Delete_management(in_data = "riparian_inventory")
        arcpy$Delete_management(in_data = "lowland_complex")
        
        # return results
        results
        
}
