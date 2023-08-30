#
# Title: Functions for calculating "terrestrial" and "aquatic" native cover
# Created: October 7th, 2022
# Last Updated: July 24th, 2023
# Author: Brandon Allen
# Objectives: Define functions for calculating percent native cover
# Keywords: Native cover
#

################
# Native cover # 
################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

native_cover <- function(landcover, riparian, hfi.inventory, harvest.areas, recovery.curve, boundaries, huc.id, hfi.year, results, arcpy) {
        
        # Define the geodatabase for storing results
        arcpy$CreateFileGDB_management(out_folder_path = paste0(getwd(), "/data/processed/", hfi.year, "/gis/"), 
                                       out_name = paste0(huc.id, ".gdb"))
        
        # Define workspace
        arcpy$env$workspace <- paste0(getwd(), "/data/processed/", hfi.year, "/gis/", huc.id, ".gdb")
        
        # Select watershed
        arcpy$Select_analysis(in_features = boundaries, 
                              out_feature_class = "boundary", 
                              where_clause =  paste0('"HUC_8" = ', "'", paste(huc.id), "'"))
        
        # Clip the footprint to the boundary of interest
        arcpy$PairwiseClip_analysis(in_features = hfi.inventory, 
                                    clip_features = "boundary", 
                                    out_feature_class = "footprint")
        
        # Clip the wetland and riparian inventories to the boundary of interest
        arcpy$PairwiseClip_analysis(in_features = landcover, 
                                    clip_features = "boundary", 
                                    out_feature_class = "wetland_inventory")
        
        arcpy$PairwiseClip_analysis(in_features = riparian, 
                                    clip_features = "boundary", 
                                    out_feature_class = "riparian_inventory")
        
        arcpy$Merge_management(inputs = "wetland_inventory; riparian_inventory", 
                               output = "aquatic_complex")
        
        # Dissolve the wetland layer
        arcpy$PairwiseDissolve_analysis(in_features = "aquatic_complex", 
                                        out_feature_class = "aquatic_reference")
        
        # Calculate terrestrial layer (inverse of aquatic)
        arcpy$SymDiff_analysis(in_features = "aquatic_reference", 
                               update_features = "boundary", 
                               out_feature_class = "terrestrial_reference")
        
        # Pull out the harvest areas from the footprint data
        harvest.id <- "HARVEST-AREA"  # HFI 2010 uses the same names as 2018
        # if (hfi.year == 2010) {
        #         
        #         harvest.id <- "CUTBLOCK"
        #         
        # } else {
        #               
        #         harvest.id <- "HARVEST-AREA"  
        #         
        # }
        
        arcpy$Select_analysis(in_features = "footprint", 
                              out_feature_class = "harvested", 
                              where_clause =  paste0('"FEATURE_TY" = ', "'", harvest.id, "'"))
        
        # Clip the harvest areas within the reference conditions
        arcpy$PairwiseClip_analysis(in_features = "harvested", 
                                    clip_features = "terrestrial_reference", 
                                    out_feature_class = "terrestrial_harvest")
        
        arcpy$PairwiseClip_analysis(in_features = "harvested", 
                                    clip_features = "aquatic_reference", 
                                    out_feature_class = "aquatic_harvest")
        
        # Remove footprint from terrestrial layer
        arcpy$Erase_analysis(in_features = "terrestrial_reference", 
                             erase_features = "footprint", 
                             out_feature_class = "terrestrial_current")
        
        # Remove footprint from aquatic layer
        arcpy$Erase_analysis(in_features = "aquatic_reference", 
                             erase_features = "footprint", 
                             out_feature_class = "aquatic_current")
        
        # Calculate reference condition
        results[results$HUC_8 == huc.id, "LowRef"] <- as.numeric(sum(st_area(read_sf(dsn = arcpy$env$workspace, 
                                                                                     layer =  "aquatic_reference"))))
        results[results$HUC_8 == huc.id, "UpRef"] <- as.numeric(sum(st_area(read_sf(dsn = arcpy$env$workspace, 
                                                                                    layer =  "terrestrial_reference"))))
        
        # Calculate current condition with harvest recovery for aquatic
        aquatic.current <- as.numeric(sum(st_area(read_sf(dsn = arcpy$env$workspace, 
                                                          layer =  "aquatic_current"))))
        
        # Intersect the terrestrial harvest by the nsr
        arcpy$Intersect_analysis(in_features = "aquatic_harvest; boundary", 
                                 out_feature_class = "aquatic_harvest_nsr")
        
        aquatic.harvest <- read_sf(dsn = arcpy$env$workspace, 
                                       layer =  "aquatic_harvest_nsr")
        
        if(nrow(aquatic.harvest) != 0) { 
                
                # Simplify into broad stand types and calculate age
                aquatic.harvest <- data.frame(Habitat = aquatic.harvest$FEATURE_TY,
                                                  Year = aquatic.harvest$YEAR, 
                                                  Area = as.numeric(st_area(aquatic.harvest)),
                                                  NSRNAME = aquatic.harvest$NSRNAME, 
                                                  Adjustment = NA)
                # Calculate age
                aquatic.harvest$Age <- hfi.year - aquatic.harvest$Year
                
                # If age is greater than the maximum age present in the recovery curve, set to the max
                aquatic.harvest$Age[aquatic.harvest$Age > max(recovery.curve$Age)] <- max(recovery.curve$Age)
                
                # Define blank value
                aquatic.harvest.area <- 0
                
                for (nsr in unique(aquatic.harvest$NSRNAME)) {
                        
                        # Calculate a weighted recovery curve based on the subregion
                        nsr.recovery <- recovery.curve
                        nsr.recovery$Recovery <- (nsr.recovery$Deciduous * harvest.areas[harvest.areas$NSR == nsr, "Deciduous"] / 100) + 
                                (nsr.recovery$Coniferous * harvest.areas[harvest.areas$NSR == nsr, "Coniferous"] / 100)
                        
                        # Create subset
                        aquatic.nsr <- aquatic.harvest[aquatic.harvest$NSRNAME == nsr, ]
                        
                        # Create adjustment
                        aquatic.nsr$Adjustment <- nsr.recovery$Recovery[match(aquatic.nsr$Age, nsr.recovery$Age)] / 100
                        
                        aquatic.harvest.area <- aquatic.harvest.area + sum(aquatic.nsr$Area * aquatic.nsr$Adjustment)
                        
                }
                
        } else {
                
                aquatic.harvest.area <- 0
                
        }
        
        # Calculate current condition with harvest recovery for terrestrial
        terrestrial.current <- as.numeric(sum(st_area(read_sf(dsn = arcpy$env$workspace, 
                                                              layer =  "terrestrial_current"))))
        
        # Intersect the terrestrial harvest by the nsr
        arcpy$Intersect_analysis(in_features = "terrestrial_harvest; boundary", 
                                 out_feature_class = "terrestrial_harvest_nsr")
        
        
        terrestrial.harvest <- read_sf(dsn = arcpy$env$workspace, 
                                       layer =  "terrestrial_harvest_nsr")
        
        if(nrow(terrestrial.harvest) != 0) { 
                
                # Simplify into broad stand types and calculate age
                terrestrial.harvest <- data.frame(Habitat = terrestrial.harvest$FEATURE_TY,
                                                  Year = terrestrial.harvest$YEAR, 
                                                  Area = as.numeric(st_area(terrestrial.harvest)),
                                                  NSRNAME = terrestrial.harvest$NSRNAME, 
                                                  Adjustment = NA)
                # Calculate age
                terrestrial.harvest$Age <- hfi.year - terrestrial.harvest$Year
                
                # If age is greater than the maximum age present in the recovery curve, set to the max
                terrestrial.harvest$Age[terrestrial.harvest$Age > max(recovery.curve$Age)] <- max(recovery.curve$Age)
                
                # Define blank value
                terrestrial.harvest.area <- 0
                
                for (nsr in unique(terrestrial.harvest$NSRNAME)) {
                        
                        # Calculate a weighted recovery curve based on the subregion
                        nsr.recovery <- recovery.curve
                        nsr.recovery$Recovery <- (nsr.recovery$Deciduous * harvest.areas[harvest.areas$NSR == nsr, "Deciduous"] / 100) + 
                                (nsr.recovery$Coniferous * harvest.areas[harvest.areas$NSR == nsr, "Coniferous"] / 100)
                        
                        # Create subset
                        terrestrial.nsr <- terrestrial.harvest[terrestrial.harvest$NSRNAME == nsr, ]
                        
                        # Create adjustment
                        terrestrial.nsr$Adjustment <- nsr.recovery$Recovery[match(terrestrial.nsr$Age, nsr.recovery$Age)] / 100
                        
                        terrestrial.harvest.area <- terrestrial.harvest.area + sum(terrestrial.nsr$Area * terrestrial.nsr$Adjustment)
                        
                }
                
        } else {
                
                terrestrial.harvest.area <- 0
                
        }
        
        # Add the adjusted forest stand areas
        results[results$HUC_8 == huc.id, "LowCur"] <- aquatic.current + aquatic.harvest.area
        results[results$HUC_8 == huc.id, "UpCur"] <- terrestrial.current + terrestrial.harvest.area
        
        # Calculate percent cover
        results[results$HUC_8 == huc.id, "LowCov"] <- (results$LowCur[results$HUC_8 == huc.id] / results$LowRef[results$HUC_8 == huc.id]) * 100
        results[results$HUC_8 == huc.id, "UpCov"] <- (results$UpCur[results$HUC_8 == huc.id] / results$UpRef[results$HUC_8 == huc.id]) * 100
        
        # Remove layers that are no longer required
        arcpy$Delete_management(in_data = "boundary")
        arcpy$Delete_management(in_data = "footprint")
        arcpy$Delete_management(in_data = "wetland_inventory")
        arcpy$Delete_management(in_data = "riparian_inventory")
        arcpy$Delete_management(in_data = "aquatic_complex")
        
        # return results
        results
        
}
