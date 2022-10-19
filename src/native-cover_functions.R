#
# Title: Functions for calculating "terrestrial" and "aquatic" native cover
# Created: October 7, 2022
# Last Updated: October 19, 2022
# Author: Brandon Allen
# Objectives: Define functions for calculating percent native cover
# Keywords: Native cover
#


################
# Native cover # 
################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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
        
        # Clip the harvest areas within the reference conditions
        arcpy$PairwiseClip_analysis(in_features = harvest.areas, 
                                    clip_features = "terrestrial_reference", 
                                    out_feature_class = "terrestrial_harvest")
        
        arcpy$PairwiseClip_analysis(in_features = harvest.areas, 
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
        results[results$HUC_8 == huc.id, "LowRef"] <- sum(st_area(read_sf(dsn = arcpy$env$workspace, 
                                                                          layer =  "aquatic_reference")))
        results[results$HUC_8 == huc.id, "UpRef"] <- sum(st_area(read_sf(dsn = arcpy$env$workspace, 
                                                                         layer =  "terrestrial_reference")))
        
        # Calculate current condition with harvest recovery for aquatic
        aquatic.current <- as.numeric(sum(st_area(read_sf(dsn = arcpy$env$workspace, 
                                               layer =  "aquatic_current"))))
        
        aquatic.harvest <- read_sf(dsn = arcpy$env$workspace, 
                                   layer =  "aquatic_harvest")

        # Simplify into broad stand types and calculate age
        aquatic.harvest <- data.frame(Habitat = aquatic.harvest$Combined_ChgByCWCS,
                                      Year = aquatic.harvest$YEAR, 
                                      Area = as.numeric(st_area(aquatic.harvest)),
                                      Adjustment = NA)
        
        aquatic.harvest$Habitat[aquatic.harvest$Habitat %in% c("Decid", "Mixedwood")] <- "Deciduous"
        aquatic.harvest$Habitat[aquatic.harvest$Habitat %in% c("Fir", "Pine", "Spruce")] <- "Coniferous"
        
        aquatic.harvest$Year <- hfi.year - aquatic.harvest$Year
        
        aquatic.harvest$Adjustment[aquatic.harvest$Habitat == "Deciduous"] <- harvest.recovery$Deciduous[match(aquatic.harvest$Year[aquatic.harvest$Habitat == "Deciduous"], harvest.recovery$Age)]
        aquatic.harvest$Adjustment[aquatic.harvest$Habitat == "Coniferous"] <- harvest.recovery$Coniferous[match(aquatic.harvest$Year[aquatic.harvest$Habitat == "Coniferous"], harvest.recovery$Age)]
        
        aquatic.harvest$AreaAdjusted <- aquatic.harvest$Area * (aquatic.harvest$Adjustment / 100)
        
        # Calculate current condition with harvest recovery for terrestrial
        
        terrestrial.current <- as.numeric(sum(st_area(read_sf(dsn = arcpy$env$workspace, 
                                              layer =  "terrestrial_current"))))
        
        terrestrial.harvest <- read_sf(dsn = arcpy$env$workspace, 
                                   layer =  "terrestrial_harvest")
        
        # Simplify into broad stand types and calculate age
        terrestrial.harvest <- data.frame(Habitat = terrestrial.harvest$Combined_ChgByCWCS,
                                      Year = terrestrial.harvest$YEAR, 
                                      Area = as.numeric(st_area(terrestrial.harvest)),
                                      Adjustment = NA)
        
        terrestrial.harvest$Habitat[terrestrial.harvest$Habitat %in% c("Decid", "Mixedwood")] <- "Deciduous"
        terrestrial.harvest$Habitat[terrestrial.harvest$Habitat %in% c("Fir", "Pine", "Spruce")] <- "Coniferous"
        
        terrestrial.harvest$Year <- hfi.year - terrestrial.harvest$Year
        
        terrestrial.harvest$Adjustment[terrestrial.harvest$Habitat == "Deciduous"] <- harvest.recovery$Deciduous[match(terrestrial.harvest$Year[terrestrial.harvest$Habitat == "Deciduous"], harvest.recovery$Age)]
        terrestrial.harvest$Adjustment[terrestrial.harvest$Habitat == "Coniferous"] <- harvest.recovery$Coniferous[match(terrestrial.harvest$Year[terrestrial.harvest$Habitat == "Coniferous"], harvest.recovery$Age)]
        
        terrestrial.harvest$AreaAdjusted <- terrestrial.harvest$Area * (terrestrial.harvest$Adjustment / 100)
        
        
        results[results$HUC_8 == huc.id, "LowCur"] <- aquatic.current + sum(aquatic.harvest$AreaAdjusted)

        results[results$HUC_8 == huc.id, "UpCur"] <- terrestrial.current + sum(terrestrial.harvest$AreaAdjusted)
        
        # Remove all layers
        arcpy$Delete_management(in_data = "boundary")
        arcpy$Delete_management(in_data = "footprint")
        arcpy$Delete_management(in_data = "wetland_inventory")
        arcpy$Delete_management(in_data = "riparian_inventory")
        arcpy$Delete_management(in_data = "aquatic_complex")
        
        # return results
        results
        
}
