#
# Title: Functions for calculating "terrestrial" and "aquatic" native cover
# Created: October 7th, 2022
# Last Updated: May 15th, 2025
# Author: Brandon Allen
# Objectives: Define functions for calculating percent native cover
# Keywords: Footprint cleaning, Native cover cleaning, Native cover index
# The footprint cleaning function standardizes the harvest area polygons and naming conventions between HFI versions (HARVEST-AREA)
# The native cover cleaning function is used to create a single provincial layer for generating indicator status.
# The native cover index function is used to generate indicator status for custom AOIs.

######################
# Footprint cleaning # 
######################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

footprint_cleaning <- function(hfi.inventory, hfi.year, arcpy) {
        
        # If 2022 or 2023, just copy and clean
        if(hfi.year %in% c(2022, 2023)) {
                
                # Define the geodatabase for storing results
                arcpy$CreateFileGDB_management(out_folder_path = paste0(getwd(), "/data/processed/hfi/"), 
                                               out_name = paste0("hfi_", hfi.year, ".gdb"))
                
                # Define workspace
                arcpy$env$workspace <-paste0(getwd(), "/data/processed/hfi/hfi_", hfi.year, ".gdb")
                
                # Select the harvest area polygons from the layer of interest
                arcpy$Copy_management(in_data = hfi.inventory[hfi.inventory$HFI == hfi.year, "Path"], 
                                      out_data = paste0("hfi_", hfi.year))
                
                # Correct the naming convention in the new harvest polygons ("HARVEST-AREA")
                cursor <- arcpy$da$UpdateCursor(in_table = paste0("hfi_", hfi.year), 
                                                field_names = c("FEATURE_TY", "YEAR"), 
                                                where_clause = "FEATURE_TY IN ('TIMBER-HARVEST-GREEN-AREA', 'TIMBER-HARVEST-WHITE-AREA')")
                
                # Update each row in the cursor
                row <- iter_next(cursor)
                while (!is.null(row)) {
                        
                        # Update Feature Type
                        row[[1]] <- "HARVEST-AREA"
                        
                        # Update the value
                        cursor$updateRow(row)
                        
                        # Go to the next row
                        row <- iter_next(cursor)  
                        
                }
                
                rm(row, cursor)
                
        } else {
                
                # Otherwise perform the standardization
                
                # Define the geodatabase for storing results
                arcpy$CreateFileGDB_management(out_folder_path = paste0(getwd(), "/data/processed/hfi/"), 
                                               out_name = paste0("hfi_", hfi.year, ".gdb"))
                
                # Define workspace
                arcpy$env$workspace <-paste0(getwd(), "/data/processed/hfi/hfi_", hfi.year, ".gdb")
                
                # Select the potential harvest area polygons from the layer of interest
                arcpy$Select_analysis(in_features = hfi.inventory[hfi.inventory$HFI == hfi.year, "Path"], 
                                      out_feature_class = "potential_harvest", 
                                      where_clause = "FEATURE_TY IN ('HARVEST-AREA', 'HARVEST-AREA-WHITE-ZONE')")
                
                # Select the harvest area polygons from the 2022 HFI
                arcpy$Select_analysis(in_features = hfi.inventory[hfi.inventory$HFI == 2022, "Path"], 
                                      out_feature_class = "harvest_2022", 
                                      where_clause = "FEATURE_TY IN ('TIMBER-HARVEST-GREEN-AREA', 'TIMBER-HARVEST-WHITE-AREA')")
                
                # Clip the potential polygons to the extent of the 2022 harvest
                arcpy$PairwiseClip_analysis(in_features = "potential_harvest", 
                                            clip_features = "harvest_2022",
                                            out_feature_class = "harvest")
                
                # Erase the defined harvest areas from the original focal year to isolate permanent polygons that
                # are in a transition to another feature type.
                arcpy$PairwiseErase_analysis(in_features = "potential_harvest", 
                                             erase_features = "harvest", 
                                             out_feature_class = "disturbance")
                
                # Correct the naming convention in the new harvest polygons ("HARVEST-AREA")
                cursor <- arcpy$da$UpdateCursor(in_table = "harvest", 
                                                field_names = c("FEATURE_TY", "YEAR"))
                
                # Update each row in the cursor
                row <- iter_next(cursor)
                while (!is.null(row)) {
                        
                        # Update Feature Type
                        row[[1]] <- "HARVEST-AREA"
                        
                        # Update the value
                        cursor$updateRow(row)
                        
                        # Go to the next row
                        row <- iter_next(cursor)  
                        
                }
                
                # Clear row and cursor
                rm(row, cursor)
                
                # Correct the naming convention in the remaining disturbed polygons ("WOODY-VEGETATION-REMOVAL")
                cursor <- arcpy$da$UpdateCursor(in_table = "disturbance", 
                                                field_names = c("FEATURE_TY", "YEAR"))
                
                # Update each row in the cursor
                row <- iter_next(cursor)
                while (!is.null(row)) {
                        
                        # Update Feature Type
                        row[[1]] <-"WOODY-VEGETATION-REMOVAL"
                        
                        # Update the value
                        cursor$updateRow(row)
                        
                        # Go to the next row
                        row <- iter_next(cursor)  
                        
                }
                
                # Clear row and cursor
                rm(row, cursor)
                
                # Create a new HFI layer with the harvest areas erased
                arcpy$PairwiseErase_analysis(in_features = hfi.inventory[hfi.inventory$HFI == hfi.year, "Path"], 
                                             erase_features = "potential_harvest", 
                                             out_feature_class = "hfi_no_harvest")
                
                # Add the newly updated harvest and disturbed polygons
                arcpy$Merge_management(inputs = "hfi_no_harvest; disturbance; harvest", 
                                       output = paste0("hfi_", hfi.year))
                
                arcpy$Delete_management(in_data = "harvest")
                arcpy$Delete_management(in_data = "harvest_2022")
                arcpy$Delete_management(in_data = "potential_harvest")
                arcpy$Delete_management(in_data = "disturbance")
                arcpy$Delete_management(in_data = "hfi_no_harvest")
                
        }
        
}

#########################
# Native cover cleaning # 
#########################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

native_cover_cleaning <- function(landcover, riparian, hfi.inventory, boundaries, harvest.areas, recovery.curve, huc.id, hfi.year, arcpy, numpy) {
        
        # Define the geodatabase for storing results
        arcpy$CreateFileGDB_management(out_folder_path = paste0(getwd(), "/data/processed/", hfi.year, "/gis/"), 
                                       out_name = paste0(huc.id, ".gdb"))
        
        # Define workspace
        arcpy$env$workspace <- paste0(getwd(), "/data/processed/", hfi.year, "/gis/", huc.id, ".gdb")
        
        # Select the watershed
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
        harvest.id <- "HARVEST-AREA" 
        
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
        
        # Add the default modifer for the aquatic reference condition (1)
        aquatic.reference <- read_sf(dsn = arcpy$env$workspace, 
                                     layer =  "aquatic_reference")
        
        # Create list of tuple results
        recovery <- list()
        
        for(x in 1:nrow(aquatic.reference)) {
                
                recovery[[x]] <- tuple(x, 1)
                
        }
        
        # Convert to a numpy array
        recovery <- numpy$array(recovery,
                                dtype = list(tuple('OBJECTID','float'),
                                             tuple('Recovery','float')))
        
        # Store using extend table
        arcpy$da$ExtendTable("aquatic_reference",
                             "OBJECTID",
                             recovery,
                             "OBJECTID",
                             FALSE)
        
        # Add the default modifier aquatic current conditions (1)
        aquatic.current <- read_sf(dsn = arcpy$env$workspace, 
                                   layer =  "aquatic_current")
        
        # Create list of tuple results
        recovery <- list()
        
        for(x in 1:nrow(aquatic.current)) {
                
                recovery[[x]] <- tuple(x, 1)
                
        }
        
        # Convert to a numpy array
        recovery <- numpy$array(recovery,
                                dtype = list(tuple('OBJECTID','float'),
                                             tuple('Recovery','float')))
        
        # Store using extend table
        arcpy$da$ExtendTable("aquatic_current",
                             "OBJECTID",
                             recovery,
                             "OBJECTID",
                             FALSE)
        
        #
        # Harvest areas in aquatic
        #
        
        # Intersect the terrestrial harvest by the nsr
        arcpy$Intersect_analysis(in_features = "aquatic_harvest; boundary", 
                                 out_feature_class = "aquatic_harvest_nsr")
        
        aquatic.harvest <- read_sf(dsn = arcpy$env$workspace, 
                                   layer =  "aquatic_harvest_nsr")
        
        
        if(nrow(aquatic.harvest) != 0) { 
                
                # Simplify into broad stand types and calculate age
                aquatic.harvest <- data.frame(Habitat = aquatic.harvest$FEATURE_TY,
                                              Year = aquatic.harvest$YEAR, 
                                              UID = 1:nrow(aquatic.harvest),
                                              NSRNAME = aquatic.harvest$NSRNAME, 
                                              Adjustment = NA)
                # Calculate age
                aquatic.harvest$Age <- hfi.year - aquatic.harvest$Year
                
                # If age is greater than the maximum age present in the recovery curve, set to the max
                aquatic.harvest$Age[aquatic.harvest$Age > max(recovery.curve$Age)] <- max(recovery.curve$Age)
                
                aquatic.nsr.combined <- NULL
                
                for (nsr in unique(aquatic.harvest$NSRNAME)) {
                        
                        # Calculate a weighted recovery curve based on the subregion
                        nsr.recovery <- recovery.curve
                        nsr.recovery$Recovery <- (nsr.recovery$Deciduous * harvest.areas[harvest.areas$NSR == nsr, "Deciduous"] / 100) + 
                                (nsr.recovery$Coniferous * harvest.areas[harvest.areas$NSR == nsr, "Coniferous"] / 100)
                        
                        # Create subset
                        aquatic.nsr <- aquatic.harvest[aquatic.harvest$NSRNAME == nsr, ]
                        
                        # Create adjustment
                        aquatic.nsr$Adjustment <- nsr.recovery$Recovery[match(aquatic.nsr$Age, nsr.recovery$Age)] / 100
                        
                        # Match
                        aquatic.nsr.combined <- rbind(aquatic.nsr, aquatic.nsr.combined)
                }
                
                aquatic.nsr.combined <- aquatic.nsr.combined[order(aquatic.nsr.combined$UID), ]
                
                ##############################
                # Append the adjusted values #
                ##############################
                
                # Create list of tuple results
                recovery <- list()
                
                for(x in 1:nrow(aquatic.nsr.combined)) {
                        
                        recovery[[x]] <- tuple(x, aquatic.nsr.combined$Adjustment[x])
                        
                }
                
                # Convert to a numpy array
                recovery <- numpy$array(recovery,
                                        dtype = list(tuple('OBJECTID','float'),
                                                     tuple('Recovery','float')))
                
                # Store using extend table
                arcpy$da$ExtendTable("aquatic_harvest_nsr",
                                     "OBJECTID",
                                     recovery,
                                     "OBJECTID",
                                     FALSE)
                
                # Merge the current area and harvest area
                arcpy$Merge_management(inputs = "aquatic_current; aquatic_harvest_nsr", 
                                       output = "aquatic_current_complete")
                
        } else {
                
                # If there are no harvest areas present, copy over the file and rename for simplicity
                # Merge the current area and harvest area
                arcpy$Copy_management(in_data = "aquatic_current", 
                                      out_data = "aquatic_current_complete")
                
        }
        
        #
        # Harvest areas in terrestrial
        #
        
        # Add the default modifier terrestrial reference conditions (1)
        terrestrial.reference <- read_sf(dsn = arcpy$env$workspace, 
                                         layer =  "terrestrial_reference")
        
        # Create list of tuple results
        recovery <- list()
        
        for(x in 1:nrow(terrestrial.reference)) {
                
                recovery[[x]] <- tuple(x, 1)
                
        }
        
        # Convert to a numpy array
        recovery <- numpy$array(recovery,
                                dtype = list(tuple('OBJECTID','float'),
                                             tuple('Recovery','float')))
        
        # Store using extend table
        arcpy$da$ExtendTable("terrestrial_reference",
                             "OBJECTID",
                             recovery,
                             "OBJECTID",
                             FALSE)
        
        # Add the default modifier terrestrial current conditions (1)
        terrestrial.current <- read_sf(dsn = arcpy$env$workspace, 
                                       layer =  "terrestrial_current")
        
        # Create list of tuple results
        recovery <- list()
        
        for(x in 1:nrow(terrestrial.current)) {
                
                recovery[[x]] <- tuple(x, 1)
                
        }
        
        # Convert to a numpy array
        recovery <- numpy$array(recovery,
                                dtype = list(tuple('OBJECTID','float'),
                                             tuple('Recovery','float')))
        
        # Store using extend table
        arcpy$da$ExtendTable("terrestrial_current",
                             "OBJECTID",
                             recovery,
                             "OBJECTID",
                             FALSE)
        
        # Intersect the terrestrial harvest by the nsr
        arcpy$Intersect_analysis(in_features = "terrestrial_harvest; boundary", 
                                 out_feature_class = "terrestrial_harvest_nsr")
        
        terrestrial.harvest <- read_sf(dsn = arcpy$env$workspace, 
                                       layer =  "terrestrial_harvest_nsr")
        
        if(nrow(terrestrial.harvest) != 0) { 
                
                # Simplify into broad stand types and calculate age
                terrestrial.harvest <- data.frame(Habitat = terrestrial.harvest$FEATURE_TY,
                                                  Year = terrestrial.harvest$YEAR, 
                                                  UID = 1:nrow(terrestrial.harvest),
                                                  NSRNAME = terrestrial.harvest$NSRNAME, 
                                                  Adjustment = NA)
                # Calculate age
                terrestrial.harvest$Age <- hfi.year - terrestrial.harvest$Year
                
                # If age is greater than the maximum age present in the recovery curve, set to the max
                terrestrial.harvest$Age[terrestrial.harvest$Age > max(recovery.curve$Age)] <- max(recovery.curve$Age)
                
                terrestrial.nsr.combined <- NULL
                
                for (nsr in unique(terrestrial.harvest$NSRNAME)) {
                        
                        # Calculate a weighted recovery curve based on the subregion
                        nsr.recovery <- recovery.curve
                        nsr.recovery$Recovery <- (nsr.recovery$Deciduous * harvest.areas[harvest.areas$NSR == nsr, "Deciduous"] / 100) + 
                                (nsr.recovery$Coniferous * harvest.areas[harvest.areas$NSR == nsr, "Coniferous"] / 100)
                        
                        # Create subset
                        terrestrial.nsr <- terrestrial.harvest[terrestrial.harvest$NSRNAME == nsr, ]
                        
                        # Create adjustment
                        terrestrial.nsr$Adjustment <- nsr.recovery$Recovery[match(terrestrial.nsr$Age, nsr.recovery$Age)] / 100
                        
                        # Match
                        terrestrial.nsr.combined <- rbind(terrestrial.nsr, terrestrial.nsr.combined)
                }
                
                terrestrial.nsr.combined <- terrestrial.nsr.combined[order(terrestrial.nsr.combined$UID), ]
                
                ##############################
                # Append the adjusted values #
                ##############################
                
                # Create list of tuple results
                recovery <- list()
                
                for(x in 1:nrow(terrestrial.nsr.combined)) {
                        
                        recovery[[x]] <- tuple(x, terrestrial.nsr.combined$Adjustment[x])
                        
                }
                
                # Convert to a numpy array
                recovery <- numpy$array(recovery,
                                        dtype = list(tuple('OBJECTID','float'),
                                                     tuple('Recovery','float')))
                
                # Store using extend table
                arcpy$da$ExtendTable("terrestrial_harvest_nsr",
                                     "OBJECTID",
                                     recovery,
                                     "OBJECTID",
                                     FALSE)
                
                # Merge the current area and harvest area
                arcpy$Merge_management(inputs = "terrestrial_current; terrestrial_harvest_nsr", 
                                       output = "terrestrial_current_complete")
                
        }  else {
                
                # If there are no harvest areas present, copy over the file and rename for simplicity
                # Merge the current area and harvest area
                arcpy$Copy_management(in_data = "terrestrial_current", 
                                      out_data = "terrestrial_current_complete")
                
        }
        
        
        # Remove layers that are no longer required
        arcpy$Delete_management(in_data = "boundary")
        arcpy$Delete_management(in_data = "footprint")
        arcpy$Delete_management(in_data = "wetland_inventory")
        arcpy$Delete_management(in_data = "riparian_inventory")
        arcpy$Delete_management(in_data = "harvested")
        arcpy$Delete_management(in_data = "aquatic_complex")
        arcpy$Delete_management(in_data = "aquatic_current")
        arcpy$Delete_management(in_data = "aquatic_harvest")
        arcpy$Delete_management(in_data = "aquatic_harvest_nsr")
        arcpy$Delete_management(in_data = "terrestrial_harvest")
        arcpy$Delete_management(in_data = "terrestrial_harvest_nsr")
        arcpy$Delete_management(in_data = "terrestrial_current")
        
        
}

######################
# Native cover index # 
######################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

native_cover_index <- function(landcover, riparian, hfi.inventory, harvest.areas, recovery.curve, boundaries, huc.id, hfi.year, arcpy, numpy) {
        
        # Define the geodatabase for storing results
        arcpy$CreateFileGDB_management(out_folder_path = paste0(getwd(), "/data/processed/mapping/", hfi.year, "/gis/"), 
                                       out_name = paste0(huc.id, ".gdb"))
        
        # Define workspace
        arcpy$env$workspace <- paste0(getwd(), "/data/processed/mapping/", hfi.year, "/gis/", huc.id, ".gdb")
        
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
        harvest.id <- "HARVEST-AREA" 
        
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
        
        # Add the default modifer for the aquatic reference condition (1)
        aquatic.reference <- read_sf(dsn = arcpy$env$workspace, 
                                     layer =  "aquatic_reference")
        
        # Create list of tuple results
        recovery <- list()
        
        for(x in 1:nrow(aquatic.reference)) {
                
                recovery[[x]] <- tuple(x, 1)
                
        }
        
        # Convert to a numpy array
        recovery <- numpy$array(recovery,
                                dtype = list(tuple('OBJECTID','float'),
                                             tuple('Recovery','float')))
        
        # Store using extend table
        arcpy$da$ExtendTable("aquatic_reference",
                             "OBJECTID",
                             recovery,
                             "OBJECTID",
                             FALSE)
        
        # Add the default modifier aquatic current conditions (1)
        aquatic.current <- read_sf(dsn = arcpy$env$workspace, 
                                   layer =  "aquatic_current")
        
        # Create list of tuple results
        recovery <- list()
        
        for(x in 1:nrow(aquatic.current)) {
                
                recovery[[x]] <- tuple(x, 1)
                
        }
        
        # Convert to a numpy array
        recovery <- numpy$array(recovery,
                                dtype = list(tuple('OBJECTID','float'),
                                             tuple('Recovery','float')))
        
        # Store using extend table
        arcpy$da$ExtendTable("aquatic_current",
                             "OBJECTID",
                             recovery,
                             "OBJECTID",
                             FALSE)
        
        #
        # Harvest areas in aquatic
        #
        
        # Intersect the terrestrial harvest by the nsr
        arcpy$Intersect_analysis(in_features = "aquatic_harvest; boundary", 
                                 out_feature_class = "aquatic_harvest_nsr")
        
        aquatic.harvest <- read_sf(dsn = arcpy$env$workspace, 
                                   layer =  "aquatic_harvest_nsr")
        
        
        if(nrow(aquatic.harvest) != 0) { 
                
                # Simplify into broad stand types and calculate age
                aquatic.harvest <- data.frame(Habitat = aquatic.harvest$FEATURE_TY,
                                              Year = aquatic.harvest$YEAR, 
                                              UID = 1:nrow(aquatic.harvest),
                                              NSRNAME = aquatic.harvest$NSRNAME, 
                                              Adjustment = NA)
                # Calculate age
                aquatic.harvest$Age <- hfi.year - aquatic.harvest$Year
                
                # If age is greater than the maximum age present in the recovery curve, set to the max
                aquatic.harvest$Age[aquatic.harvest$Age > max(recovery.curve$Age)] <- max(recovery.curve$Age)
                
                aquatic.nsr.combined <- NULL
                
                for (nsr in unique(aquatic.harvest$NSRNAME)) {
                        
                        # Calculate a weighted recovery curve based on the subregion
                        nsr.recovery <- recovery.curve
                        nsr.recovery$Recovery <- (nsr.recovery$Deciduous * harvest.areas[harvest.areas$NSR == nsr, "Deciduous"] / 100) + 
                                (nsr.recovery$Coniferous * harvest.areas[harvest.areas$NSR == nsr, "Coniferous"] / 100)
                        
                        # Create subset
                        aquatic.nsr <- aquatic.harvest[aquatic.harvest$NSRNAME == nsr, ]
                        
                        # Create adjustment
                        aquatic.nsr$Adjustment <- nsr.recovery$Recovery[match(aquatic.nsr$Age, nsr.recovery$Age)] / 100
                        
                        # Match
                        aquatic.nsr.combined <- rbind(aquatic.nsr, aquatic.nsr.combined)
                }
                
                aquatic.nsr.combined <- aquatic.nsr.combined[order(aquatic.nsr.combined$UID), ]
                
                ##############################
                # Append the adjusted values #
                ##############################
                
                # Create list of tuple results
                recovery <- list()
                
                for(x in 1:nrow(aquatic.nsr.combined)) {
                        
                        recovery[[x]] <- tuple(x, aquatic.nsr.combined$Adjustment[x])
                        
                }
                
                # Convert to a numpy array
                recovery <- numpy$array(recovery,
                                        dtype = list(tuple('OBJECTID','float'),
                                                     tuple('Recovery','float')))
                
                # Store using extend table
                arcpy$da$ExtendTable("aquatic_harvest_nsr",
                                     "OBJECTID",
                                     recovery,
                                     "OBJECTID",
                                     FALSE)
                
                # Merge the current area and harvest area
                arcpy$Merge_management(inputs = "aquatic_current; aquatic_harvest_nsr", 
                                       output = "aquatic_current_complete")
                
        } else {
                
                # If there are no harvest areas present, copy over the file and rename for simplicity
                # Merge the current area and harvest area
                arcpy$Copy_management(in_data = "aquatic_current", 
                                      out_data = "aquatic_current_complete")
                
        }
        
        #
        # Harvest areas in terrestrial
        #
        
        # Add the default modifier terrestrial reference conditions (1)
        terrestrial.reference <- read_sf(dsn = arcpy$env$workspace, 
                                         layer =  "terrestrial_reference")
        
        # Create list of tuple results
        recovery <- list()
        
        for(x in 1:nrow(terrestrial.reference)) {
                
                recovery[[x]] <- tuple(x, 1)
                
        }
        
        # Convert to a numpy array
        recovery <- numpy$array(recovery,
                                dtype = list(tuple('OBJECTID','float'),
                                             tuple('Recovery','float')))
        
        # Store using extend table
        arcpy$da$ExtendTable("terrestrial_reference",
                             "OBJECTID",
                             recovery,
                             "OBJECTID",
                             FALSE)
        
        # Add the default modifier terrestrial current conditions (1)
        terrestrial.current <- read_sf(dsn = arcpy$env$workspace, 
                                       layer =  "terrestrial_current")
        
        # Create list of tuple results
        recovery <- list()
        
        for(x in 1:nrow(terrestrial.current)) {
                
                recovery[[x]] <- tuple(x, 1)
                
        }
        
        # Convert to a numpy array
        recovery <- numpy$array(recovery,
                                dtype = list(tuple('OBJECTID','float'),
                                             tuple('Recovery','float')))
        
        # Store using extend table
        arcpy$da$ExtendTable("terrestrial_current",
                             "OBJECTID",
                             recovery,
                             "OBJECTID",
                             FALSE)
        
        # Intersect the terrestrial harvest by the nsr
        arcpy$Intersect_analysis(in_features = "terrestrial_harvest; boundary", 
                                 out_feature_class = "terrestrial_harvest_nsr")
        
        terrestrial.harvest <- read_sf(dsn = arcpy$env$workspace, 
                                       layer =  "terrestrial_harvest_nsr")
        
        if(nrow(terrestrial.harvest) != 0) { 
                
                # Simplify into broad stand types and calculate age
                terrestrial.harvest <- data.frame(Habitat = terrestrial.harvest$FEATURE_TY,
                                                  Year = terrestrial.harvest$YEAR, 
                                                  UID = 1:nrow(terrestrial.harvest),
                                                  NSRNAME = terrestrial.harvest$NSRNAME, 
                                                  Adjustment = NA)
                # Calculate age
                terrestrial.harvest$Age <- hfi.year - terrestrial.harvest$Year
                
                # If age is greater than the maximum age present in the recovery curve, set to the max
                terrestrial.harvest$Age[terrestrial.harvest$Age > max(recovery.curve$Age)] <- max(recovery.curve$Age)
                
                terrestrial.nsr.combined <- NULL
                
                for (nsr in unique(terrestrial.harvest$NSRNAME)) {
                        
                        # Calculate a weighted recovery curve based on the subregion
                        nsr.recovery <- recovery.curve
                        nsr.recovery$Recovery <- (nsr.recovery$Deciduous * harvest.areas[harvest.areas$NSR == nsr, "Deciduous"] / 100) + 
                                (nsr.recovery$Coniferous * harvest.areas[harvest.areas$NSR == nsr, "Coniferous"] / 100)
                        
                        # Create subset
                        terrestrial.nsr <- terrestrial.harvest[terrestrial.harvest$NSRNAME == nsr, ]
                        
                        # Create adjustment
                        terrestrial.nsr$Adjustment <- nsr.recovery$Recovery[match(terrestrial.nsr$Age, nsr.recovery$Age)] / 100
                        
                        # Match
                        terrestrial.nsr.combined <- rbind(terrestrial.nsr, terrestrial.nsr.combined)
                }
                
                terrestrial.nsr.combined <- terrestrial.nsr.combined[order(terrestrial.nsr.combined$UID), ]
                
                ##############################
                # Append the adjusted values #
                ##############################
                
                # Create list of tuple results
                recovery <- list()
                
                for(x in 1:nrow(terrestrial.nsr.combined)) {
                        
                        recovery[[x]] <- tuple(x, terrestrial.nsr.combined$Adjustment[x])
                        
                }
                
                # Convert to a numpy array
                recovery <- numpy$array(recovery,
                                        dtype = list(tuple('OBJECTID','float'),
                                                     tuple('Recovery','float')))
                
                # Store using extend table
                arcpy$da$ExtendTable("terrestrial_harvest_nsr",
                                     "OBJECTID",
                                     recovery,
                                     "OBJECTID",
                                     FALSE)
                
                # Merge the current area and harvest area
                arcpy$Merge_management(inputs = "terrestrial_current; terrestrial_harvest_nsr", 
                                       output = "terrestrial_current_complete")
                
        }  else {
                
                # If there are no harvest areas present, copy over the file and rename for simplicity
                # Merge the current area and harvest area
                arcpy$Copy_management(in_data = "terrestrial_current", 
                                      out_data = "terrestrial_current_complete")
                
        }
        
        
        # Remove layers that are no longer required
        arcpy$Delete_management(in_data = "boundary")
        arcpy$Delete_management(in_data = "footprint")
        arcpy$Delete_management(in_data = "wetland_inventory")
        arcpy$Delete_management(in_data = "riparian_inventory")
        arcpy$Delete_management(in_data = "harvested")
        arcpy$Delete_management(in_data = "aquatic_complex")
        arcpy$Delete_management(in_data = "aquatic_current")
        arcpy$Delete_management(in_data = "aquatic_harvest")
        arcpy$Delete_management(in_data = "aquatic_harvest_nsr")
        arcpy$Delete_management(in_data = "terrestrial_harvest")
        arcpy$Delete_management(in_data = "terrestrial_harvest_nsr")
        arcpy$Delete_management(in_data = "terrestrial_current")
        
        
}

