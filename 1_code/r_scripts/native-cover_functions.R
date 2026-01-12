#' Native Cover Provincial 
#'
#' @description This function is used to create a single provincial layer for generating indicator status.
#'
#' @param landcover Lookup table of file paths for relevant HFI
#' @param riparian Year of HFI 
#' @param hfi.inventory File path for storing the geodatabase
#' @param geodatabase.path Lookup table of file paths for relevant HFI
#' @param boundaries Lookup table of file paths for relevant HFI
#' @param harvest.areas Year of HFI 
#' @param recovery.curve File path for storing the geodatabase
#' @param huc.id Arcpy object defined using reticulate
#' @param hfi.year Year of HFI 
#' @param arcpy Arcpy object defined using reticulate
#' @param numpy Numpy object defined using reticulate
#'
#' @export
#'
#'

native_cover_provincial <- function(landcover, riparian, hfi.inventory, geodatabase.path,
                                  boundaries, harvest.areas, recovery.curve, 
                                  huc.id, hfi.year, arcpy, numpy) {
        
        # If folder is not present, create it.
        if(!dir.exists(geodatabase.path)) {
                
                dir.create(geodatabase.path)
                
        }
        
        # Define the geodatabase for storing results
        arcpy$CreateFileGDB_management(out_folder_path = geodatabase.path, 
                                       out_name = paste0(huc.id, ".gdb"))
        
        # Define workspace
        arcpy$env$workspace <- paste0(getwd(), "/", geodatabase.path, "/", huc.id, ".gdb")
        
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

#' Native Cover Summary 
#'
#' @description This function calculates the native cover indicator for custom areas of interest based on the output of the native_cover_provincial function.
#'
#' @param current File path to the current landscape (TNC or AWNC)
#' @param reference File path to the reference landscape (TNC or AWNC)
#' @param region.id File path for focal region of interest
#' @param index Flag that determines if the indicator percentage, or total area in km2 is returned (Index, Area, Both).
#' @param geodatabase Path to the working scratch directory
#' @param arcpy Arcpy object defined using reticulate
#'
#' @export
#'
#'
native_cover_summary <- function(current, reference, region.id, index = "Index", geodatabase, arcpy) {
        
        # Clip to the region of interest
        arcpy$PairwiseClip_analysis(in_features = current, 
                                    clip_features = region.id, 
                                    out_feature_class = "current_region")
        
        arcpy$PairwiseClip_analysis(in_features = reference, 
                                    clip_features = region.id, 
                                    out_feature_class = "reference_region")
        
        # Load polygon into R to calculate the final score
        current.condition <- read_sf(dsn = geodatabase,
                                     layer = "current_region")
        
        reference.condition <- read_sf(dsn = geodatabase,
                                       layer = "reference_region")
        
        area.km <- sum(current.condition$Recovery * current.condition$Shape_Area) / 1000000
        area.condition <- (sum(current.condition$Recovery * current.condition$Shape_Area) /
                                   sum(reference.condition$Recovery * reference.condition$Shape_Area)) * 100
        
        # Delete the temporary files
        arcpy$Delete_management(in_data = "current_region")
        arcpy$Delete_management(in_data = "reference_region")
        
        # Return the relevant results to the user
        if(index == "Index") {
                
                return(area.condition)
        }
        
        if(index == "Area") {
                
                return(area.km)
        }
        
        if(index == "Both") {
                
                return(c(area.condition, area.km))
        }
        
}
