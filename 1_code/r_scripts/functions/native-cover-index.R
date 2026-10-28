#' Native Cover Index
#'
#' @description Generates a unified provincial geospatial layer tracking native vegetation, 
#' recovering vegetation, permanent footprint, and habitat type (Aquatic vs Terrestrial).
#'
#' @param landcover Path for wetland inventory
#' @param riparian Path for riparian inventory
#' @param hfi_inventory Path for human footprint inventory (HFI)
#' @param geodatabase_path Base directory path for storing output geodatabases
#' @param boundaries Path for watershed boundaries
#' @param harvest_areas Data frame containing NSR harvest proportions
#' @param recovery_curve Data frame containing recovery curve by Age
#' @param huc_id Watershed HUC identifier
#' @param hfi_year Numeric year of HFI
#' @param arcpy Arcpy object defined using reticulate
#' @param numpy Retained for signature compatibility (optional)
#'
#' @export
native_cover_index <- function(landcover, riparian, hfi_inventory, geodatabase_path,
                                    boundaries, harvest_areas, recovery_curve, 
                                    huc_id, hfi_year, arcpy, numpy = NULL) {
        
        # ---------------------------------------------------------------------------
        # 1. Environment & Parallel Worker Configuration
        # ---------------------------------------------------------------------------
        arcpy$env$overwriteOutput <- TRUE
        arcpy$SetLogHistory(FALSE)                     # Disable XML log file locks
        arcpy$env$parallelProcessingFactor <- "0%"      # Prevent multi-threading collisions per R worker
        
        gdb_name <- paste0(huc_id, ".gdb")
        full_gdb_path <- file.path(geodatabase_path, gdb_name)
        
        if (!dir.exists(geodatabase_path)) {
                dir.create(geodatabase_path, recursive = TRUE)
        }
        
        if (!arcpy$Exists(full_gdb_path)) {
                arcpy$CreateFileGDB_management(out_folder_path = geodatabase_path, out_name = gdb_name)
        }
        
        arcpy$env$workspace <- full_gdb_path
        
        # Clean in-memory workspace for current thread session
        try(arcpy$Delete_management("memory"), silent = TRUE)
        init_python_helpers()
        
        # ---------------------------------------------------------------------------
        # 2. Vectorized Recovery Curve Lookup Construction
        # ---------------------------------------------------------------------------
        harvest_id <- "HARVEST-AREA"
        max_age <- max(recovery_curve$Age, na.rm = TRUE)
        min_age <- min(recovery_curve$Age, na.rm = TRUE)
        
        recovery_lookup <- list()
        unique_nsrs <- unique(harvest_areas$NSR)
        
        for (nsr in unique_nsrs) {
                har_sub <- harvest_areas[harvest_areas$NSR == nsr, ]
                dec_prop <- har_sub$Deciduous[1] / 100
                con_prop <- har_sub$Coniferous[1] / 100
                
                for (i in 1:nrow(recovery_curve)) {
                        age_val <- recovery_curve$Age[i]
                        dec_rec <- recovery_curve$Deciduous[i] / 100
                        con_rec <- recovery_curve$Coniferous[i] / 100
                        
                        rec_factor <- (dec_rec * dec_prop) + (con_rec * con_prop)
                        key <- paste0(nsr, "_", age_val)
                        recovery_lookup[[key]] <- as.numeric(rec_factor)
                }
        }
        
        py_lookup <- reticulate::r_to_py(recovery_lookup)
        
        # ---------------------------------------------------------------------------
        # 3. Process Entire Boundary Footprint & Recovery Metrics (Single Pass)
        # ---------------------------------------------------------------------------
        # A. Select HUC boundary
        where_clause <- paste0('"HUC_8" = \'', huc_id, '\'')
        arcpy$Select_analysis(in_features = boundaries, 
                              out_feature_class = "memory/boundary", 
                              where_clause = where_clause)
        
        # B. Intersect footprint with boundary to attach NSRNAME attribute
        arcpy$PairwiseIntersect_analysis(in_features = c(hfi_inventory, "memory/boundary"), 
                                         out_feature_class = "memory/footprint")
        
        # C. Erase footprint from boundary to isolate all Native Vegetation
        arcpy$Erase_analysis(in_features = "memory/boundary", 
                             erase_features = "memory/footprint", 
                             out_feature_class = "memory/native_boundary")
        
        arcpy$AddField_management("memory/native_boundary", "COVER_TYPE", "TEXT", field_length = 20)
        arcpy$AddField_management("memory/native_boundary", "RECOVERY", "DOUBLE")
        arcpy$AddField_management("memory/native_boundary", "FEATURE_TY", "TEXT", field_length = 100)
        
        arcpy$CalculateField_management("memory/native_boundary", "COVER_TYPE", "'NATIVE'", "PYTHON3")
        arcpy$CalculateField_management("memory/native_boundary", "RECOVERY", "1.0", "PYTHON3")
        arcpy$CalculateField_management("memory/native_boundary", "FEATURE_TY", "'NATIVE'", "PYTHON3")
        
        # D. Process footprint impact attributes across entire boundary
        footprint_count <- as.integer(arcpy$GetCount_management("memory/footprint")$getOutput(0L))
        
        if (footprint_count > 0) {
                arcpy$AddField_management("memory/footprint", "COVER_TYPE", "TEXT", field_length = 20)
                arcpy$AddField_management("memory/footprint", "RECOVERY", "DOUBLE")
                
                # Execute fast C-level Python cursor across whole footprint
                reticulate::py$update_footprint_attributes(
                        fc_path = "memory/footprint", 
                        harvest_id = harvest_id, 
                        hfi_year = hfi_year, 
                        min_age = min_age, 
                        max_age = max_age, 
                        lookup = py_lookup
                )
                
                # Merge native and footprint components into boundary cover master
                arcpy$Merge_management(inputs = c("memory/native_boundary", "memory/footprint"), 
                                       output = "memory/boundary_cover")
        } else {
                arcpy$CopyFeatures_management(in_features = "memory/native_boundary", 
                                              out_feature_class = "memory/boundary_cover")
        }
        
        # ---------------------------------------------------------------------------
        # 4. Classify Aquatic vs. Terrestrial Habitat Types
        # ---------------------------------------------------------------------------
        # Clip Wetland & Riparian Inventories
        arcpy$PairwiseClip_analysis(in_features = landcover, 
                                    clip_features = "memory/boundary", 
                                    out_feature_class = "memory/wetland_inventory")
        
        arcpy$PairwiseClip_analysis(in_features = riparian, 
                                    clip_features = "memory/boundary", 
                                    out_feature_class = "memory/riparian_inventory")
        
        arcpy$Merge_management(inputs = c("memory/wetland_inventory", "memory/riparian_inventory"), 
                               output = "memory/aquatic_complex")
        
        arcpy$PairwiseDissolve_analysis(in_features = "memory/aquatic_complex", 
                                        out_feature_class = "memory/aquatic_reference")
        
        # Intersect with Aquatic Reference -> Aquatic Native Cover
        arcpy$PairwiseIntersect_analysis(in_features = c("memory/boundary_cover", 
                                                         "memory/aquatic_reference"), 
                                         out_feature_class = "memory/aquatic_cover")
        
        arcpy$AddField_management("memory/aquatic_cover", "HABITAT_TYPE", "TEXT", field_length = 20)
        arcpy$CalculateField_management("memory/aquatic_cover", "HABITAT_TYPE", "'AQUATIC'", "PYTHON3")
        
        # Erase Aquatic Reference -> Terrestrial Native Cover
        arcpy$Erase_analysis(in_features = "memory/boundary_cover", 
                             erase_features = "memory/aquatic_reference", 
                             out_feature_class = "memory/terrestrial_cover")
        
        arcpy$AddField_management("memory/terrestrial_cover", "HABITAT_TYPE", "TEXT", field_length = 20)
        arcpy$CalculateField_management("memory/terrestrial_cover", "HABITAT_TYPE", "'TERRESTRIAL'", "PYTHON3")
        
        # ---------------------------------------------------------------------------
        # 5. Filter Fields & Output Final Geodatabase Feature Classes
        # ---------------------------------------------------------------------------
        user_fields_to_keep <- c("HABITAT_TYPE", "COVER_TYPE", "RECOVERY", "FEATURE_TY")
        
        # Safely drop intermediate attributes while protecting system geometry fields
        drop_unwanted_fields("memory/aquatic_cover", user_fields_to_keep, arcpy)
        drop_unwanted_fields("memory/terrestrial_cover", user_fields_to_keep, arcpy)
        
        # Export individual habitat feature classes to File GDB
        arcpy$CopyFeatures_management(
                in_features = "memory/aquatic_cover", 
                out_feature_class = file.path(full_gdb_path, "aquatic_native_cover")
        )
        
        arcpy$CopyFeatures_management(
                in_features = "memory/terrestrial_cover", 
                out_feature_class = file.path(full_gdb_path, "terrestrial_native_cover")
        )
        
        # Clean up memory workspace for this thread
        try(arcpy$Delete_management("memory"), silent = TRUE)
        
        invisible(TRUE)
}