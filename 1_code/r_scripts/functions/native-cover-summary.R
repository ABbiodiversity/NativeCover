#' Native Cover Summary 
#'
#' @description This function calculates the native cover indicator for custom areas of interest based on the output of the native_cover_provincial function.
#'
#' @param gdb_path File path to the native cover landscape (TNC or AWNC)
#' @param region_id File path for focal region of interest
#' @param index Flag that determines if the indicator percentage, or total area in km2 is returned (Index, Area, Both).
#' @param geodatabase Path to the working scratch directory
#' @param arcpy Arcpy object defined using reticulate
#'
#' @export
#'
#'
native_cover_summary <- function(gdb_path, region_id, index = "Index", geodatabase, arcpy) {
        
        # Clip to the region of interest
        arcpy$PairwiseClip_analysis(in_features = gdb.path, 
                                    clip_features = region.id, 
                                    out_feature_class = "native_cover")
        
        # Load polygon into R to calculate the final score
        native_cover <- read_sf(dsn = geodatabase,
                                layer = "native_cover")
        
        # Recalculate shape area
        native_cover$Shape_Area <- as.numeric(st_area(native_cover))
        
        # Calculate total area and index
        area_km_cur <- sum(native_cover$RECOVERY * native_cover$Shape_Area) / 1000000
        area_km_ref <- sum(native_cover$Shape_Area) / 1000000
        area_condition <- (area_km_cur / area_km_ref) * 100
        
        # Delete the temporary files
        arcpy$Delete_management(in_data = "native_cover")
        
        # Return the relevant results to the user
        if(index == "Index") {
                
                return(area_condition)
        }
        
        if(index == "Area") {
                
                results <- c(area_km_cur, area_km_ref)
                names(results) <- c("Current", "Reference")
                return(results)
        }
        
        if(index == "Both") {
                
                results <- c(area_condition, area_km_cur, area_km_ref)
                names(results) <- c("Index", "Current", "Reference")
                return(results)
        }
        
}
