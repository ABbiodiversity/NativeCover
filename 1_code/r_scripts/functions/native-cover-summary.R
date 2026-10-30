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
native_cover_summary <- function(gdb.path, region.id, index = "Index", geodatabase, arcpy) {
        
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
        area.km.cur <- sum(native_cover$RECOVERY * native_cover$Shape_Area) / 1000000
        area.km.ref <- sum(native_cover$Shape_Area) / 1000000
        area.condition <- (area.km.cur / area.km.ref) * 100
        
        # Delete the temporary files
        arcpy$Delete_management(in_data = "native_cover")
        
        # Return the relevant results to the user
        if(index == "Index") {
                
                return(area.condition)
        }
        
        if(index == "Area") {
                
                results <- c(area.km.cur, area.km.ref)
                names(results) <- c("Current", "Reference")
                return(results)
        }
        
        if(index == "Both") {
                
                results <- c(area.condition, area.km.cur, area.km.ref)
                names(results) <- c("Index", "Current", "Reference")
                return(results)
        }
        
}
