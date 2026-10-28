#' Drop Unwanted Fields Safely from ArcPy Feature Class
#'
#' @param fc_path Path to the feature class
#' @param keep_fields Character vector of user field names to retain
#' @param arcpy Reticulate arcpy module reference
drop_unwanted_fields <- function(fc_path, keep_fields, arcpy) {
        fields <- arcpy$ListFields(fc_path)
        keep_upper <- toupper(keep_fields)
        
        # Protected system field types and reserved geometry names
        system_types <- c("OID", "GEOMETRY", "GLOBALID")
        system_names <- c("OBJECTID", "FID", "SHAPE", "SHAPE_LENGTH", "SHAPE_AREA", "SHAPE.LEN", "SHAPE.AREA")
        
        drop_list <- c()
        for (f in fields) {
                f_name_upper <- toupper(f$name)
                f_type_upper <- toupper(f$type)
                
                # Evaluate protection criteria
                is_system_type  <- f_type_upper %in% system_types
                is_system_name  <- f_name_upper %in% system_names
                is_required     <- isTRUE(f$required)
                is_user_kept    <- f_name_upper %in% keep_upper
                
                # Only drop fields that pass none of the protection checks
                if (!is_system_type && !is_system_name && !is_required && !is_user_kept) {
                        drop_list <- c(drop_list, f$name)
                }
        }
        
        if (length(drop_list) > 0) {
                arcpy$DeleteField_management(in_table = fc_path, drop_field = drop_list)
        }
        
        invisible(TRUE)
}
