#' Initialize Python Helper Functions in Reticulate Environment
#'
#' @description Compiles ArcPy Python helper functions within the Reticulate session if not already loaded.
#' @return Invisible TRUE
#' @export
init_python_helpers <- function() {
        if (!reticulate::py_has_attr(reticulate::py, "update_footprint_attributes")) {
                reticulate::py_run_string("
import arcpy

def update_footprint_attributes(fc_path, harvest_id, hfi_year, min_age, max_age, lookup):
    '''
    Updates COVER_TYPE and RECOVERY fields directly via C-level UpdateCursor.
    Preserves original FEATURE_TY attribute.
    '''
    fields = ['FEATURE_TY', 'YEAR', 'NSRNAME', 'COVER_TYPE', 'RECOVERY']
    with arcpy.da.UpdateCursor(fc_path, fields) as cursor:
        for row in cursor:
            feat_type = str(row[0]) if row[0] is not None else ''
            year_val = row[1]
            nsr_val = str(row[2]) if row[2] is not None else ''
            
            if feat_type == harvest_id:
                cover_type = 'RECOVERING'
                if year_val is not None and nsr_val != '':
                    try:
                        age = int(hfi_year) - int(year_val)
                    except (ValueError, TypeError):
                        age = 0
                    clamped_age = max(min_age, min(max_age, age))
                    key = f'{nsr_val}_{clamped_age}'
                    rec_val = float(lookup.get(key, 0.0))
                else:
                    rec_val = 0.0
            else:
                cover_type = 'PERMANENT'
                rec_val = 0.0
                
            cursor.updateRow([row[0], row[1], row[2], cover_type, rec_val])
")
        }
        invisible(TRUE)
}