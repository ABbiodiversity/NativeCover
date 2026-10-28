#' Native Cover plot 
#'
#' @description This function is used to create a visualiztion of the native cover indicator for QAQC
#'
#' @param data.in Shapefile of watershed boundaries and associated native cover scores.
#' @param habitat Defines 
#' @param title Plot title
#'
#' @export
#'
#'

nc_plot <- function(data.in, habitat, title) {
        
        ggplot() + 
                geom_sf(data = data.in, aes_string(fill = habitat), show.legend = TRUE) +
                scale_fill_gradientn(name = paste0("Native Cover (%)"), colors = met.brewer(name = "Hiroshige", n = 100, type = "continuous"), guide = "colourbar") +
                ggtitle(title) + 
                theme_light() +
                theme(axis.title = element_text(size=14),
                      axis.text.x = element_text(size=14),
                      axis.text.y = element_text(size=14),
                      title = element_text(size=14), 
                      legend.title = element_text(size=14),
                      legend.text = element_text(size=14),
                      legend.key.size = unit(0.5, "cm"),
                      axis.line = element_line(colour = "black"),
                      panel.border = element_rect(colour = "black", fill=NA, size=1),
                      legend.position = c(0.20, 0.15)) 
        
}

#' Difference plot 
#'
#' @description This function is used to create a visualiztion of the change in native cover for QAQC
#'
#' @param data.in Shapefile of watershed boundaries and associated native cover scores.
#' @param habitat Defines 
#' @param title Plot title
#'
#' @export
#'
#'

difference_plot <- function(data.in, habitat, title) {
        
        # Define the max value so everything is center properly
        max.value <- max(abs(as.numeric(as.data.frame(data.in)[, habitat])))
        
        ggplot() + 
                geom_sf(data = data.in, aes_string(fill = habitat), show.legend = TRUE) +
                scale_fill_gradientn(name = paste0("Percent Change (%)"), 
                                     colors = met.brewer(name = "Cassatt2", n = 100, type = "continuous"), 
                                     guide = "colourbar", 
                                     limits = c(-1 * max.value, max.value)) +
                ggtitle(title) + 
                theme_light() +
                theme(axis.title = element_text(size=14),
                      axis.text.x = element_text(size=14),
                      axis.text.y = element_text(size=14),
                      title = element_text(size=14), 
                      legend.title = element_text(size=14),
                      legend.text = element_text(size=14),
                      legend.key.size = unit(0.5, "cm"),
                      axis.line = element_line(colour = "black"),
                      panel.border = element_rect(colour = "black", fill=NA, size=1),
                      legend.position = c(0.20, 0.15)) 
}

#' Trend plot 
#'
#' @description This function is used to create a visualiztion of the trend in native cover
#'
#' @param data.in Shapefile of watershed boundaries and associated native cover scores.
#' @param x Focal initial year (e.g., 2010) 
#' @param y Focal end year (e.g., 2022) 
#' @param title Plot title
#'
#' @export
#'

trend_plot <- function(data.in, x, y, title) {
        
        ggplot(data = data.in, aes_string(x = paste0("NC_", x), y = paste0("NC_", y))) + 
                geom_point() +
                ggtitle(title) + 
                geom_abline(slope = 1) +
                ylim(c(0,100)) +
                xlim(c(0,100)) +
                xlab(paste0("Native Cover ", x, " (%)")) +
                ylab(paste0("Native Cover ", y, " (%)")) +
                theme_light() +
                theme(axis.title = element_text(size=14))
}

