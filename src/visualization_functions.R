#
# Title: Functions for visualizing native cover
# Created: October 19th, 2022
# Last Updated: July 26th, 2023
# Author: Brandon Allen
# Objectives: Define functions for visualizing native cover
# Keywords: Native cover, Trend
#


################
# Native cover # 
################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

nc_plot <- function(data.in, habitat, title) {
        
        ggplot() + 
                geom_sf(data = data.in, aes_string(fill = habitat), show.legend = TRUE) +
                scale_fill_gradientn(name = paste0("Native Cover (%)"), colors = met.brewer(name = "Hiroshige", n = 100, type = "continuous"), guide = "colourbar") +
                ggtitle(title) + 
                theme_light() +
                theme_abmi(font = "Montserrat") +
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

##############
# Difference # 
##############~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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
                theme_abmi(font = "Montserrat") +
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

#########
# Trend # 
#########~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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
                theme(axis.title = element_text(size=14)) +
                theme_abmi(font = "Montserrat")
}

