#
# Title: Visualization of "terrestrial" and "aquatic" native cover
# Created: June 6th, 2022
# Last Updated: June 6th, 2022
# Author: Brandon Allen
# Objectives: Visualize the two native cover indicators
# Keywords: Notes, Native Cover ALPHA, Native Cover Combo
#

#########
# Notes #
#########~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# 1) This analysis is run for each HUC 8 across the province.
# 2) We are using the ABMI Alberta Wetland Inventory. It is assumed areas not delineated with "aquatic" habitats are upland
#
######################
# Native Cover ALPHA # 
######################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Clear memory
rm(list=ls())
gc()

# Load libraries
library(abmi.themes)
library(ggplot2)
library(MetBrewer)
library(sf)

watershed.layer <- read_sf("data/processed/2018/native_cover_2018.shp")

#
# Terrestrial
#

png(paste0("results/figures/terrestrial-native-cover-HFI2018.png"),
    width = 1800,
    height = 2400, 
    res = 300)

ggplot() + 
        geom_sf(data = watershed.layer, aes(fill = UpCov), show.legend = TRUE) +
        scale_fill_gradientn(name = paste0("Native Cover (%)"), colors = met.brewer(name = "Hiroshige", n = 100, type = "continuous"), guide = "colourbar") +
        ggtitle(paste0("Terrestrial Native Cover")) + 
        theme_light() +
        theme_abmi(font = "Montserrat") +
        theme(axis.title = element_text(size=12),
              axis.text.x = element_text(size=12),
              axis.text.y = element_text(size=12),
              title = element_text(size=12), 
              legend.title = element_text(size=12),
              legend.text = element_text(size=12),
              legend.key.size = unit(0.5, "cm"),
              axis.line = element_line(colour = "black"),
              panel.border = element_rect(colour = "black", fill=NA, size=1),
              legend.position = c(0.20, 0.15)) 

dev.off()

#
# Aquatic
#

png(paste0("results/figures/aquatic-native-cover-HFI2018.png"),
    width = 1800,
    height = 2400, 
    res = 300)

ggplot() + 
        geom_sf(data = watershed.layer, aes(fill = LowCov), show.legend = TRUE) +
        scale_fill_gradientn(name = paste0("Native Cover (%)"), colors = met.brewer(name = "Hiroshige", n = 100, type = "continuous"), guide = "colourbar") +
        ggtitle(paste0("Aquatic Native Cover")) + 
        theme_light() +
        theme_abmi(font = "Montserrat") +
        theme(axis.title = element_text(size=12),
              axis.text.x = element_text(size=12),
              axis.text.y = element_text(size=12),
              title = element_text(size=12), 
              legend.title = element_text(size=12),
              legend.text = element_text(size=12),
              legend.key.size = unit(0.5, "cm"),
              axis.line = element_line(colour = "black"),
              panel.border = element_rect(colour = "black", fill=NA, size=1),
              legend.position = c(0.20, 0.15)) 

dev.off()

######################
# Native Cover Combo # 
######################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Clear memory
rm(list=ls())
gc()

# Load libraries
library(abmi.themes)
library(ggplot2)
library(MetBrewer)
library(sf)

watershed.layer <- read_sf("data/processed/2018/native_cover_combo_2018.shp")

#
# Terrestrial
#

png(paste0("results/figures/terrestrial-native-cover-combo-HFI2018.png"),
    width = 1800,
    height = 2400, 
    res = 300)

ggplot() + 
        geom_sf(data = watershed.layer, aes(fill = UpCov), show.legend = TRUE) +
        scale_fill_gradientn(name = paste0("Native Cover (%)"), colors = met.brewer(name = "Hiroshige", n = 100, type = "continuous"), guide = "colourbar") +
        ggtitle(paste0("Terrestrial Native Cover")) + 
        theme_light() +
        theme_abmi(font = "Montserrat") +
        theme(axis.title = element_text(size=12),
              axis.text.x = element_text(size=12),
              axis.text.y = element_text(size=12),
              title = element_text(size=12), 
              legend.title = element_text(size=12),
              legend.text = element_text(size=12),
              legend.key.size = unit(0.5, "cm"),
              axis.line = element_line(colour = "black"),
              panel.border = element_rect(colour = "black", fill=NA, size=1),
              legend.position = c(0.20, 0.15)) 

dev.off()

#
# Aquatic
#

png(paste0("results/figures/aquatic-native-cover-combo-HFI2018.png"),
    width = 1800,
    height = 2400, 
    res = 300)

ggplot() + 
        geom_sf(data = watershed.layer, aes(fill = LowCov), show.legend = TRUE) +
        scale_fill_gradientn(name = paste0("Native Cover (%)"), colors = met.brewer(name = "Hiroshige", n = 100, type = "continuous"), guide = "colourbar") +
        ggtitle(paste0("Aquatic Native Cover")) + 
        theme_light() +
        theme_abmi(font = "Montserrat") +
        theme(axis.title = element_text(size=12),
              axis.text.x = element_text(size=12),
              axis.text.y = element_text(size=12),
              title = element_text(size=12), 
              legend.title = element_text(size=12),
              legend.text = element_text(size=12),
              legend.key.size = unit(0.5, "cm"),
              axis.line = element_line(colour = "black"),
              panel.border = element_rect(colour = "black", fill=NA, size=1),
              legend.position = c(0.20, 0.15)) 

dev.off()

#
# Correlation of the two 
#

ggplot(data = watershed.layer.combo, aes(x = UpCov, y = LowCov)) +
        geom_point() +
        geom_smooth() +
        xlab("Upland Cover") +
        ylab("Lowland Cover") +
        theme_light()
