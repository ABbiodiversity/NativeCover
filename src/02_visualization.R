#
# Title: Visualization of "terrestrial" and "aquatic" native cover
# Created: June 6th, 2022
# Last Updated: October 11th, 2022
# Author: Brandon Allen
# Objectives: Visualize the two native cover indicators
# Keywords: Notes, Visualization
#

#########
# Notes #
#########~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# 1) This analysis is run for each HUC 8 across the province.
# 2) We are using the ABMI Alberta Wetland Inventory. Areas not delineated as "aquatic" habitats are upland
# 3) Areas that fall within the Lotic Riparian layer are also delineated as "aquatic"
#
#################
# Visualization # 
#################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Clear memory
rm(list=ls())
gc()

# Load libraries
library(abmi.themes)
library(ggplot2)
library(MetBrewer)
library(sf)

# Load the results
native.cover.2010 <- read_sf("data/processed/2010/native_cover_HFI2010.shp")
native.cover.2018 <- read_sf("data/processed/2018/native_cover_HFI2018.shp")

#
# Terrestrial
#

png(paste0("results/figures/terrestrial-native-cover-HFI2010.png"),
    width = 1800,
    height = 2400, 
    res = 300)

ggplot() + 
        geom_sf(data = native.cover.2010, aes(fill = UpCov), show.legend = TRUE) +
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

png(paste0("results/figures/terrestrial-native-cover-HFI2018.png"),
    width = 1800,
    height = 2400, 
    res = 300)

ggplot() + 
        geom_sf(data = native.cover.2018, aes(fill = UpCov), show.legend = TRUE) +
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

terrestrial.trend <- data.frame(NC_2010 = native.cover.2010$UpCov,
                                NC_2018 = native.cover.2018$UpCov)

png(paste0("results/figures/terrestrial-native-cover-trend.png"),
    width = 1800,
    height = 1800, 
    res = 300)

ggplot(data = terrestrial.trend, aes(x = NC_2010, y = NC_2018)) + 
        geom_point() +
        ggtitle(paste0("Terrestrial Native Cover")) + 
        geom_abline(slope = 1) +
        ylim(c(0,100)) +
        xlim(c(0,100)) +
        xlab("Native Cover 2010") +
        ylab("Native Cover 2018") +
        theme_light() +
        theme_abmi(font = "Montserrat")

dev.off()

#
# Aquatic
#

png(paste0("results/figures/aquatic-native-cover-HFI2010.png"),
    width = 1800,
    height = 2400, 
    res = 300)

ggplot() + 
        geom_sf(data = native.cover.2010, aes(fill = LowCov), show.legend = TRUE) +
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

png(paste0("results/figures/aquatic-native-cover-HFI2018.png"),
    width = 1800,
    height = 2400, 
    res = 300)

ggplot() + 
        geom_sf(data = native.cover.2018, aes(fill = LowCov), show.legend = TRUE) +
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

aquatic.trend <- data.frame(NC_2010 = native.cover.2010$LowCov,
                                NC_2018 = native.cover.2018$LowCov)

png(paste0("results/figures/aquatic-native-cover-trend.png"),
    width = 1800,
    height = 1800, 
    res = 300)

ggplot(data = aquatic.trend, aes(x = NC_2010, y = NC_2018)) + 
        geom_point() +
        ggtitle(paste0("Aquatic Native Cover")) + 
        geom_abline(slope = 1) +
        ylim(c(0,100)) +
        xlim(c(0,100)) +
        xlab("Native Cover 2010") +
        ylab("Native Cover 2018") +
        theme_light() +
        theme_abmi(font = "Montserrat")

dev.off()

#
# Aquatic versus Terrestrial comparison
#

png(paste0("results/figures/aquatic-terrestrial-2018.png"),
    width = 1800,
    height = 1800, 
    res = 300)

ggplot(data = native.cover.2018, aes(x = LowCov, y = UpCov)) + 
        geom_point() +
        ggtitle(paste0("Aquatic vs Terrestrial Cover")) + 
        geom_abline(slope = 1) +
        xlab("Aquatic Native Cover") +
        ylab("Terrestrial Native Cover") +
        ylim(c(0,100)) +
        xlim(c(0,100)) +
        theme_light() +
        theme_abmi(font = "Montserrat")

dev.off()

# Clear memory
rm(list=ls())
gc()
