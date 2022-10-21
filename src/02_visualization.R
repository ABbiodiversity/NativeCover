#
# Title: Visualization of "terrestrial" and "aquatic" native cover
# Created: June 6th, 2022
# Last Updated: October 11th, 2022
# Author: Brandon Allen
# Objectives: Visualize the two native cover indicators
# Keywords: Notes, Visualization, Forest recovery
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

# Source scripts
source("src/visualization_functions.R")

# Load the results
native.cover.2010 <- read_sf("data/processed/2010/native_cover_HFI2010.shp")
native.cover.2018 <- read_sf("data/processed/2018/native_cover_HFI2018.shp")

# Create the trend results
terrestrial.trend <- data.frame(NC_2010 = native.cover.2010$UpCov,
                                NC_2018 = native.cover.2018$UpCov)

aquatic.trend <- data.frame(NC_2010 = native.cover.2010$LowCov,
                            NC_2018 = native.cover.2018$LowCov)

#
# Terrestrial
#

png(paste0("results/figures/terrestrial-native-cover-HFI2010.png"),
    width = 1800,
    height = 2400, 
    res = 300)

nc_plot(data.in = native.cover.2010, 
        habitat = "UpCov", 
        title = "Terrestrial Native Cover")

dev.off()

png(paste0("results/figures/terrestrial-native-cover-HFI2018.png"),
    width = 1800,
    height = 2400, 
    res = 300)

nc_plot(data.in = native.cover.2018, 
        habitat = "UpCov", 
        title = "Terrestrial Native Cover")

dev.off()

png(paste0("results/figures/terrestrial-native-cover-trend.png"),
    width = 1800,
    height = 1800, 
    res = 300)

trend_plot(data.in = terrestrial.trend, 
           x = "2010", 
           y = "2018", 
           title = "Terrestrial Native Cover")

dev.off()

#
# Aquatic
#

png(paste0("results/figures/aquatic-native-cover-HFI2010.png"),
    width = 1800,
    height = 2400, 
    res = 300)

nc_plot(data.in = native.cover.2010, 
        habitat = "LowCov", 
        title = "Aquatic Native Cover")

dev.off()

png(paste0("results/figures/aquatic-native-cover-HFI2018.png"),
    width = 1800,
    height = 2400, 
    res = 300)

nc_plot(data.in = native.cover.2018, 
        habitat = "LowCov", 
        title = "Aquatic Native Cover")

dev.off()

png(paste0("results/figures/aquatic-native-cover-trend.png"),
    width = 1800,
    height = 1800, 
    res = 300)

trend_plot(data.in = aquatic.trend, 
           x = "2010", 
           y = "2018", 
           title = "Aquatic Native Cover")

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

###################
# Forest Recovery # 
###################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Clear memory
rm(list=ls())
gc()

# Load libraries
library(abmi.themes)
library(ggplot2)
library(MetBrewer)

# Source scripts
source("src/visualization_functions.R")

# Load data
load("data/lookup/harvest-recovery.Rdata")

# Standardize the data
harvest.recovery <- data.frame(Stand = c(rep("Deciduous", 101), 
                                         rep("Coniferous", 101)),
                               Age = c(harvest.recovery$Age, 
                                       harvest.recovery$Age),
                               Recovery = c(harvest.recovery$Deciduous,
                                            harvest.recovery$Coniferous))

# Visualize
png(paste0("results/figures/forest-recovery-curves.png"),
    width = 2400,
    height = 1600, 
    res = 300)

ggplot(data = harvest.recovery, aes(x = Age, y = Recovery, fill = Stand, col = Stand)) + 
        geom_point() +
        scale_color_manual(values = met.brewer(name = "Egypt", n = 2, type = "discrete")) +
        scale_fill_manual(values = met.brewer(name = "Egypt", n = 2, type = "discrete")) +
        ggtitle(paste0("Forest Recovery Curves")) + 
        xlab("Age") +
        ylab("Stand Recovery (%)") +
        ylim(c(0,100)) +
        xlim(c(0,100)) +
        theme_light() +
        theme_abmi(font = "Montserrat")

dev.off()
