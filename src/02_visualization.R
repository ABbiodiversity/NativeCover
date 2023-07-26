#
# Title: Visualization of "terrestrial" and "aquatic" native cover
# Created: June 6th, 2022
# Last Updated: July 26th, 2023
# Author: Brandon Allen
# Objectives: Visualize the two native cover indicators
# Keywords: Notes, Visualization, Forest recovery, Simple versus complex recovery
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
library(ggpubr)
library(MetBrewer)
library(sf)

# Source scripts
source("src/visualization_functions.R")

# Load the results
native.cover.2010 <- read_sf("results/gis/2010/native_cover_HFI2010.shp")
native.cover.2018 <- read_sf("results/gis/2018/native_cover_HFI2018.shp")
native.cover.2019 <- read_sf("results/gis/2019/native_cover_HFI2019.shp")
native.cover.2020 <- read_sf("results/gis/2020/native_cover_HFI2020.shp")

# Create the trend results
terrestrial.trend <- data.frame(NC_2010 = native.cover.2010$UpCov,
                                NC_2020 = native.cover.2020$UpCov,
                                Difference = native.cover.2020$UpCov - native.cover.2010$UpCov)

aquatic.trend <- data.frame(NC_2010 = native.cover.2010$LowCov,
                            NC_2020 = native.cover.2020$LowCov,
                            Difference = native.cover.2020$LowCov - native.cover.2010$LowCov)

###############
# Terrestrial #
###############

terrestrial.2010 <- nc_plot(data.in = native.cover.2010, 
                            habitat = "UpCov", 
                            title = "Terrestrial Native Cover (2010)")

terrestrial.2018 <- nc_plot(data.in = native.cover.2018, 
                            habitat = "UpCov", 
                            title = "Terrestrial Native Cover (2018)")

terrestrial.2019 <- nc_plot(data.in = native.cover.2019, 
                            habitat = "UpCov", 
                            title = "Terrestrial Native Cover (2019)")

terrestrial.2020 <- nc_plot(data.in = native.cover.2020, 
                            habitat = "UpCov", 
                            title = "Terrestrial Native Cover (2020)")

ggsave(filename = "results/figures/indicator/terrestrial-native-cover.png",
       plot = ggarrange(terrestrial.2010, terrestrial.2018,
                        terrestrial.2019, terrestrial.2020,
                        ncol = 2, nrow = 2),
       height = 2400,
       width = 1800,
       dpi = 100,
       units = "px")

trend.plot <- trend_plot(data.in = terrestrial.trend, 
                         x = "2010", 
                         y = "2020", 
                         title = "Terrestrial Native Cover")


ggsave(filename = "results/figures/support/terrestrial-native-cover-trend.png",
       plot = trend.plot,
       height = 800,
       width = 800,
       dpi = 100,
       units = "px")

trend.histogram <- ggplot(data = terrestrial.trend, aes(x = Difference, col = "#004f63", fill = "#004f63")) + 
        geom_histogram(bins = 50, show.legend = FALSE) +
        scale_color_manual(values = "#004f63") +
        scale_fill_manual(values = "#004f63") +
        ggtitle(paste0("Terrestrial Native Cover")) + 
        xlab("Percent Change (%)") +
        ylab("Frequency") +
        geom_vline(xintercept = 0) +
        xlim(c(-15,5)) +
        theme_light() +
        theme(axis.title = element_text(size=12),
              legend.title = element_text(size=12),
              legend.text = element_text(size=12)) +
        theme_abmi(font = "Montserrat")


ggsave(filename = "results/figures/support/terrestrial-native-cover-histogram.png",
       plot = trend.histogram,
       height = 800,
       width = 800,
       dpi = 100,
       units = "px")

###########
# Aquatic #
###########

aquatic.2010 <- nc_plot(data.in = native.cover.2010, 
                            habitat = "UpCov", 
                            title = "Aquatic Native Cover (2010)")

aquatic.2018 <- nc_plot(data.in = native.cover.2018, 
                            habitat = "UpCov", 
                            title = "Aquatic Native Cover (2018)")

aquatic.2019 <- nc_plot(data.in = native.cover.2019, 
                            habitat = "UpCov", 
                            title = "Aquatic Native Cover (2019)")

aquatic.2020 <- nc_plot(data.in = native.cover.2020, 
                            habitat = "UpCov", 
                            title = "Aquatic Native Cover (2020)")

ggsave(filename = "results/figures/indicator/aquatic-native-cover.png",
       plot = ggarrange(aquatic.2010, aquatic.2018,
                        aquatic.2019, aquatic.2020,
                        ncol = 2, nrow = 2),
       height = 2400,
       width = 1800,
       dpi = 100,
       units = "px")

trend.plot <- trend_plot(data.in = aquatic.trend, 
                         x = "2010", 
                         y = "2020", 
                         title = "Aquatic Native Cover")

ggsave(filename = "results/figures/support/aquatic-native-cover-trend.png",
       plot = trend.plot,
       height = 800,
       width = 800,
       dpi = 100,
       units = "px")

trend.histogram <- ggplot(data = aquatic.trend, aes(x = Difference, col = "#004f63", fill = "#004f63")) + 
        geom_histogram(bins = 50, show.legend = FALSE) +
        scale_color_manual(values = "#004f63") +
        scale_fill_manual(values = "#004f63") +
        ggtitle(paste0("Aquatic Native Cover")) + 
        xlab("Percent Change (%)") +
        ylab("Frequency") +
        geom_vline(xintercept = 0) +
        xlim(c(-15,5)) +
        theme_light() +
        theme(axis.title = element_text(size=12)) +
        theme_abmi(font = "Montserrat")


ggsave(filename = "results/figures/support/aquatic-native-cover-histogram.png",
       plot = trend.histogram,
       height = 800,
       width = 800,
       dpi = 100,
       units = "px")

#########################################
# Aquatic versus Terrestrial comparison #
#########################################

aquatic.vs.terrestiral <- ggplot(data = native.cover.2020, aes(x = LowCov, y = UpCov)) + 
        geom_point() +
        ggtitle(paste0("Aquatic vs Terrestrial Cover")) + 
        geom_abline(slope = 1) +
        xlab("Aquatic Native Cover (%)") +
        ylab("Terrestrial Native Cover (%)") +
        ylim(c(0,100)) +
        xlim(c(0,100)) +
        theme_light() +
        theme(axis.title = element_text(size=12)) +
        theme_abmi(font = "Montserrat")

ggsave(filename = "results/figures/support/aquatic-terrestrial-2020.png",
       plot = aquatic.vs.terrestiral,
       height = 800,
       width = 800,
       dpi = 100,
       units = "px")

rm(list=ls())
gc()

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

# Load data
load("data/lookup/harvest-recovery-curves_80years.Rdata")

# Standardize the data
harvest.recovery <- data.frame(Stand = c(rep("Deciduous", 81), 
                                         rep("Coniferous", 81)),
                               Age = c(recovery.curve$Age, 
                                       recovery.curve$Age),
                               Recovery = c(recovery.curve$Deciduous,
                                            recovery.curve$Coniferous))

recovery.curve <- ggplot(data = harvest.recovery, aes(x = Age, y = Recovery, fill = Stand, col = Stand)) + 
        geom_point() +
        scale_color_manual(values = met.brewer(name = "Egypt", n = 2, type = "discrete")) +
        scale_fill_manual(values = met.brewer(name = "Egypt", n = 2, type = "discrete")) +
        ggtitle(paste0("Forest Recovery Curves")) + 
        xlab("Age") +
        ylab("Stand Recovery (%)") +
        ylim(c(0,100)) +
        xlim(c(0,80)) +
        theme_light() +
        theme(axis.title = element_text(size=12),
              legend.title = element_text(size=12),
              legend.text = element_text(size=12)) +
        theme_abmi(font = "Montserrat")

ggsave(filename = "results/figures/support/forest-recovery-curves.png",
       plot = recovery.curve,
       height = 800,
       width = 1200,
       dpi = 100,
       units = "px")

rm(list=ls())
gc()

##################################
# Simple versus complex recovery #
##################################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Clear memory
rm(list=ls())
gc()

# Load libraries
library(abmi.themes)
library(ggplot2)
library(MetBrewer)

# Load final results and previous iteration
native.cover.2018 <- read_sf("results/gis/2018/native_cover_HFI2018.shp")
native.cover.2018.old <- read_sf("beta/backfil-recovery-curve//native_cover_HFI2018.shp")

# Create the trend results
terrestrial.trend <- data.frame(Simplified = native.cover.2018$UpCov,
                                Backfill = native.cover.2018.old$UpCov)

aquatic.trend <- data.frame(Simplified = native.cover.2018$LowCov,
                            Backfill = native.cover.2018.old$LowCov)

terrestrial.figure <- ggplot(data = terrestrial.trend, aes(x = Backfill, y = Simplified)) + 
        geom_point() +
        ggtitle(paste0("Terrestrial Native Cover (%)")) + 
        geom_abline(slope = 1) +
        xlab("Complex recovery curves") +
        ylab("Simplified recovery curves") +
        ylim(c(0,100)) +
        xlim(c(0,100)) +
        theme_light() +
        theme(axis.title = element_text(size=12)) +
        theme_abmi(font = "Montserrat")

ggsave(filename = "results/figures/support/terrestrial-recovery-method-comparison-2018.png",
       plot = terrestrial.figure,
       height = 800,
       width = 800,
       dpi = 100,
       units = "px")

aquatic.figure <- ggplot(data = aquatic.trend, aes(x = Backfill, y = Simplified)) + 
        geom_point() +
        ggtitle(paste0("Aquatic Native Cover (%)")) + 
        geom_abline(slope = 1) +
        xlab("Complex recovery curves") +
        ylab("Simplified recovery curves") +
        ylim(c(0,100)) +
        xlim(c(0,100)) +
        theme_light() +
        theme(axis.title = element_text(size=12)) +
        theme_abmi(font = "Montserrat")

ggsave(filename = "results/figures/support/aquatic-recovery-method-comparison-2018.png",
       plot = aquatic.figure,
       height = 800,
       width = 800,
       dpi = 100,
       units = "px")

rm(list=ls())
gc()


