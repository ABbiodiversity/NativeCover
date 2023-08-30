#
# Title: Visualization of "terrestrial" and "aquatic" native cover
# Created: June 6th, 2022
# Last Updated: August 30th, 2023
# Author: Brandon Allen
# Objectives: Visualize the two native cover indicators
# Keywords: Notes, Visualization, Forest recovery, Simple versus complex recovery
#

#########
# Notes #
#########~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# 1) This analysis is run for each HUC 8 across the province.
# 2) We are using the ABMI Alberta Wetland Inventory. Areas not delineated as "aquatic" habitats are upland
# 3) Areas that fall within the Lotic Riparian layer are also delineated as "aquatic"
#
#################
# Visualization # 
#################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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
native.cover.2021 <- read_sf("results/gis/2021/native_cover_HFI2021.shp")

# Create the trend results
terrestrial.trend <- data.frame(NC_2010 = native.cover.2010$UpCov,
                                NC_2021 = native.cover.2021$UpCov,
                                Difference = native.cover.2021$UpCov - native.cover.2010$UpCov)

aquatic.trend <- data.frame(NC_2010 = native.cover.2010$LowCov,
                            NC_2021 = native.cover.2021$LowCov,
                            Difference = native.cover.2021$LowCov - native.cover.2010$LowCov)

# Calculate the differences
native.cover.2021$UpDiff <- native.cover.2021$UpCov - native.cover.2010$UpCov
native.cover.2021$LowDiff <- native.cover.2021$LowCov - native.cover.2010$LowCov

###############
# Terrestrial #
###############

terrestrial.2010 <- nc_plot(data.in = native.cover.2010, 
                            habitat = "UpCov", 
                            title = "TNC 2010")

terrestrial.2018 <- nc_plot(data.in = native.cover.2018, 
                            habitat = "UpCov", 
                            title = "TNC 2018")

terrestrial.2019 <- nc_plot(data.in = native.cover.2019, 
                            habitat = "UpCov", 
                            title = "TNC 2019")

terrestrial.2020 <- nc_plot(data.in = native.cover.2020, 
                            habitat = "UpCov", 
                            title = "TNC 2020")

terrestrial.2021 <- nc_plot(data.in = native.cover.2021, 
                            habitat = "UpCov", 
                            title = "TNC 2021")

ggsave(filename = "results/figures/indicator/terrestrial-native-cover.png",
       plot = ggarrange(terrestrial.2010, terrestrial.2018,
                        terrestrial.2019, terrestrial.2020, terrestrial.2021,
                        ncol = 3, nrow = 2),
       height = 2400,
       width = 2700,
       dpi = 100,
       units = "px")

terrestrial.2010.2021 <- difference_plot(data.in = native.cover.2021, 
                                         habitat = "UpDiff", 
                                         title = "")

ggsave(filename = "results/figures/support/terrestrial-native-cover-trend-spatial.png",
       plot = terrestrial.2010.2021,
       height = 1200,
       width = 900,
       dpi = 100,
       units = "px")

trend.plot <- trend_plot(data.in = terrestrial.trend, 
                         x = "2010", 
                         y = "2021", 
                         title = "Terrestrial Native Cover")


ggsave(filename = "results/figures/support/terrestrial-native-cover-trend.png",
       plot = trend.plot,
       height = 800,
       width = 800,
       dpi = 100,
       units = "px")

trend.histogram <- ggplot(data = terrestrial.trend, aes(x = Difference, col = "#004f63", fill = "#004f63")) + 
        geom_histogram(bins = 100, show.legend = FALSE) +
        scale_color_manual(values = "#004f63") +
        scale_fill_manual(values = "#004f63") +
        ggtitle(paste0("Terrestrial Native Cover")) + 
        xlab("Percent Change (%)") +
        ylab("Frequency") +
        geom_vline(xintercept = 0) +
        xlim(c(-15,5)) +
        theme_light() +
        theme(axis.title = element_text(size=14),
              legend.title = element_text(size=14),
              legend.text = element_text(size=14)) +
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
                            habitat = "LowCov", 
                            title = "AWNC 2010")

aquatic.2018 <- nc_plot(data.in = native.cover.2018, 
                            habitat = "LowCov", 
                            title = "AWNC 2018")

aquatic.2019 <- nc_plot(data.in = native.cover.2019, 
                            habitat = "LowCov", 
                            title = "AWNC 2019")

aquatic.2020 <- nc_plot(data.in = native.cover.2020, 
                            habitat = "LowCov", 
                            title = "AWNC 2020")

aquatic.2021 <- nc_plot(data.in = native.cover.2021, 
                        habitat = "LowCov", 
                        title = "AWNC 2021")

ggsave(filename = "results/figures/indicator/aquatic-native-cover.png",
       plot = ggarrange(aquatic.2010, aquatic.2018,
                        aquatic.2019, aquatic.2020, aquatic.2021,
                        ncol = 3, nrow = 2),
       height = 2400,
       width = 2700,
       dpi = 100,
       units = "px")

aquatic.2010.2021 <- difference_plot(data.in = native.cover.2021, 
                                         habitat = "LowDiff", 
                                         title = "")

ggsave(filename = "results/figures/support/aquatic-native-cover-trend-spatial.png",
       plot = aquatic.2010.2021,
       height = 1200,
       width = 900,
       dpi = 100,
       units = "px")

trend.plot <- trend_plot(data.in = aquatic.trend, 
                         x = "2010", 
                         y = "2021", 
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
        theme(axis.title = element_text(size=14)) +
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

# Calculate the differences
native.cover.2021$UpDiff <- native.cover.2021$UpCov - native.cover.2010$UpCov
native.cover.2021$LowDiff <- native.cover.2021$LowCov - native.cover.2010$LowCov

aquatic.vs.terrestiral <- ggplot(data = native.cover.2021, aes(x = UpDiff, y = LowDiff)) + 
        geom_point() +
        geom_abline(slope = 1) +
        xlab("TNC Percent Change 2010-2021 (%)") +
        ylab("AWNC Percent Change 2010-2021 (%)") +
        ylim(c(-15,5)) +
        xlim(c(-15,5)) +
        theme_light() +
        theme(axis.title = element_text(size=12)) +
        theme_abmi(font = "Montserrat")

ggsave(filename = "results/figures/support/aquatic-terrestrial-2010-2021.png",
       plot = aquatic.vs.terrestiral,
       height = 800,
       width = 800,
       dpi = 100,
       units = "px")

rm(list=ls())
gc()

###################
# Forest Recovery # 
###################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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

# We are removing the 80 year class as looks weird visually. Can simply say "After 80 years, recovery is assumed 100%".
harvest.recovery <- harvest.recovery[harvest.recovery$Age != 80, ]

recovery.curve <- ggplot(data = harvest.recovery, aes(x = Age, y = Recovery, fill = Stand, col = Stand)) + 
        geom_point() +
        scale_color_manual(name = "Stand Type",values = met.brewer(name = "Egypt", n = 2, type = "discrete")) +
        scale_fill_manual(name = "Stand Type", values = met.brewer(name = "Egypt", n = 2, type = "discrete")) +
        xlab("Age (Years)") +
        ylab("Stand Recovery (%)") +
        ylim(c(0,100)) +
        xlim(c(0,80)) +
        theme_light() +
        theme(axis.title = element_text(size=16, face = "bold"),
              title = element_text(size=16, face = "bold"),
              axis.text = element_text(size=16),
              legend.title = element_text(size=16),
              legend.text = element_text(size=16), 
              panel.border = element_rect(color = "black",
                                          fill = NA,
                                          size = 1),
              panel.grid.minor = element_blank(),
              panel.grid.major = element_blank())


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
##################################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Clear memory
rm(list=ls())
gc()

# Load libraries
library(abmi.themes)
library(ggplot2)
library(MetBrewer)

# Load final results and previous iteration
native.cover.2018 <- read_sf("results/gis/2018/native_cover_HFI2018.shp")
native.cover.2018.old <- read_sf("beta/backfil-recovery-curve/native_cover_HFI2018.shp")

# Create the trend results
terrestrial.trend <- data.frame(Simplified = native.cover.2018$UpCov,
                                Backfill = native.cover.2018.old$UpCov)

aquatic.trend <- data.frame(Simplified = native.cover.2018$LowCov,
                            Backfill = native.cover.2018.old$LowCov)

terrestrial.figure <- ggplot(data = terrestrial.trend, aes(x = Backfill, y = Simplified)) + 
        geom_point() +
        geom_abline(slope = 1) +
        xlab("TNC by Complex Recovery Curves (%)") +
        ylab("TNC by Simplified Recovery Curves (%)") +
        ylim(c(0,100)) +
        xlim(c(0,100)) +
        theme_light() +
        theme(axis.title = element_text(size=16, face = "bold"),
              title = element_text(size = 16, face = "bold"),
              axis.text = element_text(size=16),
              legend.title = element_text(size=16),
              legend.text = element_text(size=16), 
              panel.grid.minor = element_blank(),
              panel.grid.major = element_blank(),
              panel.border = element_rect(color = "black",
                                          fill = NA,
                                          size = 1))



ggsave(filename = "results/figures/support/terrestrial-recovery-method-comparison-2018.png",
       plot = terrestrial.figure,
       height = 800,
       width = 800,
       dpi = 100,
       units = "px")

aquatic.figure <- ggplot(data = aquatic.trend, aes(x = Backfill, y = Simplified)) + 
        geom_point() +
        geom_abline(slope = 1) +
        xlab("AWNC by Complex Recovery Curves (%)") +
        ylab("AWNC by Simplified Recovery Curves (%)") +
        ylim(c(0,100)) +
        xlim(c(0,100)) +
        theme_light() +
        theme(axis.title = element_text(size=16, face = "bold"),
              title = element_text(size = 16, face = "bold"),
              axis.text = element_text(size=16),
              legend.title = element_text(size=16),
              legend.text = element_text(size=16), 
              panel.grid.minor = element_blank(),
              panel.grid.major = element_blank(),
              panel.border = element_rect(color = "black",
                                          fill = NA,
                                          size = 1))

ggsave(filename = "results/figures/support/aquatic-recovery-method-comparison-2018.png",
       plot = aquatic.figure,
       height = 800,
       width = 800,
       dpi = 100,
       units = "px")

rm(list=ls())
gc()


