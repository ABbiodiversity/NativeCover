# ---
# title: "Visualization of "terrestrial" and "aquatic" native cover
# author: "Brandon Allen"
# created: "2023-09-18"
# inputs: ["3_output/bsc/native cover indicator per year"]
# outputs: ["3_output/figures/summary figures"]
# notes: 
#   "This uses the outputs from the 04_packaging script to generate figures for reporting"
# ---

# 1.0 Initializing environment ----
# This section handles defining the input data 
# and required functions.

# Clear memory
rm(list=ls())
gc()

# Load libraries
library(ggplot2)
library(ggpubr)
library(MetBrewer)
library(sf)

# Source scripts
source("1_code/r_scripts/visualization_functions.R")

# Load the results
native.cover.2010 <- read_sf("3_output/reporting/bsc/native-cover_HFI2010.shp")
native.cover.2018 <- read_sf("3_output/reporting/bsc/native-cover_HFI2018.shp")
native.cover.2019 <- read_sf("3_output/reporting/bsc/native-cover_HFI2019.shp")
native.cover.2020 <- read_sf("3_output/reporting/bsc/native-cover_HFI2020.shp")
native.cover.2021 <- read_sf("3_output/reporting/bsc/native-cover_HFI2021.shp")
native.cover.2022 <- read_sf("3_output/reporting/bsc/native-cover_HFI2022.shp")
native.cover.2023 <- read_sf("3_output/reporting/bsc/native-cover_HFI2023.shp")

# Create the trend results
terrestrial.trend <- data.frame(NC_2010 = native.cover.2010$TNC,
                                NC_2023 = native.cover.2023$TNC,
                                Difference = native.cover.2023$TNC - native.cover.2010$TNC)

aquatic.trend <- data.frame(NC_2010 = native.cover.2010$AWNC,
                            NC_2023 = native.cover.2023$AWNC,
                            Difference = native.cover.2023$AWNC - native.cover.2010$AWNC)

# Calculate the differences
native.cover.2023$UpDiff <- native.cover.2023$TNC - native.cover.2010$TNC
native.cover.2023$LowDiff <- native.cover.2023$AWNC - native.cover.2010$AWNC

# 2.0 Visualize Terrestrial Native Cover ----
terrestrial.2010 <- nc_plot(data.in = native.cover.2010, 
                            habitat = "TNC", 
                            title = "TNC 2010")

terrestrial.2018 <- nc_plot(data.in = native.cover.2018, 
                            habitat = "TNC", 
                            title = "TNC 2018")

terrestrial.2019 <- nc_plot(data.in = native.cover.2019, 
                            habitat = "TNC", 
                            title = "TNC 2019")

terrestrial.2020 <- nc_plot(data.in = native.cover.2020, 
                            habitat = "TNC", 
                            title = "TNC 2020")

terrestrial.2021 <- nc_plot(data.in = native.cover.2021, 
                            habitat = "TNC", 
                            title = "TNC 2021")

terrestrial.2022 <- nc_plot(data.in = native.cover.2022, 
                            habitat = "TNC", 
                            title = "TNC 2022")

terrestrial.2023 <- nc_plot(data.in = native.cover.2023, 
                            habitat = "TNC", 
                            title = "TNC 2023")

ggsave(filename = "3_output/figures/indicator/terrestrial-native-cover.png",
       plot = ggarrange(terrestrial.2010, terrestrial.2018,
                        terrestrial.2019, terrestrial.2020, 
                        terrestrial.2021, terrestrial.2022,
                        ncol = 3, nrow = 2),
       height = 2400,
       width = 2700,
       dpi = 100,
       units = "px")

terrestrial.2010.2023 <- difference_plot(data.in = native.cover.2023, 
                                         habitat = "UpDiff", 
                                         title = "")

ggsave(filename = "3_output/figures/support/terrestrial-native-cover-trend-spatial.png",
       plot = terrestrial.2010.2023,
       height = 1200,
       width = 900,
       dpi = 100,
       units = "px")

trend.plot <- trend_plot(data.in = terrestrial.trend, 
                         x = "2010", 
                         y = "2023", 
                         title = "Terrestrial Native Cover")


ggsave(filename = "3_output/figures/support/terrestrial-native-cover-trend.png",
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
              legend.text = element_text(size=14))

ggsave(filename = "3_output/figures/support/terrestrial-native-cover-histogram.png",
       plot = trend.histogram,
       height = 800,
       width = 800,
       dpi = 100,
       units = "px")

# 3.0 Visualize Aquatic and Wetland Native Cover ----
aquatic.2010 <- nc_plot(data.in = native.cover.2010, 
                            habitat = "AWNC", 
                            title = "AWNC 2010")

aquatic.2018 <- nc_plot(data.in = native.cover.2018, 
                            habitat = "AWNC", 
                            title = "AWNC 2018")

aquatic.2019 <- nc_plot(data.in = native.cover.2019, 
                            habitat = "AWNC", 
                            title = "AWNC 2019")

aquatic.2020 <- nc_plot(data.in = native.cover.2020, 
                            habitat = "AWNC", 
                            title = "AWNC 2020")

aquatic.2021 <- nc_plot(data.in = native.cover.2021, 
                        habitat = "AWNC", 
                        title = "AWNC 2021")

aquatic.2022 <- nc_plot(data.in = native.cover.2022, 
                        habitat = "AWNC", 
                        title = "AWNC 2022")

aquatic.2023 <- nc_plot(data.in = native.cover.2023, 
                        habitat = "AWNC", 
                        title = "AWNC 2023")

ggsave(filename = "3_output/figures/indicator/aquatic-native-cover.png",
       plot = ggarrange(aquatic.2010, aquatic.2018,
                        aquatic.2019, aquatic.2020, 
                        aquatic.2021, aquatic.2022,
                        ncol = 3, nrow = 2),
       height = 2400,
       width = 2700,
       dpi = 100,
       units = "px")

aquatic.2010.2023 <- difference_plot(data.in = native.cover.2023, 
                                         habitat = "LowDiff", 
                                         title = "")

ggsave(filename = "3_output/figures/support/aquatic-native-cover-trend-spatial.png",
       plot = aquatic.2010.2023,
       height = 1200,
       width = 900,
       dpi = 100,
       units = "px")

trend.plot <- trend_plot(data.in = aquatic.trend, 
                         x = "2010", 
                         y = "2023", 
                         title = "Aquatic Native Cover")

ggsave(filename = "3_output/figures/support/aquatic-native-cover-trend.png",
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
        theme(axis.title = element_text(size=14))


ggsave(filename = "3_output/figures/support/aquatic-native-cover-histogram.png",
       plot = trend.histogram,
       height = 800,
       width = 800,
       dpi = 100,
       units = "px")

# 4.0 Comparison of TNC and AWNC ----
# Calculate the differences
native.cover.2023$UpDiff <- native.cover.2023$TNC - native.cover.2010$TNC
native.cover.2023$LowDiff <- native.cover.2023$AWNC - native.cover.2010$AWNC

aquatic.vs.terrestiral <- ggplot(data = native.cover.2023, aes(x = UpDiff, y = LowDiff)) + 
        geom_point() +
        geom_abline(slope = 1) +
        xlab("TNC Percent Change 2010-2022 (%)") +
        ylab("AWNC Percent Change 2010-2022 (%)") +
        ylim(c(-15,5)) +
        xlim(c(-15,5)) +
        theme_light() +
        theme(axis.title = element_text(size=16, face = "bold"),
              axis.text.x = element_text(size=16), 
              axis.text.y = element_text(size=16), 
              panel.border = element_rect(color = "black",
                                          fill = NA,
                                          size = 1))

ggsave(filename = "3_output/figures/support/aquatic-terrestrial-2010-2023.png",
       plot = aquatic.vs.terrestiral,
       height = 1000,
       width = 1000,
       dpi = 100,
       units = "px")

rm(list=ls())
gc()

# 5.0 Visualize Forest Recovery Curves ----
# Load data
load("0_data/external/lookup/harvest-recovery-curves_80years.Rdata")

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


ggsave(filename = "3_output/figures/support/forest-recovery-curves.png",
       plot = recovery.curve,
       height = 800,
       width = 1200,
       dpi = 100,
       units = "px")

rm(list=ls())
gc()

# 6.0 Visualize comparison of recovery curve implementation ----
# Load final results and previous iteration
native.cover.2018 <- read_sf("3_output/reporting/bsc/native-cover_HFI2018.shp")
native.cover.2018.old <- read_sf("beta/backfil-recovery-curve/native_cover_HFI2018.shp")

# Create the trend results
terrestrial.trend <- data.frame(Simplified = native.cover.2018$TNC,
                                Backfill = native.cover.2018.old$UpCov)

aquatic.trend <- data.frame(Simplified = native.cover.2018$AWNC,
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

ggsave(filename = "3_output/figures/support/terrestrial-recovery-method-comparison-2018.png",
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

ggsave(filename = "3_output/figures/support/aquatic-recovery-method-comparison-2018.png",
       plot = aquatic.figure,
       height = 800,
       width = 800,
       dpi = 100,
       units = "px")

rm(list=ls())
gc()

# 7.0 Visualize the reference boundaries for the two indicators ----
# Load base data
aquatic.reference <- read_sf(dsn = "0_data/external/boundaries/reference_boundary.gdb/",
                             layer = "aquatic_reference")
terrestrial.reference <- read_sf(dsn = "0_data/external/boundaries/reference_boundary.gdb/",
                                 layer = "terrestrial_reference")

aquatic.reference$Habitat <- "Aquatic"
terrestrial.reference$Habitat <- "Terrestrial"

# Merge the two
reference.condition <- rbind(aquatic.reference, terrestrial.reference[, colnames(aquatic.reference)])

aquatic.upland.boundary <- ggplot() + 
        geom_sf(data = reference.condition, aes(fill = Habitat, color = Habitat), size = 0, show.legend = FALSE) +
        scale_color_manual(values = c(alpha("#2C6582", 0), alpha("#F9D14A", 0))) +
        scale_fill_manual(values = c("#2C6582", alpha("#F9D14A", 0.2))) +
        scale_size_identity() +
        theme_light() +
        theme(axis.title = element_text(size=14),
              axis.text.x = element_text(size=14),
              axis.text.y = element_text(size=14),
              title = element_text(size=14),
              axis.line = element_line(colour = "black"),
              panel.border = element_rect(colour = "black", fill=NA, size=1))

ggsave(filename = "3_output/figures/indicator/aquatic-riparian-upland-boundary.png",
       plot = aquatic.upland.boundary,
       height = 2400,
       width = 1600,
       dpi = 120,
       units = "px")

rm(list=ls())
gc()