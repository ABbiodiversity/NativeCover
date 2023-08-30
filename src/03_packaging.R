#
# Title: Packaging of "terrestrial" and "aquatic" native cover
# Created: August 30th, 2023
# Last Updated: August 30th, 2023
# Author: Brandon Allen
# Objectives: Packaging of the results 
# Keywords: Notes, Packaging
#

#########
# Notes #
#########~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# 1) This analysis is run for each HUC 8 across the province.
# 2) We are using the ABMI Alberta Wetland Inventory. Areas not delineated as "aquatic" habitats are upland
# 3) Areas that fall within the Lotic Riparian layer are also delineated as "aquatic"
#
#############
# Packaging # 
#############~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Clear memory
rm(list=ls())
gc()

# Load libraries
library(sf)

# Load the results
native.cover.2010 <- read_sf("results/gis/2010/native_cover_HFI2010.shp")
native.cover.2018 <- read_sf("results/gis/2018/native_cover_HFI2018.shp")
native.cover.2019 <- read_sf("results/gis/2019/native_cover_HFI2019.shp")
native.cover.2020 <- read_sf("results/gis/2020/native_cover_HFI2020.shp")
native.cover.2021 <- read_sf("results/gis/2021/native_cover_HFI2021.shp")

# Standardize the names
native.cover <- native.cover.2010[, c("HUC_2", "HUC_4", "HUC_6", "HUC_8")]

# Add TNC
native.cover$TNC2010 <- native.cover.2010$UpCov
native.cover$TNC2018 <- native.cover.2018$UpCov
native.cover$TNC2019 <- native.cover.2019$UpCov
native.cover$TNC2020 <- native.cover.2020$UpCov
native.cover$TNC2021 <- native.cover.2021$UpCov

# Add AWNC
native.cover$AWNC2010 <- native.cover.2010$LowCov
native.cover$AWNC2018 <- native.cover.2018$LowCov
native.cover$AWNC2019 <- native.cover.2019$LowCov
native.cover$AWNC2020 <- native.cover.2020$LowCov
native.cover$AWNC2021 <- native.cover.2021$LowCov

# Save
write_sf(native.cover, file = "results/gis/native_cover_2010_2021.shp")
