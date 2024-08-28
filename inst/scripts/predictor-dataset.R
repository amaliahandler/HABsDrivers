
# install.packages("lakemorpho")
# install.packages("elevatr")
# install.packages("raster")
# install_github("USEPA/StreamCatTools", build_vignettes=FALSE, auth_token= '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', force = TRUE)

library(devtools)
library(dplyr)
library(stars)
library(nhdplusTools)
library(tidyverse)
library(tidyr)
library(sf)
library(tigris)
library(StreamCatTools)
library(ggplot2)
library(spmodel)
library(elevatr)
library(lakemorpho)
library(raster)
library(corrplot)
library(remotes)
library(units)

# Load nutrient data pulled from Meredith Brehob and Robert Sabo
# 2007 and 2012 data

PredData07_05Ws <- read.csv("C:/Users/mreyno04/OneDrive - Environmental Protection Agency (EPA)/Profile/REPOS/LakePredData/PredData/PredData07_05Ws.csv")
PredData07_07Ws <- read.csv("C:/Users/mreyno04/OneDrive - Environmental Protection Agency (EPA)/Profile/REPOS/LakePredData/PredData/PredData07_05Ws.csv")
PredData07_10Ws <- read.csv("C:/Users/mreyno04/OneDrive - Environmental Protection Agency (EPA)/Profile/REPOS/LakePredData/PredData/PredData07_10Ws.csv")
PredData12_05Ws <- read.csv("C:/Users/mreyno04/OneDrive - Environmental Protection Agency (EPA)/Profile/REPOS/LakePredData/PredData/PredData12_05Ws.csv")
PredData12_07Ws <- read.csv("C:/Users/mreyno04/OneDrive - Environmental Protection Agency (EPA)/Profile/REPOS/LakePredData/PredData/PredData12_07Ws.csv")
PredData12_10Ws <- read.csv("C:/Users/mreyno04/OneDrive - Environmental Protection Agency (EPA)/Profile/REPOS/LakePredData/PredData/PredData12_10Ws.csv")

# Function set up --------------------------------------------------------------

# Ensure the files combined as intended, checking for the number of repeated COMIDs
verify <- function(df){
  print(which(df$wbCOMID == "487"))
}

# Visually cleaner way to bind rows, more reproducible
combines <- function(x, y, z){
  x %>%
    bind_rows(y) %>%
    bind_rows(z)
}

# Aggregating and averaging data across binded data sets
mean_group <- function(df){
  df %>%
    group_by(across(wbCOMID)) %>%
    summarise(across(where(is.numeric), mean))
}

# Combining variables to main predictor data set, removing repeated columns and merging by COMID
incorp <- function(df){
  df <- subset(df, select = -c(CatAreaSqKm,WsAreaSqKm,CatPctFull,WsPctFull,inStreamCat))
  merge(PredDataMas, df, by = 'COMID')
}

# Combine 2007 and 2012 Data -----------------------------------------------

PredData2007 <- combines(PredData07_05Ws, PredData07_07Ws, PredData07_10Ws)
verify(PredData2007)

PredData2007 <- mean_group(PredData2007)
verify(PredData2007)

# 2012

PredData2012 <- combines(PredData12_05Ws, PredData12_07Ws, PredData12_10Ws)
verify(PredData2012)

PredData2012 <- mean_group(PredData2012)
verify(PredData2012)

# Create the Master Dataset ------------------------------------------------

PredDataMas <- PredData2007 %>%
  bind_rows(PredData2012)
verify(PredDataMas)

PredDataMas <- mean_group(PredDataMas)
verify(PredDataMas)

# rename the COMID variable to match other data sets
names(PredDataMas)[names(PredDataMas) == "wbCOMID"] <- "COMID"

# rename the Runoff variable to clarify it was derived from the nutrient inventory data
names(PredDataMas)[names(PredDataMas) == "RunoffWs"] <- "Runoff.Nutr"

head(PredDataMas)

# Combine Datasets -------------------------------------------------------

# pesticide data from 1997 StreamCat ------
pesticides <- read.csv("C:/Users/mreyno04/OneDrive - Environmental Protection Agency (EPA)/Profile/REPOS/LakePredData/PredData/lakecat-metrics-melanie/Pesticides97.csv")
PredDataMas <- incorp(pesticides)

# base flow index derived from Streamcat ------
BFI <- read.csv("C:/Users/mreyno04/OneDrive - Environmental Protection Agency (EPA)/Profile/REPOS/LakePredData/PredData/lakecat-metrics-melanie/BFI.csv")
names(BFI)[names(BFI) == "BFIWs"] <- "BFIWs.Str" # Renmaed for clarity
PredDataMas <- incorp(BFI)

# PRISM data derived from Stream Cat ------
PRISM <- read.csv("C:/Users/mreyno04/OneDrive - Environmental Protection Agency (EPA)/Profile/REPOS/LakePredData/PredData/lakecat-metrics-melanie/PRISM_1991_2020.csv")
PredDataMas <- incorp(PRISM)

# Runoff data derived from Stream Cat ------
runoff <- read.csv("C:/Users/mreyno04/OneDrive - Environmental Protection Agency (EPA)/Profile/REPOS/LakePredData/PredData/lakecat-metrics-melanie/runoff.csv")

names(runoff)[names(runoff) == "RunoffWs"] <- "Runoff.Str"
runoff <- subset(runoff, select = -c(RunoffCat))
PredDataMas <- incorp(runoff)

# Lithological Nitrogen, Stream Cat ------
rockN <- read.csv("C:/Users/mreyno04/OneDrive - Environmental Protection Agency (EPA)/Profile/REPOS/LakePredData/PredData/lakecat-metrics-melanie/RockN.csv")
PredDataMas <- incorp(rockN)

# Ecoregions data, derived from NLA ------
eco3 <- read.csv("C:/Users/mreyno04/OneDrive - Environmental Protection Agency (EPA)/Profile/REPOS/LakePredData/NLA_SampleFrame_L3L4Ecoregions 1.csv")
names(eco3)[names(eco3) == "comid"] <- "COMID"

PredDataMas <- merge(PredDataMas, eco3, by = 'COMID')

# Renaming variables for the merge
names(PredDataMas)[names(PredDataMas) == "BFIWs"] <- "BFIWs.Nutr" # BFIWs 2007-2012 data compiled

# Remove excess column
PredDataMas = subset(PredDataMas, select = -c(X))

colnames(PredDataMas)

# names(PredDataMas)[names(PredDataMas) == "BFIWs.2003"] <- "BFIWs"
# names(PredDataMas)[names(PredDataMas) == "RunoffWs.y"] <- "Runoff"

# Data Validation --------------------------------------------------------------
#
# mean(PredDataMas$Precip_YrMean, na.rm = TRUE)
# (mean(PredData2007$Precip_YrMean, na.rm = TRUE) + mean(PredData2012$Precip_YrMean, na.rm = TRUE)) / 2
#
# mean(PredDataMas$Total.Input, na.rm = TRUE)
# (mean(PredData2007$Total.Input, na.rm = TRUE) + mean(PredData2012$Total.Input, na.rm = TRUE)) / 2
#
# # plotting
# plot(PredDataMas$Tmean9120Ws, PredDataMas$Precip_YrMean, xlab = "Mean Temp 1991-2020",
#      ylab = " Mean Precip")
#
# plot(PredDataMas$Tmean9120Ws, PredDataMas$SNOW_YrMean, xlab = "Mean Temp 1991-2020",
#      ylab = " Mean Snowfall")
#
# # spearman correlation
# cor(PredDataMas$Pestic97Ws, PredDataMas$P_Accumulated_ag_inputs_2007,
#     method = "spearman", use = "pairwise.complete.obs")
#
# cor(PredDataMas$Tmean9120Ws, PredDataMas$SNOW_YrMean,
#     method = "spearman", use = "pairwise.complete.obs")
#
# cor(PredDataMas$Tmean9120Ws, PredDataMas$COMID,
#     method = "spearman", use = "pairwise.complete.obs") # should not be correlated

# Land Cover Data --------------------------------------------------------------

get_nlcd <- function(coms){
  lc_get_data(metric = 'PctWdWet2016, PctUrbMd2016, PctUrbLo2016, PctUrbHi2016,
                          PctMxFst2016, PctCrop2016, PctHay2016, PctDecid2016,
                          PctConif2016, PctUrbOp2016, PctHbWet2016',
                    aoi='watershed',
                    comid = coms,
                    showAreaSqKm = TRUE)
}

chunks <- split(PredDataMas$COMID, ceiling(seq_along(PredDataMas$COMID) / 10000))

ncldMas2 <- do.call(rbind, lapply(chunks, get_nlcd))

PredDataMas <- merge(PredDataMas, ncldMas2, by = 'COMID')
PredDataMas <- subset(PredDataMas, select = -c(WSAREASQKM,WsAreaSqKm.y))
names(PredDataMas)[names(PredDataMas) == "WsAreaSqKm.x"] <- "WsAreaSqKm"

# variables for the model

# agr_ws
# NLCD agricultural land cover including hay and cultivated crop. Data are paired as follows: NLA 2007 - NLCD 2006, NLA 2012 - NLCD 2011, NLA 2017 - NLCD 2016 (percent)

PredDataMas$agr_ws <- PredDataMas$PCTCROP2016WS + PredDataMas$PCTHAY2016WS

# dev_ws
# NLCD developed land cover including open spaces, low, medium, and high intensity. Data are paired as follows: NLA 2007 - NLCD 2006, NLA 2012 - NLCD 2011, NLA 2017 - NLCD 2016 (percent)

PredDataMas$dev_ws <- PredDataMas$PCTURBLO2016WS + PredDataMas$PCTURBMD2016WS + PredDataMas$PCTURBOP2016WS + PredDataMas$PCTURBHI2016WS

# fst_ws
# NLCD forested land cover including coniferous, deciduous, and mixed forest. Data are paired as follows: NLA 2007 - NLCD 2006, NLA 2012 - NLCD 2011, NLA 2017 - NLCD 2016 (percent)

PredDataMas$fst_ws <- PredDataMas$PCTMXFST2016WS + PredDataMas$PCTDECID2016WS + PredDataMas$PCTCONIF2016WS

# wet_ws
# NLCD wetland land cover including herbaceous and woody wetlands. Data are paired as follows: NLA 2007 - NLCD 2006, NLA 2012 - NLCD 2011, NLA 2017 - NLCD 2016 (percent)

PredDataMas$wet_ws <- PredDataMas$PCTWDWET2016WS + PredDataMas$PCTHBWET2016WS

# Lake Depth Data --------------------------------------------------------------

loc <- "O:/PRIV/CPHEA/PESD/COR/CORFILES/Geospatial_Library_Resource/Physical/HYDROLOGY/NHDPlusV21/NHDPlusNationalData/NHDPlusV21_National_Seamless_Flattened_Lower48.gdb"

wbd <- sf::st_read(dsn = loc, layer = "NHDWaterbody") %>%
  st_transform(5072)

wbd_copy <- subset(wbd, COMID %in% PredDataMas$COMID)

morph_it <- function(com, df){
  lake_com <- filter(df, COMID == com)
  lake_elev <- get_elev_raster(lake_com, z = 10, prj = st_crs(wbd), expand = 100, override_size_check = TRUE)
  lake_lm <- lakeSurroundTopo(lake_com, lake_elev)
  lake_maxdepth <- lakeMaxDepth(lake_lm, correctFactor = 0.6)
  data.frame(com, lake_maxdepth)
}

# full lake set estimates
# if LAGOSlakedepth & NHDLakeDepth are not equal to NA or hold a negative value
# select this row
# save to new df

y_lake_depth <- data.frame(PredDataMas %>%
  filter(!is.na(LAGOSLakeDepth) & !is.na(NHDLakeDepth) &
           LAGOSLakeDepth >= 0 & NHDLakeDepth >= 0))

large_lakes <- y_lake_depth %>%
  filter(LAGOSLakeDepth >= 75 & NHDLakeDepth >= 75)
L_COMID <- large_lakes$COMID

large_lakes <- subset(wbd, COMID %in% L_COMID)

large_lake_depth <- do.call(rbind, lapply(L_COMID, morph_it, large_lakes))

names(large_lake_depth)[names(large_lake_depth) == "lake_maxdepth"] <- "morpho_depth"
names(large_lake_depth)[names(large_lake_depth) == "com"] <- "COMID"

large_lakes <- merge(large_lake_depth, y_lake_depth, by = 'COMID')

depth_overview <- subset(large_lakes, select = c(COMID, morpho_depth, LAGOSLakeDepth, NHDLakeDepth))

summary(depth_overview)

depth_overview$lake_indiv <- 1:15

# line chart

ggplot(depth_overview, aes(x = lake_indiv)) +

  geom_point(aes(y = morpho_depth, fill = "Lake Morpho Depth"),
             color = "chartreuse4", stat = "identity") +

  geom_point(aes(y = NHDLakeDepth, fill = "NHD Lake Depth"),
             color = "darkmagenta", stat = "identity") +

  geom_point(aes(y = LAGOSLakeDepth, fill = "Measured Depth"),
             color = "black", stat = "identity") +

  labs(y = "Estimated Depth (m)", x = "Individual Lake", fill = "Legend")

# bar chart

ggplot(depth_overview, aes(x = lake_indiv)) +

  geom_bar(aes(y = morpho_depth, fill = "Lake Morpho Depth"),
           color = "darkolivegreen3", stat = "identity", position = "stack") +

  geom_bar(aes(y = NHDLakeDepth, fill = "NHD Lake Depth"),
           color = "darksalmon", stat = "identity", position = "stack") +

  geom_bar(aes(y = LAGOSLakeDepth, fill = "Measured Depth"),
           color = "black", stat = "identity", position = "stack", alpha = 0.4) +

  labs(y = "Estimated Depth (m)", x = "Individual Lake", fill = "Legend",
       title = "> 75m Deep Lakes Sample") +

  scale_fill_manual(values = c("Lake Morpho Depth" = "darkolivegreen3",
                               "NHD Lake Depth" = "darksalmon",
                               "Measured Depth" = "black"))

# Medium sized lakes

med_lakes <- y_lake_depth %>%
  filter(LAGOSLakeDepth >= 8 & LAGOSLakeDepth <= 15 & NHDLakeDepth >= 8 & NHDLakeDepth <= 15)
med_lakes <- subset(wbd, COMID %in% med_lakes$COMID)

# med_lakes <- sample_n(med_lakes, 15)
med_lakes <- subset(med_lakes, COMID %in% c("724970", "1102366", "2267173", "2615482", "3594346", "4099949", "4854979", "6718825", "6742526", "12421133", "15457344", "15477543", "15509690", "24277309", "166766656"))

med_lake_depth <- do.call(rbind, lapply(med_lakes$COMID, morph_it, med_lakes))

names(med_lake_depth)[names(med_lake_depth) == "lake_maxdepth"] <- "morpho_depth"
names(med_lake_depth)[names(med_lake_depth) == "com"] <- "COMID"

med_lakes <- merge(med_lake_depth, y_lake_depth, by = 'COMID')

med_depth_overview <- subset(med_lakes, select = c(COMID, morpho_depth, LAGOSLakeDepth, NHDLakeDepth))

summary(med_depth_overview)

med_depth_overview$lake_indiv = 1:15

# med bar chart

ggplot(med_depth_overview, aes(x = lake_indiv)) +

  geom_bar(aes(y = morpho_depth, fill = "Lake Morpho Depth"),
           color = "darkolivegreen3", stat = "identity", alpha = 0.5,
           position = position_dodge2(preserve = 'total')) +

  geom_bar(aes(y = NHDLakeDepth, fill = "NHD Lake Depth"),
           color = "darksalmon", stat = "identity", alpha = 0.5,
           position = position_dodge2(preserve = 'total')) +

  geom_bar(aes(y = LAGOSLakeDepth, fill = "Measured Depth"),
           color = "black", stat = "identity", alpha = 0.2,
           position = position_dodge2(preserve = 'total')) +

  labs(y = "Estimated Depth (m)", x = "Individual Lake", fill = "Legend",
       title = "Medium Depth Lakes Sample, Between 8m and 15m") +

  scale_fill_manual(values = c("Lake Morpho Depth" = "darkolivegreen3",
                               "NHD Lake Depth" = "darksalmon",
                               "Measured Depth" = "black"))

# plotting differences MEDIUM DEPTH

ggplot(med_depth_overview, aes(x = LAGOSLakeDepth, y = NHDLakeDepth)) +
  geom_point(aes(fill = "NHD Depth"))  +
  geom_abline(slope = 1, linetype = "dashed", color = "red") +
  labs(y = "NHD Estimated Depth (m)", x = "LAGOS Measured Depth", fill = "Legend",
       title = "NHD vs LAGOS Depths for Medium Sized Lakes Sample")

morpho_med_model <- lm(LAGOSLakeDepth ~ morpho_depth, data = med_depth_overview)
summary(morpho_med_model)

plot(morpho_med_model)

cor(med_depth_overview$LAGOSLakeDepth, med_depth_overview$morpho_depth,
    method = "spearman", use = "pairwise.complete.obs")

cor(med_depth_overview$LAGOSLakeDepth, med_depth_overview$NHDLakeDepth,
    method = "spearman", use = "pairwise.complete.obs")

ggplot(med_depth_overviLAGOSLakeDepthggplot(med_depth_overview, aes(x = LAGOSLakeDepth, y = morpho_depth)) +
  geom_point(aes(fill = "Lake Morpho Depth")) +
  geom_abline(slope = 1, linetype = "dashed", color = "red")  +
  labs(y = "LakeMorpho Estimated Depth (m)", x = "LAGOS Measured Depth", fill = "Legend",
       title = "LakeMorpho vs LAGOS Depths for Medium Sized Lakes Sample"))


# plotting differences DEEP LAKES

ggplot(depth_overview, aes(x = LAGOSLakeDepth, y = NHDLakeDepth)) +
  geom_point(aes(fill = "NHD Depth"))  +
  geom_abline(slope = 1, linetype = "dashed", color = "red") +
  labs(y = "NHD Estimated Depth (m)", x = "LAGOS Measured Depth", fill = "Legend",
       title = "NHD vs LAGOS Depths for High Depth Lakes Sample")

ggplot(depth_overview, aes(x = LAGOSLakeDepth, y = morpho_depth)) +
  geom_point(aes(fill = "Lake Morpho Depth")) +
  geom_abline(slope = 1, linetype = "dashed", color = "red")  +
  labs(y = "LakeMorpho Estimated Depth (m)", x = "LAGOS Measured Depth", fill = "Legend",
       title = "LakeMorpho vs LAGOS Depths for High Depth Lakes Sample")

cor(depth_overview$LAGOSLakeDepth, depth_overview$morpho_depth, method = "spearman", use = "pairwise.complete.obs")

cor(depth_overview$LAGOSLakeDepth, depth_overview$NHDLakeDepth, method = "spearman", use = "pairwise.complete.obs")

# could I make a full data filter to bucket small medium and large lakes?

# Lake Area Creation -----------------------------------------------------------

for (Shape in 1:length(wbd_copy)) {
  shapes <- wbd_copy$Shape
  wbd_copy$custom_area <- st_area(shapes)
}

summary(wbd_copy$custom_area) # meters squared
summary(wbd_copy$LakeArea)

wbd_copy$custom_area <- drop_units(wbd_copy$custom_area)
testtest$area <- drop_units(testtest$area)

lakes_bin <- function(lake_area) {
  if (lake_area > 400 & lake_area < 19990) {z <- 12}
  else if (lake_area > 19991 & lake_area < 37901) {z <- 11}
  else if (lake_area > 37901 & lake_area < 96911) {z <- 10}
  else {z <- 9}
  return(z)
}

testtest$z <- lakes_bin(testtest$area)

morph_it <- function(df, z){
  lake_elev <- get_elev_raster(df, z = z, prj = st_crs(df), expand = 100)
  lake_lm <- lakeSurroundTopo(df, lake_elev)
  lake_maxdepth <- lakeMaxDepth(lake_lm, correctFactor = 0.4)
  data.frame(COMID = df$COMID, lake_maxdepth)
}

morph_it(testtest, testtest$z)

wbd_copy$z <- as.numeric(lapply(wbd_copy$custom_area, lakes_bin))

#
# wbd_copy$morpho_depth <- lapply(wbd_copy, morph_it, wbd_copy$z)
#
# wbchunks <- split(wbd_copy$COMID, ceiling(seq_along(wbd_copy$COMID) / 10000))
#
# ncldMas2 <- do.call(rbind, lapply(wbchunks, morph_it, wbd_copy$z))
#


# Lake Fetch -------------------------------------------------------------------

# function to determine longest distance across a lake

fetch_it <- function(com, df){
  lake_com <- filter(df, COMID == com)
  lake_elev <- get_elev_raster(lake_com, z = 10, prj = st_crs(wbd), expand = 100, override_size_check = TRUE)
  lake_lm <- lakeSurroundTopo(lake_com, lake_elev)
  lake_max_len <- lakeMaxLength(lake_lm, correctFactor = 0.6)
  data.frame(com, lake_max_len)
}

fetch_all <- do.call(rbind, lapply(wbd_copy$COMID, fetch_it, wbd_copy))



