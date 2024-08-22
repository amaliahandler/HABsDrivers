
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

summary(y_lake_depth$NHDLakeDepth)
summary(y_lake_depth$LAGOSLakeDepth)

large_lakes <- y_lake_depth %>%
  filter(LAGOSLakeDepth >= 75 & NHDLakeDepth >= 75)
large_lakes <- subset(wbd, COMID %in% L_COMID)

L_COMID <- large_lakes$COMID

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

# could I make a full data filter to bucket small medium and large lakes?

# categorize lakes function
# cat_lakes <-

summary(PredDataMas$WsAreaSqKm)
plot(log10(y_lake_depth$NHDLakeDepth), log10(y_lake_depth$WsAreaSqKm), ylab = "Wastershed Area",
    xlab = "NHD Lake Depth")

plot(log10(wbd_copy$LakeArea), log10(wbd_copy$MaxDepth), ylab = "Lake Area",
    xlab = "Max Depth")

summary(wbd_copy$LakeArea) # st_area fill in the lake area from polygons

cor(y_lake_depth$NHDLakeDepth, y_lake_depth$WsAreaSqKm,
    method = "spearman", use = "pairwise.complete.obs")

cor(wbd_copy$LakeArea, wbd_copy$MaxDepth,
    method = "spearman", use = "pairwise.complete.obs")

cor(wbd_copy$LakeArea, wbd_copy$MeanDepth,
    method = "spearman", use = "pairwise.complete.obs")


# Full Depth Estimates

chunks_smaller <- split(wbd_copy, (seq(nrow(wbd_copy))-1) %/% 25)

# morph_it <- function(df){
#   #st_drop_geometry(df)
#   lake_elev <- get_elev_raster(df, z = 12, prj = st_crs(df), expand = 100)
#   lake_lm <- lakeSurroundTopo(df, lake_elev)
#   lake_maxdepth <- lakeMaxDepth(lake_lm, correctFactor = 0.4)
#   data.frame(COMID = df$COMID, lake_maxdepth)
# }


lake1 <- do.call(rbind, lapply(wbd_copy$COMID[1:5,], morph_it, wbd_copy))

#lake_depth <- apply(wbd_copy, 1, morph_it)
lake_depth <- do.call(rbind, lapply(wbd_copy[1:25,], morph_it))

# lake_depth <- apply(split(wbd_copy, seq_along(wbd_copy$COMID)),1, morph_it)
lake_depth <- morph_it
lake_depth_df <- bind_rows(lake_depth)
lake_depth_df

PredDataMas <- merge(PredDataMas, lake_depth_df, by = 'COMID')

# Lake Fetch -------------------------------------------------------------------

# function to determine longest distance across a lake

fetch_it <- function(df){
  lake_elev <- get_elev_raster(df, z = 9, prj = st_crs(wbd), expand = 100)
  lake_lm <- lakeSurroundTopo(as_Spatial(df), lake_elev)
  max_length <- lakeMaxLength(lake_lm, 100, addLine = TRUE)
  data.frame(COMID = df$COMID, max_length)
}

testtest <- wbd[wbd$COMID == "15985627",] #confirmed high elevation

max_len <- fetch_it(testtest)

fetch_all <- do.call(rbind, lapply(wbd_copy$COMID, fetch_it, wbd_copy))

# fetchs <- apply(split(wbd_copy, seq_along(wbd_copy$COMID)),1, fetch_it)




