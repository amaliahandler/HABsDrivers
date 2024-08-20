
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


comids <- paste(PredDataMas$COMID[0:60000], collapse = ',')

nlcd <- lc_get_data(metric = 'PctWdWet2016, PctUrbMd2016, PctUrbLo2016, PctUrbHi2016,
                          PctMxFst2016, PctCrop2016, PctHay2016, PctDecid2016,
                          PctConif2016, PctUrbOp2016, PctHbWet2016',
                    aoi='watershed',
                    comid = comids,
                    showAreaSqKm = TRUE)

comid2 <- paste(PredDataMas$COMID[60001:120000], collapse = ',')

nlcd2 <- lc_get_data(metric = 'PctWdWet2016, PctUrbMd2016, PctUrbLo2016, PctUrbHi2016,
                          PctMxFst2016, PctCrop2016, PctHay2016, PctDecid2016,
                          PctConif2016, PctUrbOp2016, PctHbWet2016',
                     aoi='watershed',
                     comid = comid2,
                     showAreaSqKm = TRUE)

comid3 <- paste(PredDataMas$COMID[120001:125778], collapse = ',')

nlcd3 <- lc_get_data(metric = 'PctWdWet2016, PctUrbMd2016, PctUrbLo2016, PctUrbHi2016,
                          PctMxFst2016, PctCrop2016, PctHay2016, PctDecid2016,
                          PctConif2016, PctUrbOp2016, PctHbWet2016',
                     aoi='watershed',
                     comid = comid3,
                     showAreaSqKm = TRUE)

nlcdMas <- nlcd %>%
  bind_rows(nlcd2) %>%
  bind_rows(nlcd3)

which(nlcdMas$COMID == "487")

rm(nlcd, nlcd2, nlcd3)

PredDataMas <- merge(PredDataMas, nlcdMas, by = 'COMID')
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

morph_it <- function(df){
  #st_drop_geometry(df)
  lake_elev <- get_elev_raster(df, z = 9, prj = st_crs(wbd), expand = 100)
  lake_lm <- lakeSurroundTopo(df, lake_elev)
  lake_maxdepth <- lakeMaxDepth(lake_lm, correctFactor = 0.4)
  data.frame(COMID = df$COMID, lake_maxdepth)
}

wb_list <- split(wbd_copy, 1:25)

#lake_depth <- apply(wbd_copy, 1, morph_it)
lake_depth <- apply(split(wbd_copy, seq_along(wbd_copy$COMID)),1, morph_it)
lake_depth_df <- bind_rows(lake_depth)
lake_depth_df

PredDataMas <- merge(PredDataMas, lake_depth_df, by = 'COMID')

# Lake Fetch -------------------------------------------------------------------

# fetch <- lakeFetch(lake_lm, bearing = -10, addLine = TRUE)

fetch_it <- function(df){
  lake_elev <- get_elev_raster(df, z = 9, prj = st_crs(wbd), expand = 100)
  lake_lm <- lakeSurroundTopo(as_Spatial(df), lake_elev)
  fetch_lake <- lakeFetch(lake_lm, bearing = -10, addLine = TRUE)
  data.frame(COMID = df$COMID, fetch_lake)
}

testtest <- wbd[wbd$COMID == "15985627",] #confirmed high elevation

fetch_it(testtest)

fetchs <- apply(wbd_copy, 1, fetch_it)

fetchs <- apply(split(wbd_copy, seq_along(wbd_copy$COMID)),1, fetch_it)




