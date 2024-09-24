
# install.packages("lakemorpho")
# install.packages("elevatr")
# install.packages("raster")
# install_github("USEPA/StreamCatTools", build_vignettes=FALSE, auth_token= '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', force = TRUE)
install.packages('fs')
install.packages("readr")

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
library(fs)
library(readr)

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
  x |>
    bind_rows(y) |>
    bind_rows(z)
}

# Aggregating and averaging data across binded data sets
mean_group <- function(df){
  df |>
    group_by(across(wbCOMID)) |>
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

PredDataMas <- PredData2007 |>
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

# Remove excess columns
PredDataMas = subset(PredDataMas, select = -c(Shape_Leng, Shape_Area, X))

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

# Water Body Data --------------------------------------------------------------

loc <- "O:/PRIV/CPHEA/PESD/COR/CORFILES/Geospatial_Library_Resource/Physical/HYDROLOGY/NHDPlusV21/NHDPlusNationalData/NHDPlusV21_National_Seamless_Flattened_Lower48.gdb"

wbd <- sf::st_read(dsn = loc, layer = "NHDWaterbody") |>
  st_transform(5072)

wbd_copy <- subset(wbd, COMID %in% PredDataMas$COMID)

# clean wbd_copy by removing excess variables
wbd_copy <- subset(wbd_copy, select = c(COMID, ELEVATION, ONOFFNET, MeanDepth, LakeVolume, MaxDepth, LakeArea, Shape))

PredDataMas <- merge(PredDataMas, wbd_copy, by = 'COMID')

# Lake Area Creation -----------------------------------------------------------

for (Shape in 1:length(wbd_copy)) {
  shapes <- wbd_copy$Shape
  wbd_copy$custom_area <- st_area(shapes)
}

wbd_copy$custom_area <- drop_units(wbd_copy$custom_area)

lakes_bin <- function(lake_area) {
  if (lake_area > 400 & lake_area < 19990) {z <- 12}
  else if (lake_area > 19991 & lake_area < 37901) {z <- 11}
  else if (lake_area > 37901 & lake_area < 96911) {z <- 10}
  else {z <- 9}
  return(z)
}

COMID <- wbd_copy$COMID
wbd_copy$z <- unlist(as.numeric(lapply(wbd_copy$custom_area, lakes_bin)))
summary(wbd_copy$z)

# missing depth data frame
mrs_depth <- wbd_copy[is.na(wbd_copy$MaxDepth) | wbd_copy$MaxDepth < 0 | wbd_copy$MaxDepth == 0,]
mrs_depth <- subset(mrs_depth, select = c(COMID, Shape))

# save(mrs_depth, file="missing_depths.rdata")
#
# load("missing-depths.rdata")



# Lake Depth Data --------------------------------------------------------------

depth_it <- function(com, df){
  lake_com <- filter(df, COMID == com)
  lake_elev <- get_elev_raster(lake_com, z = 12, prj = st_crs(df), expand = 100, override_size_check = TRUE)
  lake_lm <- lakeSurroundTopo(lake_com, lake_elev)
  lake_maxdepth <- lakeMaxDepth(lake_lm, correctFactor = 0.6)
  data.frame(com, lake_maxdepth)
}

mrs_com <- mrs_depth$COMID

# ===== remote

library(devtools)
library(dplyr)
library(stars)
install.packages('nhdplusTools')
library(nhdplusTools)
library(tidyverse)
library(tidyr)
library(sf)
library(ggplot2)
install.packages('spmodel')
library(spmodel)
install.packages('elevatr')
library(elevatr)
install.packages('lakemorpho')
library(lakemorpho)
library(raster)
install.packages("future.apply")
library(future.apply)


wbd_mini <- wbd_copy[1,]
com_mini <- wbd_mini$COMID

morph_it <- function(com, df){
  lake_com <- filter(df, COMID == com)
  lake_elev <- get_elev_raster(lake_com, z = 13, prj = st_crs(df), expand = 100, override_size_check = TRUE)
  lake_lm <- lakeSurroundTopo(lake_com, lake_elev)
  lake_maxdepth <- lakeMaxDepth(lake_lm, correctFactor = 0.6)
  lake_max_len <- lakeMaxLength(lake_lm, pointDens = 50)
  output <- data.frame(com, lake_depth = lake_maxdepth, lake_fetch = lake_max_len)
  saveRDS(output, file = paste0('lake_metrics_', com, ".rds"))
  return(output)
}

depth <- morph_it(com_mini, wbd_mini)

##############

morph_it <- function(com, df) {
  for(i in seq_along(df)) {
    lake_com <- filter(df, COMID == com)
    lake_elev <- get_elev_raster(lake_com, z = 12, prj = st_crs(df), expand = 100, override_size_check = TRUE)
    lake_lm <- lakeSurroundTopo(lake_com, lake_elev)
    save(list = df[i], file = paste0(df[i], ".RData"))
  }
}

morph_it <- function(com, df) {
  for(i in seq_along(df)) {
    lake_com <- filter(df, COMID == com)
    lake_elev <- get_elev_raster(lake_com, z = 12, prj = st_crs(df), expand = 100, override_size_check = TRUE)
    lake_lm <- lakeSurroundTopo(lake_com, lake_elev)
    save(list = df[i], file = paste0(df[i], ".RData"))
  }
}



flw <- vector("list", length(wbd_mini))

morph_it <- function(com, df) {
  for(i in seq_along(df)) {
    lake_com <- filter(df, COMID == com)
    lake_elev <- get_elev_raster(lake_com, z = 12, prj = st_crs(df), expand = 100, override_size_check = TRUE)
    lake_lm <- lakeSurroundTopo(lake_com, lake_elev)
  saveRDS(flw[[i]], file = paste0(df[i], "file.rds"))
  cat("*")
  }
}

depth <- do.call(rbind, lapply(com_mini, morph_it, wbd_mini))

depth <- morph_it(com_mini, wbd_mini)

##############


lake_chunks <- split(mrs_depth$COMID, ceiling(seq_along(mrs_depthCOMID) / 1000))

lake_elev <- do.call(rbind, future_lapply(mrs_com, morph_it, mrs_depth))

lake_maxdepth <- lakeMaxDepth(lake_elev, correctFactor = 0.6)
lake_maxlen <- lakeMaxLength(lake_elev, pointDens = 50)

# ======

elev_it <- function(com, df){
  lake_com <- filter(df, COMID == com)
  lake_elev <- get_elev_raster(lake_com, z = 12, prj = st_crs(wbd), expand = 100, override_size_check = TRUE)
}
lake_elev <- do.call(rbind, lapply(COMID, elev_it, mrs_depth))



# THIS WORKED
lake_elev <- do.call(rbind, lapply(COMID, elev_it, wbd_copy))

# lake depth with dependent z variable

wbd_sm <- wbd_copy[1:15,]
sm_COMID <- wbd_sm$COMID

lake_elev <- lapply(sm_COMID, elev_it, wbd_sm)

lake_lm <- lapply(wbd_sm, lakeSurroundTopo, lake_elev)
class(lake_elev)


# morph_it <- function(df, z){
#   lake_elev <- get_elev_raster(df, z = z, prj = st_crs(df), expand = 100)
#   #lake_lm <- lakeSurroundTopo(df, lake_elev)
#   #lake_maxdepth <- lakeMaxDepth(lake_lm, correctFactor = 0.6)
#   data.frame(COMID = df$COMID, lake_elev)
# }
#
# for (row in 1:nrow(wbd_reduc)) {
#   lake_elev <- get_elev_raster(wbd_reduc, z = wbd_reduc$z, prj = st_crs(wbd_copy), expand = 100)
#   lake_lm <- lakeSurroundTopo(wbd_reduc, lake_elev)
#   lake_maxdepth <- lakeMaxDepth(lake_lm, correctFactor = 0.6)
#   data.frame(COMID = depths$COMID, lake_maxdepth)
# }

# Lake Fetch -------------------------------------------------------------------

# function to determine longest distance across a lake

fetch_it <- function(com, df){
  lake_com <- filter(df, COMID == com)
  lake_elev <- get_elev_raster(lake_com, z = 10, prj = st_crs(wbd), expand = 100, override_size_check = TRUE)
  lake_lm <- lakeSurroundTopo(lake_com, lake_elev)
  lake_max_len <- lakeMaxLength(lake_lm, pointDens = 50)
  data.frame(com, lake_max_len)
}

fetch_all <- do.call(rbind, mapply(wbd_copy$COMID, fetch_it, wbd_copy))

# Tidying ----------------------------------------------------------------------

# remove unnecessary eco-regions data
PredDataMas <- subset(PredDataMas, select = -c(US_L4CODE, US_L4NAME, US_L3CODE, US_L3NAME, NA_L3CODE, NA_L3NAME,
                                               NA_L2CODE, NA_L2NAME, NA_L1CODE, NA_L1NAME, nla07_sf, nla12_sf, nla17_sf))

# remove all varibales that contain Cat from Pred Data mas

PredDataMas <- PredDataMas |> dplyr::select(-contains(c('Cat')))

# remove excess land cover variables
PredDataMas <- subset(PredDataMas, select = -c(PCTURBOP2016WS, PCTHBWET2016WS, PCTWDWET2016WS, PCTURBLO2016WS,
                                               PCTURBMD2016WS, PCTMXFST2016WS, PCTHAY2016WS, PCTDECID2016WS,
                                               PCTURBHI2016WS, PCTCONIF2016WS, PCTCROP2016WS))

# Nutrient Data Compiling -----------------------------------------------------------

# load files from folder
data_dir <- "O:/PRIV/CPHEA/PESD/COR/CORFILES/Geospatial_Library_Projects/AmaliaHandler/Allocation_and_Accumulation/Final_LakeCat_OnNetwork/"
csv_files <- fs::dir_ls(data_dir)

# compile files into a single data frame
nutrMas <- csv_files |>
  map_dfr(read_csv)

# ensure there's no repetition
print(which(nutrMas$COMID == "487"))

# remove unnecessary columns
nutrMas <- nutrMas |> dplyr::select(-contains(c('Cat')))

# aggregate nutrients across years

# N livestock waste
nutrMas$N_livestock.Waste_kg_Ag <- rowMeans(nutrMas[,c('N_Livestock.Waste_kg_Ag_2002Ws', 'N_Livestock.Waste_kg_Ag_2007Ws', 'N_Livestock.Waste_kg_Ag_2012Ws')], na.rm = TRUE)
nutrMas <- subset(nutrMas, select = -c(N_Livestock.Waste_kg_Ag_2002Ws, N_Livestock.Waste_kg_Ag_2007Ws, N_Livestock.Waste_kg_Ag_2012Ws))

# P livestock waste
nutrMas$P_livestock_Waste_kg_Ag <- rowMeans(nutrMas[,c('P_livestock_Waste_kg_Ag_2002Ws', 'P_livestock_Waste_kg_Ag_2007Ws', 'P_livestock_Waste_kg_Ag_2012Ws')], na.rm = TRUE)
nutrMas <- subset(nutrMas, select = -c(P_livestock_Waste_kg_Ag_2002Ws, P_livestock_Waste_kg_Ag_2007Ws, P_livestock_Waste_kg_Ag_2012Ws))

# P ag fertilizer
nutrMas$P_f_fertilizer_kg_Ag <- rowMeans(nutrMas[,c('P_f_fertilizer_kg_Ag_2002Ws', 'P_f_fertilizer_kg_Ag_2007Ws', 'P_f_fertilizer_kg_Ag_2012Ws')], na.rm = TRUE)
nutrMas <- subset(nutrMas, select = -c(P_f_fertilizer_kg_Ag_2002Ws, P_f_fertilizer_kg_Ag_2007Ws, P_f_fertilizer_kg_Ag_2012Ws))

# N human waste
nutrMas$N_Human_Waste_kg_Urb <- rowMeans(nutrMas[,c('N_Human_Waste_kg_Urb_2002Ws', 'N_Human_Waste_kg_Urb_2007Ws', 'N_Human_Waste_kg_Urb_2012Ws')], na.rm = TRUE)
nutrMas <- subset(nutrMas, select = -c(N_Human_Waste_kg_Urb_2002Ws, N_Human_Waste_kg_Urb_2007Ws, N_Human_Waste_kg_Urb_2012Ws))

# P human waste
nutrMas$P_Human_Waste_kg_Urb <- rowMeans(nutrMas[,c('P_human_waste_kg_Urb_2002Ws', 'P_human_waste_kg_Urb_2007Ws', 'P_human_waste_kg_Urb_2012Ws')], na.rm = TRUE)
nutrMas <- subset(nutrMas, select = -c(P_human_waste_kg_Urb_2002Ws, P_human_waste_kg_Urb_2007Ws, P_human_waste_kg_Urb_2012Ws))

# N urban fertilizer
nutrMas$N_Fert_Urban_kg_Urb <- rowMeans(nutrMas[,c('N_Fert_Urban_kg_Urb_2002Ws', 'N_Fert_Urban_kg_Urb_2007Ws', 'N_Fert_Urban_kg_Urb_2012Ws')], na.rm = TRUE)
nutrMas <- subset(nutrMas, select = -c(N_Fert_Urban_kg_Urb_2002Ws, N_Fert_Urban_kg_Urb_2007Ws, N_Fert_Urban_kg_Urb_2012Ws))

# P not from fertilizer
nutrMas$P_nf_fertilizer_kg_Urb <- rowMeans(nutrMas[,c('P_nf_fertilizer_kg_Urb_2002Ws', 'P_nf_fertilizer_kg_Urb_2007Ws', 'P_nf_fertilizer_kg_Urb_2012Ws')], na.rm = TRUE)
nutrMas <- subset(nutrMas, select = -c(P_nf_fertilizer_kg_Urb_2002Ws, P_nf_fertilizer_kg_Urb_2007Ws, P_nf_fertilizer_kg_Urb_2012Ws))

# check all column names
colnames(nutrMas)

# combine nutrient data with main predictor data set
PredDataMas <- merge(PredDataMas, nutrMas, by = 'COMID')

# check column names post-merge
colnames(PredDataMas)

# aggregate CBNF variable
# I have no idea what the variable ...1 is so I'm removing it too
PredDataMas$N_CBNF_kg_Ag <- (PredDataMas$N_CBNF_kg_Ag_2002Ws + PredDataMas$N_CBNF_2007) / 2
PredDataMas <- subset(PredDataMas, select = -c(N_CBNF_2007, N_CBNF_kg_Ag_2002Ws, ...1))

# remove WsAreaSqKm.y and rename WsAreaSqKm.x <- WsAreaSqKm

PredDataMas <- subset(PredDataMas, select = -c(WsAreaSqKm.y))
names(PredDataMas)[names(PredDataMas) == 'WsAreaSqKm.x'] <- 'WsAreaSqKm'



# Depth fr =====================================================================

library(devtools)
library(dplyr)
library(stars)
install.packages('nhdplusTools')
library(nhdplusTools)
library(tidyverse)
library(tidyr)
library(sf)
library(ggplot2)
install.packages('spmodel')
library(spmodel)
install.packages('elevatr')
library(elevatr)
install.packages('lakemorpho')
library(lakemorpho)
library(raster)
install.packages("future.apply")
library(future.apply)

missing_com <- missing_depth$COMID

get_morpho_obj <- function(com, df){
  lake_com <- filter(df, COMID == com)
  lake_elev <- get_elev_raster(lake_com, z = 13, prj = st_crs(df), expand = 100, override_size_check = TRUE)
  lake_lm <- lakeSurroundTopo(lake_com, lake_elev)
  saveRDS(lake_lm, file = paste0('./private/lake_morpho_objects/lake_morpho_', com, ".rds"))
}

depths <- lapply(missing_com, get_morpho_obj, missing_depth)

# load files from folder
data_dir <- "./private/lake_morpho_objects/"
lm_files <- fs::dir_ls(data_dir, regexp = "//.rds$")

# morph_it <- function(file_name){
#   morpho_obj <- readRDS(file_name)
#   if file.info(morpho_obj)$size > null:
#   max_depth <- lakeMaxDepth(morpho_obj, correctFactor = 0.6)
#   lake_fetch <- lakeMaxLength(morpho_obj, pointDens = 50)
#   COMID <- morpho_obj$lake$COMID
#   output <- data.frame(COMID = COMID, depth = max_depth, fetch = lake_fetch)
#   saveRDS(output, file = paste0('./private/metrics/lake_metric', COMID, ".rds"))
# }

morph_it <- function(file_name) {
  morpho_obj <- readRDS(file_name)

  if (class(morpho_obj) == 'lakeMorpho') {
    max_depth <- lakeMaxDepth(morpho_obj, correctFactor = 0.6)
    lake_fetch <- lakeMaxLength(morpho_obj, pointDens = 50)
    COMID <- morpho_obj$lake$COMID

    output <- data.frame(COMID = COMID, depth = max_depth, fetch = lake_fetch)
    saveRDS(output, file = paste0('./private/metrics/lake_metric', COMID, ".rds"))
  } else {
    message("Skipping file: ", file_name)
  }
}


lake_met <- lapply(lm_files, morph_it)

met_dir <- "./private/metrics/"
met_files <- fs::dir_ls(met_dir, regexp = "//.rds$")

# compile files into a single data frame
lake_met_df <- met_files |>
  map_dfr(readRDS)

# save csv with exsiting lake metrics
write.csv(lake_met_df, "./private/lake_met_df.csv")

# load into environment
lake_met_df <- read_csv('./private/lake_met_df.csv')
met_comids <- lake_met_df$COMID

# remove COMIDs with existing metrics from missing depths df
missing_depth <- mrs_depth[!(mrs_depth$COMID %in% met_comids), ]


# testing
get_raster_obj <- function(com, df){
  lake_com <- filter(df, COMID == com)
  lake_elev <- get_elev_raster(lake_com, z = 13, prj = st_crs(df), expand = 100, override_size_check = TRUE)
}

mrs_mini <- mrs_depth[1:15,]
com_mini <- mrs_mini$COMID

rast_test <- lapply(mrs_mini, get_raster_obj(com_mini, mrs_mini))



