
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
incorp <- function(df, x){
  df <- subset(df, select = -c(CatAreaSqKm,WsAreaSqKm,CatPctFull,WsPctFull,inStreamCat))
  merge(x, df, by = 'COMID')
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

rm(df_list)

# clean wbd_copy by removing excess variables
wbd_copy <- subset(wbd_copy, select = c(COMID, ELEVATION, ONOFFNET, MeanDepth, LakeVolume, MaxDepth, LakeArea, GNIS_NAME, AREASQKM, Shape))

PredDataMas <- merge(PredDataMas, wbd_copy, by = 'COMID')

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

# Lake Depth Creation -----------------------------------------------------------

lake_met_dir <- "C:/Users/mreyno04/OneDrive - Environmental Protection Agency (EPA)/Profile/Downloads/lake_met_dir"
lake_met_files <- fs::dir_ls(lake_met_dir, regexp = "\\.csv$")

#compile files into a single data frame
lake_met_df <- lake_met_files |>
  map_dfr(read.csv)

met_comids <- lake_met_df$COMID

lake_met_df <- lake_met_df |>
  distinct(COMID, .keep_all = TRUE)

get_morpho_obj <- function(com, df){
  lake_com <- filter(df, COMID == com)
  lake_elev <- get_elev_raster(lake_com, z = 13, prj = st_crs(df), expand = 100, override_size_check = TRUE)
  lake_lm <- lakeSurroundTopo(lake_com, lake_elev)
  saveRDS(lake_lm, file = paste0('C:/Users/mreyno04/OneDrive - Environmental Protection Agency (EPA)/Profile/REPOS/HABsDrivers/test_depths/', com, ".rds"))
}

#run function to the missing depths COMIDs
depths <- lapply(met_comids, get_morpho_obj, lake_met_df)

#load lake morpho object files from folder
data_dir <- "./private/lake_morpho_objects/"
lm_files <- dir_ls(data_dir, regexp = "\\.rds$")
#
#function to pull files from lake morpho objects into metrics to compile into df
morph_it <- function(file_name) {
  morpho_obj <- readRDS(file_name)
#
  if (class(morpho_obj) == 'lakeMorpho') {
    max_depth <- lakemorpho::lakeMaxDepth(morpho_obj, correctFactor = 0.6)
    lake_fetch <- lakeMaxLength(morpho_obj, pointDens = 50)
    COMID <- morpho_obj$lake$COMID
#
    output <- data.frame(COMID = COMID, depth = max_depth)
    saveRDS(output, file = paste0('./private/friday_metrics/lake_metric', COMID, ".rds"))
  } else {
    message("Skipping file: ", file_name)
  }
}

l <- lapply(lm_files, morph_it)

#pull metrics data to create final df
met_dir <- "./private/metrics/"
met_files <- fs::dir_ls(met_dir, regexp = "\\.rds$")

#compile files into a single data frame
lake_met_df <- met_files |>
  map_dfr(readRDS)

# save csv with exsiting lake metrics
# write.csv(lake_met_df, "./private/lake_met_df_9-25.csv")

#compile depths into one column using hiercarchy to give prefence to LAGOS and NHD populated depths

PredDataMas$LAGOSLakeDepth <- replace(PredDataMas$LAGOSLakeDepth, which(PredDataMas$LAGOSLakeDepth <= 0), NA)
PredDataMas$NHDLakeDepth <- replace(PredDataMas$NHDLakeDepth, which(PredDataMas$NHDLakeDepth <= 0), NA)
PredDataMas$MaxDepth <- replace(PredDataMas$MaxDepth, which(PredDataMas$MaxDepth <= 0), NA)

PredDataMas$lake_m_depth <- PredDataMas$LAGOSLakeDepth

PredDataMas$lake_m_depth <- coalesce(PredDataMas$lake_m_depth, PredDataMas$NHDLakeDepth)
PredDataMas <- left_join(PredDataMas, lake_met_df, by = 'COMID')
PredDataMas$lake_m_depth <- coalesce(PredDataMas$lake_m_depth, PredDataMas$depth)

summary(PredDataMas$lake_m_depth)

length(unique(PredDataMas$COMID))
length(PredDataMas$COMID)

PredDataMas <- PredDataMas %>%
  distinct(COMID, .keep_all = TRUE)

# fetch data

fetch <- read.csv("C:/Users/mreyno04/OneDrive - Environmental Protection Agency (EPA)/Profile/Downloads/fetch_df_21.csv")

PredDataMas <- left_join(PredDataMas, fetch, by = 'COMID')

PredDataMas$fetch <- replace(PredDataMas$fetch, which(PredDataMas$fetch <= 0), NA)
PredDataMas$fetch <- coalesce(PredDataMas$fetch, PredDataMas$max_length)

summary(PredDataMas$fetch)

#colnames(DataMas)colmax_length.ynames(DataMas)

# Modeling ---------------------------------------------------------------------

PredDataMas <- PredDataMas[!is.na(PredDataMas$lake_m_depth),]

PredDataMini <- PredDataMas

PredDataMini <- subset(PredDataMini, select = -c(BFIWs.Str))
names(PredDataMini)[names(PredDataMini) == 'BFIWs.Nutr'] <- 'BFIWs'
names(PredDataMini)[names(PredDataMini) == 'ag_eco3'] <- 'AG_ECO3'
names(PredDataMini)[names(PredDataMini) == 'lake_dep'] <- 'MAXDEPTH'

PredDataMini <- PredDataMini %>%
  rename(Tmean8110Ws = Tmean9120Ws,
         Precip8110Ws = Precip9120Ws,
         MAXDEPTH = lake_m_depth,
         lakemorpho_fetch = fetch)

# n-farm-inputs = N_Fert_Farm + N_CBNF + N_livestock_Waste
# n-dev-inputs = N_Human_Waste + N_Fert_Urban
# p farm inputs = P_f_fertilizer + P_livestock_Waste

PredDataMini$n_farm_inputs <- PredDataMini$N_livestock.Waste_kg_Ag + PredDataMini$N_CBNF_kg_Ag + PredDataMini$N_Fert_Farm_2007
PredDataMini$n_dev_inputs <- PredDataMini$N_Human_Waste_kg_Urb + PredDataMini$N_Fert_Urban_kg_Urb
PredDataMini$p_farm_inputs <- PredDataMini$P_f_fertilizer_kg_Ag + PredDataMini$P_livestock_Waste_kg_Ag
PredDataMini$p_dev_inputs <- PredDataMini$P_Human_Waste_kg_Urb + PredDataMini$P_nf_fertilizer_kg_Urb
PredDataMini$DSGN_CYCLE <- 2017
PredDataMini <- mutate(PredDataMini, DSGN_CYCLE = factor(DSGN_CYCLE))

# Normalize by the watershed area
# left_join(areas, by = "UNIQUE_ID") |>
#   mutate(mean_kg_ha = mean020712 / WSAREA_HA) |>
#   pivot_wider(id_cols = UNIQUE_ID,
#               names_from = c(nutrient, type),
#               values_from = mean_kg_ha)
#

colnames(PredDataMini)

unique_ids <- read_csv('C:/Users/mreyno04/OneDrive - Environmental Protection Agency (EPA)/Profile/Downloads/NLA071217-COMID_crosswalk(in).csv')

PredDataMini <- left_join(PredDataMini, unique_ids, by = 'COMID')
PredDataMini$UNIQUE_ID[is.na(PredDataMini$UNIQUE_ID)] <- 0

PredDataMini <- PredDataMini %>%
  distinct(COMID, .keep_all = TRUE)

PredDataMini <- subset(PredDataMini, select = -c(...1))

# convert WSArea from sq km to ha
PredDataMini$WsAreaHa <- PredDataMini$WsAreaSqKm * 100

# divide nutrient data by ha
PredDataMini$n_dev_inputs <- PredDataMini$n_dev_inputs / PredDataMini$WsAreaHa
PredDataMini$n_farm_inputs <- PredDataMini$n_farm_inputs / PredDataMini$WsAreaHa

PredDataMini$p_dev_inputs <- PredDataMini$p_dev_inputs / PredDataMini$WsAreaHa
PredDataMini$p_farm_inputs <- PredDataMini$p_farm_inputs / PredDataMini$WsAreaHa

# US basemap

remotes::install_github("mikejohnson51/AOI")
library(AOI)
conus <- AOI::aoi_get(state = "conus")
conus <- sf::st_transform(conus, 5072)

# Read in states to give some context
states <- states(cb = TRUE, progress_bar = FALSE)  %>%
  filter(!STUSPS %in% c('HI', 'PR', 'AK', 'MP', 'GU', 'AS', 'VI'))  %>%
  st_transform(crs = 5072)

# cyanobacteria modeling -------------------------------------------------------

variables <- c(names(model_cyano_nolakes$coefficients$fixed), 'DSGN_CYCLE', 'UNIQUE_ID', 'COMID', 'AG_ECO3')
variables <- variables[!variables %in% c('(Intercept)', 'AG_ECO3PLNLOW', 'AG_ECO3EHIGH')]

PredDataVar <- subset(PredDataMini, select = c(variables))

wbd_pred <- wbd_copy %>%
  st_point_on_surface() %>%
  dplyr::select(COMID) %>%
  inner_join(PredDataVar, by = 'COMID') %>%
  dplyr::select(-COMID) %>%
  drop_na()

Sys.time()
Pred <- predict(object = model_cyano_nolakes, newdata = wbd_pred,
                local = list(method = 'all', parallel = TRUE, ncores = 6),
                interval = 'confidence', level = 0.95)
Sys.time()

pred_df <- wbd_pred %>%
  mutate(pred_cyano = Pred,
         cyano_transform = 10^Pred - 1000)

# pred_df <- read_csv("C:/Users/mreyno04/OneDrive - Environmental Protection Agency (EPA)/Profile/Downloads/pred_df.csv", col_names = TRUE)

pred_df <- pred_df %>%
  mutate(disc_cyano = factor(case_when(pred_cyano < 4.41497 ~ 'B1', # under 25k
                                       pred_cyano >= 4.41497 & pred_cyano < 4.70757 ~ 'B2', # 25k - 50k
                                       pred_cyano >= 4.70757 & pred_cyano < 5.00432 ~ 'B3', # 50k - 100k
                                       pred_cyano >= 5.00432 & pred_cyano < 5.39967 ~ 'B4', # 100k - 250k
                                       pred_cyano >= 5.39967 & pred_cyano < 5.69984 ~ 'B5', # 250k-500k
                                       pred_cyano > 5.69984 ~ 'B6', # 500k
                                       TRUE ~ NA),
                             levels = c('B1', 'B2', 'B3', 'B4', 'B5', 'B6'))) %>%
  arrange(disc_cyano)

# summary(pred_df$disc_cyano)
# sum(is.na(pred_df$disc_cyano))
sub_25 <- pred_df %>%
  filter(disc_cyano == 'B1')


cyano_labels <- c('< 25', '25 - 50', '50 - 100', '100 - 250', '250 - 500', ' > 500')
cyano_colors <- c("#21618C","#5499C7","#A9CCE3","#EDBB99","#DC7633","#A04000")

# ggplot(pred_df, aes(color = disc_cyano)) +
#   geom_sf(size = 0.01,
#           alpha = 0.8) +
#   scale_color_manual(values = cyano_colors,
#                      labels = cyano_labels,
#                      name = "Cells/mL") +
#   labs(title = "Cyanobacteria Predictions") +
#   geom_sf(data = states, fill = NA, color = "black", lwd = 0.1) +
#   theme(plot.title = element_text(size = 12)) +
#   theme_classic() +
#   guides(colour = guide_legend(override.aes = list(size=4)))

ggplot() +
  geom_sf(data = pred_df,
          aes(color = disc_cyano),
          size = 0.2,
          alpha = 0.8) +
  scale_color_manual(values = cyano_colors,
                     labels = cyano_labels,
                     name = "Abundance (1000 cells/mL)") +
  geom_sf(data = sub_25,
          aes(color = disc_cyano),
          size = 0.2,
          alpha = 0.8)  +
  geom_sf(data = states, fill = NA, color = "black", lwd = 0.1) +
  theme_void() +
  theme(legend.position = c(0.80, 0.90),
        legend.text=element_text(size=14),
        legend.title=element_text(size=16)) +
  guides(color = guide_legend(ncol=2, override.aes = list(size=4, shape = 15)))

#save plot
ggsave("cyano_pred_3-13.jpeg", width = 12, height = 8, device = 'jpeg', dpi = 500)

# microcystin ------------------------------------------------------------------

m_variables <- c(names(model_micx_nolakes$coefficients$fixed), 'DSGN_CYCLE', 'UNIQUE_ID', 'COMID', 'AG_ECO3')
m_variables <- m_variables[!m_variables %in% c('(Intercept)', 'AG_ECO3PLNLOW', 'AG_ECO3EHIGH', 'AG_ECO3WMTNS')]

PredDataMicx <- subset(PredDataMini, select = c(m_variables))

micx_pred <- wbd_copy %>%
  st_point_on_surface() %>%
  dplyr::select(COMID) %>%
  inner_join(PredDataMicx, by = 'COMID') %>%
  dplyr::select(-COMID) %>%
  drop_na()

Sys.time()
Pred <- predict(object = model_micx_nolakes, newdata = micx_pred,
                local = list(method = 'all', parallel = TRUE, ncores = 12),
                interval = 'prediction', level = 0.95)
Sys.time()

micx_pred_df <- micx_pred %>%
  mutate(pred_micx = Pred,
         micx_transform = 10^Pred - 1000)

micx_pred_df$pred_micx <- boot::inv.logit(micx_pred_df$pred_micx)

micx_summary <- micx_pred_df |>
  dplyr::select(where(is.numeric)) |>


# mapping the data

micx_pred_df <- micx_pred_df %>%
  arrange(pred_micx)

labels = c("0-25", "25-50", "50-75", "75-100")
breaks <- c(0.25,0.50,0.75,1.0)
#cols <- c("#2B83BA","#ABDDA4", "#FDAE61", "#D7191C")
micx_colors <- c("#2980b9","#aed6f1","#f0b27a","#d35405")
#micx_colors2 <- c("#5499C7","#A9CCE3","#EDBB99","#DC7633")


ggplot(micx_pred_df, aes(color = pred_micx)) +
  geom_sf(size = 0.3) +
  scale_color_stepsn(colors = micx_colors,
                     breaks = breaks,
                     labels = labels,
                     name = "Probability of Detection (%)") +
  geom_sf(data = states, fill = NA, color = "black", lwd = 0.1) +
  theme_void() +
  theme(legend.position = c(0.80, 0.90),
        legend.text=element_text(size=14),
        legend.title=element_text(size=16)) +
  guides(color = guide_legend(ncol=2, override.aes = list(size=4, shape = 15)))


# save the map
ggsave("micx_pred_3-13.jpeg", width = 12, height = 8, device = 'jpeg', dpi = 500)

# individual variable mapping --------------------------------------------------

# lake depths

pred_df <- pred_df %>%
  mutate(disc_depth = factor(case_when(MAXDEPTH < 1 ~ 'B1',
                                       MAXDEPTH >= 1 & MAXDEPTH < 2 ~ 'B2',
                                       MAXDEPTH >= 2 & MAXDEPTH < 5 ~ 'B3',
                                       MAXDEPTH >= 5 & MAXDEPTH < 10 ~ 'B4',
                                       MAXDEPTH >= 10 & MAXDEPTH < 20 ~ 'B5',
                                       MAXDEPTH >= 20 & MAXDEPTH < 100 ~ 'B6',
                                       MAXDEPTH >= 100  ~ 'B7'),
                             levels = c('B1', 'B2', 'B3', 'B4', 'B5', 'B6','B7'))) %>%
  arrange(disc_depth)

depth_labels <- c('< 1m','1-2m', '2-5m', '5-10m', '10-20m', '20-100m', '> 100m')
depth_cols <- rev(RColorBrewer::brewer.pal(7, "Spectral"))


ggplot(pred_df, aes(color = disc_depth)) +
  geom_sf(size = 0.4) +
  scale_color_manual(values = depth_cols,
                     labels = depth_labels,
                     name = "Lake Depth") +
  labs(title = "Lake Depth Distribution") +
  geom_sf(data = states, fill = NA, color = "black", lwd = 0.1) +
  theme(plot.title = element_text(size = 12)) +
  guides(colour = guide_legend(override.aes = list(size=4)))

ggsave("lake_depths.jpeg", width = 12, height = 8, device = 'jpeg', dpi = 500)

# nitrogen mapping

pred_df <- pred_df %>%
  mutate(disc_n = factor(case_when(n_farm_inputs < 0.5 ~ 'B1',
                                       n_farm_inputs >= 0.5 & n_farm_inputs < 5 ~ 'B2',
                                       n_farm_inputs >= 5 & n_farm_inputs < 15 ~ 'B3',
                                       n_farm_inputs >= 15 & n_farm_inputs < 50 ~ 'B4',
                                       n_farm_inputs >= 50 & n_farm_inputs < 100 ~ 'B5',
                                       n_farm_inputs >= 100 & n_farm_inputs < 200 ~ 'B6',
                                       n_farm_inputs >= 200 & n_farm_inputs < 500 ~ 'B7',
                                       n_farm_inputs >= 500  ~ 'B8'),
                             levels = c('B1', 'B2', 'B3', 'B4', 'B5', 'B6', 'B7', 'B8'))) %>%
  arrange(disc_n)

n_labels <- c('<0.5','0.5-5','5-15','15-50','50-100','100-200','200-500','>500')
n_cols <- RColorBrewer::brewer.pal(8, "YlOrRd")

ggplot(pred_df, aes(color = disc_n)) +
  geom_sf(size = 0.75) +
  scale_color_manual(values = n_cols,
                     labels = n_labels,
                     name = "kg/ha/yr") +
  labs(title = "Nitrogen Farm Inputs") +
  geom_sf(data = states, fill = NA, color = "black", lwd = 0.1) +
  theme(plot.title = element_text(size = 12)) +
  guides(colour = guide_legend(override.aes = list(size=4)))

ggsave("nitrogen_map.jpeg", width = 12, height = 8, device = 'jpeg', dpi = 500)

# P inputs ---------------------------------------------------------------------

pred_df <- pred_df %>%
  mutate(disc_p = factor(case_when(p_dev_inputs < .0014 ~ 'B1',
                                   p_dev_inputs >= .0014 & p_dev_inputs < 0.0444 ~ 'B2',
                                   p_dev_inputs >= .0444 & p_dev_inputs < 0.2564 ~ 'B3',
                                   p_dev_inputs >= .2564 & p_dev_inputs < 1.0 ~ 'B4',
                                   p_dev_inputs >= 1.0 & p_dev_inputs < 5.0 ~ 'B5',
                                   p_dev_inputs >= 5.0  ~ 'B6'),
                         levels = c('B1', 'B2', 'B3', 'B4', 'B5', 'B6'))) %>%
  arrange(disc_p)

p_labels <- c(' < 0.001', '0.001 - 0.04', '0.04 - 0.25', '0.25 - 1.00', '1.00 - 5.00', '> 5.00')
p_cols <- RColorBrewer::brewer.pal(6, "YlOrRd")

ggplot(pred_df, aes(color = disc_p)) +
  geom_sf(size = 0.75) +
  scale_color_manual(values = p_cols,
                     labels = p_labels,
                     name = "kg/ha/yr") +
  labs(title = "Phosphorus Development Inputs") +
  geom_sf(data = states, fill = NA, color = "black", lwd = 0.1) +
  theme(plot.title = element_text(size = 12)) +
  guides(colour = guide_legend(override.aes = list(size=4)))

ggsave("phos_map.jpeg", width = 12, height = 8, device = 'jpeg', dpi = 500)

# Lake Fetch Mapping -----------------------------------------------------------

pred_df <- pred_df %>%
  mutate(disc_fetch = factor(case_when(lakemorpho_fetch >= 0 & lakemorpho_fetch < 400 ~ 'B1',
                                   lakemorpho_fetch >= 400 & lakemorpho_fetch < 800 ~ 'B2',
                                   lakemorpho_fetch >= 800 & lakemorpho_fetch < 1600 ~ 'B3',
                                   lakemorpho_fetch >= 1600 & lakemorpho_fetch < 3200 ~ 'B4',
                                   lakemorpho_fetch >= 3200 & lakemorpho_fetch < 4800 ~ 'B5',
                                   lakemorpho_fetch >= 4800  ~ 'B6'),
                         levels = c('B1', 'B2', 'B3', 'B4', 'B5', 'B6'))) %>%
  arrange(disc_fetch)

f_labels <- c('0-400', '400-800', '800-1600', '1600-3200', '3200-4800', '>4800')
f_cols <- rev(RColorBrewer::brewer.pal(6, "Spectral"))

ggplot(pred_df, aes(color = disc_fetch)) +
  geom_sf(size = 0.75) +
  scale_color_manual(values = f_cols,
                     labels = f_labels,
                     name = "Meters") +
  labs(title = "Lake Fetch Distribution") +
  geom_sf(data = states, fill = NA, color = "black", lwd = 0.1) +
  theme(plot.title = element_text(size = 12)) +
  guides(colour = guide_legend(override.aes = list(size=4)))

ggsave("fetch_map.jpeg", width = 12, height = 8, device = 'jpeg', dpi = 500)


# Lake Area Mapping ------------------------------------------------------------

for (Shape in 1:length(wbd_copy)) {
  shapes <- wbd_copy$Shape
  wbd_copy$custom_area <- st_area(shapes)
}

wbd_copy$custom_area <- drop_units(wbd_copy$custom_area)
area_df <- subset(wbd_copy, select = c('COMID', 'custom_area', 'Shape'))

#area_df$points <- st_point_on_surface(area_df$Shape)

area_pred <- area_df %>%
  st_point_on_surface() %>%
  dplyr::select(COMID) %>%
  inner_join(PredDataVar, by = 'COMID') %>%
  drop_na()

area_df <- st_join(area_pred, area_df)

area_df <- area_df %>%
  mutate(disc_area = factor(case_when(custom_area >= 0 & custom_area < 25000 ~ 'B1',
                                       custom_area >= 25000 & custom_area < 50000 ~ 'B2',
                                       custom_area >= 50000 & custom_area < 100000 ~ 'B3',
                                       custom_area >= 100000 & custom_area < 250000 ~ 'B4',
                                       custom_area >= 250000 & custom_area < 750000 ~ 'B5',
                                      custom_area >= 750000 & custom_area < 1000000 ~ 'B6',
                                       custom_area >= 1000000  ~ 'B7'),
                             levels = c('B1', 'B2', 'B3', 'B4', 'B5', 'B6', 'B7'))) %>%
arrange(disc_area)

a_labels <- c('0-10,000','10,000-50,000','50,000-100,000','100,000-250,000',
              '250,000-500,000','500,000-750,000','>750,000')
a_cols <- rev(RColorBrewer::brewer.pal(7, "Spectral"))

ggplot(area_df, aes(color = disc_area)) +
  geom_sf(size = 0.4) +
  scale_color_manual(values = a_cols,
                     labels = a_labels,
                     name = "Square Meters") +
  labs(title = "Lake Area Distribution") +
  geom_sf(data = states, fill = NA, color = "black", lwd = 0.1) +
  theme(plot.title = element_text(size = 12)) +
  guides(colour = guide_legend(override.aes = list(size=4)))

ggsave("area_map.jpeg", width = 12, height = 8, device = 'jpeg', dpi = 500)

# forest cover mapping ---------------------------------------------------------

pred_df <- pred_df %>%
  mutate(disc_fst = factor(case_when(fst_ws < 25 ~ 'B1',
                                      fst_ws >= 25 & fst_ws < 50 ~ 'B2',
                                      fst_ws >= 50 & fst_ws < 75 ~ 'B3',
                                      fst_ws >= 75 & fst_ws < 85 ~ 'B4',
                                      fst_ws >= 85 & fst_ws < 95 ~ 'B5',
                                      fst_ws >= 95  ~ 'B6'),
                            levels = c('B1', 'B2', 'B3', 'B4', 'B5', 'B6'))) %>%
  arrange(disc_fst)


fst_labels = c("0-25%", "25-50%", "50-75%", "75-85%", "85-95%", ">95%")
fst_cols <- RColorBrewer::brewer.pal(6, "YlGn")

ggplot(pred_df, aes(color = disc_fst)) +
  geom_sf(size = 0.75) +
  scale_color_manual(values = fst_cols,
                     labels = fst_labels,
                     name = "Cover (%)") +
  labs(title = "Forested Land Cover") +
  geom_sf(data = states, fill = NA, color = "black", lwd = 0.1) +
  theme(plot.title = element_text(size = 12)) +
  guides(colour = guide_legend(override.aes = list(size=4)))

ggsave("fst_cover.jpeg", width = 12, height = 8, device = 'jpeg', dpi = 500)


# comparison mapping -----------------------------------------------------------

library(cowplot)
library(biscale)

eco_lev3 <- raster::shapefile("C:/Users/mreyno04/OneDrive - Environmental Protection Agency (EPA)/Profile/Downloads/us_eco_l3/us_eco_l3.shp")
eco_lev3 <- st_as_sf(eco_lev3)
plot(eco_lev3)

eco_lev3 <- subset(eco_lev3, select = c('US_L3NAME','geometry'))

comp_df <- st_join(pred_df, micx_pred_df)
comp_df <- subset(comp_df, select = c('pred_micx', 'pred_cyano', 'cyano_transform', 'Shape'))

# create classes
comp_data <- bi_class(comp_df, x = pred_cyano, y = pred_micx, style = "quantile", dim = 3)

map <- ggplot(comp_data, aes(color = bi_class)) +
  geom_sf(size = 0.4) +
  guides(colour = guide_legend(override.aes = list(size=4)))


comp_data <- comp_data %>%
  mutate(bi_class = factor(bi_class))

# create map
cyano_map <- ggplot() +
  geom_sf(data = comp_data,
          mapping = aes(color = bi_class),
          size = 1,
          alpha = 0.5,
          shape = 15,
          show.legend = FALSE) +
  bi_scale_color(pal = "GrPink", dim = 3) +
  labs(title = "Microcystin vs Cyanobacteria") +
  geom_sf(data = states, fill = NA, color = "black", lwd = 0.1) +
  bi_theme(base_size = 12)

cyano_legend <- bi_legend(pal = "GrPink",
                    dim = 3,
                    xlab = "Higher Cyano Levels ",
                    ylab = "Higher Microcys Levels",
                    size = 6)

# combine map with legend
cyano_map <- ggdraw() +
  draw_plot(cyano_map) +
  draw_plot(cyano_legend, 0.1, 0.07, 0.2, 0.2)

ggsave("cyano_micx_map.jpeg", width = 12, height = 8, device = 'jpeg', dpi = 500)

mapview::mapview(pred_df)
pred_df %>%
  mapview(zcol = "pred_cyano", burst = TRUE)

# micx / nutrient mapping

comp_micx <- bi_class(comp_df, x = pred_micx, y = nutr_all, style = "quantile", dim = 2)

comp_micx <- comp_micx |>
  mutate(bi_class = factor(bi_class))

comp_micx |>
  filter(bi_class == '1-3') |>
  pull(nutr_all) |>
  summary()

# create map
micx_nutr_map <- ggplot() +
  geom_sf(data = comp_micx,
          mapping = aes(color = bi_class),
          size = 0.75,
          show.legend = FALSE) +
  bi_scale_color(pal = "Bluegill", dim = 2) +
  labs(title = "Nutrients vs Microcystin") +
  geom_sf(data = states, fill = NA, color = "black", lwd = 0.1) +
  bi_theme(base_size = 12)

micx_nutr_legend <- bi_legend(pal = "Bluegill",
                               dim = 2,
                               xlab = "Higher Micx Levels ",
                               ylab = "Higher Nutrient Levels",
                               size = 6)

# combine map with legend
micx_map <- ggdraw() +
  draw_plot(micx_nutr_map) +
  draw_plot(micx_nutr_legend, 0.1, 0.07, 0.2, 0.2)

ggsave("micx_nutr_map.jpeg", width = 12, height = 8, device = 'jpeg', dpi = 500)

# lake depth / cyano mapping

comp_df <- st_join(pred_df, micx_pred_df)
comp_df <- subset(comp_df, select = c('pred_micx', 'pred_cyano', 'cyano_transform', 'Shape', 'n_farm_inputs', 'p_dev_inputs', 'MAXDEPTH.x'))

comp_df$nutr_all <- comp_df$n_farm_inputs + comp_df$p_dev_inputs


comp_data <- bi_class(comp_df, x = pred_cyano, y = MAXDEPTH.x, style = "quantile", dim = 3)

comp_data <- comp_data |>
  mutate(bi_class = factor(bi_class))

# comp_data <- comp_data |>
#   arrange(desc(bi_class))
#
# comp_data |>
#   filter(bi_class == '2-2') |>
#   pull(nutr_all) |>
#   summary()

# create map
cyano_depth_map <- ggplot() +
  geom_sf(data = comp_data,
          mapping = aes(color = bi_class),
          size = 0.75,
          show.legend = FALSE) +
  bi_scale_color(pal = "BlueOr", dim = 3) +
  labs(title = "Lake Depth vs Cyanobacteria") +
  geom_sf(data = states, fill = NA, color = "black", lwd = 0.1) +
  bi_theme(base_size = 12)

cyano_depth_legend <- bi_legend(pal = "BlueOr",
                               dim = 3,
                               xlab = "Higher Cyano Levels ",
                               ylab = "Higher Depths",
                               size = 6)

# combine map with legend
cyano_depth_map <- ggdraw() +
  draw_plot(cyano_depth_map) +
  draw_plot(cyano_depth_legend, 0.1, 0.07, 0.2, 0.2)

ggsave("cyano_depth_map.jpeg", width = 12, height = 8, device = 'jpeg', dpi = 500)



# nitrogen inputs analysis -----------------------------------------------------

xs_n <- filter(pred_df, n_farm_inputs > 1000)

ggplot(xs_n, aes(color = n_farm_inputs)) +
  geom_sf(size = 1.5) +
  scale_color_viridis_c() +
  geom_sf(data = states, fill = NA, color = "black", lwd = 0.1)

xs_n <- xs_n %>%
  mutate(disc_cyano = factor(case_when(pred_cyano < 6.5 ~ 'B1', # under 1.5 million
                                       pred_cyano >= 6.5 & pred_cyano < 7.5 ~ 'B2', # 10k - 50k
                                       pred_cyano >= 7.5 & pred_cyano < 8.5 ~ 'B3', # 50k - 100k
                                       pred_cyano >= 8.5 & pred_cyano < 9.5 ~ 'B4', # 100k - 200k
                                       pred_cyano > 9.5 ~ 'B5', # above 1 mil
                                       TRUE ~ NA),
                             levels = c('B1', 'B2', 'B3', 'B4', 'B5'))) %>%
  arrange(disc_cyano)

# summary(pred_df$disc_cyano)
# sum(is.na(pred_df$disc_cyano))

xs_labels <- c('< 3 mil','3-30 mil', '30-300 mil','300 mil-1 bil','> 1 bil')
xs_col <- rev(RColorBrewer::brewer.pal(5, "Spectral"))

ggplot(xs_n, aes(color = disc_cyano)) +
  geom_sf(size = 2.5) +
  scale_color_manual(values = xs_col,
                     labels = xs_labels,
                     name = "Cells/mL") +
  labs(title = "Cyanobacteria Predictions") +
  geom_sf(data = states, fill = NA, color = "black", lwd = 0.1) +
  theme(plot.title = element_text(size = 12)) +
  guides(colour = guide_legend(override.aes = list(size=4)))

# mapping nutrient inputs

xs_n <- xs_n %>%
  mutate(disc_nitro = factor(case_when(n_farm_inputs < 1050 ~ 'B1',
                                       n_farm_inputs >= 1050 & n_farm_inputs < 1200 ~ 'B2',
                                       n_farm_inputs >= 1200 & n_farm_inputs < 1400 ~ 'B3',
                                       n_farm_inputs >= 1400 & n_farm_inputs < 2000 ~ 'B4',
                                       n_farm_inputs >= 2000 ~ 'B5'),
                             levels = c('B1', 'B2', 'B3', 'B4', 'B5'))) %>%
  arrange(disc_nitro)

nitr_labels <- c('< 1050','1000-1200', '1200-1400','1400-2000','> 2000')
nitr_col <- RColorBrewer::brewer.pal(5, "YlOrRd")

ggplot(xs_n, aes(color = disc_nitro)) +
  geom_sf(size = 2) +
  scale_color_manual(values = nitr_col,
                     labels = nitr_labels,
                     name = "kg/ha/yr") +
  labs(title = "Nitrogen Farm Inputs (> 1000)") +
  geom_sf(data = states, fill = NA, color = "black", lwd = 0.1) +
  theme(plot.title = element_text(size = 12)) +
  guides(colour = guide_legend(override.aes = list(size=4)))


ggsave("k_nutr_map.jpeg", width = 12, height = 8, device = 'jpeg', dpi = 500)


# phosphorus inputs

xs_p <- filter(PredDataMini, n_dev_inputs > 1000)

# Oregon Mapping ==============================================================

OR <- states(cb = TRUE, progress_bar = FALSE)  %>%
  filter(STUSPS %in% c("OR"))  %>%
  st_transform(crs = 5070) # change crs to match pred_df

or_shp <- counties(state = 'Oregon')

or_df <- st_join(pred_df, states)

or_df <- or_df |>
  filter(STUSPS == 'OR') %>%
  st_transform(crs = 4326)

ggplot(or_df, aes(color = disc_cyano)) +
  geom_sf(size = 2) +
  scale_color_manual(values = c("#9f07f7", "#2B83BA", "#ABDDA4", "#f7d577", "#FDAE61","#D7191C"),
                     labels = cyano_labels,
                     name = "Cells/mL") +
  labs(title = "Oregon Cyanobacteria Predictions") +
  geom_sf(data = OR, fill = NA, color = "black", lwd = 0.1) +
  theme(plot.title = element_text(size = 12)) +
  guides(colour = guide_legend(override.aes = list(size=4)))

ggsave("cyano_or_state.jpeg", width = 12, height = 8, device = 'jpeg', dpi = 500)

# State Count ==================================================================

states <- states(cb = TRUE, progress_bar = FALSE)  %>%
  filter(!STUSPS %in% c('HI', 'PR', 'AK', 'MP', 'GU', 'AS', 'VI'))  %>%
  st_transform(crs = 5070)

state_df <- st_join(pred_df, states)

state_count <- state_df |>
  count(STUSPS)

ggplot(state_count, aes(color = n)) +
  geom_sf(size = 2) +
  labs(title = "Oregon Cyanobacteria Predictions") +
  geom_sf(data = states, fill = NA, color = "black", lwd = 0.1)

state_exp <- subset(state_count, select = -c(Shape))

write.csv(state_exp, file = 'state_count.csv')





