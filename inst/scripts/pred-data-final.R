# HABS Predictor Dataset =======================================================

# install packages, load libraries

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

# compile 2007 and 2012 nutrient inventory data --------------------------------

nutr_inv_directory <- "C:/Users/mreyno04/OneDrive - Environmental Protection Agency (EPA)/Profile/REPOS/HABsDrivers/inst/source_data/nutr_inventory"
nutr_pred_data <- fs::dir_ls(nutr_inv_directory, regexp = "\\.csv$")

#compile files into a single data frame
PredData <- nutr_pred_data |>
  map_dfr(read.csv)

PredData <- PredData |>
  group_by(across(wbCOMID)) |>
  summarise(across(where(is.numeric), mean)) |>
  rename(COMID = wbCOMID) |>
  dplyr::select(-catCOMID) |>
  dplyr::select(c(N_Fert_Farm_2007,N_CBNF_2007, BFIWs, LAGOSLakeDepth, NHDLakeDepth, COMID))

# Combine Datasets -------------------------------------------------------------

# PRISM data derived from Stream Cat ------
PRISM <- read.csv("C:/Users/mreyno04/OneDrive - Environmental Protection Agency (EPA)/Profile/REPOS/HABsDrivers/inst/source_data/PRISM_1991_2020.csv")

PredData <- left_join(PredData, dplyr::select(PRISM, Tmean9120Ws, Precip9120Ws, COMID), by = 'COMID')

# Ecoregions data, derived from NLA ------
eco3 <- read.csv("C:/Users/mreyno04/OneDrive - Environmental Protection Agency (EPA)/Profile/REPOS/HABsDrivers/inst/source_data/NLA_SampleFrame_L3L4Ecoregions1.csv")

eco3 <- eco3 |>
  rename(COMID = comid)

PredData <- left_join(PredData, dplyr::select(eco3, ag_eco3, COMID), by = 'COMID')

# Land Cover Data --------------------------------------------------------------

get_nlcd <- function(coms){
  lc_get_data(metric = 'PctWdWet2016, PctUrbMd2016, PctUrbLo2016, PctUrbHi2016,
                          PctMxFst2016, PctCrop2016, PctHay2016, PctDecid2016,
                          PctConif2016, PctUrbOp2016, PctHbWet2016',
              aoi='watershed',
              comid = coms,
              showAreaSqKm = TRUE)
}

chunks <- split(PredData$COMID, ceiling(seq_along(PredData$COMID) / 10000))

ncldMas <- do.call(rbind, lapply(chunks, get_nlcd))

# ncldMas <- ncldMas |>
#   dplyr::select(-WSAREASQKM)

ncldMas <- ncldMas |>
  mutate(agr_ws = PCTCROP2016WS + PCTHAY2016WS,
         dev_ws = PCTURBLO2016WS + PCTURBMD2016WS + PCTURBOP2016WS + PCTURBHI2016WS,
         fst_ws = PCTMXFST2016WS + PCTDECID2016WS + PCTCONIF2016WS,
         wet_ws = PCTWDWET2016WS + PCTHBWET2016WS,
         COMID = COMID,
         .keep = 'unused')

PredData <- ncldMas |>
  dplyr::select(COMID, fst_ws, WSAREASQKM) |>
  merge(PredData, by = 'COMID')

# Water Body Data --------------------------------------------------------------

loc <- "O:/PRIV/CPHEA/PESD/COR/CORFILES/Geospatial_Library_Resource/Physical/HYDROLOGY/NHDPlusV21/NHDPlusNationalData/NHDPlusV21_National_Seamless_Flattened_Lower48.gdb"

wbd <- sf::st_read(dsn = loc, layer = "NHDWaterbody") |>
  st_transform(5072)

wbd_copy <- wbd |>
  subset(COMID %in% PredData$COMID) |>
  dplyr::select(COMID, MaxDepth, Shape)

PredData <- merge(PredData, wbd_copy, by = 'COMID')

# Nutrient Data Compiling ------------------------------------------------------

# load files from folder
data_dir <- "C:/Users/mreyno04/OneDrive - Environmental Protection Agency (EPA)/Profile/REPOS/HABsDrivers/inst/source_data/nutrient_inputs"
csv_files <- fs::dir_ls(data_dir)

# compile files into a single data frame
nutrMas <- csv_files |>
  map_dfr(read_csv)

nutrMas <- nutrMas |>
  dplyr::select(-contains(c('Cat'))) |>
  rowwise() |>
  mutate(COMID = COMID,
         n_cbnf = N_CBNF_kg_Ag_2002Ws,
         n_livestock = mean(c(N_Livestock.Waste_kg_Ag_2002Ws, N_Livestock.Waste_kg_Ag_2007Ws, N_Livestock.Waste_kg_Ag_2012Ws)),
         p_livestock = mean(c(P_livestock_Waste_kg_Ag_2002Ws, P_livestock_Waste_kg_Ag_2007Ws, P_livestock_Waste_kg_Ag_2012Ws)),
         p_f_fert = mean(c(P_f_fertilizer_kg_Ag_2002Ws, P_f_fertilizer_kg_Ag_2007Ws, P_f_fertilizer_kg_Ag_2012Ws)),
         n_human = mean(c(N_Human_Waste_kg_Urb_2002Ws,N_Human_Waste_kg_Urb_2007Ws, N_Human_Waste_kg_Urb_2012Ws)),
         p_human = mean(c(P_human_waste_kg_Urb_2002Ws, P_human_waste_kg_Urb_2007Ws, P_human_waste_kg_Urb_2012Ws)),
         n_fert_urb = mean(c(N_Fert_Urban_kg_Urb_2002Ws, N_Fert_Urban_kg_Urb_2007Ws, N_Fert_Urban_kg_Urb_2012Ws)),
         p_nf_fert = mean(c(P_nf_fertilizer_kg_Urb_2002Ws, P_nf_fertilizer_kg_Urb_2007Ws, P_nf_fertilizer_kg_Urb_2012Ws)),
         .keep = 'none')

PredData <- merge(PredData, nutrMas, by = 'COMID')

PredData <- PredData |>
  mutate(n_cbnf = ((n_cbnf + N_CBNF_2007)/2),
         .keep = 'unused')

# Lake Depth -------------------------------------------------------------------

lake_met_dir <- "C:/Users/mreyno04/OneDrive - Environmental Protection Agency (EPA)/Profile/REPOS/HABsDrivers/inst/source_data/lake_metrics"
lake_met_files <- fs::dir_ls(lake_met_dir, regexp = "\\.csv$")

#compile files into a single data frame
lake_met_df <- lake_met_files |>
  map_dfr(read.csv)

lake_met_df <- lake_met_df |>
  distinct(COMID, .keep_all = TRUE)

# Depth Creation

# met_comids <- lake_met_df$COMID
#
#
# get_morpho_obj <- function(com, df){
#   lake_com <- filter(df, COMID == com)
#   lake_elev <- get_elev_raster(lake_com, z = 13, prj = st_crs(df), expand = 100, override_size_check = TRUE)
#   lake_lm <- lakeSurroundTopo(lake_com, lake_elev)
#   saveRDS(lake_lm, file = paste0('C:/Users/mreyno04/OneDrive - Environmental Protection Agency (EPA)/Profile/REPOS/HABsDrivers/test_depths/', com, ".rds"))
# }
#
# #run function to the missing depths COMIDs
# depths <- lapply(met_comids, get_morpho_obj, lake_met_df)
#
# #load lake morpho object files from folder
# data_dir <- "./private/lake_morpho_objects/"
# lm_files <- dir_ls(data_dir, regexp = "\\.rds$")
#
# #function to pull files from lake morpho objects into metrics to compile into df
# morph_it <- function(file_name) {
#   morpho_obj <- readRDS(file_name)
#
#   if (class(morpho_obj) == 'lakeMorpho') {
#     max_depth <- lakemorpho::lakeMaxDepth(morpho_obj, correctFactor = 0.6)
#     lake_fetch <- lakeMaxLength(morpho_obj, pointDens = 50)
#     COMID <- morpho_obj$lake$COMID
#
#     output <- data.frame(COMID = COMID, depth = max_depth)
#     saveRDS(output, file = paste0('./private/friday_metrics/lake_metric', COMID, ".rds"))
#   } else {
#     message("Skipping file: ", file_name)
#   }
# }
#
# l <- lapply(lm_files, morph_it)
#
# #pull metrics data to create final df
# met_dir <- "./private/metrics/"
# met_files <- fs::dir_ls(met_dir, regexp = "\\.rds$")
#
# #compile files into a single data frame
# lake_met_df <- met_files |>
#   map_dfr(readRDS)

#compile depths into one column using hierarchy to give preference to LAGOS and NHD populated depths

PredData <- left_join(PredData, lake_met_df, by = 'COMID')

PredData$LAGOSLakeDepth <- replace(PredData$LAGOSLakeDepth, which(PredData$LAGOSLakeDepth <= 0), NA)
PredData$NHDLakeDepth <- replace(PredData$NHDLakeDepth, which(PredData$NHDLakeDepth <= 0), NA)
PredData$MaxDepth <- replace(PredData$MaxDepth, which(PredData$MaxDepth <= 0), NA)

PredData$lake_m_depth <- PredData$LAGOSLakeDepth
PredData$lake_m_depth <- coalesce(PredData$lake_m_depth, PredData$NHDLakeDepth)
PredData$lake_m_depth <- coalesce(PredData$lake_m_depth, PredData$depth)

PredData <- PredData |>
  distinct(COMID, .keep_all = TRUE) |>
  filter(!is.na(lake_m_depth))

# fetch data

fetch <- read.csv("C:/Users/mreyno04/OneDrive - Environmental Protection Agency (EPA)/Profile/REPOS/HABsDrivers/inst/source_data/fetch_df_21.csv")

PredData <- left_join(PredData, fetch, by = 'COMID')

PredData$fetch <- replace(PredData$fetch, which(PredData$fetch <= 0), NA)
PredData$fetch <- coalesce(PredData$fetch, PredData$max_length)

PredData <- PredData |>
  filter(!is.na(fetch))

# Modeling ---------------------------------------------------------------------

PredData <- PredData |>
  mutate(n_farm_inputs = n_livestock + n_cbnf + N_Fert_Farm_2007,
         n_dev_inputs = n_human + n_fert_urb,
         p_farm_inputs = p_f_fert + p_livestock,
         p_dev_inputs = p_human + p_nf_fert,
         .keep = 'unused') |>
  dplyr::select(-c(X.x,X.y))

PredData$DSGN_CYCLE <- 2017
PredData <- mutate(PredData, DSGN_CYCLE = factor(DSGN_CYCLE))

unique_ids <- read_csv('C:/Users/mreyno04/OneDrive - Environmental Protection Agency (EPA)/Profile/Downloads/NLA071217-COMID_crosswalk(in).csv')

PredData <- left_join(PredData, unique_ids, by = 'COMID')
PredData$UNIQUE_ID[is.na(PredData$UNIQUE_ID)] <- 0

PredData <- PredData %>%
  distinct(COMID, .keep_all = TRUE)

PredData <- subset(PredData, select = -c(...1))

PredData <- PredData |>
  mutate(WSAREAHA = WSAREASQKM * 100,
         n_dev_inputs = n_dev_inputs / WSAREAHA,
         n_farm_inputs = n_farm_inputs / WSAREAHA,
         p_dev_inputs = p_dev_inputs / WSAREAHA,
         p_farm_inputs = p_farm_inputs / WSAREAHA) |>
  rename(Tmean8110Ws = Tmean9120Ws,
         Precip8110Ws = Precip9120Ws,
         MAXDEPTH = lake_m_depth,
         lakemorpho_fetch = fetch,
         AG_ECO3 = ag_eco3) |>
  dplyr::select(c(COMID, n_dev_inputs, n_farm_inputs, p_dev_inputs, p_farm_inputs,
                  BFIWs, MAXDEPTH,lakemorpho_fetch,
                  Precip8110Ws, Tmean8110Ws,
                  fst_ws, AG_ECO3,
                  DSGN_CYCLE, UNIQUE_ID)) |>
  drop_na()






