
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

PredData07_05Ws <- read.csv("C:/Users/mreyno04/OneDrive - Environmental Protection Agency (EPA)/Profile/REPOS/LakePredData/PredData/PredData07_05Ws.csv")
PredData07_07Ws <- read.csv("C:/Users/mreyno04/OneDrive - Environmental Protection Agency (EPA)/Profile/REPOS/LakePredData/PredData/PredData07_05Ws.csv")
PredData07_10Ws <- read.csv("C:/Users/mreyno04/OneDrive - Environmental Protection Agency (EPA)/Profile/REPOS/LakePredData/PredData/PredData07_10Ws.csv")
PredData12_05Ws <- read.csv("C:/Users/mreyno04/OneDrive - Environmental Protection Agency (EPA)/Profile/REPOS/LakePredData/PredData/PredData12_05Ws.csv")
PredData12_07Ws <- read.csv("C:/Users/mreyno04/OneDrive - Environmental Protection Agency (EPA)/Profile/REPOS/LakePredData/PredData/PredData12_07Ws.csv")
PredData12_10Ws <- read.csv("C:/Users/mreyno04/OneDrive - Environmental Protection Agency (EPA)/Profile/REPOS/LakePredData/PredData/PredData12_10Ws.csv")

# function set up

verify <- function(df){
  print(which(df$wbCOMID == "487"))
}

mean_group <- function(df){
  df %>%
    group_by(across(wbCOMID)) %>%
    summarise(across(where(is.numeric), mean))
}

incorp <- function(df){
  df <- subset(df, select = -c(CatAreaSqKm,WsAreaSqKm,CatPctFull,WsPctFull,inStreamCat))
  merge(PredDataMas, df, by = 'COMID')
}


# Combine 2007 and 2012 Data -----------------------------------------------

PredData2007 <- PredData07_05Ws %>%
  bind_rows(PredData07_07Ws) %>%
  bind_rows(PredData07_10Ws)
verify(PredData2007)

PredData2007 <- mean_group(PredData2007)


# 2012

PredData2012 <- PredData12_05Ws %>%
  bind_rows(PredData12_07Ws) %>%
  bind_rows(PredData12_10Ws)
verify(PredData2012)

PredData2012 <- mean_group(PredData2012)
verify(PredData2012)


# Create the Master Dataset ------------------------------------------------

PredDataMas <- PredData2007 %>%
  bind_rows(PredData2012)
verify(PredDataMas)

PredDataMas <- mean_group(PredDataMas)
verify(PredDataMas)

names(PredDataMas)[names(PredDataMas) == "wbCOMID"] <- "COMID"

head(PredDataMas)

# Combine Datasets -------------------------------------------------------

pesticides <- read.csv("C:/Users/mreyno04/OneDrive - Environmental Protection Agency (EPA)/Profile/REPOS/LakePredData/PredData/lakecat-metrics-melanie/Pesticides97.csv")
BFI <- read.csv("C:/Users/mreyno04/OneDrive - Environmental Protection Agency (EPA)/Profile/REPOS/LakePredData/PredData/lakecat-metrics-melanie/BFI.csv")
PRISM <- read.csv("C:/Users/mreyno04/OneDrive - Environmental Protection Agency (EPA)/Profile/REPOS/LakePredData/PredData/lakecat-metrics-melanie/PRISM_1991_2020.csv")
runoff <- read.csv("C:/Users/mreyno04/OneDrive - Environmental Protection Agency (EPA)/Profile/REPOS/LakePredData/PredData/lakecat-metrics-melanie/runoff.csv")
rockN <- read.csv("C:/Users/mreyno04/OneDrive - Environmental Protection Agency (EPA)/Profile/REPOS/LakePredData/PredData/lakecat-metrics-melanie/RockN.csv")
eco3 <- read.csv("C:/Users/mreyno04/OneDrive - Environmental Protection Agency (EPA)/Profile/REPOS/LakePredData/NLA_SampleFrame_L3L4Ecoregions 1.csv")

names(BFI)[names(BFI) == "BFIWs"] <- "BFIWs.Str" # USGS BFIWs compiled up to 2003
names(PredDataMas)[names(PredDataMas) == "BFIWs"] <- "BFIWs.Nutr" # BFIWs 2007-2012 data compiled

names(eco3)[names(eco3) == "comid"] <- "COMID"

PredDataMas <- incorp(pesticides)
PredDataMas <- incorp(BFI)
PredDataMas <- incorp(PRISM)
PredDataMas <- incorp(runoff)
PredDataMas <- incorp(rockN)

PredDataMas = subset(PredDataMas, select = -c(RunoffWs.x, X))
PredDataMas <- merge(PredDataMas, eco3, by = 'COMID')

# names(PredDataMas)[names(PredDataMas) == "BFIWs.2003"] <- "BFIWs"
# names(PredDataMas)[names(PredDataMas) == "RunoffWs.y"] <- "Runoff"

# Data Validation --------------------------------------------------------------

mean(PredDataMas$Precip_YrMean, na.rm = TRUE)
(mean(PredData2007$Precip_YrMean, na.rm = TRUE) + mean(PredData2012$Precip_YrMean, na.rm = TRUE)) / 2

mean(PredDataMas$Total.Input, na.rm = TRUE)
(mean(PredData2007$Total.Input, na.rm = TRUE) + mean(PredData2012$Total.Input, na.rm = TRUE)) / 2

# plotting
plot(PredDataMas$Tmean9120Ws, PredDataMas$Precip_YrMean, xlab = "Mean Temp 1991-2020",
     ylab = " Mean Precip")

plot(PredDataMas$Tmean9120Ws, PredDataMas$SNOW_YrMean, xlab = "Mean Temp 1991-2020",
     ylab = " Mean Snowfall")

# spearman correlation
cor(PredDataMas$Pestic97Ws, PredDataMas$P_Accumulated_ag_inputs_2007,
    method = "spearman", use = "pairwise.complete.obs")

cor(PredDataMas$Tmean9120Ws, PredDataMas$SNOW_YrMean,
    method = "spearman", use = "pairwise.complete.obs")

cor(PredDataMas$Tmean9120Ws, PredDataMas$COMID,
    method = "spearman", use = "pairwise.complete.obs") # should not be correlated

# Lake Depth Data --------------------------------------------------------------

loc <- "O:/PRIV/CPHEA/PESD/COR/CORFILES/Geospatial_Library_Resource/Physical/HYDROLOGY/NHDPlusV21/NHDPlusNationalData/NHDPlusV21_National_Seamless_Flattened_Lower48.gdb"

wbd <- sf::st_read(dsn = loc, layer = "NHDWaterbody") %>%
  st_transform(5072)

# This just worked for like no reason, not sure what was changed but it worked kind of

wbd_copy <- subset(wbd, COMID %in% PredDataMas$COMID)
# wbd_copy <- wbd %>%
#   slice(1:30)

# wbd_list <- split(wbd_copy[1:50,], 1:50)

testtest <- wbd[wbd$COMID == "15985627",] #confirmed high elevation

lake_elev <- get_elev_raster(testtest, z = 8, prj = st_crs(wbd), expand = 10)
# lake_lm <- lakeSurroundTopo(testtest, lake_elev)
temp_lakemorpho <- lakeSurroundTopo(as_Spatial(testtest), lake_elev)

lake_meandepth <- lakeMeanDepth(temp_lakemorpho, correctFactor = 1)
lake_maxdepth <- lakeMaxDepth(temp_lakemorpho, correctFactor = 1)
# lake_maxdepth <- lakeMaxDepth_func(temp_lakemorpho, correctFactor = 1)

# data.frame(COMID = wbd_copy$COMID, lake_maxdepth)

plot(lake_elev)
raster(lake_elev)

# test fetch
fetch <- lakeFetch(lake_lm, bearing = -10, addLine = TRUE)

lake_elev_ras <- projectRaster(lake_elev,
                                   crs = 5072)

# make raster polygon

elev_poly <- rasterToPolygons(lake_elev)

st_crs(testtest)

elev_sf <- st_as_sf(elev_poly) %>%
  st_transform(5072)

summary(sf::st_intersects(elev_sf, testtest, sparse = FALSE)) # check if overlapping
# 41 overlapping cells

# bad code? function not working

# morph_it <- function(df){
#   #st_drop_geometry(df)
#   lake_elev <- get_elev_raster(df, z = 8, prj = st_crs(df), expand = 100)
#   lake_lm <- lakeSurroundTopo(df, lake_elev)
#   lake_maxdepth <- lakeMaxDepth(lake_lm, correctFactor = 0.4)
#   data.frame(COMID = df$COMID, lake_maxdepth)
# }
#
# lake_depth <- lapply(wbd_copy, morph_it)
# lake_depth_df <- bind_rows(lake_depth)
# lake_depth_df
#
# PredDataMas <- merge(PredDataMas, lake_depth_df, by = 'COMID')

# found this lake analysis theme online- helpful!
# also based on online data for big creek lake, colorado lake area is in square meters
lake_theme = ggplot2::theme(axis.text = ggplot2::element_text(size=12),
                            panel.background = ggplot2::element_rect(fill="white"),
                            panel.grid = ggplot2::element_line(color="black"),
                            axis.text.x = ggplot2::element_text(angle = 90))

ggplot2::ggplot() +
  ggplot2::geom_sf(data = testtest$Shape) +
  lake_theme #lake looks perfect!

ggplot2::ggplot() +
  ggplot2::geom_sf(data = elev_sf) +
  lake_theme

# lake morpho function test

lakeMaxDepth_func <- function(inLakeMorpho, slope_quant = 0.5, correctFactor = 1) {
  if (!inherits(inLakeMorpho, "lakeMorpho")) {
    stop("Input data is not of class 'lakeMorpho'.  Run lakeSurround Topo or lakeMorphoClass first.")
  }
  if(is.null(inLakeMorpho$elev)){
    warning("Input elevation dataset required to estimate depth related metrics. Returning NA.
             Run lakeSurround Topo first with elevation included.")
    return(NA)
  }
  slope <- raster::getValues(terrain(inLakeMorpho$elev, "slope"))
  slope_med <- as.numeric(quantile(slope, probs = slope_quant, na.rm = TRUE))
  if (is.na(slope_med)) {
    return(NA)
  }
  if (slope_med == 0) {
    slope_med <- mean(slope, na.rm = TRUE)
  }
  maxDist <- max(raster::getValues(inLakeMorpho$lakeDistance), na.rm = TRUE)
  return(round(correctFactor * (slope_med * maxDist), 4))
}

rm(lakeMaxDepth)
raster(lake_elev)


