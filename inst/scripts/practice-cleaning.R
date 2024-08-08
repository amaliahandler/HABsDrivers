
install.packages("lakemorpho")
install.packages("elevatr")
install.packages("raster")
install_github("USEPA/StreamCatTools", build_vignettes=FALSE, auth_token= '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', force = TRUE)

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


# Combine the 2007 and 2012 data --------------------------------------------------------------------------

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


# Create the multi-year dataset --------------------------------------------------------------------------

PredDataMas <- PredData2007 %>%
  bind_rows(PredData2012)
verify(PredDataMas)

PredDataMas <- mean_group(PredDataMas)
verify(PredDataMas)

names(PredDataMas)[names(PredDataMas) == "wbCOMID"] <- "COMID"

head(PredDataMas)

# Combine other datasets --------------------------------------------------------------------------------

pesticides <- read.csv("C:/Users/mreyno04/OneDrive - Environmental Protection Agency (EPA)/Profile/REPOS/LakePredData/PredData/lakecat-metrics-melanie/Pesticides97.csv")
BFI <- read.csv("C:/Users/mreyno04/OneDrive - Environmental Protection Agency (EPA)/Profile/REPOS/LakePredData/PredData/lakecat-metrics-melanie/BFI.csv")
PRISM <- read.csv("C:/Users/mreyno04/OneDrive - Environmental Protection Agency (EPA)/Profile/REPOS/LakePredData/PredData/lakecat-metrics-melanie/PRISM_1991_2020.csv")
runoff <- read.csv("C:/Users/mreyno04/OneDrive - Environmental Protection Agency (EPA)/Profile/REPOS/LakePredData/PredData/lakecat-metrics-melanie/runoff.csv")
rockN <- read.csv("C:/Users/mreyno04/OneDrive - Environmental Protection Agency (EPA)/Profile/REPOS/LakePredData/PredData/lakecat-metrics-melanie/RockN.csv")

names(BFI)[names(BFI) == "BFIWs"] <- "BFIWs.2003" # USGS BFIWs compiled up to 2003
names(PredDataMas)[names(PredDataMas) == "BFIWs"] <- "BFIWs.2012" # BFIWs 2007-2012 data compiled

PredDataMas <- incorp(pesticides)
PredDataMas <- incorp(BFI)
PredDataMas <- incorp(PRISM)
PredDataMas <- incorp(runoff)
PredDataMas <- incorp(rockN)

PredDataMas = subset(PredDataMas, select = -c(RunoffWs.x, BFIWs.2012))

names(PredDataMas)[names(PredDataMas) == "BFIWs.2003"] <- "BFIWs"
names(PredDataMas)[names(PredDataMas) == "RunoffWs.y"] <- "Runoff"

# Data Validation --------------------------------------------------------------------------------------

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


