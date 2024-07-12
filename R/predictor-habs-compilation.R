
# Predictor / HABs dataset compilation

# ~ I'm not proud of how messy this is right now it's just a work in progress ~

library(devtools)
library(dplyr)
library(ggplot2)
library(scales)
library(ggpubr)
library(tidyr)
library(stars)

# create mean of 2002/2007/2012 nutrient inventories, 2002 nutrient inventories from Meredith
# reach out to meredith to see if she has the 2002 data for the same locations, create mean of all variables if possible

# Anticipated Steps:
# 1. wb(COMID)
#      - national hydrology data set
#      - determine how combining variables should be done
#      - remove NLA data during cleaning
# 2. See what variables we have left over to work with
# 3. fill in gaps of missing usable data between the three years
#      - retrieve information from lakecat, streamtools API package to possibly retrieve data
#      - Marc or Ryan can help if having issues
# lakecat sticker
# 4. amend model to employ existing variables

habs <- habs |>
  # Robert suggested using agricultural inputs rather than land cover
  mutate(n_farm_inputs = N_Fert_Farm + N_CBNF + N_livestock_Waste,
         n_dev_inputs = N_Human_Waste + N_Fert_Urban,
         p_farm_inputs = P_f_fertilizer + P_livestock_Waste,
         p_dev_inputs = P_human_waste_kg + P_nf_fertilizer) |> # ,
  # nfarm_inputs_pres = ifelse(n_farm_inputs == 0, 0, 1),
  # Creating a categorical variable for lake depth
  mutate(lake_dep = ifelse(MAXDEPTH <= 10, "shallow", "deep"))

model_cyano_nolakes <- readRDS('./inst/model_objects/model_cyano_nolakedata.rds')
model_micx_nolakes <- readRDS('./inst/model_objects/model_micx_nolakedata.rds')

model_cyano_nolakedata$formula
model_micx_nolakes$formula

# needed variables <- B_G_DENS + BFIWs + Tmean8110Ws + Precip8110Ws +
# n_farm_inputs + n_dev_inputs + p_farm_inputs + lake_dep +
# lakemorpho_fetch
# MICX_DET ~ p_farm_inputs + fst_ws + Precip_Minus_EVTWs + MAXDEPTH +
# lakemorpho_fetch + BFIWs + AG_ECO3


# ================= ACTUAL CODE COMP ===================

# loading and compiling Meredith's predictor datasets

PredData07_05Ws <- read.csv("C:/Users/mreyno04/OneDrive - Environmental Protection Agency (EPA)/Profile/REPOS/LakePredData/PredData/PredData07_05Ws.csv")
head(PredData07_05Ws) # looks good
PredData07_07Ws <- read.csv("C:/Users/mreyno04/OneDrive - Environmental Protection Agency (EPA)/Profile/REPOS/LakePredData/PredData/PredData07_05Ws.csv")
PredData07_10Ws <- read.csv("C:/Users/mreyno04/OneDrive - Environmental Protection Agency (EPA)/Profile/REPOS/LakePredData/PredData/PredData07_10Ws.csv")
PredData12_05Ws <- read.csv("C:/Users/mreyno04/OneDrive - Environmental Protection Agency (EPA)/Profile/REPOS/LakePredData/PredData/PredData12_05Ws.csv")
PredData12_07Ws <- read.csv("C:/Users/mreyno04/OneDrive - Environmental Protection Agency (EPA)/Profile/REPOS/LakePredData/PredData/PredData12_07Ws.csv")
PredData12_10Ws <- read.csv("C:/Users/mreyno04/OneDrive - Environmental Protection Agency (EPA)/Profile/REPOS/LakePredData/PredData/PredData12_10Ws.csv")

# combine the data by 2007 and 2012 first

colnames(PredData07_10Ws) #check column names to see which ones we want to take with

# averaging the 2007 three data sets
PredData2007 <- bind_rows(PredData07_05Ws, PredData07_07Ws, PredData07_10Ws) %>%
  group_by(wbCOMID) %>%
  summarise(X= mean(X, na.rm= TRUE),
            NPP_YrMean = mean(NPP_YrMean, na.rm= TRUE),
            NPP = mean(NPP, na.rm=TRUE),
            LST_YrMean = mean(LST_YrMean, na.rm=TRUE),
            Tot_Ndep_2007 = mean(Tot_Ndep_2007, na.rm=TRUE),
            Total.Input = mean(Total.Input, na.rm=TRUE),
            N_Fert_Farm_2007 = mean(N_Fert_Farm_2007, na.rm=TRUE),
            RunoffWs = mean(RunoffWs, na.rm=TRUE),
            Atmo_Pdep_2007 = mean(Atmo_Pdep_2007, na.rm=TRUE),
            P_Accumulated_ag_inputs_2007 = mean(P_Accumulated_ag_inputs_2007, na.rm=TRUE),
            nani = mean(nani, na.rm=TRUE),
            Precip_YrMean = mean(Precip_YrMean, na.rm=TRUE),
            Tot_Sdep_2007 = mean(Tot_Sdep_2007, na.rm=TRUE),
            N_CBNF_2007 = mean(N_CBNF_2007, na.rm=TRUE),
            WsAreaSqKm = mean(WsAreaSqKm, na.rm=TRUE),
            BFIWs = mean(BFIWs, na.rm=TRUE),
            NHDLakeDepth = mean(NHDLakeDepth, na.rm=TRUE)
            )

# excluded variables during the merge
# ClayWs, SandWs, SNOW_YrMean, catCOMID

head(PredData2007)

# averaging the 2012 three data sets
PredData2012 <- bind_rows(PredData12_05Ws, PredData12_07Ws, PredData12_10Ws) %>% # maybe make a loop that does this instead melanie
  group_by(wbCOMID) %>%
  summarise(X= mean(X, na.rm= TRUE),
            NPP_YrMean = mean(NPP_YrMean, na.rm= TRUE),
            NPP = mean(NPP, na.rm=TRUE),
            LST_YrMean = mean(LST_YrMean, na.rm=TRUE),
            Tot_Ndep_2012 = mean(Tot_Ndep_2007, na.rm=TRUE),
            Total.Input = mean(Total.Input, na.rm=TRUE),
            N_Fert_Farm_2012 = mean(N_Fert_Farm_2007, na.rm=TRUE),
            RunoffWs = mean(RunoffWs, na.rm=TRUE),
            Atmo_Pdep_2012 = mean(Atmo_Pdep_2007, na.rm=TRUE),
            P_Accumulated_ag_inputs_2007 = mean(P_Accumulated_ag_inputs_2007, na.rm=TRUE),
            nani = mean(nani, na.rm=TRUE),
            Precip_YrMean = mean(Precip_YrMean, na.rm=TRUE),
            Tot_Sdep_2012 = mean(Tot_Sdep_2007, na.rm=TRUE),
            N_CBNF_2012 = mean(N_CBNF_2007, na.rm=TRUE),
            WsAreaSqKm = mean(WsAreaSqKm, na.rm=TRUE),
            BFIWs = mean(BFIWs, na.rm=TRUE),
            NHDLakeDepth = mean(NHDLakeDepth, na.rm=TRUE)
  )

head(PredData2012)











