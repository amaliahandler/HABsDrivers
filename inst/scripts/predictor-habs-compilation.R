
# Predictor / HABs dataset compilation

# ~ I'm not proud of how messy this is right now it's just a work in progress ~

library(devtools)
library(dplyr)
library(ggplot2)
library(scales)
library(ggpubr)
library(tidyr)
library(stars)

# install.packages("magrittr")
# library(magrittr)


# create mean of 2002/2007/2012 nutrient inventories, 2002 nutrient inventories from Meredith
# reach out to meredith to see if she has the 2002 data for the same locations, create mean of all variables if possible

# Anticipated Steps: --------------------------------------------------------------------------
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

# habs <- habs |>
#   # Robert suggested using agricultural inputs rather than land cover
#   mutate(n_farm_inputs = N_Fert_Farm + N_CBNF + N_livestock_Waste,
#          n_dev_inputs = N_Human_Waste + N_Fert_Urban,
#          p_farm_inputs = P_f_fertilizer + P_livestock_Waste,
#          p_dev_inputs = P_human_waste_kg + P_nf_fertilizer) |> # ,
#   # nfarm_inputs_pres = ifelse(n_farm_inputs == 0, 0, 1),
#   # Creating a categorical variable for lake depth
#   mutate(lake_dep = ifelse(MAXDEPTH <= 10, "shallow", "deep"))
#
model_cyano_nolakes <- readRDS('./inst/model_objects/model_cyano_nolakedata.rds')
model_micx_nolakes <- readRDS('./inst/model_objects/model_micx_nolakedata.rds')
model_micx_lakes <- readRDS('./inst/model_objects/model_micx_withlakedata.rds')


model_cyano_nolakedata$formula
model_micx_nolakes$formula

# needed variables <- B_G_DENS + BFIWs + Tmean8110Ws + Precip8110Ws +
# n_farm_inputs + n_dev_inputs + p_farm_inputs + lake_dep +
# lakemorpho_fetch
# MICX_DET ~ p_farm_inputs + fst_ws + Precip_Minus_EVTWs + MAXDEPTH +
# lakemorpho_fetch + BFIWs + AG_ECO3


# ================= ACTUAL CODE COMP ===================

# Loading and compiling Meredith's predictor datasets --------------------------------------------------------------------------

PredData07_05Ws <- read.csv("C:/Users/mreyno04/OneDrive - Environmental Protection Agency (EPA)/Profile/REPOS/LakePredData/PredData/PredData07_05Ws.csv")
head(PredData07_05Ws) # looks good
PredData07_07Ws <- read.csv("C:/Users/mreyno04/OneDrive - Environmental Protection Agency (EPA)/Profile/REPOS/LakePredData/PredData/PredData07_05Ws.csv")
PredData07_10Ws <- read.csv("C:/Users/mreyno04/OneDrive - Environmental Protection Agency (EPA)/Profile/REPOS/LakePredData/PredData/PredData07_10Ws.csv")
PredData12_05Ws <- read.csv("C:/Users/mreyno04/OneDrive - Environmental Protection Agency (EPA)/Profile/REPOS/LakePredData/PredData/PredData12_05Ws.csv")
PredData12_07Ws <- read.csv("C:/Users/mreyno04/OneDrive - Environmental Protection Agency (EPA)/Profile/REPOS/LakePredData/PredData/PredData12_07Ws.csv")
PredData12_10Ws <- read.csv("C:/Users/mreyno04/OneDrive - Environmental Protection Agency (EPA)/Profile/REPOS/LakePredData/PredData/PredData12_10Ws.csv")

# Combine the data by 2007 and 2012 --------------------------------------------------------------------------

PredData2007 <- PredData07_05Ws %>%
  bind_rows(PredData07_07Ws) %>%
  bind_rows(PredData07_10Ws)

# PredData2007$Obs <- 1:nrow(PredData2007) # new column with indiv. values for each row

which(PredData2007$wbCOMID == "487") # checking to make sure the data combined as intended

PredData2007 <- PredData2007 %>%
  group_by(across(wbCOMID)) %>%
  summarise(across(where(is.numeric), mean))

which(PredData2007$wbCOMID == "487") # checking to make sure the data combined as intended again


# 2012

PredData2012 <- PredData12_05Ws %>%
  bind_rows(PredData12_07Ws) %>%
  bind_rows(PredData12_10Ws)

which(PredData2012$wbCOMID == "487") # checking to make sure the data combined as intended, should be 3

PredData2012 <- PredData2012%>%
  group_by(across(wbCOMID)) %>%
  summarise(across(where(is.numeric), mean))

which(PredData2012$wbCOMID == "487") # checking to make sure the data combined as intended, should be 1

# Failed attempt, embarrassing -------------------------------------------------------------

# # averaging the 2007 three data sets
# PredData2007 <- bind_rows(PredData07_05Ws, PredData07_07Ws, PredData07_10Ws) %>%
#   group_by(wbCOMID) %>%
#   summarise(X= mean(X, na.rm= TRUE),
#             NPP_YrMean = mean(NPP_YrMean, na.rm= TRUE),
#             NPP = mean(NPP, na.rm=TRUE),
#             LST_YrMean = mean(LST_YrMean, na.rm=TRUE),
#             Tot_Ndep_2007 = mean(Tot_Ndep_2007, na.rm=TRUE),
#             Total.Input = mean(Total.Input, na.rm=TRUE),
#             N_Fert_Farm_2007 = mean(N_Fert_Farm_2007, na.rm=TRUE),
#             RunoffWs = mean(RunoffWs, na.rm=TRUE),
#             Atmo_Pdep_2007 = mean(Atmo_Pdep_2007, na.rm=TRUE),
#             P_Accumulated_ag_inputs_2007 = mean(P_Accumulated_ag_inputs_2007, na.rm=TRUE),
#             nani = mean(nani, na.rm=TRUE),
#             Precip_YrMean = mean(Precip_YrMean, na.rm=TRUE),
#             Tot_Sdep_2007 = mean(Tot_Sdep_2007, na.rm=TRUE),
#             N_CBNF_2007 = mean(N_CBNF_2007, na.rm=TRUE),
#             WsAreaSqKm = mean(WsAreaSqKm, na.rm=TRUE),
#             BFIWs = mean(BFIWs, na.rm=TRUE),
#             NHDLakeDepth = mean(NHDLakeDepth, na.rm=TRUE)
#             )
#
# # excluded variables during the merge
# # ClayWs, SandWs, SNOW_YrMean, catCOMID
#
# head(PredData2007)
#
# # averaging the 2012 three data sets
# PredData2012 <- bind_rows(PredData12_05Ws, PredData12_07Ws, PredData12_10Ws) %>% # maybe make a loop that does this instead melanie
#   group_by(wbCOMID) %>% # 2012 variables still named with 2007, renamed while creating new variables with the means
#   summarise(X= mean(X, na.rm= TRUE),
#             NPP_YrMean = mean(NPP_YrMean, na.rm= TRUE),
#             NPP = mean(NPP, na.rm=TRUE),
#             LST_YrMean = mean(LST_YrMean, na.rm=TRUE),
#             Tot_Ndep_2012 = mean(Tot_Ndep_2007, na.rm=TRUE),
#             Total.Input = mean(Total.Input, na.rm=TRUE),
#             N_Fert_Farm_2012 = mean(N_Fert_Farm_2007, na.rm=TRUE),
#             RunoffWs = mean(RunoffWs, na.rm=TRUE),
#             Atmo_Pdep_2012 = mean(Atmo_Pdep_2007, na.rm=TRUE),
#             P_Accumulated_ag_inputs_2012 = mean(P_Accumulated_ag_inputs_2007, na.rm=TRUE),
#             nani = mean(nani, na.rm=TRUE),
#             Precip_YrMean = mean(Precip_YrMean, na.rm=TRUE),
#             Tot_Sdep_2012 = mean(Tot_Sdep_2007, na.rm=TRUE),
#             N_CBNF_2012 = mean(N_CBNF_2007, na.rm=TRUE),
#             WsAreaSqKm = mean(WsAreaSqKm, na.rm=TRUE),
#             BFIWs = mean(BFIWs, na.rm=TRUE),
#             NHDLakeDepth = mean(NHDLakeDepth, na.rm=TRUE)
#   )
#
# head(PredData2012)

# Create the multi-year dataset --------------------------------------------------------------------------

PredDataMas <- PredData2007 %>%
  bind_rows(PredData2012)

which(PredDataMas$wbCOMID == "487") # checking to make sure the data combined as intended, should be 2

PredDataMas <- PredDataMas%>%
  group_by(across(wbCOMID)) %>%
  summarise(across(where(is.numeric), mean))

which(PredDataMas$wbCOMID == "487") # checking to make sure the data combined as intended, should be 1
head(PredDataMas)

# check for the N/As

sum(is.na(PredDataMas)) # ouch
colSums(is.na(PredDataMas))

sum(is.na(PredData2007$NHDLakeDepth))
sum(is.na(PredData2012$NHDLakeDepth))

sum(is.na(PredData07_05Ws$NHDLakeDepth))
sum(is.na(PredData07_07Ws$NHDLakeDepth))
sum(is.na(PredData07_10Ws$NHDLakeDepth))

count(PredDataMas$NHDLakeDepth == "-9998")
sum(is.na(PredData07_10Ws$NHDLakeDepth))

class(PredDataMas$NHDLakeDepth)

print(table(PredDataMas$NHDLakeDepth))
# no longer needed -------------------------------------------------------------------------------------

# most of the NA values seem to be LAGOS data set
# limno LAGOS data set link: https://portal.edirepository.org/nis/metadataviewer?packageid=edi.1439.5

# r code link: https://portal.edirepository.org/nis/codeGeneration?packageId=edi.1439.5&statisticalFileType=r

# locus LAGOS data set : https://portal.edirepository.org/nis/mapbrowse?packageid=edi.854.1

# geo LAGOS data set : https://portal.edirepository.org/nis/mapbrowse?scope=edi&identifier=1136&revision=3

# we have NHD lake depth but what are the units, different values than LAGOS lake depth- seems uncorrelated which feels like a bad thing

# library(remotes)
# install_github("USEPA/StreamCatTools", build_vignettes=FALSE)
# vignette("Introduction", "StreamCatTools")
#
# df <- lc_get_data(metric='PctUrbMd2006,DamDens')

# # Start interactive setup
# $ gh auth login
#
# # Authenticate against github.com by reading the token from a file
# $ gh auth login --with-token < mytoken.txt
#
# # Authenticate with specific host
# $ gh auth login --hostname enterprise.internal


# Add in the LakeCat data sourced from Ryan ---------------------------------------------------------

pesticides <- read.csv("C:/Users/mreyno04/OneDrive - Environmental Protection Agency (EPA)/Profile/REPOS/LakePredData/PredData/lakecat-metrics-melanie/Pesticides97.csv")

# head(pesticides)
# mean(pesticides$CatPctFull)

# wbCOMID and/or catCOMID merge with pesticide$COMID variable

# colnames(pesticides)
# colnames(PredDataMas)

names(PredDataMas)[names(PredDataMas) == "wbCOMID"] <- "COMID"

pesticides = subset(pesticides, select = -c(WsAreaSqKm,CatPctFull,WsPctFull))

PredDataMas <- merge(PredDataMas, pesticides, by = "COMID") # need to change the name of the COMID variables

# pestic97cat average pre merge: 60.609
# pestic97cat average post merge: 45.88692 hmmmmmmmmm

# mean(PredDataMas$Pestic97Cat)
# sum(PredDataMas$Pestic97Cat)

# sum(is.na(PredDataMas$Pestic97Cat))

# load the BFI data ---------------------------------------------------------------------------------

BFI <- read.csv("C:/Users/mreyno04/OneDrive - Environmental Protection Agency (EPA)/Profile/REPOS/LakePredData/PredData/lakecat-metrics-melanie/BFI.csv")

head(BFI)

BFI = subset(BFI, select = -c(CatAreaSqKm,WsAreaSqKm,CatPctFull,WsPctFull,inStreamCat))

head(PredDataMas) # check to make sure it went well


# PredDataMas$BFIWs.x[is.zero(PredDataMas$BFIWs.x)] <- na reverse this
# PredDataMas$BFIWs.y[is.zero(PredDataMas$BFIWs.y)] <- na

# sum(PredDataMas$BFIWs.y) # resulted in duplicate BFIW columns, check to see if theyr're identical
# sum(PredDataMas$BFIWs.x)

colnames(PredDataMas)

# PredDataMas = subset(PredDataMas, select = -c(BFIWs.x)) # remove duplicate

names(BFI)[names(BFI) == "BFIWs"] <- "BFIWs.2003" # USGS BFIWs compiled up to 2003
names(PredDataMas)[names(PredDataMas) == "BFIWs"] <- "BFIWs.2012" # BFIWs 2007-2012 data compiled

PredDataMas <- merge(PredDataMas, BFI, by = "COMID")

# load PRISM data ----------------------------------------------------------------------------

PRISM <- read.csv("C:/Users/mreyno04/OneDrive - Environmental Protection Agency (EPA)/Profile/REPOS/LakePredData/PredData/lakecat-metrics-melanie/PRISM_1991_2020.csv")

PRISM = subset(PRISM, select = -c(CatAreaSqKm, WsAreaSqKm, CatPctFull, WsPctFull, inStreamCat))

head(PRISM) # check to make sure all is well

PredDataMas <- merge(PredDataMas, PRISM, by = "COMID")

# load runoff data ---------------------------------------------------------------------------

runoff <- read.csv("C:/Users/mreyno04/OneDrive - Environmental Protection Agency (EPA)/Profile/REPOS/LakePredData/PredData/lakecat-metrics-melanie/runoff.csv")

# rm(Runoff)

summary(PredDataMas$NHDLakeDepth) # nhd lake depth numbers look into!

runoff = subset(runoff, select = -c(CatAreaSqKm, WsAreaSqKm, CatPctFull, WsPctFull, inStreamCat))

head(runoff) # check to make sure all is well

PredDataMas <- merge(PredDataMas, runoff, by = "COMID")

# load rock nitrogen data ---------------------------------------------------------------------------

rockN <- read.csv("C:/Users/mreyno04/OneDrive - Environmental Protection Agency (EPA)/Profile/REPOS/LakePredData/PredData/lakecat-metrics-melanie/RockN.csv")

rockN = subset(rockN, select = -c(CatAreaSqKm, WsAreaSqKm, CatPctFull, WsPctFull, inStreamCat))

head(rockN) # check to make sure all is well

PredDataMas <- merge(PredDataMas, rockN, by = "COMID")

# total data set compiled -----------------------------------------------------------------------------

# clean up final data set

PredDataMas = subset(PredDataMas, select = -c(X))

colnames(PredDataMas)

# duplicated columns = inStreamCat.x, CatAreaSqKm.y, WsAreaSqKm.x, WsAreaSqKm.y, inStreamCat.y, RunoffWs.y,
# RunoffWs.x, CatAreaSqKm.x

PredDataMas$inStreamCat.x == PredDataMas$inStreamCat.y # all true
PredDataMas$CatAreaSqKm.x == PredDataMas$CatAreaSqKm.y # all true
PredDataMas$RunoffWs.x == PredDataMas$RunoffWs.y # hmmmmm not all true
PredDataMas$WsAreaSqKm.x == PredDataMas$WsAreaSqKm.y # all true

# remove and rename duplicate variables

# names(PredDataMas)[names(PredDataMas) == "inStreamCat.x"] <- "inStreamCat"
# PredDataMas = subset(PredDataMas, select = -c(inStreamCat.y))

# names(PredDataMas)[names(PredDataMas) == "CatAreaSqKm.x"] <- "CatAreaSqKm"
# PredDataMas = subset(PredDataMas, select = -c(CatAreaSqKm.y))

# names(PredDataMas)[names(PredDataMas) == "WsAreaSqKm.x"] <- "WsAreaSqKm"
# PredDataMas = subset(PredDataMas, select = -c(WsAreaSqKm.y))

# runoff x is from 2007-2012 data compilation, runoff y is from lakecat data loaded recently

names(PredDataMas)[names(PredDataMas) == "RunoffWs.x"] <- "Runoff.2012"
names(PredDataMas)[names(PredDataMas) == "RunoffWs.y"] <- "Runoff.2003"

colnames(PredDataMas)
colSums(is.na(PredDataMas))
sum(is.na(PredDataMas))

mean(PredDataMas$BFIWs.2003, na.rm = TRUE)
mean(PredDataMas$BFIWs.2012, na.rm = TRUE)

summary(PredDataMas$Runoff.2012, na.rm = TRUE)
summary(PredDataMas$Runoff.2003, na.rm = TRUE)

#remove bfiw
#remove runoff 1340

PredDataMas = subset(PredDataMas, select = -c(Runoff.2012))
PredDataMas = subset(PredDataMas, select = -c(BFIWs.2012))

names(PredDataMas)[names(PredDataMas) == "BFIWs.2003"] <- "BFIWs"
names(PredDataMas)[names(PredDataMas) == "Runoff.2003"] <- "Runoff"



# correlation testing ---------------------------------------------------------------------

library(corrplot)

corrplot(cor(PredDataMas[,29:42], use="pairwise.complete.obs", method = c("pearson")))
corrplot(cor(PredDataMas[,29:42], use="pairwise.complete.obs", method = c("spearman")))
corrplot(cor(PredDataMas[,29:42], use="pairwise.complete.obs", method = c("kendall")))

# double checking the merges, all numbers should be the same

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

sum(is.na(PredDataMas$LAGOSLakeDepth)) # ouch
sum(is.na(PredDataMas$NHDLakeDepth))

# Land Cover Data -----------------------------------------------------------------------

library(remotes)
install_github("USEPA/StreamCatTools", build_vignettes=FALSE, auth_token= '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')
library(StreamCatTools)

# COMIDs <- PredDataMas$COMID
# COMIDs <- as.character(PredDataMas$COMID)
# COMIDs[] <- lapply(COMIDs, as.character)
COMIDs <- paste(PredDataMas$COMID, collapse =",")

sum(is.na(PredDataMas$COMID))


land_cover <- lc_get_data(metric = 'PctWdWet2016, PctUrbMd2016, PctUrbLo2016, PctUrbHi2016,
                          PctMxFst2016, PctCrop2016, PctHay2016, PctGrs2016, PctDecid2016,
                          PctConif2016',
                  aoi='watershed',
                  comid = COMIDs,
                  showAreaSqKm = TRUE)

# test <- merge(PredDataMas, land_cover, by = 'COMID') #hmmm
# rm(test)




# model compilation ----------------------------------------------------------------------------


# habs <- habs |>
#   # Robert suggested using agricultural inputs rather than land cover
#   mutate(n_farm_inputs = N_Fert_Farm + N_CBNF + N_livestock_Waste,
#          n_dev_inputs = N_Human_Waste + N_Fert_Urban,
#          p_farm_inputs = P_f_fertilizer + P_livestock_Waste,
#          p_dev_inputs = P_human_waste_kg + P_nf_fertilizer) |> # ,
#   # nfarm_inputs_pres = ifelse(n_farm_inputs == 0, 0, 1),
#   # Creating a categorical variable for lake depth
#   mutate(lake_dep = ifelse(MAXDEPTH <= 10, "shallow", "deep"))

# > model_cyano_nolakedata$formula
# log10(B_G_DENS + 1000) ~ BFIWs + Tmean8110Ws + Precip8110Ws +
#   n_farm_inputs + n_dev_inputs + p_farm_inputs + lake_dep +
#   lakemorpho_fetch
#
# > model_micx_nolakes$formula
# MICX_DET ~ p_farm_inputs + fst_ws + Precip_Minus_EVTWs + MAXDEPTH +
#   lakemorpho_fetch + BFIWs + AG_ECO3







