#' Compiled data for the lake HABs risk modeling project
#'
#' A dataset containing data from the National Lakes Assessment as well as
#'  watershed, climate, and lake characteristics. The variables are as follows:
#'
#' @format A data frame with 3678 rows and 100 variables:
#' \describe{
#'   \item{SITE_ID}{A site ID assigned to a lake within an NLA survey cycle (character)}
#'   \item{VISIT_NO}{The first or second visit to a site (1--2)}
#'   \item{UNIQUE_ID}{A unique ID assigned to each lake sampled across NLAs (NLA_STATE-NUMBER)}
#'   \item{DSGN_CYCLE}{The year of the NLA survey cycle (2007, 2012, 2017)}
#'   \item{DATE_COL}{Date sample was collected (MM/DD/YYYY)}
#'   \item{LAT_DD83}{Lake latitude (degrees)}
#'   \item{LON_DD83}{Lake logitude (degrees)}
#'   \item{COMID}{National Hydrography Dataset Version 1 waterbody identifier (integer)}
#'   \item{TEMPERATURE}{NLA water temperature, mean of top 2 meters of the water column (degrees celcius)}
#'   \item{MAXDEPTH}{NLA measured maximum lake depth (meters)}
#'   \item{STRATIFIED}{Lake stratification status at the time of NLA sampling (1 - Stratified, 2 = Not stratified)}
#'   \item{AMMONIA_N}{NLA ammonia as N concentration (mg N/L)}
#'   \item{DO_SURF}{NLA dissolved oxygen concentration, mean of top 2 meters of the water column (mg/L)}
#'   \item{DOC}{NLA dissolved organic carbon concentration (mg C/L)}
#'   \item{NTL}{NLA total nitrogen concentration (mg/L)}
#'   \item{PTL}{NLA total phosphorus concentration (mg/L)}
#'   \item{TURB}{NLA turbidity, mean of the 2 meters of the water column (NTU)}
#'   \item{NITRATE_N}{NLA nitrate as N concentration (mg N/L)}
#'   \item{PH}{NLA pH, generally lab measured, but some missing values replaced with
#'             mean of top 2 meters of the water column (unitless)}
#'   \item{CHLA_RESULT}{NLA chlorophyll a concentration (ug/L)}
#'   \item{MICX}{NLA microcystin concentration (ug/L)}
#'   \item{B_G_DENS}{NLA cyanobacteria cell abundance (cells/mL)}
#'   \item{BG_BIOVOL}{NLA cyanobacteria biovolume (m^3/mL)}
#'   \item{PHYT_BIOVOL}{NLA total phytoplankton biovolume (m^3/mL)}
#'   \item{CYLSPER}{NLA cylindrospermopsin concentration (ug/L)}
#'   \item{EVAP_INFL}{NLA-derived evaporation to inflow ratio (unitless)}
#'   \item{D_EXCESS}{NLA-derived deuterium excess in water (per mil)}
#'   \item{agr_ws}{NLCD agricultural land cover including hay and cultivated crop.
#'                 Data are paired as follows: NLA 2007 - NLCD 2006, NLA 2012 - NLCD 2011,
#'                 NLA 2017 - NLCD 2016 (percent)}
#'   \item{dev_ws}{NLCD developed land cover including open spaces, low, medium, and high intensity.
#'                 Data are paired as follows: NLA 2007 - NLCD 2006, NLA 2012 - NLCD 2011,
#'                 NLA 2017 - NLCD 2016 (percent)}
#'   \item{fst_ws}{NLCD forested land cover including coniferous, deciduous, and mixed forest.
#'                 Data are paired as follows: NLA 2007 - NLCD 2006, NLA 2012 - NLCD 2011,
#'                 NLA 2017 - NLCD 2016 (percent)}
#'   \item{wet_ws}{NLCD wetland land cover including herbaceous and woody wetlands.
#'                 Data are paired as follows: NLA 2007 - NLCD 2006, NLA 2012 - NLCD 2011,
#'                 NLA 2017 - NLCD 2016 (percent)}
#'   \item{precip_mean_month}{Mean precipitation for the month in which the NLA sample was collected (mm)}
#'   \item{temp_mean_month}{Mean temperature for the month in which the NLA sample was collected (celcius)}
#'   \item{lakemorpho_depth}{Estimated lake maximum depth from lakemorpho package (m)}
#'   \item{lakemorpho_fetch}{Estimated lake fetch from lakemorpho package (m)}
#'   \item{lakemorpho_shoreline.length}{Estimated lake shoreline length from lakemorpho package (m)}
#'   \item{BFIWs}{Base flow index (percent of total flow that is base flow) for the lake watershed
#'                from LakeCat (percent)}
#'   \item{AgKffactWs}{Agriculural soil erodability Kf factor for the lake watershed from LakeCat (unitless)}
#'   \item{KffactWs}{Soil erodability Kf factor for the lake watershed from LakeCat (unitless)}
#'   \item{RunoffWs}{Runoff for the lake watershed from LakeCat (mm)}
#'   \item{Precip_Minus_EVTWs}{30-year mean annual precipitation minus evapotranspiration for the
#'                             lake watershed from LakeCat (mm)}
#'   \item{WWTPWs}{Waste water treatment plants within the watershed from LakeCat (plants/km^2)}
#'   \item{Precip8110Ws}{30-year mean annual precipitation for the lake watershed, 1981-2010,
#'                       from LakeCat (mm)}
#'   \item{Tmean8110Ws}{30-year mean annual air temperature for the lake watershed, 1981-2010,
#'                      from LakeCat (per mil)}
#'   \item{ElevWs}{Mean watershed elevation from LakeCat (meters)}
#'   \item{SlopeWs}{Mean watershed slope from LakeCat (meters)}
#'   \item{N_CBNF}{Annual total cultivated crop biological nitrogen fixation (kg N/ha)}
#'   \item{N_Crop_N_Rem}{Annual total nitrogen in crops removed from the watershed (kg N/ha)}
#'   \item{N_Fert_Farm}{Annual total nitrogen inputs from farm fertilizer into the watershed (kg N/ha)}
#'   \item{N_Fert_Urban}{Annual total nitrogen inputs from non-farm fertilizer to the watershed (kg N/ha)}
#'   \item{N_Forest_Fire}{Annual total nitrogen inputs from forest fire to the watershed (kg N/ha)}
#'   \item{N_Human_N_Demand}{Annual total nitrogen human food nitrogen demand (kg N/ha)}
#'   \item{N_Human_Waste}{Annual total nitrogen in human waste in the watershed (kg N/ha)}
#'   \item{N_Manure_Recov}{Annual total nitrogen in livestock manure recovered in the watershed (kg N/ha)}
#'   \item{N_Rock}{Annual total nitrogen inputs estimated from rock weathering in the watershed (kg N/ha)}
#'   \item{N_Surplus}{Annual total nitrogen inputs minus outputs in the watershed (kg N/ha)}
#'   \item{N_Total_Deposition}{Annual total nitrogen inputs from atmospheric deposition in the watershed (kg N/ha)}
#'   \item{N_Inputs}{Annual total nitrogen inputs in the watershed (kg N/ha)}
#'   \item{N_Outputs}{Annual total nitrogen outputs from the watershed (kg N/ha)}
#'   \item{N_nBNF}{Annual total nitrogen inputs from non-farm biological fixation in the watershed (kg N/ha)}
#'   \item{N_livestock_Waste}{Annual total nitrogen inputs livestock waste in the watershed (kg N/ha)}
#'   \item{P_Accumulated_ag_inputs}{Annual total phosphorus inputs accumulated over the period of
#'                                  record to the watershed (kg P/ha)}
#'   \item{P_Crop_removal}{Annual total phosphorus in crops that are removed from the watershed (kg P/ha)}
#'   \item{P_Deposition}{Annual total phosphorus from atmospheric deposition to the watershed (kg P/ha)}
#'   \item{P_Legacy_P}{Annual total phosphorus from legacy inputs to the watershed (kg P/ha)}
#'   \item{P_Recovered_P}{Annual total phosphorus recovered from inputs to the watershed (kg P/ha)}
#'   \item{P_Surplus}{Annual total phosphorus inputs minus outputs to the watershed (kg P/ha)}
#'   \item{P_f_fertilizer}{Annual total phosphorus inputs as farm fertilizer to the watershed (kg P/ha)}
#'   \item{P_human_waste_kg}{Annual total phosphorus inputs as human waste to the watershed (kg P/ha)}
#'   \item{P_livestock_Waste}{Annual total phosphorus inputs as livestock waste to the watershed (kg P/ha)}
#'   \item{P_nf_fertilizer}{Annual total phosphorus inputs non-farm fertilizer to the watershed (kg P/ha)}
#'   \item{AG_ECO3}{The aggregated 3 ecoregional location of the lake (character)}
#'   \item{AG_ECO9_NM}{The aggregated 9 ecoregional location of the lake (character)}
#'   \item{geometry}{The centroid of the lake (point)}
#' }
#'
#' @source {HABsDrivers} Package for estimating lake HABs risk.
