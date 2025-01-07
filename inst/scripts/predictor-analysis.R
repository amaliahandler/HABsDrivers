
# Micx/Cyano Predictors comparison analysis
# 11-21-2024
# must load all pred results and packages

states <- states(cb = TRUE, progress_bar = FALSE)  %>%
  filter(!STUSPS %in% c('HI', 'PR', 'AK', 'MP', 'GU', 'AS', 'VI'))  %>%
  st_transform(crs = 5072)

# Microcystin ==================================================================

var_pred <- st_join(wbd_copy, micx_pred_df)

pred_cols <- PredDataMini |>
  select(c(COMID, Runoff.Str, state, drain_ratio))

comp_micx <- left_join(var_pred, pred_cols, by = 'COMID')

comp_micx <- unique(comp_micx, by = "COMID")

micx_pred_df <- micx_pred_df %>%
  mutate(y_partial_p_farm = (coef(model_micx_nolakes)[2]) * p_farm_inputs,
         y_partial_n_dev = (coef(model_micx_nolakes)[3]) * n_dev_inputs,
         y_partial_nutr_all = y_partial_p_farm + y_partial_n_dev)

# Nutrients --------------------------------------------------------------------

micx_pred_df$nutr_all <- micx_pred_df$p_farm_inputs + micx_pred_df$n_dev_inputs

micx_pred_df <- micx_pred_df %>%
  # mutate(micx_class = factor(case_when(
  #   pred_micx >= 0.50 ~ 'HM',
  #   pred_micx < 0.50 ~'LM',
  #   TRUE ~ 'OTHER'
  # ))) %>%
  # mutate(p_class = factor(case_when(
  #   p_farm_inputs >= 4 ~ 'HP',
  #   p_farm_inputs < 4 ~ 'LP',
  #   TRUE ~ 'OTHER'
  # )))  %>%
  # mutate(n_class = factor(case_when(
  #   n_dev_inputs >= 10 ~ 'HN',
  #   n_dev_inputs < 10 ~ 'LN',
  #   TRUE ~ 'OTHER'
  # ))) %>%
  # mutate(alln_class = factor(case_when(
  #   n_dev_inputs >= 10 | p_farm_inputs >= 4 ~ 'HN',
  #   n_dev_inputs < 10 | p_farm_inputs < 4 ~ 'LN',
  #   TRUE ~ 'OTHER'
  # ))) %>%
  # mutate(check_nutr = factor(case_when(
  #   n_class == "HN" | p_class == "HP" ~ 'HC',
  #   n_class == "LN" | p_class == "LP" ~ 'LC',
  #   TRUE ~ 'OTHER'
  # ))) %>%
  # mutate(high_pred = factor(case_when(
  #   (n_dev_inputs >= 10 | p_farm_inputs >= 4) & pred_micx >= 0.50 ~ 'HNHM',
  #   (n_dev_inputs < 10 | p_farm_inputs < 4) & pred_micx >= 0.50 ~ 'LNHM',
  #   TRUE ~ 'OTHER'
  # )))  %>%
  # mutate(low_pred = factor(case_when(
  #   (n_dev_inputs >= 10 | p_farm_inputs >= 4) & pred_micx < 0.50 ~ 'HNLM',
  #   (n_dev_inputs < 10 | p_farm_inputs < 4) & pred_micx < 0.50 ~ 'LNLM',
  #   TRUE ~ 'OTHER'
  # ))) %>%
  mutate(all_pred = factor(case_when(
    (n_dev_inputs >= 10 | p_farm_inputs >= 4) & pred_micx >= 0.50 ~ 'HNHM',
    (n_dev_inputs < 10 | p_farm_inputs < 4) & pred_micx >= 0.50 ~ 'LNHM',
    (n_dev_inputs >= 10 | p_farm_inputs >= 4) & pred_micx < 0.50 ~ 'HNLM',
    (n_dev_inputs < 10 | p_farm_inputs < 4) & pred_micx < 0.50 ~ 'LNLM',
    TRUE ~ 'OTHER'
  )))

comp_micx_filter <- comp_micx |>
  filter(!is.na(nutr_class))

comp_micx_filter$Shape <- st_point_on_surface(comp_micx_filter$Shape)|>
  st_transform(crs=5072)


# Ratios --------------------------------------------------------------------

comp_micx_filter <- comp_micx_filter %>%
  mutate(area_km = LakeArea / 1000000,
         ad_ratio = sqrt(area_km) / MAXDEPTH)

is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))

comp_micx_filter$ad_ratio[is.nan(comp_micx_filter$ad_ratio)] <- 0

comp_micx_filter <- comp_micx_filter %>%
  mutate(AD_class = factor(case_when(ad_ratio <= 0.088 ~ 'B1',
                                       ad_ratio >= 0.088 & ad_ratio < 0.166 ~ 'B2',
                                       ad_ratio >= 0.166 & ad_ratio < 0.353 ~ 'B3',
                                       ad_ratio >= 0.353 ~ 'B4')))

AD_labels <- c('< Q1','Q1-Q2', 'Q2-Q3','> Q3')
AD_col <- rev(RColorBrewer::brewer.pal(4, "YlOrBr"))

ggplot(comp_micx_filter, aes(color = AD_class)) +
  geom_sf(size = 0.5) +
  scale_color_manual(values = AD_col,
                     labels = AD_labels,
                     name = "Ratio") +
  labs(title = "AREA:DEPTH Ratio") +
  geom_sf(data = states, fill = NA, color = "black", lwd = 0.1) +
  theme(plot.title = element_text(size = 12)) +
  guides(colour = guide_legend(override.aes = list(size=4)))

cor(comp_micx_filter$ad_ratio, comp_micx_filter$pred_micx,
    method = 'spearman', use = "pairwise.complete.obs")

ggplot(comp_micx_filter, aes(x=ad_ratio, fill = all_pred)) +
  geom_density(size = 0.75, alpha = 0.5) +
  facet_wrap(~all_pred) +
  labs(y = "Density", x = "A:D Ratio", fill = 'Class',
       title = '√lake area(km^2) / depth (m)')

# Iowa and North Dakota --------------------------------------------------------

IA <- comp_micx_filter |>
  filter(state == 'IA')

ND <- comp_micx_filter |>
  filter(state == 'ND')

 # high nutrients df -----------------------------------------------------------

high_micx <- comp_micx |>
  filter(nutr_class == 'HNHM' | nutr_class == 'LNHM') |>
  select(COMID, p_farm_inputs, n_dev_inputs, fst_ws, Precip8110Ws, Tmean8110Ws,
         MAXDEPTH, lakemorpho_fetch, BFIWs, AG_ECO3, nutr_class) |>
  st_drop_geometry()

high_micx <- subset(high_micx, select = -c(Shape))

# need to add from PredDataMini:
# state, WSarea ha and sqkm,
# P_f_fert_Kg_Ag_20**Ws, P_livestock_Waste_kg_ag_20**Ws, N_human_waste_kg_urb_20**Ws, N_fert_urban_kg_urb_20**Ws

model_nutr <- nutrMas |>
  select(COMID,
         P_f_fertilizer_kg_Ag_2002Ws, P_f_fertilizer_kg_Ag_2007Ws, P_f_fertilizer_kg_Ag_2012Ws,
         P_livestock_Waste_kg_Ag_2002Ws, P_livestock_Waste_kg_Ag_2007Ws, P_livestock_Waste_kg_Ag_2012Ws,
         N_Human_Waste_kg_Urb_2002Ws, N_Human_Waste_kg_Urb_2007Ws, N_Human_Waste_kg_Urb_2012Ws,
         N_Fert_Urban_kg_Urb_2002Ws, N_Fert_Urban_kg_Urb_2007Ws, N_Fert_Urban_kg_Urb_2012Ws
  )

high_micx <- PredDataMini |>
  select(COMID, state, WsAreaHa, WsAreaSqKm, P_f_fertilizer_kg_Ag, P_livestock_Waste_kg_Ag, N_Human_Waste_kg_Urb, N_Fert_Urban_kg_Urb) |>
  merge(high_micx, by = 'COMID')

high_micx <- merge(high_micx, model_nutr, by = 'COMID')

write.csv(high_micx, col_names = TRUE, "C:/Users/mreyno04/OneDrive - Environmental Protection Agency (EPA)/Profile/REPOS/HABsDrivers/high_micx.csv")

# filtering by class -----------------------------------------------------------

HNHM <- comp_micx_filter |>
  filter(all_pred == 'HNHM')

LNLM <- comp_micx_filter |>
  filter(all_pred == 'LNLM')

LNHM <- comp_micx_filter |>
  filter(all_pred == 'LNHM')

HNLM <- comp_micx_filter |>
  filter(all_pred == 'HNLM')

ggplot(comp_micx_filter, aes(color = all_pred)) +
  geom_sf(size = 0.5) +
  facet_wrap(~all_pred) +
  labs(title = "Where are the nutrient/micx @ 50% classes?") +
  geom_sf(data = states, fill = NA, color = "black", lwd = 0.1) +
  theme(plot.title = element_text(size = 12)) +
  guides(colour = guide_legend(override.aes = list(size=4)))

# ggsave("mas_50_micx.jpeg", width = 12, height = 8, device = 'jpeg', dpi = 500)

# BFIW -------------------------------------------------------------------------

comp_micx_filter <- comp_micx_filter %>%
  mutate(BFIW_class = factor(case_when(BFIWs <= 25 ~ 'B1',
                                        BFIWs >= 25 & BFIWs < 50 ~ 'B2',
                                        BFIWs >= 50 & BFIWs < 75 ~ 'B3',
                                        BFIWs >= 75 ~ 'B4')))

# comp_micx_filter$SandWs[is.na(comp_micx_filter$SandWs)] <- 0

BFIW_labels <- c('< 25%','25-50%', '50-75%','> 75%')
BFIW_col <- rev(RColorBrewer::brewer.pal(4, "YlOrBr"))

ggplot(comp_micx_filter, aes(color = BFIW_class)) +
  geom_sf(size = 0.5) +
  facet_wrap(~nutr_class) +
  scale_color_manual(values = BFIW_col,
                     labels = BFIW_labels,
                     name = "Base Flow") +
  labs(title = "% of flow that is comporised of Base Flow by Nutrient Class") +
  geom_sf(data = states, fill = NA, color = "black", lwd = 0.1) +
  theme(plot.title = element_text(size = 12))

# runoff -----------------------------------------------------------------------

comp_micx_filter <- comp_micx_filter %>%
  mutate(runoff_class = factor(case_when(Runoff.Str <= 100 ~ 'D1',
                                         Runoff.Str >= 100 & Runoff.Str < 200 ~ 'D2',
                                         Runoff.Str >= 200 & Runoff.Str < 300 ~ 'D3',
                                         Runoff.Str >= 300 & Runoff.Str < 400 ~ 'D4',
                                         Runoff.Str >= 400 ~ 'D5')))

run_labels <- c('< 100mm','100-200mm', '200-300mm','300-400mm', '> 400mm')
run_col <- rev(RColorBrewer::brewer.pal(5, "Spectral"))

comp_micx_filter$Runoff.Str[is.na(comp_micx_filter$Runoff.Str)] <- 0

ggplot(comp_micx_filter, aes(color = runoff_class)) +
  geom_sf(size = 0.5) +
  scale_color_manual(values = run_col,
                     labels = run_labels,
                     name = "Runoff (mm)") +
  facet_wrap(~nutr_class) +
  labs(title = "Runoff by Nutrient Class") +
  geom_sf(data = states, fill = NA, color = "black", lwd = 0.1) +
  theme(plot.title = element_text(size = 12)) +
  guides(colour = guide_legend(override.aes = list(size=4)))

ggsave("runoff_75_micx.jpeg", width = 12, height = 8, device = 'jpeg', dpi = 500)
table(comp_micx_filter$nutr_class)

# ecoregions -------------------------------------------------------------------

ggplot(comp_micx_filter, aes(color = AG_ECO3)) +
  geom_sf(size = 0.2) +
  facet_wrap(~nutr_class) +
  # scale_color_manual(values = BFIW_col,
  #                    labels = BFIW_labels,
  #                    name = "Sand % in Soil") +
  labs(title = "Ecoregion with 50% Micx threshold") +
  geom_sf(data = states, fill = NA, color = "black", lwd = 0.1) +
  theme(plot.title = element_text(size = 12)) +
  guides(colour = guide_legend(override.aes = list(size=4)))

ggsave("eco_3_micx.jpeg", width = 12, height = 8, device = 'jpeg', dpi = 500)

# Temp / Precip ----------------------------------------------------------------

comp_micx_filter <- comp_micx_filter %>%
  mutate(temp_class = factor(case_when(Tmean8110Ws <= 0 ~ 'T1',
                                         Tmean8110Ws >= 0 & Tmean8110Ws < 5 ~ 'T2',
                                         Tmean8110Ws >= 5 & Tmean8110Ws < 10 ~ 'T3',
                                         Tmean8110Ws >= 10 & Tmean8110Ws < 15 ~ 'T4',
                                         Tmean8110Ws >= 15 & Tmean8110Ws < 20 ~ 'T5',
                                         Tmean8110Ws >= 20 ~ 'T6')))

temp_labels <- c('< 0', '0-5','5-10','10-15', '15-20', '> 20')
temp_col <- rev(RColorBrewer::brewer.pal(6, "YlGnBu"))


ggplot(comp_micx_filter, aes(color = temp_class)) +
  geom_sf(size = 0.2) +
  facet_wrap(~nutr_class) +
  scale_color_manual(values = temp_col,
                     labels = temp_labels,
                     name = "°C") +
  labs(title = "30 Year Temperature Avg. with 50% Micx threshold") +
  geom_sf(data = states, fill = NA, color = "black", lwd = 0.1) +
  theme(plot.title = element_text(size = 12))+
  guides(colour = guide_legend(override.aes = list(size=4)))

ggsave("temp_class_micx.jpeg", width = 12, height = 8, device = 'jpeg', dpi = 500)

# precip

comp_micx_filter <- comp_micx_filter %>%
  mutate(precip_class = factor(case_when(Precip8110Ws <= 250 ~ 'P1',
                                       Precip8110Ws >= 250 & Precip8110Ws < 500 ~ 'P2',
                                       Precip8110Ws >= 500 & Precip8110Ws < 750 ~ 'P3',
                                       Precip8110Ws >= 750 & Precip8110Ws < 1000 ~ 'P4',
                                       Precip8110Ws >= 1000 & Precip8110Ws < 1250 ~ 'P5',
                                       Precip8110Ws >= 1250 & Precip8110Ws < 1500 ~ 'P6',
                                       Precip8110Ws >= 1500 ~ 'P7')))

precip_labels <- c('< 250','250-500','500-750','750-1000','1000-1250','1250-1500','> 1500')
precip_col <- RColorBrewer::brewer.pal(7, "BuPu")

ggplot(comp_micx_filter, aes(color = precip_class)) +
  geom_sf(size = 0.2) +
  facet_wrap(~nutr_class) +
  scale_color_manual(values = precip_col,
                     labels = precip_labels,
                     name = "mm") +
  labs(title = "30 Year Precipitation Avg. with 50% Micx threshold") +
  geom_sf(data = states, fill = NA, color = "black", lwd = 0.1) +
  theme(plot.title = element_text(size = 12))+
  guides(colour = guide_legend(override.aes = list(size=4)))

ggsave("precip_class_micx.jpeg", width = 12, height = 8, device = 'jpeg', dpi = 500)

# forest cover -----------------------------------------------------------------

comp_micx_filter <- comp_micx_filter %>%
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

ggplot(comp_micx_filter, aes(color = disc_fst)) +
  geom_sf(size = 0.2) +
  facet_wrap(~nutr_class) +
  scale_color_manual(values = fst_cols,
                     labels = fst_labels,
                     name = "Cover (%)") +
  labs(title = "Forested Land Cover with 50% Micx threshold") +
  geom_sf(data = states, fill = NA, color = "black", lwd = 0.1) +
  theme(plot.title = element_text(size = 12)) +
  guides(colour = guide_legend(override.aes = list(size=4)))

ggsave("fst_class_micx.jpeg", width = 12, height = 8, device = 'jpeg', dpi = 500)

# nutrients --------------------------------------------------------------------

# p_farm_inputs

comp_micx_filter <- comp_micx_filter %>%
  mutate(disc_p = factor(case_when(p_farm_inputs < 1 ~ 'B1',
                                   p_farm_inputs >= 1 & p_farm_inputs < 5 ~ 'B2',
                                   p_farm_inputs >= 5 & p_farm_inputs < 10 ~ 'B3',
                                   p_farm_inputs >= 10 & p_farm_inputs < 15 ~ 'B4',
                                   p_farm_inputs >= 15 & p_farm_inputs < 20 ~ 'B5',
                                   p_farm_inputs >= 20 & p_farm_inputs < 100 ~ 'B6',
                                   p_farm_inputs >=  100 ~ 'B7'),
                         levels = c('B1', 'B2', 'B3', 'B4', 'B5', 'B6', 'B7'))) %>%
  arrange(disc_p)

p_labels <- c('< 1', '1-5', '5-10','10-15','15-20', '20-100', '> 100')
p_cols <- rev(RColorBrewer::brewer.pal(7, "Spectral"))

ggplot(comp_micx_filter, aes(color = disc_p)) +
  geom_sf(size = 0.2) +
  facet_wrap(~nutr_class) +
  scale_color_manual(values = p_cols,
                     labels = p_labels,
                     name = "kg/ha/yr") +
  labs(title = "Phosphorus Farm Inputs with 50% Micx threshold") +
  geom_sf(data = states, fill = NA, color = "black", lwd = 0.1) +
  theme(plot.title = element_text(size = 12)) +
  guides(colour = guide_legend(override.aes = list(size=4)))

ggsave("phos_class_micx.jpeg", width = 12, height = 8, device = 'jpeg', dpi = 500)

# n_dev_inputs

comp_micx_filter <- comp_micx_filter %>%
  mutate(disc_n = factor(case_when(n_dev_inputs < 0.1 ~ 'B1',
                                   n_dev_inputs >= 0.1 & n_dev_inputs < 1 ~ 'B2',
                                   n_dev_inputs >= 1 & n_dev_inputs < 2.5 ~ 'B3',
                                   n_dev_inputs >= 2.5 & n_dev_inputs < 5 ~ 'B4',
                                   n_dev_inputs >= 5 & n_dev_inputs < 10 ~ 'B5',
                                   n_dev_inputs >= 10 & n_dev_inputs < 20 ~ 'B6',
                                   n_dev_inputs >= 20  ~ 'B7'),
                         levels = c('B1', 'B2', 'B3', 'B4', 'B5', 'B6', 'B7'))) %>%
  arrange(disc_n)

n_labels <- c('< 0.1', '0.1-1','1-2.5','2.5-5','5-10','10-20','> 20')
n_cols <- rev(RColorBrewer::brewer.pal(7, "Spectral"))

ggplot(comp_micx_filter, aes(color = disc_n)) +
  geom_sf(size = 0.2) +
  facet_wrap(~nutr_class) +
  scale_color_manual(values = n_cols,
                     labels = n_labels,
                     name = "kg/ha/yr") +
  labs(title = "Nitrogen Development Inputs with 50% Micx threshold") +
  geom_sf(data = states, fill = NA, color = "black", lwd = 0.1) +
  theme(plot.title = element_text(size = 12)) +
  guides(colour = guide_legend(override.aes = list(size=4)))

ggsave("nitr_class_micx.jpeg", width = 12, height = 8, device = 'jpeg', dpi = 500)


# Lake Fetch Mapping -----------------------------------------------------------

comp_micx_filter <- comp_micx_filter %>%
  mutate(disc_fetch = factor(case_when(lakemorpho_fetch >= 0 & lakemorpho_fetch < 400 ~ 'B1',
                                       lakemorpho_fetch >= 400 & lakemorpho_fetch < 800 ~ 'B2',
                                       lakemorpho_fetch >= 800 & lakemorpho_fetch < 1600 ~ 'B3',
                                       lakemorpho_fetch >= 1600 & lakemorpho_fetch < 3200 ~ 'B4',
                                       lakemorpho_fetch >= 3200 & lakemorpho_fetch < 4800 ~ 'B5',
                                       lakemorpho_fetch >= 4800  ~ 'B6'),
                             levels = c('B1', 'B2', 'B3', 'B4', 'B5', 'B6'))) %>%
  arrange(disc_fetch)

f_labels <- c('0-400', '400-800', '800-1600', '1600-3200', '3200-4800', '>4800')
f_cols <- RColorBrewer::brewer.pal(6, "PuBuGn")

ggplot(comp_micx_filter, aes(color = disc_fetch)) +
  geom_sf(size = 0.2) +
  facet_wrap(~nutr_class) +
  scale_color_manual(values = f_cols,
                     labels = f_labels,
                     name = "Meters") +
  labs(title = "Lake Fetch Distribution with 50% Micx threshold") +
  geom_sf(data = states, fill = NA, color = "black", lwd = 0.1) +
  theme(plot.title = element_text(size = 12)) +
  guides(colour = guide_legend(override.aes = list(size=4)))

ggsave("fetch_class_micx.jpeg", width = 12, height = 8, device = 'jpeg', dpi = 500)

# lake depth -------------------------------------------------------------------

# lake depths

comp_micx_filter <- comp_micx_filter %>%
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


ggplot(comp_micx_filter, aes(color = disc_depth)) +
  geom_sf(size = 0.2) +
  facet_wrap(~nutr_class) +
  scale_color_manual(values = depth_cols,
                     labels = depth_labels,
                     name = "Lake Depth") +
  labs(title = "Lake Depth Distribution with 50% Micx threshold") +
  geom_sf(data = states, fill = NA, color = "black", lwd = 0.1) +
  theme(plot.title = element_text(size = 12)) +
  guides(colour = guide_legend(override.aes = list(size=4)))

ggsave("depth_class_micx.jpeg", width = 12, height = 8, device = 'jpeg', dpi = 500)

# excess nutrient analysis

# N_Fert_Farm : PredDataMas -> N_Fert_Farm_2007
# N_CBNF : PredDataMas -> N_CBNF_2007
# N_livestock_Waste : nutrMas -> N_Livestock.Waste_kg_Ag_20**Ws
# P_human_waste_kg : nutrMas -> P_human_waste_kg_Urb_20**Ws
# P_nf_fertilizer : nutrMas -> P_nf_fertilizer_kg_Urb_20**Ws


model_nutr <- nutrMas |>
  select(COMID,
         P_f_fertilizer_kg_Ag_2002Ws, P_f_fertilizer_kg_Ag_2007Ws, P_f_fertilizer_kg_Ag_2012Ws,
         P_livestock_Waste_kg_Ag_2002Ws, P_livestock_Waste_kg_Ag_2007Ws, P_livestock_Waste_kg_Ag_2012Ws,
         N_Human_Waste_kg_Urb_2002Ws, N_Human_Waste_kg_Urb_2007Ws, N_Human_Waste_kg_Urb_2012Ws,
         N_Fert_Urban_kg_Urb_2002Ws, N_Fert_Urban_kg_Urb_2007Ws, N_Fert_Urban_kg_Urb_2012Ws,
         N_Livestock.Waste_kg_Ag_2002Ws, N_Livestock.Waste_kg_Ag_2007Ws, N_Livestock.Waste_kg_Ag_2012Ws,
         P_human_waste_kg_Urb_2002Ws, P_human_waste_kg_Urb_2007Ws, P_human_waste_kg_Urb_2012Ws,
         P_nf_fertilizer_kg_Urb_2002Ws, P_nf_fertilizer_kg_Urb_2007Ws, P_nf_fertilizer_kg_Urb_2012Ws
  )

predmas_nutr <- PredDataMas |>
  select(COMID, N_Fert_Farm_2007, N_CBNF_2007)

model_nutr <- merge(model_nutr, predmas_nutr, by ='COMID')

xs_n <- PredDataMini |>
  select(COMID, state, n_dev_inputs, n_farm_inputs, p_farm_inputs, p_dev_inputs, WsAreaHa, WsAreaSqKm,
         P_f_fertilizer_kg_Ag, P_livestock_Waste_kg_Ag, N_Human_Waste_kg_Urb, N_Fert_Urban_kg_Urb) |>
  filter(n_dev_inputs > 1000 | n_farm_inputs > 1000 | p_farm_inputs > 1000 | p_dev_inputs > 1000)

high_nutr <- merge(xs_n, model_nutr, by = 'COMID')

write_csv(high_nutr, col_names = TRUE, "C:/Users/mreyno04/OneDrive - Environmental Protection Agency (EPA)/Profile/REPOS/HABsDrivers/high_nutr.csv")

#  --------------------------------------------------------------------

ggplot(LNHM, aes(color = fst_ws)) +
  geom_sf(size = 1)

ggplot(comp_micx_filter, aes(fill = Runoff.Str)) +
  geom_sf(size = 1)

ggplot(comp_micx_filter, aes(x=fst_ws, fill = nutr_class)) +
  geom_density(size = 0.75, alpha = 0.5) +
  labs(y = "Density", x = "Forest Cover %", fill = 'Class',
       title = 'Forest Cover')

ggplot(comp_micx_filter, aes(x=Tmean8110Ws, fill=nutr_class)) +
  geom_density(size = 0.75, alpha = 0.5) +
  labs(y = "Density", x = "Temp (°C)", fill = 'Class',
       title = "30 Year Average Temperature: Micx @ 50%")

ggplot(comp_micx_filter, aes(x=Precip8110Ws, fill = nutr_class)) +
  geom_density(size = 0.75, alpha = 0.5) +
  facet_wrap(~nutr_class, nrow=4, ncol=1)+
  xlim(0,2000) +
  labs(y = "Density", x = "Precipitation (mm)", fill = 'Class',
       title = 'Precipitation: Micx @ 50%')

ggplot(comp_micx_filter, aes(y=BFIWs, x=nutr_class, fill=nutr_class)) +
  geom_violin() +
  coord_flip()
  labs(y = "Density", x = "% of Flow that is Base Flow", fill = 'Class',
       title = "Base Flow: Micx @ 50%")

ggplot(comp_micx_filter, aes(x=Runoff.Str, fill = nutr_class)) +
  geom_density(size = 0.75, alpha = 0.5) +
  xlim(0,1000) +
  labs(y = "Density", x = "Runoff", fill = 'Class',
       title = "Runoff")

ggplot(comp_micx_filter, aes(LakeVolume.x, fill = nutr_class)) +
  geom_density(size = 0.75, alpha = 0.5) +
  xlim(0,50000) +
  labs(y = "Density", x = "lake volume", fill = 'Class',
       title = "lake volume")

ggplot(comp_micx_filter, aes(x=Tot_Sdep_2007, fill = nutr_class)) +
  geom_density(size = 0.75, alpha = 0.5) +
  #xlim(0,50000) +
  labs(y = "Density", x = "S Dep", fill = 'Class',
       title = "micx + S dep")

ggplot(comp_micx_filter, aes(x=SandWs, fill = nutr_class)) +
  geom_density(size = 0.75, alpha = 0.5) +
  labs(y = "Density", x = "Sand Soil", fill = 'Class',
       title = "Micx / Soil Sand")

ggsave("BFIW_micx_den_4.jpeg", width = 12, height = 8, device = 'jpeg', dpi = 500)


table(LNLM$AG_ECO3)

# Cyanobacteria ================================================================

cy_pred <- st_join(wbd_copy, pred_df)

pred_cols <- PredDataMini |>
  select(c(COMID, Runoff.Str, wet_ws, agr_ws, LakeVolume, drain_ratio, Tot_Sdep_2007, ag_eco9,
           ClayWs, SandWs, AgKffactWs))

comp_cyano <- left_join(cy_pred, pred_cols, by = 'COMID')

comp_cyano$Shape <- st_point_on_surface(comp_cyano$Shape) |>
  st_transform(crs=5072)

# Nutrients --------------------------------------------------------------------

comp_cyano$nutr_all <- comp_cyano$n_farm_inputs + comp_cyano$p_dev_inputs

comp_cyano <- comp_cyano %>%
  mutate(nutr_class = factor(case_when(nutr_all >= 10 & pred_cyano <= 5 ~ 'HNLC',
                                       nutr_all <= 10 & pred_cyano >= 5 ~ 'LNHC',
                                       nutr_all >= 10 & pred_cyano >= 5 ~ 'HNHC',
                                       nutr_all <= 10 & pred_cyano <= 5 ~ 'LNLC')))
comp_cyano_filter <- comp_cyano |>
  filter(!is.na(nutr_class))

comp_micx_filter[is.na(comp_micx_filter)] <- 0


# HNLC ~ high nutrients, low cyanobacteria


ggplot(comp_cyano_filter, aes(color = nutr_class)) +
  geom_sf(size = 0.5) +
  facet_wrap(~nutr_class) +
  labs(title = "Where are the nutrient/cyano classes?") +
  geom_sf(data = states, fill = NA, color = "black", lwd = 0.1) +
  theme(plot.title = element_text(size = 12)) +
  guides(colour = guide_legend(override.aes = list(size=4)))

ggsave("mas_100k_cyano.jpeg", width = 12, height = 8, device = 'jpeg', dpi = 500)

# Base flow --------------------------------------------------------------------

comp_cyano_filter <- comp_cyano_filter %>%
  mutate(BFIW_class = factor(case_when(BFIWs <= 25 ~ 'B1',
                                       BFIWs >= 25 & BFIWs < 50 ~ 'B2',
                                       BFIWs >= 50 & BFIWs < 75 ~ 'B3',
                                       BFIWs >= 75 ~ 'B4')))

# comp_cyano_filter$SandWs[is.na(comp_cyano_filter$SandWs)] <- 0

BFIW_labels <- c('< 25%','25-50%', '50-75%','> 75%')
BFIW_col <- RColorBrewer::brewer.pal(4, "YlGnBu")

ggplot(comp_cyano_filter, aes(color = BFIW_class)) +
  geom_sf(size = 0.5) +
  facet_wrap(~nutr_class) +
  scale_color_manual(values = BFIW_col,
                     labels = BFIW_labels,
                     name = "Base Flow") +
  labs(title = "% Base Flow by Nutrient Class") +
  geom_sf(data = states, fill = NA, color = "black", lwd = 0.1) +
  theme(plot.title = element_text(size = 12)) +
  guides(colour = guide_legend(override.aes = list(size=4)))

ggsave("BFIW_100k_cyano.jpeg", width = 12, height = 8, device = 'jpeg', dpi = 500)

# runoff -----------------------------------------------------------------------

comp_cyano_filter <- comp_cyano_filter %>%
  mutate(runoff_class = factor(case_when(Runoff.Str <= 100 ~ 'D1',
                                        Runoff.Str >= 100 & Runoff.Str < 200 ~ 'D2',
                                        Runoff.Str >= 200 & Runoff.Str < 300 ~ 'D3',
                                        Runoff.Str >= 300 & Runoff.Str < 400 ~ 'D4',
                                        Runoff.Str >= 400 ~ 'D5')))

run_labels <- c('< 100mm','100-200mm', '200-300mm','300-400mm', '> 400mm')
run_col <- rev(RColorBrewer::brewer.pal(5, "Spectral"))

comp_cyano_filter$Runoff.Str[is.na(comp_cyano_filter$Runoff.Str)] <- 0

ggplot(comp_cyano_filter, aes(color = runoff_class)) +
  geom_sf(size = 0.5) +
  scale_color_manual(values = run_col,
                     labels = run_labels,
                     name = "Runoff (mm)") +
  facet_wrap(~nutr_class) +
  labs(title = "Runoff by Nutrient Class") +
  geom_sf(data = states, fill = NA, color = "black", lwd = 0.1) +
  theme(plot.title = element_text(size = 12)) +
  guides(colour = guide_legend(override.aes = list(size=4)))

ggsave("runoff_100k_cyano.jpeg", width = 12, height = 8, device = 'jpeg', dpi = 500)

# forest cover -----------------------------------------------------------------

comp_cyano_filter <- comp_cyano_filter %>%
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

ggplot(comp_cyano_filter, aes(color = disc_fst)) +
  geom_sf(size = 0.75) +
  facet_wrap(~nutr_class) +
  scale_color_manual(values = fst_cols,
                     labels = fst_labels,
                     name = "Cover (%)") +
  labs(title = "Forested Land Cover- cyano @ 100k") +
  geom_sf(data = states, fill = NA, color = "black", lwd = 0.1) +
  theme(plot.title = element_text(size = 12)) +
  guides(colour = guide_legend(override.aes = list(size=4)))

ggsave("fst_100k_cyano.jpeg", width = 12, height = 8, device = 'jpeg', dpi = 500)


# agricultural erodability -----------------------------------------------------

ggplot(comp_cyano_filter, aes(color = AgKffactWs)) +
  geom_sf(size = 0.5) +
  facet_wrap(~nutr_class) +
  labs(title = "Ag Erode with 100k Cyano threshold") +
  geom_sf(data = states, fill = NA, color = "black", lwd = 0.1) +
  theme(plot.title = element_text(size = 12)) +
  guides(colour = guide_legend(override.aes = list(size=4)))

ggsave("eco_100k_cyano.jpeg", width = 12, height = 8, device = 'jpeg', dpi = 500)

#  -------------------------------------------------------------------

ggplot(LNHM, aes(color = fst_ws)) +
  geom_sf(size = 1)

ggplot(HNLM, aes(color = fst_ws)) +
  geom_sf(size = 1)

ggplot(comp_cyano_filter, aes(x=fst_ws, fill = nutr_class)) +
  geom_density(size = 0.75, alpha = 0.5) +
  labs(y = "Density", x = "Forest Cover %", fill = 'Class',
       title = 'Forest Cover - Cyano')

ggplot(comp_cyano_filter, aes(x=Tmean8110Ws, fill=nutr_class)) +
  geom_density(size = 0.75, alpha = 0.5) +
  labs(y = "Density", x = "30 Year Temp Average", fill = 'Class',
       title = "Temperature - Cyano")

ggplot(comp_cyano, aes(x=Precip8110Ws, fill = nutr_class)) +
  geom_density(size = 0.75, alpha = 0.5) +
  labs(y = "Density", x = "30 Year Precip Average", fill = 'Class',
       title = 'Precipitation - Cyano')

ggplot(comp_cyano, aes(x=BFIWs, fill = nutr_class)) +
  geom_density(size = 0.75, alpha = 0.5) +
  labs(y = "Density", x = "% of Flow that is Base Flow", fill = 'Class',
       title = "Base Flow - Cyano")

ggplot(comp_cyano_filter, aes(x=Runoff.Str, fill = nutr_class)) +
  geom_density(size = 0.75, alpha = 0.5) +
  xlim(0,1000) +
  labs(y = "Density", x = "Runoff", fill = 'Class',
       title = "Runoff - Cyano")

ggplot(comp_cyano_filter, aes(x=LakeVolume.x, fill = nutr_class)) +
  geom_density(size = 0.75, alpha = 0.5) +
  xlim(0,1000) +
  labs(y = "Density", x = "Lake Volume", fill = 'Class',
       title = "Lake Volume - Cyano")

ggplot(comp_cyano_filter, aes(x=SandWs, fill = nutr_class)) +
  geom_density(size = 0.75, alpha = 0.5) +
  labs(y = "Density", x = "Soil Sand", fill = 'Class',
       title = "Sand Soil - Cyano")


ggsave("cyano_sand.jpeg", width = 12, height = 8, device = 'jpeg', dpi = 500)


# Print the result
print(result_base_R)

ggplot(comp_cyano_filter, aes(x=AG_ECO3, fill = nutr_class)) +
  geom_histogram(position="dodge", stat = "count") +
  labs(y = "Count", x = "Ecoregion", fill = 'Class',
       title = "Ecoregions - Cyano")

ggplot(comp_cyano_filter, aes(fill=nutr_class))


