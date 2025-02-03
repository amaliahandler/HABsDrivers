
# Micx/Cyano Predictors comparison analysis
# 11-21-2024
# must load all pred results and packages

states <- states(cb = TRUE, progress_bar = FALSE)  %>%
  filter(!STUSPS %in% c('HI', 'PR', 'AK', 'MP', 'GU', 'AS', 'VI'))  %>%
  st_transform(crs = 5072)

# Microcystin ==================================================================

micx_pred <- st_join(wbd_copy, micx_pred_df)

pred_cols <- PredDataMini |>
  select(c(COMID, Runoff.Str, state, drain_ratio, ag_eco9, WsAreaSqKm))

comp_micx <- left_join(micx_pred, pred_cols, by = 'COMID')

comp_micx <- comp_micx |>
  drop_na(pred_micx)

# micx_pred_df <- micx_pred_df %>%
#   mutate(y_partial_p_farm = (coef(model_micx_nolakes)[2]) * p_farm_inputs,
#          y_partial_n_dev = (coef(model_micx_nolakes)[3]) * n_dev_inputs,
#          y_partial_nutr_all = y_partial_p_farm + y_partial_n_dev)

# Nutrients --------------------------------------------------------------------

comp_micx$nutr_all <- comp_micx$p_farm_inputs + comp_micx$n_dev_inputs

comp_micx <- comp_micx %>%
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

# comp_micx_filter <- comp_micx |>
#   filter(!is.na(all_pred))

# Only run after area has been calculated
comp_micx$Shape <- st_point_on_surface(comp_micx$Shape)|>
  st_transform(crs=5072)

# y partial --------------------------------------------------------------------

micx_pred_df <- micx_pred_df %>%
  mutate(y_partial_p_farm = (coef(model_micx_nolakes)[2]) * p_farm_inputs,
         y_partial_n_dev = (coef(model_micx_nolakes)[3]) * n_dev_inputs,
         y_partial_nutr_all = y_partial_p_farm + y_partial_n_dev)

micx_pred_df <- micx_pred_df %>%
  mutate(ypar_class = factor(case_when(y_partial_nutr_all <= 0.005 ~ 'B1',
                                       y_partial_nutr_all >= 0.005 & y_partial_nutr_all < 0.1 ~ 'B2',
                                       y_partial_nutr_all >= 0.1 & y_partial_nutr_all < 0.5 ~ 'B3',
                                       y_partial_nutr_all >= 0.5 & y_partial_nutr_all < 1 ~ 'B4',
                                       y_partial_nutr_all >= 1 ~ 'B5'),
                             levels = c('B1', 'B2', 'B3', 'B4', 'B5'))) %>%
  arrange(ypar_class)

# comp_cyano_filter$SandWs[is.na(comp_cyano_filter$SandWs)] <- 0

yparm_labels <- c('< 0.005','0.005-0.1', '0.1-0.5','0.5-1', '>1')
yparm_col <- rev(RColorBrewer::brewer.pal(5, "Spectral"))

ggplot(micx_pred_df, aes(color = ypar_class)) +
  geom_sf(size = 0.5) +
  scale_color_manual(values = yparm_col,
                     labels = yparm_labels,
                     name = "Y Partial") +
  labs(title = "Y Partial / All nutrients (Micx)") +
  geom_sf(data = states, fill = NA, color = "black", lwd = 0.1) +
  theme(plot.title = element_text(size = 12)) +
  guides(colour = guide_legend(override.aes = list(size=4)))

ggsave("ypar_micx.jpeg", width = 12, height = 8, device = 'jpeg', dpi = 500)

# Ratios --------------------------------------------------------------------

# poly_col <- wbd_copy |>
#   select(c(COMID, Shape))
# comp_micx_t <- st_join(poly_col, comp_micx_filter)

for (Shape in 1:length(comp_micx)) {
  shapes <- comp_micx$Shape
  comp_micx$custom_area <- st_area(shapes)
}

comp_micx$custom_area <- drop_units(comp_micx$custom_area)
comp_micx <- comp_micx |>
  mutate(area_ha = (custom_area / 10000))


# custom_area <- comp_micx_t |>
#   select(c(COMID.x, custom_area)) |>
#   sf::st_drop_geometry()
#
# names(custom_area)[names(custom_area) == 'COMID.x'] <- 'COMID'
# comp_micx_filter <- merge(comp_micx_filter, custom_area, by = 'COMID')

comp_micx <- comp_micx %>%
  mutate(area_km = custom_area / 1000000,
         ad_ratio = (sqrt(area_km)) / MAXDEPTH) %>%
  filter(MAXDEPTH > 0)

comp_micx <- comp_micx %>%
  mutate(drain_manual = WsAreaSqKm / area_km)

drain_mean <- comp_micx %>%
  st_drop_geometry() %>%
  group_by(ag_eco9) %>%
  summarize(drain_mean = mean(drain_manual, na.rm=TRUE))

comp_micx <- comp_micx %>%
  group_by(ag_eco9) %>%
  mutate(dist_mean = drain_manual - mean(drain_manual))

ggplot(comp_micx, aes(x= drain_manual, y=dist_mean))+
  geom_point()

# na_ad <- comp_micx_filter |>
#   filter(ad_ratio == Inf)
#
# names <- PredDataMini |>
#   select(c(COMID, nars_name)) |>
#   st_drop_geometry()
# na_ad <- merge(na_ad, names, by='COMID')

comp_micx <- comp_micx |>
  mutate(all_pred = factor(all_pred, levels = c('HNHM','HNLM','LNHM','LNLM')))

micx_sample <- sample_n(comp_micx, 5000)

ad_plot <- ggplot(comp_micx, aes(x=pred_micx, y=ad_ratio, color = all_pred)) +
  geom_point(alpha = 0.25) +
  geom_smooth(method = "lm") +
#  facet_wrap(~all_pred) +
  # ylim(0,50) +
  # xlim(0,100) +
  labs(y = "Lake Depth (m)", x = "Lake Area (km^2)", fill = 'Class',
       title = 'Area:Depth Ratio and Micx classes')

plot <- ggplot(comp_micx, aes(x=drain_manual, y=nutr_all)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = 'lm') +
  labs(y = "Nutrients", x = "Drain Ratio",
       title = 'Nutrients / Drain Ratio')

plot +
  scale_y_continuous(trans = 'log10')

ggsave("AD_micx_scat.jpeg", width = 10, height = 8, device = 'jpeg', dpi = 500)

ggplot(comp_micx, aes(x=drain_ratio, y=nutr_all, color = ag_eco9)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm") +
  xlim(0,1) +
  ylim(0,100)

comp_micx <- comp_micx %>%
  mutate(AD_class = factor(case_when(ad_ratio <= 0.1 ~ 'B1',
                                       ad_ratio >= 0.1 & ad_ratio < 0.2 ~ 'B2',
                                       ad_ratio >= 0.2 & ad_ratio < 0.5 ~ 'B3',
                                       ad_ratio >= 0.5 & ad_ratio < 2.5 ~ 'B4',
                                       ad_ratio >= 2.5 & ad_ratio < 5 ~ 'B5',
                                       ad_ratio >= 5 ~ 'B6'),
                           levels = c('B1', 'B2', 'B3', 'B4', 'B5','B6'))) %>%
  arrange(AD_class)

# na_ratio <- comp_micx_filter |>
#   filter(AD_class == 'OTHER')

AD_labels <- c('< 0.1','0.1-0.2', '0.2-0.5', '0.5-2.5','2.5-1', '> 5')
AD_col <- rev(RColorBrewer::brewer.pal(6, "Spectral"))

ggplot(comp_micx, aes(color = AD_class)) +
  geom_sf(size = 0.3) +
  facet_wrap(~all_pred) +
  scale_color_manual(values = AD_col,
                     labels = AD_labels,
                     name = "Ratio") +
  labs(title = "AREA:DEPTH Ratio") +
  geom_sf(data = states, fill = NA, color = "black", lwd = 0.1) +
  theme(plot.title = element_text(size = 12)) +
  guides(colour = guide_legend(override.aes = list(size=4)))

ggplot(comp_micx, aes(x=ag_eco9, y=drain_manual, fill = ag_eco9)) +
  geom_boxplot() +
  ylim(0,250)

ggsave("AD_ratio_class.jpeg", width = 12, height = 8, device = 'jpeg', dpi = 500)

cor(comp_micx_filter$ad_ratio, comp_micx_filter$pred_micx,
    method = 'spearman', use = "pairwise.complete.obs")

ggplot(comp_micx, aes(x=(ad_ratio), fill = all_pred)) +
  geom_density(size = 0.75) +
  facet_wrap(~all_pred, nrow=4, ncol=1) +
  xlim(0,0.5) +
  labs(y = "Density", x = "A:D Ratio", fill = 'Class',
       title = 'A:D Ratio - MICX')

# drainage ratio

comp_micx <- comp_micx %>%
  mutate(drain_class = factor(case_when(drain_ratio <= 0.0048 ~ 'B1',
                                        drain_ratio >= 0.0048 & drain_ratio < 0.0163 ~ 'B2',
                                        drain_ratio >= 0.0163 & drain_ratio < 0.0720 ~ 'B3',
                                        drain_ratio >= 0.0720 ~ 'B4',
                                        TRUE ~ 'OTHER')))

drain_labels <- c('<= 0.0048', '0.0048-0.0163','0.0163-0.0720','>=0.0720')
drain_col <- rev(RColorBrewer::brewer.pal(4, "Spectral"))


ggplot(comp_micx, aes(color = drain_class)) +
  geom_sf(size = 0.3) +
  # facet_wrap(~all_pred) +
  scale_color_manual(values = drain_col,
                     labels = drain_labels,
                     name = "Drain Ratio") +
  labs(title = "Drain Ratio - All Observations") +
  geom_sf(data = states, fill = NA, color = "black", lwd = 0.1) +
  theme(plot.title = element_text(size = 12)) +
  guides(colour = guide_legend(override.aes = list(size=4)))

ggsave("drain_map.jpeg", width = 12, height = 8, device = 'jpeg', dpi = 500)

ggplot(comp_micx, aes(x=(drain_ratio), fill = all_pred)) +
  geom_density(size = 0.75) +
  facet_wrap(~all_pred, nrow=4, ncol=1) +
  xlim(0,0.075) +
  labs(y = "Density", x = "Drainage Ratio", fill = 'Class',
       title = 'Drain Ratio - MICX')

ggsave("drain_micx_den.jpeg", width = 8, height = 12, device = 'jpeg', dpi = 500)

# Iowa and North Dakota --------------------------------------------------------

IA <- comp_micx |>
  filter(state == 'IA')

IA_map <- states(cb = TRUE, progress_bar = FALSE)  %>%
  filter(STUSPS %in% c('IA'))  %>%
  st_transform(crs = 5072)

IA <- IA %>%
  mutate(micx_class = factor(case_when(
    pred_micx >= 0.50 ~ 'HM',
    pred_micx < 0.50 ~'LM',
    TRUE ~ 'OTHER'
  ))) %>%
  mutate(p_class = factor(case_when(
    p_farm_inputs >= 4 ~ 'HP',
    p_farm_inputs < 4 ~ 'LP',
    TRUE ~ 'OTHER'
  )))  %>%
  mutate(n_class = factor(case_when(
    n_dev_inputs >= 10 ~ 'HN',
    n_dev_inputs < 10 ~ 'LN',
    TRUE ~ 'OTHER'
  ))) %>%
  mutate(alln_class = factor(case_when(
    n_dev_inputs >= 10 | p_farm_inputs >= 4 ~ 'HN',
    n_dev_inputs < 10 | p_farm_inputs < 4 ~ 'LN',
    TRUE ~ 'OTHER'
  )))

IA_HN <- IA |>
  filter(alln_class == 'HN')

IA_LN <- IA |>
  filter(alln_class == 'LN')

ggplot(IA, aes(color = all_pred)) +
  geom_sf(size = 0.7) +
  facet_wrap(~all_pred) +
  labs(title = "Iowa Nutrient/Micx Classes") +
  geom_sf(data = IA_map, fill = NA, color = "black", lwd = 0.1) +
  theme(plot.title = element_text(size = 12)) +
  guides(colour = guide_legend(override.aes = list(size=4)))

ggsave("IA_class_micx.jpeg", width = 12, height = 8, device = 'jpeg', dpi = 500)

# north dakota

ND <- comp_micx |>
  filter(state == 'ND')

ND_map <- states(cb = TRUE, progress_bar = FALSE)  %>%
  filter(STUSPS %in% c('ND'))  %>%
  st_transform(crs = 5072)

ND <- ND %>%
  mutate(micx_class = factor(case_when(
    pred_micx >= 0.50 ~ 'HM',
    pred_micx < 0.50 ~'LM',
    TRUE ~ 'OTHER'
  ))) %>%
  mutate(p_class = factor(case_when(
    p_farm_inputs >= 4 ~ 'HP',
    p_farm_inputs < 4 ~ 'LP',
    TRUE ~ 'OTHER'
  )))  %>%
  mutate(n_class = factor(case_when(
    n_dev_inputs >= 10 ~ 'HN',
    n_dev_inputs < 10 ~ 'LN',
    TRUE ~ 'OTHER'
  ))) %>%
  mutate(alln_class = factor(case_when(
    n_dev_inputs >= 10 | p_farm_inputs >= 4 ~ 'HN',
    n_dev_inputs < 10 | p_farm_inputs < 4 ~ 'LN',
    TRUE ~ 'OTHER'
  )))

ND_HN <- ND |>
  filter(alln_class == 'HN')

ND_LN <- ND |>
  filter(alln_class == 'LN')


ggplot(ND, aes(color = all_pred)) +
  geom_sf(size = 0.7) +
  facet_wrap(~all_pred) +
  labs(title = "North Dakota Nutrient/Micx Classes") +
  geom_sf(data = ND_map, fill = NA, color = "black", lwd = 0.1) +
  theme(plot.title = element_text(size = 12)) +
  guides(colour = guide_legend(override.aes = list(size=4)))

ggsave("ND_class_micx.jpeg", width = 12, height = 8, device = 'jpeg', dpi = 500)

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

HNHM <- comp_micx |>
  filter(all_pred == 'HNHM')

LNLM <- comp_micx |>
  filter(all_pred == 'LNLM')

LNHM <- comp_micx |>
  filter(all_pred == 'LNHM')

HNLM <- comp_micx |>
  filter(all_pred == 'HNLM')

ggplot(comp_micx_filter, aes(color = all_pred)) +
  geom_sf(size = 0.4) +
  facet_wrap(~all_pred) +
  labs(title = "Where are the nutrient/micx @ 50% classes?") +
  geom_sf(data = states, fill = NA, color = "black", lwd = 0.1) +
  theme(plot.title = element_text(size = 12)) +
  guides(colour = guide_legend(override.aes = list(size=4)))

ggsave("new_all_micx1.jpeg", width = 12, height = 8, device = 'jpeg', dpi = 500)

# y partial --------------------------------------------------------------------

ggplot(comp_micx_filter, aes(color = y_partial_nutr_all)) +
  geom_sf(size = 0.5) +
  facet_wrap(~all_pred) +
  labs(title = "Y Partial for all nutrients") +
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

ggplot(comp_micx_filter, aes(x=fst_ws, fill = all_pred)) +
  geom_density(size = 0.75, alpha = 0.5) +
  facet_wrap(~all_pred, nrow=4, ncol=1) +
  xlim(0,50) +
  labs(y = "Density", x = "Forest Cover %", fill = 'Class',
       title = 'Forest Cover - Micx')

ggsave("new_fst_micx_den.jpeg", width = 8, height = 12, device = 'jpeg', dpi = 500)

ggplot(comp_micx_filter, aes(x=Tmean8110Ws, fill= all_pred)) +
  geom_density(size = 0.75, alpha = 0.5) +
  facet_wrap(~all_pred, nrow=4, ncol=1) +
  labs(y = "Density", x = "Temp (°C)", fill = 'Class',
       title = "30 Year Average Temperature - Micx")

ggsave("new_temp_micx_den.jpeg", width = 8, height = 12, device = 'jpeg', dpi = 500)

ggplot(comp_micx_filter, aes(x=Precip8110Ws, fill = all_pred)) +
  geom_density(size = 0.75, alpha = 0.5) +
  facet_wrap(~all_pred, nrow=4, ncol=1)+
  xlim(0,2000) +
  labs(y = "Density", x = "Precipitation (mm)", fill = 'Class',
       title = '30 Year Average Precipitation - Micx')

ggsave("new_precip_micx_den.jpeg", width = 8, height = 12, device = 'jpeg', dpi = 500)

ggplot(comp_micx_filter, aes(x=BFIWs, fill = all_pred)) +
  geom_density(size = 0.75, alpha = 0.5) +
  facet_wrap(~all_pred, nrow=4, ncol=1) +
  labs(y = "Density", x = "% of Flow that is Base Flow", fill = 'Class',
       title = "Base Flow - Micx")

ggsave("new_BFIW_micx_den.jpeg", width = 8, height = 12, device = 'jpeg', dpi = 500)

ggplot(comp_micx_filter, aes(x=drain_ratio, fill = all_pred)) +
  geom_density(size = 0.75, alpha = 0.5) +
  facet_wrap(~all_pred, nrow=4, ncol=1) +
  xlim(0,0.10) +
  labs(y = "Density", x = "Runoff (mm)", fill = 'Class',
       title = "Mean Watershed Runoff")

ggsave("new_runoff_micx_den.jpeg", width = 8, height = 12, device = 'jpeg', dpi = 500)

ggplot(pred_filter, aes(x=MAXDEPTH, fill = all_pred)) +
  geom_density(size = 0.75, alpha = 0.5) +
  facet_wrap(~all_pred, nrow=4, ncol=1) +
  xlim(0,5) +
  labs(y = "Density", x = "Depth (m)", fill = 'Class',
       title = "Max Lake Depth - cyano")

ggsave("new_depth_micx_den.jpeg", width = 8, height = 12, device = 'jpeg', dpi = 500)

ggplot(pred_filter, aes(x=ad_ratio, fill = all_pred)) +
  geom_density(size = 0.75, alpha = 0.5) +
  facet_wrap(~all_pred, nrow=4, ncol=1) +
  xlim(0,100000) +
  labs(y = "Density", x = "Area (m^2)", fill = 'Class',
       title = "Lake Area - cyano")

ggsave("new_fetch_micx_den.jpeg", width = 8, height = 12, device = 'jpeg', dpi = 500)

# Cyanobacteria ================================================================

cy_pred <- st_join(wbd_copy, pred_df)

pred_cols <- PredDataMini |>
  select(c(COMID, Runoff.Str, state, drain_ratio))

comp_cyano <- left_join(cy_pred, pred_cols, by = 'COMID')

comp_cyano <- comp_cyano |>
  drop_na(pred_cyano)

# y partial --------------------------------------------------------------------

pred_filter <- pred_filter %>%
  mutate(y_partial_p_dev = (coef(model_cyano_nolakes)[3]) * p_dev_inputs,
         y_partial_n_farm = (coef(model_cyano_nolakes)[2]) * n_farm_inputs,
         y_partial_nutr_all = y_partial_p_dev + y_partial_n_farm)

pred_filter <- pred_filter %>%
  mutate(ypar_class = factor(case_when(y_partial_nutr_all <= 0.001 ~ 'B1',
                                       y_partial_nutr_all >= 0.001 & y_partial_nutr_all < 0.005 ~ 'B2',
                                       y_partial_nutr_all >= 0.005 & y_partial_nutr_all < 0.1 ~ 'B3',
                                       y_partial_nutr_all >= 0.1 & y_partial_nutr_all < 1 ~ 'B4',
                                       y_partial_nutr_all >= 1 ~ 'B5'),
                             levels = c('B1', 'B2', 'B3', 'B4', 'B5'))) %>%
  arrange(ypar_class)

# comp_cyano_filter$SandWs[is.na(comp_cyano_filter$SandWs)] <- 0

ypar_labels <- c('< 0.001','0.001-0.005', '0.005-0.1','0.1-1', '>1')
ypar_col <- rev(RColorBrewer::brewer.pal(5, "Spectral"))


ggplot(pred_filter, aes(color = ypar_class)) +
  geom_sf(size = 0.5) +
  scale_color_manual(values = ypar_col,
                     labels = ypar_labels,
                     name = "Y Partial") +
  labs(title = "Y Partial / All nutrients (Cyano)") +
  geom_sf(data = states, fill = NA, color = "black", lwd = 0.1) +
  theme(plot.title = element_text(size = 12)) +
  guides(colour = guide_legend(override.aes = list(size=4)))

ggsave("ypar_cyano.jpeg", width = 12, height = 8, device = 'jpeg', dpi = 500)

# Nutrients --------------------------------------------------------------------

comp_cyano$nutr_all <- comp_cyano$n_farm_inputs + comp_cyano$p_dev_inputs

# comp_cyano <- comp_cyano %>%
#   mutate(nutr_class = factor(case_when(nutr_all >= 10 & pred_cyano <= 5 ~ 'HNLC',
#                                        nutr_all <= 10 & pred_cyano >= 5 ~ 'LNHC',
#                                        nutr_all >= 10 & pred_cyano >= 5 ~ 'HNHC',
#                                        nutr_all <= 10 & pred_cyano <= 5 ~ 'LNLC')))

comp_cyano <- comp_cyano %>%
   # mutate(cyano_class = factor(case_when(
   #   pred_cyano >= 5 ~ 'HC',
   #   pred_cyano < 5 ~'LC',
   #   TRUE ~ 'OTHER'
   # ))) %>%
   # mutate(p_class = factor(case_when(
   #   p_dev_inputs >= 4 ~ 'HP',
   #   p_dev_inputs < 4 ~ 'LP',
   #   TRUE ~ 'OTHER'
   # )))  %>%
   # mutate(n_class = factor(case_when(
   #   n_farm_inputs >= 10 ~ 'HN',
   #   n_farm_inputs < 10 ~ 'LN',
   #   TRUE ~ 'OTHER'
   # ))) %>%
   # mutate(alln_class = factor(case_when(
   #   n_farm_inputs >= 10 | p_dev_inputs >= 4 ~ 'HN',
   #   n_farm_inputs < 10 | p_dev_inputs < 4 ~ 'LN',
   #   TRUE ~ 'OTHER'
   # ))) %>%
   # mutate(check_nutr = factor(case_when(
   #   n_class == "HN" | p_class == "HP" ~ 'HC',
   #   n_class == "LN" | p_class == "LP" ~ 'LC',
   #   TRUE ~ 'OTHER'
   # ))) %>%
   # mutate(high_pred = factor(case_when(
   #   (n_farm_inputs >= 10 | p_dev_inputs >= 4) & pred_cyano >= 5 ~ 'HNHC',
   #   (n_farm_inputs < 10 | p_dev_inputs < 4) & pred_cyano >= 5 ~ 'LNHC',
   #   TRUE ~ 'OTHER'
   # )))  %>%
   # mutate(low_pred = factor(case_when(
   #   (n_farm_inputs >= 10 | p_dev_inputs >= 4) & pred_cyano < 5 ~ 'HNLC',
   #   (n_farm_inputs < 10 | p_dev_inputs < 4) & pred_cyano < 5 ~ 'LNLC',
   #   TRUE ~ 'OTHER'
   # ))) %>%
  mutate(all_pred = factor(case_when(
    (n_farm_inputs >= 10 | p_dev_inputs >= 4) & pred_cyano >= 5 ~ 'HNHC',
    (n_farm_inputs < 10 | p_dev_inputs < 4) & pred_cyano >= 5 ~ 'LNHC',
    (n_farm_inputs >= 10 | p_dev_inputs >= 4) & pred_cyano < 5 ~ 'HNLC',
    (n_farm_inputs < 10 | p_dev_inputs < 4) & pred_cyano < 5 ~ 'LNLC',
    TRUE ~ 'OTHER'
  )))

pred_filter <- comp_cyano |>
  filter(!is.na(all_pred))

comp_cyano$Shape <- st_point_on_surface(comp_cyano$Shape)|>
  st_transform(crs=5072)

HNHC <- comp_cyano |>
  filter(all_pred == 'HNHC')

LNLC <- comp_cyano |>
  filter(all_pred == 'LNLC')

LNHC <- comp_cyano |>
  filter(all_pred == 'LNHC')

HNLC <- comp_cyano |>
  filter(all_pred == 'HNLC')

ggplot(pred_filter, aes(color = all_pred)) +
  geom_sf(size = 0.4) +
  facet_wrap(~all_pred) +
  labs(title = "Where are the nutrient/cyano @ 100k cutoff?") +
  geom_sf(data = states, fill = NA, color = "black", lwd = 0.1) +
  theme(plot.title = element_text(size = 12)) +
  guides(colour = guide_legend(override.aes = list(size=4)))

ggsave("new_100k_cyano3.jpeg", width = 12, height = 8, device = 'jpeg', dpi = 500)

# Ratios --------------------------------------------------------------------

for (Shape in 1:length(comp_cyano)) {
  shapes <- comp_cyano$Shape
  comp_cyano$custom_area <- st_area(shapes)
}

comp_cyano$custom_area <- drop_units(comp_cyano$custom_area)

# custom_area <- pred_df_t |>
#   select(c(COMID, custom_area))
#
# pred_filter <- st_join(pred_df, custom_area)

comp_cyano <- comp_cyano %>%
  mutate(area_km = custom_area / 1000000,
         ad_ratio = sqrt(area_km) / MAXDEPTH) %>%
  filter(MAXDEPTH > 0)

# na_ad <- comp_micx_filter |>
#   filter(ad_ratio == Inf)
#
# names <- PredDataMini |>
#   select(c(COMID, nars_name)) |>
#   st_drop_geometry()
# na_ad <- merge(na_ad, names, by='COMID')

comp_sample <- sample_n(comp_cyano, 5000)

ggplot(comp_cyano, aes(x=area_km, y=MAXDEPTH, color = all_pred)) +
  geom_point(alpha = 0.25) +
  geom_smooth(method = "lm") +
  #  facet_wrap(~all_pred) +
  ylim(0,50) +
  xlim(0,100) +
  labs(y = "Lake Depth (m)", x = "Lake Area (km^2)", fill = 'Class',
       title = 'Area:Depth Ratio and Cyano classes')

ggsave("AD_cyano_scat.jpeg", width = 10, height = 8, device = 'jpeg', dpi = 500)

#
# names <- PredDataMini |>
#   select(c(COMID, nars_name)) |>
#   st_drop_geometry()
# na_ad <- merge(na_ad, names, by='COMID')

comp_cyano <- comp_cyano %>%
  mutate(AD_class = factor(case_when(ad_ratio <= 0.1 ~ 'B1',
                                     ad_ratio >= 0.1 & ad_ratio < 0.2 ~ 'B2',
                                     ad_ratio >= 0.2 & ad_ratio < 0.5 ~ 'B3',
                                     ad_ratio >= 0.5 & ad_ratio < 2.5 ~ 'B4',
                                     ad_ratio >= 2.5 & ad_ratio < 5 ~ 'B5',
                                     ad_ratio >= 5 ~ 'B6'),
                           levels = c('B1', 'B2', 'B3', 'B4', 'B5','B6'))) %>%
  arrange(AD_class)

ggplot(comp_micx, aes(x=all_pred, y=drain_ratio, fill=all_pred)) +
  geom_boxplot() +
  ylim(0,0.1)

ggsave("box_micx_drainage.jpeg", width = 10, height = 8, device = 'jpeg', dpi = 500)


# na_ratio <- comp_micx_filter |>
#   filter(AD_class == 'OTHER')

# cor(pred_filter$ad_ratio, pred_filter$pred_cyano,
#     method = 'spearman', use = "pairwise.complete.obs")

AD_labels <- c('< 0.1','0.1-0.2', '0.2-0.5', '0.5-2.5','2.5-1', '> 5')
AD_col <- rev(RColorBrewer::brewer.pal(6, "Spectral"))

ggplot(comp_cyano, aes(color = AD_class)) +
  geom_sf(size = 0.3) +
  facet_wrap(~all_pred) +
  scale_color_manual(values = AD_col,
                     labels = AD_labels,
                     name = "Ratio") +
  labs(title = "AREA:DEPTH Ratio - CYANO") +
  geom_sf(data = states, fill = NA, color = "black", lwd = 0.1) +
  theme(plot.title = element_text(size = 12)) +
  guides(colour = guide_legend(override.aes = list(size=4)))

ggsave("AD_ratio_class_cyano.jpeg", width = 12, height = 8, device = 'jpeg', dpi = 500)

ggplot(comp_cyano, aes(x=(ad_ratio), fill = all_pred)) +
  geom_density(size = 0.75) +
  facet_wrap(~all_pred, nrow=4, ncol=1) +
  xlim(0,0.5) +
  labs(y = "Density", x = "A:D Ratio", fill = 'Class',
       title = 'A:D Ratio- CYANO')

ggsave("AD_cy_den_16.jpeg", width = 8, height = 12, device = 'jpeg', dpi = 500)

# drainage ratio

comp_cyano <- comp_cyano %>%
  mutate(drain_class = factor(case_when(drain_ratio <= 0.0048 ~ 'B1',
                                     drain_ratio >= 0.0048 & drain_ratio < 0.0163 ~ 'B2',
                                     drain_ratio >= 0.0163 & drain_ratio < 0.0720 ~ 'B3',
                                     drain_ratio >= 0.0720 ~ 'B4',
                                     TRUE ~ 'OTHER')))

drain_labels <- c('<= 0.0048', '0.0048-0.0163','0.0163-0.0720','>=0.0720')
drain_col <- rev(RColorBrewer::brewer.pal(4, "Spectral"))



ggplot(comp_cyano, aes(color = drain_class)) +
  geom_sf(size = 0.3) +
  facet_wrap(~all_pred) +
  scale_color_manual(values = drain_col,
                     labels = drain_labels,
                     name = "Drain Ratio") +
  labs(title = "Drain Ratio by Nutrient Class - CYANO") +
  geom_sf(data = states, fill = NA, color = "black", lwd = 0.1) +
  theme(plot.title = element_text(size = 12)) +
  guides(colour = guide_legend(override.aes = list(size=4)))

ggsave("drain_cyano_map.jpeg", width = 12, height = 8, device = 'jpeg', dpi = 500)

ggplot(comp_cyano, aes(x=(drain_ratio), fill = all_pred)) +
  geom_density(size = 0.75) +
  facet_wrap(~all_pred, nrow=4, ncol=1) +
  xlim(0,0.075) +
  labs(y = "Density", x = "Drainage Ratio", fill = 'Class',
       title = 'Drain Ratio - CYANO')

ggsave("box_cyano_ad.jpeg", width = 10, height = 8, device = 'jpeg', dpi = 500)


# Iowa and North Dakota --------------------------------------------------------

IA_cyano <- pred_filter |>
  filter(state == 'IA')

IA_map <- states(cb = TRUE, progress_bar = FALSE)  %>%
  filter(STUSPS %in% c('IA'))  %>%
  st_transform(crs = 5072)

IA_cyano <- IA_cyano %>%
  mutate(cyano_class = factor(case_when(
    pred_cyano >= 5 ~ 'HC',
    pred_cyano < 5 ~'LC',
    TRUE ~ 'OTHER'
  ))) %>%
  mutate(p_class = factor(case_when(
    p_dev_inputs >= 4 ~ 'HP',
    p_dev_inputs < 4 ~ 'LP',
    TRUE ~ 'OTHER'
  )))  %>%
  mutate(n_class = factor(case_when(
    n_farm_inputs >= 10 ~ 'HN',
    n_farm_inputs < 10 ~ 'LN',
    TRUE ~ 'OTHER'
  ))) %>%
  mutate(alln_class = factor(case_when(
    n_farm_inputs >= 10 | p_dev_inputs >= 4 ~ 'HN',
    n_farm_inputs < 10 | p_dev_inputs < 4 ~ 'LN',
    TRUE ~ 'OTHER'
  )))

CIA_HN <- IA_cyano |>
  filter(alln_class == 'HN')

CIA_LN <- IA_cyano |>
  filter(alln_class == 'LN')


ggplot(IA_cyano, aes(color = all_pred)) +
  geom_sf(size = 0.7) +
  facet_wrap(~all_pred) +
  labs(title = "Iowa Nutrient/Cyano Classes") +
  geom_sf(data = IA_map, fill = NA, color = "black", lwd = 0.1) +
  theme(plot.title = element_text(size = 12)) +
  guides(colour = guide_legend(override.aes = list(size=4)))

ggsave("IA_class_cyano.jpeg", width = 12, height = 8, device = 'jpeg', dpi = 500)

# north dakota

ND_cyano <- pred_filter |>
  filter(state == 'ND')

ND_map <- states(cb = TRUE, progress_bar = FALSE)  %>%
  filter(STUSPS %in% c('ND'))  %>%
  st_transform(crs = 5072)

ND_cyano <- ND_cyano %>%
  mutate(cyano_class = factor(case_when(
    pred_cyano >= 5 ~ 'HC',
    pred_cyano < 5 ~'LC',
    TRUE ~ 'OTHER'
  ))) %>%
  mutate(p_class = factor(case_when(
    p_dev_inputs >= 4 ~ 'HP',
    p_dev_inputs < 4 ~ 'LP',
    TRUE ~ 'OTHER'
  )))  %>%
  mutate(n_class = factor(case_when(
    n_farm_inputs >= 10 ~ 'HN',
    n_farm_inputs < 10 ~ 'LN',
    TRUE ~ 'OTHER'
  ))) %>%
  mutate(alln_class = factor(case_when(
    n_farm_inputs >= 10 | p_dev_inputs >= 4 ~ 'HN',
    n_farm_inputs < 10 | p_dev_inputs < 4 ~ 'LN',
    TRUE ~ 'OTHER'
  )))

CND_HN <- ND_cyano |>
  filter(alln_class == 'HN')

CND_LN <- ND_cyano |>
  filter(alln_class == 'LN')


ggplot(ND_cyano, aes(color = all_pred)) +
  geom_sf(size = 0.7) +
  facet_wrap(~all_pred) +
  labs(title = "North Dakota Nutrient/Cyano Classes") +
  geom_sf(data = ND_map, fill = NA, color = "black", lwd = 0.1) +
  theme(plot.title = element_text(size = 12)) +
  guides(colour = guide_legend(override.aes = list(size=4)))

ggsave("ND_class_cyano2.jpeg", width = 12, height = 8, device = 'jpeg', dpi = 500)

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

ggplot(pred_filter, aes(x=fst_ws, fill = all_pred)) +
  geom_density(size = 0.75, alpha = 0.5) +
  facet_wrap(~all_pred, nrow=4, ncol=1) +
  xlim(0,50) +
  labs(y = "Density", x = "Forest Cover %", fill = 'Class',
       title = 'Forest Cover - Cyano')

ggsave("new_fst_cyano.jpeg", width = 8, height = 12, device = 'jpeg', dpi = 500)

ggplot(pred_filter, aes(x=Tmean8110Ws, fill=all_pred)) +
  geom_density(size = 0.75, alpha = 0.5) +
  facet_wrap(~all_pred, nrow = 4, ncol =1) +
  labs(y = "Density", x = "30 Year Temp Average", fill = 'Class',
       title = "Temperature - Cyano")

ggsave("new_temp_cyano.jpeg", width = 12, height = 8, device = 'jpeg', dpi = 500)

ggplot(pred_filter, aes(x=Precip8110Ws, fill = all_pred)) +
  geom_density(size = 0.75, alpha = 0.5) +
  facet_wrap(~all_pred, nrow = 4, ncol =1) +
  xlim(0,2000) +
  labs(y = "Density", x = "30 Year Precip Average", fill = 'Class',
       title = 'Precipitation - Cyano')

ggsave("new_precip_cyano.jpeg", width = 12, height = 8, device = 'jpeg', dpi = 500)

ggplot(pred_filter, aes(x=BFIWs, fill = all_pred)) +
  geom_density(size = 0.75, alpha = 0.5) +
  facet_wrap(~all_pred, nrow = 4, ncol =1) +
  labs(y = "Density", x = "% of Flow that is Base Flow", fill = 'Class',
       title = "Base Flow - Cyano")

ggsave("new_BFIW_cyano2.jpeg", width = 8, height = 12, device = 'jpeg', dpi = 500)

ggplot(pred_filter, aes(x=Runoff.Str, fill = all_pred)) +
  geom_density(size = 0.75, alpha = 0.5) +
  facet_wrap(~all_pred, nrow = 4, ncol =1) +
  xlim(0,1000) +
  labs(y = "Density", x = "Runoff", fill = 'Class',
       title = "Runoff - Cyano")

ggsave("new_runoff_cyano.jpeg", width = 8, height = 12, device = 'jpeg', dpi = 500)

ggplot(pred_filter, aes(lakemorpho_fetch, fill = all_pred)) +
  geom_density(size = 0.75, alpha = 0.5) +
  facet_wrap(~all_pred, nrow = 4, ncol =1) +
  xlim(0,1000) +
  labs(y = "Density", x = "Fetch (m)", fill = 'Class',
       title = "Lake Fetch - Cyano")

ggsave("new_fetch_cyano.jpeg", width = 8, height = 12, device = 'jpeg', dpi = 500)


# Print the result
print(result_base_R)

ggplot(comp_cyano_filter, aes(x=AG_ECO3, fill = nutr_class)) +
  geom_histogram(position="dodge", stat = "count") +
  labs(y = "Count", x = "Ecoregion", fill = 'Class',
       title = "Ecoregions - Cyano")

ggplot(comp_cyano_filter, aes(fill=nutr_class))


