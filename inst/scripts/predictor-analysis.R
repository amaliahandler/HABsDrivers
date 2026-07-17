
# Micx/Cyano Predictors comparison analysis
# 11-21-2024
# must load all pred results and packages

states <- states(cb = TRUE, progress_bar = FALSE)  %>%
  filter(!STUSPS %in% c('HI', 'PR', 'AK', 'MP', 'GU', 'AS', 'VI'))  %>%
  st_transform(crs = 5072)

# Microcystin ==================================================================

comp_micx <- st_join(wbd_copy, micx_pred_df)

pred_cols <- PredDataMini |>
  dplyr::select(c(COMID, state, ag_eco9, nars_name, WsAreaSqKm, LAGOSLakeDepth, NHDLakeDepth))

state_df <- left_join(PredData, pred_cols, by = 'COMID')

state_lakes <- state_df |>
  filter(state == 'CA' | state == 'ID' | state == "OR" | state == "WA") |>
  dplyr::select(c(COMID, state, ag_eco9, nars_name, WsAreaSqKm, LAGOSLakeDepth, NHDLakeDepth, MAXDEPTH))

state_lakes <- state_lakes |>
  st_transform(crs = 4269) |>
  mutate(coords = st_coordinates(Shape),
         lat.x = coords[, 'X'],
         lon.y = coords[, 'Y']) |>
  dplyr::select(-c(coords)) |>
  st_drop_geometry()

# comp_micx <- comp_micx |>
#   drop_na(pred_micx)

micx_pred_df <- micx_pred_df %>%
  mutate(y_partial_p_farm = (coef(model_micx_nolakes)[2]) * p_farm_inputs,
         y_partial_n_dev = (coef(model_micx_nolakes)[3]) * n_dev_inputs,
         y_partial_nutr_all = y_partial_p_farm + y_partial_n_dev)

# Nutrients --------------------------------------------------------------------

comp_micx$nutr_all <- comp_micx$p_farm_inputs + comp_micx$n_dev_inputs

PredData <- PredData |>
  rowwise() |>
  mutate(all_n = n_dev_inputs + n_farm_inputs,
         all_p = p_dev_inputs + p_farm_inputs)

PredData <- PredData %>%
  # mutate(micx_class = factor(case_when(
  #   pred_micx_fit < 0.25 ~ 'B1',
  #   pred_micx_fit >= 0.25 & pred_micx_fit < 0.50 ~ 'B2',
  #   pred_micx_fit >= 0.50 & pred_micx_fit < 0.75 ~ 'B3',
  #   pred_micx_fit > 0.75 ~ 'B4',
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
  #))) %>%
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

PredData <- PredData |>
  mutate(micx_pred = factor(case_when(
    (n_dev_inputs >= 10 | p_farm_inputs >= 4) & pred_micx_fit >= 0.50 ~ 'HNHM',
    (n_dev_inputs < 10 | p_farm_inputs < 4) & pred_micx_fit >= 0.50 ~ 'LNHM',
    (n_dev_inputs >= 10 | p_farm_inputs >= 4) & pred_micx_fit < 0.50 ~ 'HNLM',
    (n_dev_inputs < 10 | p_farm_inputs < 4) & pred_micx_fit < 0.50 ~ 'LNLM',
    TRUE ~ 'OTHER'),
    levels = c('HNHM','HNLM','LNHM','LNLM'))) %>%
    arrange(micx_pred)

# Ratios -----------------------------------------------------------------------

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

PredData <- PredData %>%
  # mutate(area_km = custom_area / 1000000,
  #        ad_ratio2 = (sqrt(area_km)) / MAXDEPTH) %>%
  filter(MAXDEPTH > 0)

# comp_micx <- comp_micx %>%
#   mutate(drain_manual = WsAreaSqKm / area_km,
#          WsAreaHa = WsAreaSqKm * 100)

# Only run after area has been calculated
comp_micx$Shape <- st_point_on_surface(comp_micx$Shape)|>
  st_transform(crs=5072)

# y partial --------------------------------------------------------------------

# micx_pred_df <- micx_pred_df %>%
#   mutate(y_partial_p_farm = (coef(model_micx_nolakes)[2]) * p_farm_inputs,
#          y_partial_n_dev = (coef(model_micx_nolakes)[3]) * n_dev_inputs,
#          y_partial_nutr_all = y_partial_p_farm + y_partial_n_dev)
#
# micx_pred_df <- micx_pred_df %>%
#   mutate(ypar_class = factor(case_when(y_partial_nutr_all <= 0.005 ~ 'B1',
#                                        y_partial_nutr_all >= 0.005 & y_partial_nutr_all < 0.1 ~ 'B2',
#                                        y_partial_nutr_all >= 0.1 & y_partial_nutr_all < 0.5 ~ 'B3',
#                                        y_partial_nutr_all >= 0.5 & y_partial_nutr_all < 1 ~ 'B4',
#                                        y_partial_nutr_all >= 1 ~ 'B5'),
#                              levels = c('B1', 'B2', 'B3', 'B4', 'B5'))) %>%
#   arrange(ypar_class)
#
# # comp_cyano_filter$SandWs[is.na(comp_cyano_filter$SandWs)] <- 0

#
# yparm_labels <- c('< 0.005','0.005-0.1', '0.1-0.5','0.5-1', '>1')
# yparm_col <- rev(RColorBrewer::brewer.pal(5, "Spectral"))
#
# ggplot(micx_pred_df, aes(color = ypar_class)) +
#   geom_sf(size = 0.5) +
#   scale_color_manual(values = yparm_col,
#                      labels = yparm_labels,
#                      name = "Y Partial") +
#   labs(title = "Y Partial / All nutrients (Micx)") +
#   geom_sf(data = states, fill = NA, color = "black", lwd = 0.1) +
#   theme(plot.title = element_text(size = 12)) +
#   guides(colour = guide_legend(override.aes = list(size=4)))
#
# ggsave("ypar_micx.jpeg", width = 12, height = 8, device = 'jpeg', dpi = 500)

# Oregon -----------------------------------------------------------------------

oregon <- comp_micx |>
  filter(state == 'OR') |>
  st_transform(crs=5072)

oregon_counties <- counties("OR") |>
  st_transform(crs=5072)

pblg <- places('OR') |>
  subset(NAME %in% c("Portland","Burns", "La Grande", "Bend")) |>
  st_point_on_surface() |>
  st_transform(crs=2992)

medford <- places("OR") |>
  subset(NAME %in% c("Medford")) |>
  st_point_on_surface() |>
  st_transform(crs=2992)

corvo <- places("OR") |>
  subset(NAME %in% c("Corvallis")) |>
  st_point_on_surface() |>
  st_transform(crs=2992)

oregon_map <- states(cb = TRUE, progress_bar = FALSE)  %>%
  filter(STUSPS %in% c('OR'))  %>%
  st_transform(crs = 2992)

oregon <- oregon %>%
  arrange(pred_micx[, "fit"])

micx_colors <- c("#2980b9","#aed6f1","#f0b27a","#d35405")
micx_labels <- c("< 25%", "25-50%", "50-75%", "> 75%")

oregon <- oregon %>%
  mutate(disc_micx = factor(case_when(pred_micx[, "fit"] < 0.25 ~ 'B1', # under 25k
                                       pred_micx[, "fit"] >= 0.25 & pred_micx[, "fit"] < 0.50 ~ 'B2', # 25k - 50k
                                       pred_micx[, "fit"] >= 0.50 & pred_micx[, "fit"] < 0.75 ~ 'B3', # 50k - 100k
                                       pred_micx[, "fit"] > 0.75 ~ 'B4', # 500k
                                       TRUE ~ NA),
                             levels = c('B1', 'B2', 'B3', 'B4'))) %>%
  arrange(disc_micx)

p <- ggplot() +
    geom_sf(data = oregon_counties, fill = NA, color = "darkgrey", lwd = 0.7, alpha = 0.7) +
    geom_sf(data = oregon,
          aes(color = disc_micx),
          size = 3) +
  scale_color_manual(values = micx_colors,
                     labels = micx_labels,
                     name = "Probability") +
  geom_sf(data = pblg, fill = NA, color = "black", size = 2, shape = 15, alpha = 0.7) +
  geom_sf_text(data = pblg, aes(label = NAME, fontface = "bold"), size = 3.5, nudge_x = 0.17, nudge_y = 0.09) +
  geom_sf(data = medford, fill = NA, color = "black", size = 2, shape = 15, alpha = 0.7) +
  geom_sf_text(data = medford, aes(label = NAME, fontface = "bold"), size = 3.5, nudge_x = -0.15, nudge_y = -0.09) +
  geom_sf(data = corvo, fill = NA, color = "black", size = 2, shape = 15, alpha = 0.7) +
  geom_sf_text(data = corvo, aes(label = NAME, fontface = "bold"), size = 3.5, nudge_x = 0.27, nudge_y = -0.05) +
  theme_void() +
  theme(axis.title.y=element_blank(),
        legend.position = "none")

ggsave("micx_oregon.jpeg", width = 12, height = 8, device = 'jpeg', dpi = 1500)


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

comp_micx <- comp_micx %>%
  mutate(disc_fst = factor(case_when(fst_ws < 25 ~ 'B1',
                                     fst_ws >= 25 & fst_ws < 50 ~ 'B2',
                                     fst_ws >= 50 & fst_ws < 75 ~ 'B3',
                                     fst_ws >= 75  ~ 'B4'),
                           levels = c('B1', 'B2', 'B3', 'B4'))) %>%
  arrange(disc_fst)


fst_labels = c("0-25%", "25-50%", "50-75%", ">75%")
fst_cols <- RColorBrewer::brewer.pal(4, "YlGn")

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

comp_cyano <- st_join(wbd_copy, pred_df)

pred_cols <- PredDataMini |>
  dplyr::select(c(COMID, Runoff.Str, state, drain_ratio, ag_eco9, WsAreaSqKm))

comp_cyano <- left_join(comp_cyano, pred_cols, by = 'COMID')

comp_cyano <- comp_cyano |>
  drop_na(pred_cyano)

# # y partial --------------------------------------------------------------------
#
# pred_filter <- pred_filter %>%
#   mutate(y_partial_p_dev = (coef(model_cyano_nolakes)[3]) * p_dev_inputs,
#          y_partial_n_farm = (coef(model_cyano_nolakes)[2]) * n_farm_inputs,
#          y_partial_nutr_all = y_partial_p_dev + y_partial_n_farm)
#
# pred_filter <- pred_filter %>%
#   mutate(ypar_class = factor(case_when(y_partial_nutr_all <= 0.001 ~ 'B1',
#                                        y_partial_nutr_all >= 0.001 & y_partial_nutr_all < 0.005 ~ 'B2',
#                                        y_partial_nutr_all >= 0.005 & y_partial_nutr_all < 0.1 ~ 'B3',
#                                        y_partial_nutr_all >= 0.1 & y_partial_nutr_all < 1 ~ 'B4',
#                                        y_partial_nutr_all >= 1 ~ 'B5'),
#                              levels = c('B1', 'B2', 'B3', 'B4', 'B5'))) %>%
#   arrange(ypar_class)
#
# # comp_cyano_filter$SandWs[is.na(comp_cyano_filter$SandWs)] <- 0
#
# ypar_labels <- c('< 0.001','0.001-0.005', '0.005-0.1','0.1-1', '>1')
# ypar_col <- rev(RColorBrewer::brewer.pal(5, "Spectral"))
#
#
# ggplot(pred_filter, aes(color = ypar_class)) +
#   geom_sf(size = 0.5) +
#   scale_color_manual(values = ypar_col,
#                      labels = ypar_labels,
#                      name = "Y Partial") +
#   labs(title = "Y Partial / All nutrients (Cyano)") +
#   geom_sf(data = states, fill = NA, color = "black", lwd = 0.1) +
#   theme(plot.title = element_text(size = 12)) +
#   guides(colour = guide_legend(override.aes = list(size=4)))
#
# ggsave("ypar_cyano.jpeg", width = 12, height = 8, device = 'jpeg', dpi = 500)

# Nutrients --------------------------------------------------------------------

comp_cyano$nutr_all <- comp_cyano$n_farm_inputs + comp_cyano$p_dev_inputs

# comp_cyano <- comp_cyano %>%
#   mutate(nutr_class = factor(case_when(nutr_all >= 10 & pred_cyano <= 5 ~ 'HNLC',
#                                        nutr_all <= 10 & pred_cyano >= 5 ~ 'LNHC',
#                                        nutr_all >= 10 & pred_cyano >= 5 ~ 'HNHC',
#                                        nutr_all <= 10 & pred_cyano <= 5 ~ 'LNLC')))

# PredData <- PredData %>%
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

PredData <- PredData |>
  mutate(cyano_pred = factor(case_when(
    (n_farm_inputs >= 10 | p_dev_inputs >= 4) & pred_cyano_fit >= 5 ~ 'HNHC',
    (n_farm_inputs < 10 | p_dev_inputs < 4) & pred_cyano_fit >= 5 ~ 'LNHC',
    (n_farm_inputs >= 10 | p_dev_inputs >= 4) & pred_cyano_fit < 5 ~ 'HNLC',
    (n_farm_inputs < 10 | p_dev_inputs < 4) & pred_cyano_fit < 5 ~ 'LNLC',
    TRUE ~ 'OTHER'),
    levels = c('HNHC','HNLC','LNHC','LNLC'))) %>%
  arrange(cyano_pred)

# nutr_grp_cols <- c("#9c0082","#cc6de4","#4e8562", "#8bd1a5")
# high_low <- c("#9c0082", "#8bd1a5")
# high_low_labels <- c("High Nutrient", "Low Nutrient")
#
# ggplot(comp_cyano, aes(color = alln_class)) +
#   geom_sf(size = 0.5) +
#   scale_color_manual(values = high_low,
#                      labels = high_low_labels ,
#                      name = "Nutrient Levels") +
#   geom_sf(data = states, fill = NA, color = "black", lwd = 0.1) +
#   theme(plot.title = element_text(size = 12)) +
#   theme_void() +
#   theme(legend.position = c(0.15, 0.12)) +
#   guides(color = guide_legend(ncol=2, override.aes = list(size=4, shape = 15)))
#
# #   scale_color_manual(values = cyano_colors,
# #                      labels = cyano_labels,
# #                      name = "Cells/mL") +
#
#
# ggsave("nutrient_biplot.jpeg", width = 12, height = 8, device = 'jpeg', dpi = 500)

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

# comp_cyano <- comp_cyano %>%
#   mutate(drain_manual = WsAreaSqKm / area_km,
#          WsAreaHa = WsAreaSqKm * 100)

comp_cyano$Shape <- st_point_on_surface(comp_cyano$Shape)|>
  st_transform(crs=5072)

# Oregon -----------------------------------------------------------------------

oregon_cyano <- comp_cyano |>
  filter(state == 'OR') |>
  st_transform(crs = 2992)

oregon_river <- linear_water("OR")

oregon_cyano <- oregon_cyano %>%
  mutate(disc_cyano = factor(case_when(pred_cyano[, "fit"] < 4.41497 ~ 'B1', # under 25k
                                       pred_cyano[, "fit"] >= 4.41497 & pred_cyano[, "fit"] < 4.70757 ~ 'B2', # 25k - 50k
                                       pred_cyano[, "fit"] >= 4.70757 & pred_cyano[, "fit"] < 5.00432 ~ 'B3', # 50k - 100k
                                       pred_cyano[, "fit"] >= 5.00432 & pred_cyano[, "fit"] < 5.39967 ~ 'B4', # 100k - 250k
                                       pred_cyano[, "fit"] >= 5.39967 & pred_cyano[, "fit"] < 5.69984 ~ 'B5', # 250k-500k
                                       pred_cyano[, "fit"] > 5.69984 ~ 'B6', # 500k
                                       TRUE ~ NA),
                             levels = c('B1', 'B2', 'B3', 'B4', 'B5', 'B6'))) %>%
  arrange(disc_cyano)

# summary(pred_df$disc_cyano)
# sum(is.na(pred_df$disc_cyano))
sub_25 <- oregon_cyano %>%
  filter(disc_cyano == 'B1')


cyano_labels <- c('< 25', '25 - 50', '50 - 100', '100 - 250', '250 - 500', ' > 500')
cyano_colors <- c("#21618C","#5499C7","#A9CCE3","#EDBB99","#DC7633","#A04000")

q <- ggplot() +
  geom_sf(data = oregon_counties, fill = NA, color = "darkgrey", lwd = 0.7, alpha = 0.7) +
  geom_sf(data = oregon_cyano,
          aes(color = disc_cyano),
          size = 3) +
  scale_color_manual(values = cyano_colors,
                     labels = cyano_labels,
                     name = "Abundance (1000 cells/mL)") +
  geom_sf(data = sub_25,
          aes(color = disc_cyano),
          size = 2)  +  geom_sf(data = pblg, fill = NA, color = "black", size = 2, shape = 15, alpha = 0.7) +
  geom_sf_text(data = pblg, aes(label = NAME, fontface = "bold"), size = 3.5, nudge_x = 0.17, nudge_y = 0.09) +
  geom_sf(data = medford, fill = NA, color = "black", size = 2, shape = 15, alpha = 0.7) +
  geom_sf_text(data = medford, aes(label = NAME, fontface = "bold"), size = 3.5, nudge_x = -0.15, nudge_y = -0.09) +
  geom_sf(data = corvo, fill = NA, color = "black", size = 2, shape = 15, alpha = 0.7) +
  geom_sf_text(data = corvo, aes(label = NAME, fontface = "bold"), size = 3.5, nudge_x = 0.27, nudge_y = -0.05) +
  theme_void() +
  theme(axis.title.y=element_blank(),
        legend.position = "none")

ggsave("new_micx_preds26.jpeg", width = 10, height = 6, device = 'jpeg', dpi = 600)


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

# comparison mapping all nutrients ---------------------------------------------

PredData <- PredData %>%
  mutate(alln_class = factor(case_when(
    n_farm_inputs >= 10 | p_dev_inputs >= 4 | n_dev_inputs >= 10 | p_farm_inputs >= 4 ~ 'HN',
    n_farm_inputs < 10 | p_dev_inputs < 4 | n_dev_inputs < 10 | p_farm_inputs < 4 ~ 'LN',
    TRUE ~ 'OTHER'),
    levels = c('LN','HN'))) %>%
  arrange(alln_class)

labels <- c('Low Nutrient',
            'Low Nutrient')

ggplot(PredData, aes(color = alln_class)) +
  geom_sf(size = 0.5) +
  scale_color_manual(values = c("#8bd1a5","#9c0082"),
                     labels = labels,
                     name = "") +
  geom_sf(data = states, fill = NA, color = "black", lwd = 0.1) +
  theme_void() +
  theme(legend.position = c(0.75, 0.90),
        legend.text=element_text(size=14)) +
  guides(colour = guide_legend(override.aes = list(size=4)))

ggsave("all_nutrient.jpeg", width = 12, height = 8, device = 'jpeg', dpi = 1600)

grid <- states %>%
  st_make_grid(n = c(150,150),
               what = 'polygons',
               square = TRUE)

grid_map <- st_intersection(states, grid) %>%
  st_as_sf() %>%
  mutate(grid_id = 1:n())

ggplot() +
  geom_sf(data = cyanohabs_grid) +
  theme_void()

Sys.time()
cyanohabs_grid <- grid_map %>%
  st_join(comp_cyano) %>%
  group_by(grid_id) %>%
  summarize(cyano_avg = mean(pred_cyano))
Sys.time()

cyanohabs_grid <- cyanohabs_grid |>
  drop_na(disc_cyano)

cyanohabs_grid <- cyanohabs_grid %>%
  mutate(disc_cyano = factor(case_when(cyano_avg < 4 ~ 'B1', # under 10k
                                       cyano_avg >= 4 & cyano_avg < 4.7 ~ 'B2', # 10k - 50k
                                       cyano_avg >= 4.7 & cyano_avg < 5 ~ 'B3', # 50k - 100k
                                       cyano_avg >= 5 & cyano_avg < 5.3 ~ 'B4', # 100k - 200k
                                       cyano_avg >= 5.3 & cyano_avg < 6 ~ 'B5', # 200k - 1 mil
                                       cyano_avg > 6 ~ 'B6', # above 1 mil
                                       TRUE ~ NA),
                             levels = c('B1', 'B2', 'B3', 'B4', 'B5', 'B6'))) %>%
  arrange(disc_cyano)

# summary(pred_df$disc_cyano)
# sum(is.na(pred_df$disc_cyano))

cyano_labels <- c('< 10k', '10k - 50k', '50k - 100k', '100k - 200k', '200k - 1 million', ' > 1 million')

ggplot(cyanohabs_grid, aes(color = disc_cyano)) +
  geom_sf(size = 0.4) +
  scale_color_manual(values = c("#9f07f7", "#2B83BA", "#ABDDA4", "#f7d577", "#FDAE61","#D7191C"),
                     labels = cyano_labels,
                     name = "Cells/mL") +
  labs(title = "Cyanobacteria Predictions") +
  geom_sf(data = states, fill = NA, color = "black", lwd = 0.1) +
  theme_void() +
  guides(colour = guide_legend(override.aes = list(size=4)))

# Final Density Plots ==========================================================

# Precipitation

comp_cyano <- st_join(wbd_copy, PredData) |>
  dplyr::select(-c(pred_micx, micx_transform, COMID.x)) |>
  rename(COMID = COMID.y) |>
  drop_na(pred_cyano)

micxcat_labels <- c('High Nutrient, High HABs',
                    'High Nutrient, Low HABs',
                    'Low Nutrient, High HABs',
                    'Low Nutrient, Low HABs')

precip_den_micx <- ggplot(comp_micx, aes(x=ad_ratio, y= micx_class)) +
  ggridges::geom_density_ridges(aes(fill = micx_class),
                                scale = 2,
                                alpha = 0.85,
                                quantile_lines = TRUE, quantiles = 2) +
  scale_fill_manual(values = c("#9c0082","#cc6de4","#4e8562", "#8bd1a5")) +
  xlim(0,5) +
  labs(x = "ad:ratio", fill = 'Class',
       title = 'Microcystin') +
  theme(axis.title.y=element_blank(),
        legend.position = "none")


precip_den_cyano <- ggplot(comp_cyano, aes(x=Precip8110Ws, y= all_pred)) +
  ggridges::geom_density_ridges(aes(fill = all_pred),
                                scale = 2,
                                alpha = 0.85,
                                quantile_lines = TRUE, quantiles = 2) +
  scale_fill_manual(values = c("#9c0082","#cc6de4","#4e8562", "#8bd1a5"),
                    labels = micxcat_labels) +
  xlim(0,2000) +
  labs(x = "Precipitation (mm)", y = "Density Distribution",  fill = 'Class',
       title = 'Cyanobacteria') +
  guides(color = guide_legend(ncol=2, override.aes = list(size=4, shape = 15))) +
  theme(axis.title.y=element_blank(),
        legend.position = "none")


p <- ggpubr::ggarrange(precip_den_cyano, precip_den_micx,
                  ncol = 2, nrow = 1,
                  common.legend = TRUE)

obj <- ggpubr::get_legend(p)
ggpubr::as_ggplot(obj)

ggsave("ad_micx.jpeg", width = 7, height = 10, device = 'jpeg', dpi = 1600)

# Base Flow

micxcat_labels <- c('High Nutrient, High HABs',
                    'Low Nutrient, High HABs',
                    'High Nutrient, Low HABs',
                    'Low Nutrient, Low HABs')

baseflow_den_micx <- ggplot(comp_micx, aes(x=BFIWs, y= all_pred)) +
  ggridges::geom_density_ridges(aes(fill = all_pred),
                                scale = 2,
                                alpha = 0.85,
                                quantile_lines = TRUE, quantiles = 2) +
  scale_fill_manual(values = c("#9c0082","#4e8562", "#cc6de4", "#8bd1a5"),
                    labels = micxcat_labels) +
  xlim(0,100) +
  labs(x = "BaseFlow (%)", fill = 'Class',
       title = 'Microcystin') +
  theme(axis.title.y=element_blank())

baseflow_den_cyano <- ggplot(comp_cyano, aes(x=BFIWs, y= all_pred)) +
  ggridges::geom_density_ridges(aes(fill = all_pred),
                                scale = 2,
                                alpha = 0.85,
                                quantile_lines = TRUE, quantiles = 2) +
  scale_fill_manual(values = c("#9c0082","#4e8562", "#cc6de4", "#8bd1a5"),
                    labels = micxcat_labels) +
  xlim(0,100) +
  labs(x = "BaseFlow (%)", y = "Density Distribution",  fill = 'Class',
       title = 'Cyanobacteria') +
  theme(axis.title.y=element_blank()) +
  guides(color = guide_legend(ncol=2, override.aes = list(size=4, shape = 15)))

ggpubr::ggarrange(baseflow_den_cyano, baseflow_den_micx,
                  ncol = 2, nrow = 1,
                  common.legend = TRUE)

ggsave("BFIW_density_panel_prpgrn.jpeg", width = 12, height = 7, device = 'jpeg', dpi = 700)

# Area:Depth Ratio

ad_den_micx <- ggplot(comp_micx, aes(x=ad_ratio, y= all_pred)) +
  ggridges::geom_density_ridges(aes(fill = all_pred),
                                scale = 2,
                                alpha = 0.85,
                                quantile_lines = TRUE, quantiles = 2) +
  scale_fill_manual(values = c("#9c0082","#cc6de4","#4e8562", "#8bd1a5"),
                    labels = micxcat_labels) +
  xlim(0,0.5) +
  labs(x = "Area:Depth Ratio", y = "Density Distribution",  fill = 'Class',
       title = 'Microcystin') +
  guides(color = guide_legend(ncol=2, override.aes = list(size=4, shape = 15))) +
  theme(axis.title.y=element_blank(),
        legend.position = "none")

ad_den_cyano <- ggplot(comp_cyano, aes(x=ad_ratio, y= all_pred)) +
  ggridges::geom_density_ridges(aes(fill = all_pred),
                                scale = 2,
                                alpha = 0.85,
                                quantile_lines = TRUE, quantiles = 2) +
  scale_fill_manual(values = c("#9c0082","#cc6de4","#4e8562", "#8bd1a5"),
                    labels = micxcat_labels) +
  xlim(0,0.5) +
  labs(x = "Area:Depth Ratio", y = "Density Distribution",  fill = 'Class',
       title = 'Cyanobacteria') +
  guides(color = guide_legend(ncol=2, override.aes = list(size=4, shape = 15))) +
  theme(axis.title.y=element_blank(),
        legend.position = "none")

ggpubr::ggarrange(ad_den_micx, ad_den_cyano,
                  ncol = 2, nrow = 1,
                  common.legend = TRUE)

ggsave("ad_all.jpeg", width = 12, height = 7, device = 'jpeg', dpi = 500)

# personal interest analysis ---------------------------------------------------

get_nlcd_2 <- function(coms){
  lc_get_data(metric = 'Om, WtDep, HydrlCond, Perm',
              aoi='watershed',
              comid = coms,
              showAreaSqKm = TRUE)
}

hydro <- comp_cyano |>
  dplyr::select(pred_cyano, COMID, all_pred) |>
  merge(PredDataVar) |>
  rename(cyano_grp = all_pred) |>
  st_drop_geometry()

hydro_all <- comp_micx |>
  dplyr::select(pred_micx, COMID, Shape, all_pred) |>
  merge(hydro) |>
  rename(micx_grp = all_pred)
rm(hydro)

chunks <- split(hydro_all$COMID, ceiling(seq_along(hydro_all$COMID) / 10000))

ncldMas <- do.call(rbind, lapply(chunks, get_nlcd_2))

hydro_all <- merge(hydro_all, ncldMas, by = 'COMID')

cor(hydro_all$MAXDEPTH, hydro_all$pred_cyano,
    method = "spearman", use = "pairwise.complete.obs")


ggplot(hydro_all, aes(x=PERMWS), fill = cyano_grp) +
  geom_density() +
  facet_wrap(~cyano_grp) +
  labs(x = "Mean permeability (cm/hour) of soils", y = "Density Distribution",  fill = 'Class')

hydro_all |>
  filter(cyano_grp == "LNHC") |>
  summary()

# farm vs development nutrient maps --------------------------------------------

final_pred <- wbd_copy %>%
  st_point_on_surface() %>%
  dplyr::select(COMID) %>%
  inner_join(PredData, by = 'COMID') %>%
  dplyr::select(-COMID) %>%
  drop_na()

compiled_nutr <- final_pred %>%
  mutate(farm_class = factor(case_when(n_farm_inputs < 10 | p_farm_inputs < 4 ~ "LF",
                                       n_farm_inputs >= 10 | p_farm_inputs >= 4 ~ "HF",
                                      TRUE ~ NA)),
         dev_class = factor(case_when(n_dev_inputs < 10 | p_dev_inputs < 4 ~ 'LD',
                                      n_dev_inputs >= 10 | p_dev_inputs >= 4 ~ 'HD',
                                      TRUE ~ NA))) |>
  dplyr::select(n_dev_inputs, n_farm_inputs, p_dev_inputs, p_farm_inputs, farm_class, dev_class, Shape)

class_labels <- c('High Inputs', 'Low Inputs')
class_cols <- c("darkorange","lightblue")

ggplot() +
  geom_sf(data = compiled_nutr,
          aes(color = farm_class),
          size = 0.2,
          alpha = 0.8) +
  scale_color_manual(values = class_cols,
                     labels = class_labels,
                     name = "Farm Input Levels") +
  geom_sf(data = states, fill = NA, color = "black", lwd = 0.1) +
  theme_void() +
  theme(legend.position = c(0.80, 0.90),
        legend.text=element_text(size=14),
        legend.title=element_text(size=16)) +
  guides(color = guide_legend(ncol=2, override.aes = list(size=4, shape = 15)))

ggsave("farm_nutrients.jpeg", width = 12, height = 8, device = 'jpeg', dpi = 500)

ggplot(compiled_nutr) +
  geom_sf(data = compiled_nutr,
          aes(color = dev_class),
          size = 0.2,
          alpha = 0.8) +

# fit anlysis ------------------------------------------------------------------

micx_pred_df <- micx_pred_df |>
  mutate(range = (pred_micx[, "upr"] - pred_micx[, "lwr"]))

labels = c("0-25", "25-40", "40-60", "60-75", "75-100")
breaks <- c(0.25,0.4,0.60,0.75,1.0)
micx_colors <- rev(RColorBrewer::brewer.pal(5, "RdYlGn"))

ggplot(micx_pred_df, aes(color = range)) +
  geom_sf(size = 0.3) +
  scale_color_stepsn(colors = micx_colors,
                     breaks = breaks,
                     labels = labels,
                     name = "Range") +
  geom_sf(data = states, fill = NA, color = "black", lwd = 0.1) +
  theme_void() +
  theme(legend.position = c(0.80, 0.90),
        legend.text=element_text(size=14),
        legend.title=element_text(size=16)) +
  guides(color = guide_legend(ncol=2, override.aes = list(size=4, shape = 15))) +
  labs(title = 'Prediction Interval Range - Micx')

ggsave("micx_fit.jpeg", width = 12, height = 8, device = 'jpeg', dpi = 1200)

# cyano fit

pred_df <- pred_df |>
  mutate(range = (cyano_transform[, "upr"] - cyano_transform[, "lwr"]))

pred_df <- pred_df %>%
  mutate(disc_range = factor(case_when(range < 1622000 ~ 'B1',
                                       range >= 1622000 & range < 2586000 ~ 'B2',
                                       range >= 2586000 & range < 4239000 ~ 'B3',
                                       range >= 4239000 & range < 50000000 ~ 'B4',
                                       range > 50000000 ~ 'B5',
                                       TRUE ~ NA),
                             levels = c('B1', 'B2', 'B3', 'B4', 'B5'))) %>%
  arrange(disc_range)

labels = c('<Q1','Q1-Q2','Q2-Q3', 'Q3-50mil', '>50mil')
cyano_colors <- rev(RColorBrewer::brewer.pal(5, "RdYlGn"))

ggplot(pred_df, aes(color = disc_range)) +
  geom_sf(size = 0.3) +
  scale_color_manual(values = cyano_colors,
                     labels = labels,
                     name = "Range") +
  geom_sf(data = states, fill = NA, color = "black", lwd = 0.1) +
  theme_void() +
  theme(legend.position = c(0.80, 0.90),
        legend.text=element_text(size=14),
        legend.title=element_text(size=16)) +
  guides(color = guide_legend(ncol=2, override.aes = list(size=4, shape = 15))) +
  labs(title = 'Prediction Interval Range - Cyano')

ggsave("cyano_fit.jpeg", width = 12, height = 8, device = 'jpeg', dpi = 1200)

# ANOVA? -----------------------------------------------------------------------

cyano_a <- pred_df |>
  st_drop_geometry()

micx_a <- micx_pred_df |>
  st_drop_geometry()

micx_a |>
  group_by(all_pred) |>
  get_summary_stats(BFIWs, type = 'mean_sd')

ggboxplot(cyano_a, x = "all_pred", y = "BFIWs")

# ANOVA ------------------------------------------------------------------------

model <- lm(BFIWs ~ all_pred, data = cyano_a)
ggqqplot(residuals(model))

res.aov <- micx_a %>% anova_test(Precip8110Ws ~ all_pred)
res.aov

cyano_a |>
  tukey_hsd(log_ad ~ all_pred)

micx_a <- micx_a |>
  mutate(log_ad = log10(ad_ratio),
         log_precip = log10(Precip8110Ws))

oneway.test(Precip8110Ws ~ all_pred, micx_a, var.equal = FALSE)

# building model for spatial analysis

# cyanos
bf_model <- splm(BFIWs ~ all_pred, comp_cyano, spcov_type = "exponential")
anova(bf_model)

ad_model <- splm(log10(ad_ratio) ~ all_pred, comp_cyano, spcov_type = "exponential")
anova(ad_model)

precip <- splm(log10(Precip8110Ws) ~ all_pred, comp_cyano, spcov_type = "exponential")
anova(precip)

# micx
bfm_model <- splm(BFIWs ~ all_pred, comp_micx, spcov_type = "exponential")
anova(bfm_model)

comp_cyano <- comp_cyano |> filter(ad_ratio != Inf) |> drop_na(ad_ratio)
adm_model <- splm(log10(ad_ratio) ~ all_pred, comp_micx, spcov_type = "exponential")
anova(adm_model)

precipm <- splm(log10(Precip8110Ws) ~ all_pred, comp_micx, spcov_type = "exponential")
anova(precipm)

# state totals -----------------------------------------------------------------

state_cols <- PredData |>
  st_drop_geometry() |>
  left_join((PredDataMini |> dplyr::select(c(COMID, state))), by = 'COMID')

pred_pct <- state_cols |>
  group_by(state) |>
  count(micx_ecocond) |>
  mutate(percentage = (n / sum(n)) * 100)


# state_cols <- state_cols |>
#   mutate(class_micx = factor(case_when(
#     pred_micx_fit >= 0.50 ~ 'HM',
#     pred_micx_fit < 0.50 ~ 'LM',
#     TRUE ~ 'OTHER'),
#     levels = c('HM','LM'))) %>%
#   arrange(class_micx) |>
#   mutate(class_cyano = factor(case_when(
#     pred_cyano_fit >= 5 ~ 'HC',
#     pred_cyano_fit < 5 ~ 'LC',
#     TRUE ~ 'OTHER'),
#     levels = c('HC','LC'))) %>%
#   arrange(class_cyano) |>
#   drop_na()

risk_summary <- state_cols |>
  group_by(state) |>
  summarise(hm_percent = round((sum(class_micx == "HM")/n()) * 100, digits = 2),
            lm_percent = round((sum(class_micx == "LM")/n()) * 100, digits = 2),
            hc_percent = round((sum(class_cyano == "HC")/n())* 100, digits = 2),
            lc_percent = round((sum(class_cyano == 'LC')/n())* 100, digits = 2),
            hm_count = sum(class_micx == "HM"),
            lm_count = sum(class_micx == "LM"),
            hc_count = sum(class_cyano == "HC"),
            lc_count = sum(class_cyano == 'LC'))

ggplot(risk_summary, aes(x = fct_rev(fct_reorder(state, hc_percent)), y = hc_percent)) +
  geom_col() +
  labs(x = 'State', y = "% of Lakes at High Risk")

geom_state <- merge(states, risk_summary, by = 'STUSPS')

ggplot() +
  geom_sf(geom_state, mapping = aes(fill = hc_percent)) +
  scale_fill_stepsn(colors = RColorBrewer::brewer.pal(4, "YlOrRd"),
                    breaks = breaks) +
  theme_void() +
  theme(axis.title.y=element_blank(),
        legend.position = "none")


breaks <- c(25,50,75,100)

ggplot() +
  geom_sf(geom_state, mapping = aes(fill = hm_percent)) +
  scale_fill_stepsn(colors = RColorBrewer::brewer.pal(4, "YlOrRd"),
                    breaks = breaks) +
  theme_void() +
  theme(axis.title.y=element_blank(),
        legend.position = "none")

ggsave("micx_natty_risk.jpeg", width = 8, height = 4, device = 'jpeg', dpi = 1200)

# Oregon Drinking Water --------------------------------------------------------

name_cols <- wbd |>
  dplyr::select(c(COMID, GNIS_NAME, FTYPE)) |>
  st_drop_geometry()

comp_micx <- merge(comp_micx, name_cols, by = 'COMID')

pred_cols <- PredDataMini |>
  dplyr::select(c(COMID, state))

comp_micx <- comp_micx |>
  left_join(comp_micx, pred_cols, by = 'COMID') |>
  drop_na(pred_micx)

dw_data <- read.csv("C:/Users/mreyno04/OneDrive - Environmental Protection Agency (EPA)/Profile/Downloads/Active public water systems.csv")

dw_data <- dw_data |>
  st_as_sf(coords = c("x","y"), crs = 5072)

toxin_2018 <- read.csv("C:/Users/mreyno04/OneDrive - Environmental Protection Agency (EPA)/Profile/Downloads/Cyanotoxin Sample Results_2018.csv")

toxin_2018 <- toxin_2018 |>
  rename(total_micx = Total.Microcystins.ug.L.) |>
  dplyr::select(-Cylindrospermopsin.ug.L.) |>
  dplyr::filter(total_micx != "")

toxin_2018$total_micx <- as.numeric(as.character(toxin_2018$total_micx))

toxin_2018 <- toxin_2018 %>%
  mutate(total_micx = coalesce(total_micx, 0))

toxin_2018 <- toxin_2018 %>%
  mutate(detect = factor(case_when(total_micx <= 0 ~ "LD", # low detect
                                   total_micx > 0 ~ "> HD", # high detect
                                       TRUE ~ NA)))

toxin_2018 <- toxin_2018 |>
  group_by(across(PWS.ID)) |>
  summarise(across(where(is.numeric), mean))

data <- left_join(toxin_2018, dw_data, by = "PWS.ID")
data <- data |>
  st_as_sf() |>
  st_transform(crs = 2992)

data_buffers <- data |>
  mutate(buffer_poly = st_buffer(geometry, 800)) |>
  st_transform(crs = 2992)

oregon <- comp_micx |>
  filter(state == 'OR') |>
  st_transform(crs=2992)

wash <- comp_micx |>
  filter(state == 'WA') |>
  st_transform(crs = 5072)

ggplot() +
  geom_sf(data = wash)

wa_toxin <- read.csv("C:/Users/mreyno04/OneDrive - Environmental Protection Agency (EPA)/Profile/Downloads/WA_toxin_2018.csv", check.names = FALSE)

wa_toxin$toxin_con <- as.numeric(as.character(wa_toxin$toxin_con))

wa_toxin <- wa_toxin %>%
  mutate(toxin_con = coalesce(toxin_con, 0))

wa_toxin <- wa_toxin |>
  group_by(across(Site)) |>
  summarise(across(where(is.numeric), mean))

wash <- wash |>
  rename(Site = GNIS_NAME)

wa_toxin_fit <- wash |>
  subset(Site %in% wa_toxin$Site)

wa_toxin_fit <- left_join(wa_toxin_fit, wa_toxin, by = 'Site')
wa_toxin_fit <- drop_na(wa_toxin_fit)

p <- ggplot(wa_toxin_fit, aes(pred_micx[, "upr"], toxin_con)) +
  geom_point() +
  #geom_smooth(method = "lm") +
  ylim(0,5) +
  xlab("Upper Micx Prediction") +
  ylab("Measured Toxin Concentration (ug/L)")

p + geom_point()

q <- ggplot() +
  geom_sf(data = oregon, fill = NA, color = "darkgrey", lwd = 0.7, alpha = 0.7) +
  geom_sf(data = data_buffers,
          aes(color = total_micx),
          size = 1) +
  scale_color_manual(values = cyano_colors,
                     labels = cyano_labels,
                     name = "Abundance (1000 cells/mL)")

ggsave("upper_micx_wa.jpeg", width = 12, height = 8, device = 'jpeg', dpi = 1500)

# PredData csv -----------------------------------------------------------------

PredData <- PredData |>
  mutate(coords = st_coordinates(Shape),
         lat.x = coords[, 'X'],
         lon.y = coords[, 'Y']) |>
  dplyr::select(-c(coords)) |>
  relocate(pred_micx, micx_transform, pred_cyano, cyano_transform, .after = UNIQUE_ID) |>
  st_drop_geometry()

write.csv(PredData, "C:/Users/mreyno04/OneDrive - Environmental Protection Agency (EPA)/Profile/REPOS/HABsDrivers/inst/PredData.csv")

# checking that everything worked

PredData2 <- read.csv("C:/Users/mreyno04/OneDrive - Environmental Protection Agency (EPA)/Profile/REPOS/HABsDrivers/inst/PredData.csv")

PredData2 <- PredData2 |>
  st_as_sf(coords = c("lat.x","lon.y"), crs = 5072)

ggplot() +
  geom_sf(data = PredData2)


# Crop Land Cover Classification ===============================================

pred_cols <- PredDataMini |>
  dplyr::select(c(COMID, state))

micx_pred_df <- micx_pred_df |>
  left_join(micx_pred_df, pred_cols, by = 'COMID') |>
  drop_na(pred_micx)

pred_buffers <- PredData |>
  mutate(buffer_poly = st_buffer(Shape, 1600))

crop_cov <- raster("C:/Users/mreyno04/OneDrive - Environmental Protection Agency (EPA)/Profile/Downloads/2017_30m_cdls/2017_30m_cdls.tif")

extracted_values <- terra::extract(crop_cov, pred_buffers)

# flextable? -------------------------------------------------------------------


PredData <- PredData |>
  mutate(micx_class = factor(case_when(
    (n_dev_inputs >= 10 | p_farm_inputs >= 4) & pred_micx[, "fit"] >= 0.50 ~ 'HNHM',
    (n_dev_inputs < 10 | p_farm_inputs < 4) & pred_micx[, "fit"] >= 0.50 ~ 'LNHM',
    (n_dev_inputs >= 10 | p_farm_inputs >= 4) & pred_micx[, "fit"] < 0.50 ~ 'HNLM',
    (n_dev_inputs < 10 | p_farm_inputs < 4) & pred_micx[, "fit"] < 0.50 ~ 'LNLM',
    TRUE ~ 'OTHER'),
    levels = c('HNHM','HNLM','LNHM','LNLM'))) %>%
  arrange(micx_class) |>
  mutate(cyano_class = factor(case_when(
    (n_farm_inputs >= 10 | p_dev_inputs >= 4) & pred_cyano[, "fit"] >= 5 ~ 'HNHC',
    (n_farm_inputs < 10 | p_dev_inputs < 4) & pred_cyano[, "fit"] >= 5 ~ 'LNHC',
    (n_farm_inputs >= 10 | p_dev_inputs >= 4) & pred_cyano[, "fit"] < 5 ~ 'HNLC',
    (n_farm_inputs < 10 | p_dev_inputs < 4) & pred_cyano[, "fit"] < 5 ~ 'LNLC',
    TRUE ~ 'OTHER'),
    levels = c('HNHC','HNLC','LNHC','LNLC'))) %>%
  arrange(cyano_class)


cy_sample <- sample_n(comp_cyano, 15)
cy_sample <- sample(cy_sample, 5)

cy_sample |>
  group_by(all_pred) |>
  summarise(across(where(is.numeric), mean)) |>
  flextable()

# i for rows selection and j for columns selection can be expressed in different ways

ft <- PredData |>
  st_drop_geometry() |>
  group_by(micx_class) |>
  dplyr::select(-c(micx_transform, pred_cyano, cyano_transform, COMID, cyano_class)) |>
  summarise((across(where(is.numeric), mean))) |>
  flextable() |>
  bold(~BFIWs == max(BFIWs), 7)
ft

cyft <- comp_cyano |>
  st_drop_geometry() |>
  group_by(all_pred) |>
  dplyr::select(c(BFIWs, all_pred, MAXDEPTH, area_km)) |>
  summarize(median_bfi = median(BFIWs),
            mean_bfi   = mean(BFIWs)) |>
  flextable()

comp_cyano <- comp_cyano |>
  filter(MAXDEPTH > 0)

# isotopes! --------------------------------------------------------------------

iso_2012 <- read.csv("C:/Users/mreyno04/OneDrive - Environmental Protection Agency (EPA)/Profile/Downloads/NLA_2012.csv")
iso_2017 <- read.csv("C:/Users/mreyno04/OneDrive - Environmental Protection Agency (EPA)/Profile/Downloads/NLA_2017.csv")

iso_overlap <- iso_2012 |>
  filter(!(UNIQUE_ID %in% iso_2017$UNIQUE_ID))

iso_all <- bind_rows(iso_overlap, iso_2017)

PredData <- PredData |>
  merge(iso_2012, by = 'UNIQUE_ID')

ggplot(comp_micx, aes(x=RT_iso, y= all_pred)) +
  ggridges::geom_density_ridges(aes(fill = all_pred),
                                scale = 2,
                                alpha = 0.85,
                                quantile_lines = TRUE, quantiles = 2) +
  scale_fill_manual(values = c("#9c0082","#cc6de4","#4e8562", "#8bd1a5")) +
  xlim(0,5)

ggsave("micx_restime.jpeg", width = 12, height = 8, device = 'jpeg', dpi = 500)

iso_model <- spmodel::splm(E_I ~ all_pred, comp_micx, spcov_type = "exponential")

restime_model <- spmodel::splm(RT_iso ~ all_pred, comp_micx, spcov_type = 'exponential')

anova(restime_model)
ggpubr::ggqqplot(residuals(restime_model, type = "standardized"))

# flextable --------------------------------------------------------------------

comp_micx <- comp_micx |>
  dplyr::select(COMID, pred_micx, all_pred) |>
  st_drop_geometry() |>
  rename(prediction = pred_micx)

comp_cyano <- comp_cyano |>
  dplyr::select(COMID, pred_cyano, all_pred) |>
  st_drop_geometry() |>
  rename(prediction = pred_cyano)

pred_all <- rbind(comp_cyano, comp_micx)

pred_all |>
  group_by(all_pred) |>
  mutate
  count() |>
  summarise(across(where(is.numeric), mean)) |>
  flextable()

options(digits = 2)
comp_micx|>
  mutate(cat_percent = ) |>
  group_by(all_pred) |>
  flextable() |>
  set_header_labels(values = list(
    resp_var = "Response Variable",
    lake_data = "In-Lake Conditions",
    spatial_mod = "Error Structure",
    bias = "Bias",
    RMSPE = "RMSPE",
    fit = "Fit")) |>
  merge_v(part = "body", j = c(1:2)) |>
  footnote(i = 1, j = 6,
           value = as_paragraph("Fit is R^2 for cyanobacteria models and AUC for microcystin models."), ref_symbols = "a", part = "header") |>
  theme_booktabs(bold_header = TRUE) |>
  fontsize(size = 10, part = "body") |>
  fontsize(size = 10, part = "header") |>
  fontsize(size = 9, part = "footer") |>
  theme_box() |>
  align(align = "center", part = "header")

# DRAIN RATIO & DOC ------------------------------------------------------------

# drain ratio ------------------------------------------------------------------

get_sal <- function(coms){
  StreamCatTools::lc_get_data(metric = 'pctsallake',
                              aoi='ws',
                              comid = coms,
                              showAreaSqKm = TRUE)
}

# divide existing predictor data set into groups of 500 by COMID
# increasing efficiency in pulling NLCD data
chunks <- split(doc_test$COMID, ceiling(seq_along(doc_test$COMID) / 100))


# using lapply function to pull NLCD data from each chunk by COMID
salinity <- do.call(rbind, lapply(chunks, get_sal))

ws_area <- ws_area |>
  rename(COMID = comid)

PredData <- ws_area |>
  dplyr::select(COMID, wsareasqkm) |>
  left_join(PredData, by = 'COMID') |>
  st_as_sf()

PredData <-PredData |>
  rowwise() |>
  mutate(wsareaha = wsareasqkm * 100,
         lake_areaha = lake_area / 10000,
         drain_ratio = wsareaha / lake_areaha)

test <- PredData |>
  st_drop_geometry() |>
  ungroup()

test <- dplyr::sample_n(test, size = 1500, replace = FALSE)

shapiro.test(test$drain_ratio)

dr_model_cy <- splm(log10(drain_ratio) ~ cyano_cat, PredData, spcov_type = "exponential")
anova(dr_model_cy)

dr_model_mx <- splm(drain_ratio ~ micx_cat, PredData, spcov_type = "exponential")
anova(dr_model_mx)

bf_model_cy <- splm(BFIWs ~ cyano_cat, PredData, spcov_type = "exponential")
anova(bf_model_cy)

bf_model_mx <- splm(BFIWs ~ micx_cat, PredData, spcov_type = "exponential")
anova(bf_model_mx)

ad_model_cy <- splm(ad_ratio ~ cyano_cat, PredData, spcov_type = "exponential")
anova(ad_model_cy)

ad_model_mx <- splm(ad_ratio ~ micx_cat, PredData, spcov_type = "exponential")
anova(ad_model_mx)


saveRDS(bf_model_cy,
        file = 'C:/Users/mreyno04/OneDrive - Environmental Protection Agency (EPA)/Profile/REPOS/HABsDrivers/inst/model_objects/bf_model_cy26.rds')

kruskal.test(drain_ratio ~ cyano_cat, data = PredData)

ggplot(PredData, aes(x=drain_ratio, y= cyano_cat)) +
  ggridges::geom_density_ridges(aes(fill = cyano_cat), quantile_lines = TRUE, quantiles = 2) +
  scale_fill_manual(values = c("#9c0082","#cc6de4","#4e8562", "#8bd1a5")) +
  xlim(0, 300) +
  labs(x = "base flow", fill = 'Class',
       title = 'micx') +
  theme(axis.title.y=element_blank(),
        legend.position = "none")

ggplot(PredData, aes(drain_ratio, pred_cyano_fit)) +
  geom_point(aes(colour = cyano_cat)) +
  stat_poly_line() +
  stat_poly_eq(use_label(c("eq", "P", "adj.R2"))) +
  scale_y_continuous(trans = "log") +
  scale_x_continuous(trans = "log")

PredData <- PredData |>
  relocate(drain_ratio, .after = ad_ratio)

# drain ratio with habs data ---------------------------------------------------

wbd_copy <- wbd |>
  subset(COMID %in% PredData$COMID) |>
  dplyr::select(COMID, Shape)

# create lake area metric
for (Shape in 1:length(wbd_copy)) {
  shapes <- wbd_copy$Shape
  wbd_copy$lake_area <- st_area(shapes)
}

# drop units and convert polygons to points
wbd_copy <- wbd_copy |>
  mutate(lake_area = drop_units(lake_area)) |>
  st_point_on_surface()

# merge water boundary data with existing predictor data
PredData <- merge(wbd_copy, PredData, by = 'COMID')

get_nlcd <- function(coms){
  StreamCatTools::lc_get_data(metric = 'pctconif2016',
                              aoi='ws',
                              comid = coms,
                              showAreaSqKm = TRUE)
}

# divide existing predictor data set into groups of 500 by COMID
# increasing efficiency in pulling NLCD data
chunks <- split(PredData$COMID, ceiling(seq_along(PredData$COMID) / 500))


# using lapply function to pull NLCD data from each chunk by COMID
ws_area <- do.call(rbind, lapply(chunks, get_nlcd))

ws_area <- ws_area |>
  rename(COMID = comid)

PredData <- ws_area |>
  dplyr::select(COMID, wsareasqkm) |>
  left_join(PredData, by = 'COMID') |>
  st_as_sf()

wbd_copy <- wbd_copy |>
  st_drop_geometry()

habs <- habs |>
  left_join(wbd_copy, by = 'COMID')

habs <- habs |>
  rowwise() |>
  mutate(wsareaha = wsareasqkm * 100,
         lake_areaha = lake_area / 10000,
         drain_ratio = wsareaha / lake_areaha)

habs <- habs %>%
  mutate(disc_fst = factor(case_when(fst_ws < 25 ~ 'B1',
                                     fst_ws >= 25 & fst_ws < 50 ~ 'B2',
                                     fst_ws >= 50 & fst_ws < 75 ~ 'B3',
                                     fst_ws >= 75  ~ 'B4'),
                           levels = c('B1', 'B2', 'B3', 'B4'))) %>%
  arrange(disc_fst)


fst_labels = c("0-25%", "25-50%", "50-75%", ">75%")
fst_cols <- RColorBrewer::brewer.pal(4, "YlGn")

ggplot(habs, aes(drain_ratio, DOC)) +
  geom_point(aes(color = disc_fst)) +
  scale_color_manual(values = fst_cols,
                     labels = fst_labels,
                     name = "Cover (%)") +
  # ylim(0,20) +
  # xlim(0,500) +
  stat_poly_line() +
  stat_poly_eq(use_label(c("eq", "P", "adj.R2"))) +
  scale_y_continuous(trans = "log10") +
  scale_x_continuous(trans = "log10")


ggplot(habs, aes(drain_ratio, DOC)) +
  geom_point(aes(color = disc_fst)) +
  scale_color_manual(values = fst_cols,
                     labels = fst_labels,
                     name = "Cover (%)") +
  ylim(0,15) +
  xlim(0,150)

habs <- habs %>%
  mutate(disc_cyano = factor(case_when(B_G_DENS < 2337 ~ 'B1',
                                       B_G_DENS >= 2337 & B_G_DENS < 19206 ~ 'B2',
                                       B_G_DENS >= 19206 & B_G_DENS < 90659 ~ 'B3',
                                       B_G_DENS >= 90659  ~ 'B4'),
                             levels = c('B1', 'B2', 'B3', 'B4'))) %>%
  arrange(disc_cyano)


labels = c("< Q1", "Q1-Q2", "Q2-Q3", ">Q3")
cols <- rev(RColorBrewer::brewer.pal(4, "Spectral"))


ggplot(habs, aes(drain_ratio, DOC)) +
  geom_point(aes(color = disc_cyano)) +
  facet_wrap(~disc_cyano) +
  scale_color_manual(values = cols,
                     labels = labels,
                     name = "CYANO") +
  # ylim(2,12) +
  # xlim(0,150) +
  stat_poly_line() +
  stat_poly_eq(use_label(c("eq", "P", "adj.R2"))) +
  scale_y_continuous(trans = "log10") +
  scale_x_continuous(trans = "log10")


get_nni <- function(coms){
  lc_get_data(metric = 'n_tin2016,p_tin2016',
              aoi='ws',
              comid = 487,
              showAreaSqKm = TRUE)
}

chunks <- split(PredData$COMID, ceiling(seq_along(PredData$COMID) / 500))
test <- paste(unlist(chunks[1]),collapse=",")
nni_inputs <- do.call(rbind, lapply(chunks, get_nni))

df <- lc_get_data(metric = 'pctwdwet2016, pcturbmd2016, pcturblo2016,pctmxfst2016, pctcrop2016, pcthay2016,pctdecid2016,pctconif2016, pcturbop2016, pcthbwet2016',
                  aoi='ws',
                  comid = test,
                  showAreaSqKm = TRUE)


write.csv(habs, file = 'inst/all_habs_vars.csv')

habs <- habs %>%
  mutate(disc_doc = factor(case_when(DOC < 3.397 ~ 'B1',
                                       DOC >= 3.397 & DOC < 5.580 ~ 'B2',
                                       DOC >= 5.580 & DOC < 8.725 ~ 'B3',
                                       DOC >= 8.725 ~ 'B4'),
                             levels = c('B1', 'B2', 'B3', 'B4'))) %>%
  arrange(disc_doc)


labels = c("< Q1", "Q1-Q2", "Q2-Q3", ">Q3")
cols <- rev(RColorBrewer::brewer.pal(4, "Spectral"))


ggplot(habs, aes(color = disc_doc)) +
  geom_sf(size = 3) +
  scale_color_manual(values = cols,
                     labels = labels,
                     name = "Cover (%)") +
  geom_sf(data = states, fill = NA, color = "black", lwd = 0.1) +
  theme(plot.title = element_text(size = 12)) +
  guides(colour = guide_legend(override.aes = list(size=4)))

habs <- habs |>
  st_transform(crs = 4326)

habs <- habs |>
  mutate(coordinates = st_coordinates(geometry))

habs <- habs |>
  mutate(lat = coordinates[, "X"],
         lon = coordinates[, "Y"])

lake_coord <- habs |>
  dplyr::select(lat, lon)

no_sf <- lake_coord |>
  st_drop_geometry()

st_write(polygon, "all_coords.shp", driver = "ESRI Shapefile")

convex_hull <- st_convex_hull(st_union(habs$geometry))


nd <- state_cols |>
  filter(state == 'ND')


hnlc_nd <- nd |>
  filter(cyano_pred == 'HNLC')


# hnlc_nd <- hnlc_nd |>
#   mutate(coordinates = st_coordinates(Shape))
#
# hnlc_nd <- hnlc_nd |>
#   mutate(lat = coordinates[, "X"],
#          lon = coordinates[, "Y"])
#
#

hnlc_nd <- hnlc_nd |>
  st_transform(crs = 4326)


st_write(hnlc_nd, "hnlc_nd.shp", driver = "ESRI Shapefile")

all_df <- habs |>
  st_drop_geometry() |>
  dplyr::select(COMID, DOC) |>
  left_join(PredData, by = 'COMID')

doc_test <- doc_test %>%
  mutate(Q3_doc = factor(case_when(DOC < 11.312 ~ 'B1',
                                     DOC >= 11.312 & DOC ~ 'B2'),
                           levels = c('B1', 'B2'))) %>%
  arrange(Q3_doc)


ggplot(doc_test, aes(color = Q3_doc)) +
  geom_sf(size = 2) +
  geom_sf(data = states, fill = NA, color = "black", lwd = 0.1) +
  theme(plot.title = element_text(size = 12)) +
  guides(colour = guide_legend(override.aes = list(size=4)))

ggplot(doc_test, aes(drain_ratio, DOC)) +
  geom_point() +
  #facet_wrap(~cyano_pred) +
  # ylim(2,12) +
  # xlim(0,150) +
  stat_poly_line() +
  stat_poly_eq(use_label(c("eq", "P", "adj.R2"))) +
  scale_y_continuous(trans = "log10") +
  scale_x_continuous(trans = "log10")

ggplot(all_df, aes(x=DOC, y=cyano_pred)) +
  ggridges::geom_density_ridges(aes(fill = cyano_pred),
                                scale = 2,
                                alpha = 0.85,
                                quantile_lines = TRUE, quantiles = 4) +
  scale_fill_manual(values = c("#9c0082","#cc6de4","#4e8562", "#8bd1a5")) +
  xlim(0,25) +
  labs(x = "ad:ratio", fill = 'Class',
       title = 'Microcystin') +
  theme(axis.title.y=element_blank(),
        legend.position = "none")

# comparing with nrsa doc values

nrsa89_geom <- read.csv('https://www.epa.gov/system/files/other-files/2023-01/NRSA_1819_SiteInfo.csv')
nrsa89 <- read.csv("https://www.epa.gov/sites/default/files/2015-09/chem.csv")

nrsa89_geom <- nrsa89_geom |>
  filter(VISIT_NO == '1') |>
  dplyr::select(UNIQUE_ID, SITE_ID, LAT_DD83, LON_DD83)

nrsa89 <- nrsa89 |>
  filter(VISIT_NO == '1') |>
  dplyr::select(SITE_ID, DOC)

nrsa89 <- nrsa89_geom |>
  left_join(nrsa89, by = 'SITE_ID')

nrsa1314 <- read.csv("https://www.epa.gov/sites/default/files/2019-04/nrsa1314_widechem_04232019.csv")
nrsa1314_geom <- read.csv('https://www.epa.gov/sites/default/files/2019-04/nrsa1314_siteinformation_wide_04292019.csv')

nrsa89_geom <- nrsa89_geom |>
  filter(VISIT_NO == '1') |>
  dplyr::select(UNIQUE_ID, SITE_ID, LAT_DD83, LON_DD83)

nrsa89 <- nrsa89 |>
  filter(VISIT_NO == '1') |>
  dplyr::select(UNIQUE_ID, SITE_ID, DOC)


nrsa1819 <- read.csv("https://www.epa.gov/sites/default/files/2021-04/nrsa_1819_water_chemistry_chla_-_data.csv")
nrsa1819_geom <- read.csv('https://www.epa.gov/system/files/other-files/2023-01/NRSA_1819_SiteInfo.csv')

nrsa_chem <- bind_rows(nrsa89, nrsa1314, nrsa1819)

# lake area % for cyAN comparison ----------------------------------------------

PredData <- PredData |>
  mutate(above_cyan = if_else(pred_cyano_fit >= 5.00432, 1, 0),
         above_micx = if_else(pred_micx_fit >= 0.5, 1, 0))

state_cols <- PredDataMini |>
  dplyr::select(c(COMID, state))

PredData <- PredData |>
  merge(state_cols, by ='COMID')

PredData <- PredData |>
  mutate(alt_regions = case_when(state == 'WA' | state == 'ID' | state == 'OR' ~ 'PNW',
                                 state == 'CA' | state == 'NV' ~ 'W',
                                 state == 'MT' | state == 'WY' | state == 'ND' | state == 'SD' | state == 'NE' ~ 'NRP',
                                 state == 'NM' | state == 'UT' | state == 'CO' | state == 'AZ' ~ 'SW',
                                 state == 'TX' | state == 'LA' | state == 'OK' | state == 'KS' | state == 'MS' | state == 'AR' ~ 'S',
                                 state == 'IA' | state == 'MN' | state == 'WI' | state =='MI' ~ 'UMW',
                                 state == 'AL' | state == 'FL' | state == 'GA' | state == 'SC' | state == 'NC' | state == 'VA' ~ 'SE',
                                 state == 'MO' | state == 'IN' | state == 'IL' | state == 'OH' | state == 'KY' | state == 'TN' | state == 'WV' ~ 'OV',
                                 state == 'MD' | state == 'DE' | state == 'PA' | state == 'NJ' | state == 'NY' | state == 'MA' | state == 'RI' | state == 'CT' | state == 'VT' | state == 'NH' | state == 'ME' ~ 'NE'
                                 ))

PredData$above_cyan <- as.character(PredData$above_cyan)

test <- PredData |>
  dplyr::group_by(alt_regions, above_cyan) |>
  summarize(sum_area = sum(lake_area)) |>
  st_drop_geometry()

final <- test |>
  group_by(alt_regions) |>
  summarize(total = sum(sum_area)) |>
  left_join(test, by = 'alt_regions')

final <- final |>
  rowwise() |>
  mutate(prop_cyano = sum_area / total) |>
  filter(above_cyan == 1)

micx <- PredData |>
  dplyr::group_by(alt_regions, above_micx) |>
  summarize(sum_area = sum(lake_area)) |>
  st_drop_geometry()

final_m <- micx |>
  group_by(alt_regions) |>
  summarize(total = sum(sum_area)) |>
  left_join(micx, by = 'alt_regions')

final_m <- final_m |>
  rowwise() |>
  mutate(prop_micx = sum_area / total) |>
  filter(above_micx == 1)

# mapping this

states <- states |>
  mutate(alt_regions = case_when(STUSPS == 'WA' | STUSPS == 'ID' | STUSPS == 'OR' ~ 'PNW',
                                 STUSPS == 'CA' | STUSPS == 'NV' ~ 'W',
                                 STUSPS == 'MT' | STUSPS == 'WY' | STUSPS == 'ND' | STUSPS == 'SD' | STUSPS == 'NE' ~ 'NRP',
                                 STUSPS == 'NM' | STUSPS == 'UT' | STUSPS == 'CO' | STUSPS == 'AZ' ~ 'SW',
                                 STUSPS == 'TX' | STUSPS == 'LA' | STUSPS == 'OK' | STUSPS == 'KS' | STUSPS == 'MS' | STUSPS == 'AR' ~ 'S',
                                 STUSPS == 'IA' | STUSPS == 'MN' | STUSPS == 'WI' | STUSPS =='MI' ~ 'UMW',
                                 STUSPS == 'AL' | STUSPS == 'FL' | STUSPS == 'GA' | STUSPS == 'SC' | STUSPS == 'NC' | STUSPS == 'VA' ~ 'SE',
                                 STUSPS == 'MO' | STUSPS == 'IN' | STUSPS == 'IL' | STUSPS == 'OH' | STUSPS == 'KY' | STUSPS == 'TN' | STUSPS == 'WV' ~ 'OV',
                                 STUSPS == 'MD' | STUSPS == 'DE' | STUSPS == 'PA' | STUSPS == 'NJ' | STUSPS == 'NY' | STUSPS == 'MA' | STUSPS == 'RI' | STUSPS == 'CT' | STUSPS == 'VT' | STUSPS == 'NH' | STUSPS == 'ME' ~ 'NE'
  ))


ggplot(states) +
  geom_sf(aes(fill = alt_regions)) +
  theme_void()

ggsave("alt_regions.jpeg", width = 12, height = 8, device = 'jpeg', dpi = 1500)

final_m <- final_m |>
  rowwise() |>
  mutate(sq_mi = sum_area * 0.0000003861)


###

loc <- "O:/LAB/COR/Geospatial_Library_Resource/Physical/HYDROLOGY/NHDPlusV21/NHDPlusNationalData/NHDPlusV21_National_Seamless_Flattened_Lower48.gdb"

wbd <- sf::st_read(dsn = loc, layer = 'NHDWaterbody') |>
  st_transform(5072)

wbd_copy <- wbd |>
  subset(COMID %in% PredData$COMID) |>
  dplyr::select(COMID, GNIS_NAME) |>
  st_drop_geometry()

wbd_copy |>
  filter(str_detect(GNIS_NAME, "Fern Ridge"))

PredData <- left_join(PredData, wbd_copy, by = 'COMID')

fern_ridge <- PredData |>
  filter(str_detect(COMID, "23769069"))

ggplot(fern_ridge, aes(color = pred_micx_fit)) +
  geom_sf(size = 2) +
  geom_sf(data = oregon_counties, fill = NA, color = "black", lwd = 0.1)

fr_nla <- fern_ridge_nla |>
  rowwise() |>
  distinct()

write.csv(fern_ridge_nla, file = "fern_ridge_nla_report.csv", row.names = TRUE)

fern_ridge_nla <- habs |>
  filter(str_detect(UNIQUE_ID, "NLA_OR-10039"))

states <- states(cb = TRUE, progress_bar = FALSE)  %>%
  filter(!STUSPS %in% c('HI', 'PR', 'AK', 'MP', 'GU', 'AS', 'VI'))  %>%
  st_transform(crs = 5072)

# Cyano Predictions Map

PredData <- PredData %>%
  mutate(disc_cyano = factor(case_when(pred_cyano_fit < 4.41497 ~ 'B1', # under 25k
                                       pred_cyano_fit >= 4.41497 & pred_cyano_fit < 4.70757 ~ 'B2', # 25k - 50k
                                       pred_cyano_fit >= 4.70757 & pred_cyano_fit < 5.00432 ~ 'B3', # 50k - 100k
                                       pred_cyano_fit >= 5.00432 & pred_cyano_fit < 5.39967 ~ 'B4', # 100k - 250k
                                       pred_cyano_fit >= 5.39967 & pred_cyano_fit < 5.69984 ~ 'B5', # 250k-500k
                                       pred_cyano_fit >= 5.69984 ~ 'B6', # 500k
                                       TRUE ~ NA),
                             levels = c('B1', 'B2', 'B3', 'B4', 'B5', 'B6'))) %>%
  arrange(disc_cyano)

sub_25 <- PredData %>%
  filter(disc_cyano == 'B1')

cyano_labels <- c('< 25', '≥ 25 - 50', '≥ 50 - 100', '≥ 100 - 250', '≥ 250 - 500', '≥ 500')
cyano_colors <- c("#21618C","#5499C7","#A9CCE3","#EDBB99","#DC7633","#A04000")

cyano_pred_map <- ggplot() +
  geom_sf(data = PredData,
          aes(color = disc_cyano),
          size = 5.5,
          alpha = 0.8) +
  scale_color_manual(values = cyano_colors,
                     labels = cyano_labels,
                     name = "Abundance \n(1000 cells/mL)") +
  geom_sf(data = sub_25,
          aes(color = disc_cyano),
          size = 5.5,
          alpha = 0.8)  +
  geom_sf(data = states, fill = NA, color = "black", lwd = 1.5) +
  theme_void() +
  theme(legend.position = "none")

ggsave("habs_large.png", width = 50, height = 40, device = 'png', dpi = 500, limitsize = FALSE)


# ecoregion threshold test =====================================================

test <- PredData |>
  st_drop_geometry() |>
  group_by(AG_ECO3) |>
  mutate(micx_mean = mean(pred_micx_fit, na.rm = TRUE),
         cyano_mean = mean(pred_cyano_fit, na.rm = TRUE)) |>
  ungroup()

test <- test |>
  rowwise() |>
  mutate(micx_relative = factor(case_when(pred_micx_fit < micx_mean ~ 'B1',
                                       pred_micx_fit >= micx_mean ~ 'B2',
                                       TRUE ~ NA)),
         cyano_relative = factor(case_when(pred_cyano_fit < cyano_mean ~ 'B1',
                                      pred_cyano_fit >= cyano_mean ~ 'B2',
                                      TRUE ~ NA)))

test.geo <- PredData |>
  dplyr::select(COMID, Shape) |>
  left_join(test, by = 'COMID')

ggplot() +
  geom_sf(data = test.geo,
          aes(color = cyano_relative),
          size = 0.75,
          alpha = 0.8) +
  geom_sf(data = states, fill = NA, color = "black", lwd = 1.5) +
  theme_void()

# national thresholds  =========================================================

site2007 <- read_csv("inst/nla2007_sampledlakeinformation.csv")
site2012 <- read_csv("inst/nla2012_siteinfo.csv")
site2017 <- read_csv("inst/nla_2017_site_information-data.csv")

ref_2007 <- site2007 |>
  filter(RT_NLA == "REF")|>
  distinct() |>
  dplyr::select(SITE_ID, WSA_ECO3, COM_ID,
                RT_NLA, VISIT_NO,
                LON_DD, LAT_DD) |>
  mutate(year = 2007)

ref_2012 <- site2012 |>
  filter(RT_NLA12 == 'R') |>
  distinct() |>
  dplyr::select(SITE_ID, AGGR_ECO3_2015, COMID2012,
                RT_NLA12, VISIT_NO,
                LON_DD83, LAT_DD83) |>
  mutate(year = 2012) |>
  rename(WSA_ECO3 = AGGR_ECO3_2015,
         COM_ID = COMID2012,
         RT_NLA = RT_NLA12,
         LON_DD = LON_DD83,
         LAT_DD = LAT_DD83)

ref_2017 <- site2017 |>
  filter(RT_NLA17 == 'R') |>
  distinct() |>
  dplyr::select(SITE_ID, AG_ECO3, COMID,
                RT_NLA17, VISIT_NO,
                LON_DD83, LAT_DD83) |>
  mutate(year = 2017) |>
  rename(WSA_ECO3 = AG_ECO3,
         COM_ID = COMID,
         RT_NLA = RT_NLA17,
         LON_DD = LON_DD83,
         LAT_DD = LAT_DD83)


ref_all <- bind_rows(ref_2007, ref_2012, ref_2017)

ref_all <- ref_all |>
  filter(VISIT_NO == '1') |>
  distinct()

cyano_ref <- ref_all |>
  left_join(habs, by = 'SITE_ID')|>
  drop_na(B_G_DENS)

cyano_ref <- cyano_ref |>
  mutate(state = substr(UNIQUE_ID, 5, 6))

ore_ref <- cyano_ref |>
  filter(state == 'OR')

or_habs <- habs |>
  mutate(state = substr(UNIQUE_ID, 5, 6)) |>
  filter(state == 'OR')

# 1.5 IQR
Q1 <- quantile(cyano_ref$B_G_DENS, 0.25)
Q3 <- quantile(cyano_ref$B_G_DENS, 0.75)
IQR_val <- Q3 - Q1

lower_bound <- Q1 - 1.5 * IQR_val
upper_bound <- Q3 + 1.5 * IQR_val

outliers <- cyano_ref$B_G_DENS[cyano_ref$B_G_DENS < lower_bound | cyano_ref$B_G_DENS > upper_bound]
print(outliers)

habs_filter <- cyano_ref |>
  filter(!B_G_DENS %in% outliers)


# national thresholds?

# good | fair cutoff at 40,438
g.f <- quantile(habs_filter$B_G_DENS, 0.75)

# fair | poor cutoff at 96,051
f.p <- quantile(habs_filter$B_G_DENS, 0.99)

# eco region thresholds ========================================================

# EHIGH
ehigh_ref <- cyano_ref |>
  filter(WSA_ECO3 == 'EHIGH')

test_out <- ehigh_ref |> identify_outliers(B_G_DENS)

test_filter <- ehigh_ref |>
  filter(!B_G_DENS %in% test_out$B_G_DENS)

# good | fair cutoff at 35,670
g.f.ehigh <- quantile(test_filter$B_G_DENS, 0.75)

# fair | poor cutoff at 77,792
f.p.ehigh <- quantile(test_filter$B_G_DENS, 0.99)


# PLNLOW

pln_ref <- cyano_ref |>
  filter(WSA_ECO3 == 'PLNLOW')

test_out <- pln_ref |> identify_outliers(B_G_DENS)

test_filter <- pln_ref |>
  filter(!B_G_DENS %in% test_out$B_G_DENS)

# good | fair cutoff at 89,726
g.f.pln <- quantile(test_filter$B_G_DENS, 0.75)

# fair | poor cutoff at 169,365
f.p.pln <- quantile(test_filter$B_G_DENS, 0.99)


# WMTNS

west_ref <- cyano_ref |>
  filter(WSA_ECO3 == 'WMTNS')

test_out <- west_ref |> identify_outliers(B_G_DENS)

test_filter <- west_ref |>
  filter(!B_G_DENS %in% test_out$B_G_DENS)

# good | fair cutoff at 22,249
g.f.west <- quantile(test_filter$B_G_DENS, 0.75)

# fair | poor cutoff at 30,391
f.p.west <- quantile(test_filter$B_G_DENS, 0.99)


# apply gfp to pred habs =======================================================

PredData <- PredData |>
  mutate(cyano_ecocond = factor(case_when(AG_ECO3 == "EHIGH" & cyano_transform_fit <= g.f.ehigh ~ 'Good',
                                       AG_ECO3 == "EHIGH" & cyano_transform_fit >= g.f.ehigh & cyano_transform_fit < f.p.ehigh ~ 'Fair',
                                       AG_ECO3 == "EHIGH" & cyano_transform_fit >= f.p.ehigh ~ 'Poor',

                                       AG_ECO3 == "PLNLOW" & cyano_transform_fit <= g.f.pln ~ 'Good',
                                       AG_ECO3 == "PLNLOW" & cyano_transform_fit >= g.f.pln & cyano_transform_fit < f.p.pln ~ 'Fair',
                                       AG_ECO3 == "PLNLOW" & cyano_transform_fit >= f.p.pln ~ 'Poor',

                                       AG_ECO3 == "WMTNS" & cyano_transform_fit <= g.f.west ~ 'Good',
                                       AG_ECO3 == "WMTNS" & cyano_transform_fit >= g.f.west & cyano_transform_fit < f.p.west ~ 'Fair',
                                       AG_ECO3 == "WMTNS" & cyano_transform_fit >= f.p.west ~ 'Poor'),
  levels = c("Poor","Fair","Good" ))) |>
  arrange(cyano_ecocond)

colorscond <- c('Good' = '#85B2DC',
                'Fair'='#FFCF7A',
                'Poor'='#FF7978')

ggplot(PredData, aes(color = cyano_ecocond)) +
  geom_sf(size = 0.8) +
  scale_color_manual(values = colorscond) +
  geom_sf(data = states, fill = NA, color = "black", lwd = 0.1) +
 # geom_sf(data = eco_ag3_simpl, fill = NA, color = "black", lwd = 0.3) +
  theme_void()

ggplot(PredData, aes(color = micx_ecocond)) +
  geom_sf(size = 0.8) +
  scale_color_manual(values = colorscond) +
  geom_sf(data = states, fill = NA, color = "black", lwd = 0.1) +
  geom_sf(data = eco_ag3_simpl, fill = NA, color = "blue", lwd = 0.1) +
  theme_void()

cyano_ref |>
  ggplot(aes(B_G_DENS)) +
  facet_wrap(~AG_ECO3) +
  geom_histogram()


# micx threshold ===============================================================

micx_ref_grp <- cyano_ref |>
  st_drop_geometry() |>
  drop_na(MICX_DET) |>
  group_by(WSA_ECO3) |>
  count(MICX_DET) |>
  mutate(total = n()) |>
  mutate(prop = n / sum(n))

PredData <- PredData |>
  mutate(micx_refcond = factor(case_when(AG_ECO3 == "EHIGH" & pred_micx_fit <= 0.0877 ~ 'Below',
                                         AG_ECO3 == "EHIGH" & pred_micx_fit > 0.0877 ~ 'Above',

                                         AG_ECO3 == "PLNLOW" & pred_micx_fit <= 0.246  ~ 'Below',
                                         AG_ECO3 == "PLNLOW" & pred_micx_fit > 0.246  ~ 'Above',

                                         AG_ECO3 == "WMTNS" & pred_micx_fit <= 0.0223 ~ 'Below',
                                         AG_ECO3 == "WMTNS" & pred_micx_fit > 0.0223 ~ 'Above'),
                               levels = c("Above","Below" ))) |>
  arrange(micx_refcond)

total_micx_ref <- PredData |>
  st_drop_geometry() |>
  dplyr::select(AG_ECO3, micx_refcond) |>
  group_by(micx_refcond, AG_ECO3) |>
  mutate(total = n()) |>
  distinct()

eco2007 <- site2007 |>
  select(SITE_ID, WSA_ECO3, LAT_DD, LON_DD)

taxa07 <- phyto2007 |>
  left_join(eco2007, by = 'SITE_ID', relationship = 'one-to-many') |>
  distinct() |>
  select(WSA_ECO3, TAXANAME, ABUND) |>
  group_by(WSA_ECO3, TAXANAME) |>
  mutate(taxa_count = sum(ABUND))

# thresholds w/ HABs data ======================================================

library(rstatix)
library(gtsummary)
load_all()
habs <- habs |> drop_na(B_G_DENS)

# EHIGH
ehigh_habs <- habs |>
  filter(AG_ECO3 == 'EHIGH')

test_out <- ehigh_habs |> identify_outliers(B_G_DENS)

test_filter <- ehigh_habs |>
  filter(!B_G_DENS %in% test_out$B_G_DENS)

# good | fair cutoff at 29,855
g.f.ehigh <- quantile(test_filter$B_G_DENS, 0.75)

# fair | poor cutoff at 92,127
f.p.ehigh <- quantile(test_filter$B_G_DENS, 0.95)


# PLNLOW

pln_habs <- habs |>
  filter(AG_ECO3 == 'PLNLOW')

test_out <- pln_habs |> identify_outliers(B_G_DENS)

test_filter <- pln_habs |>
  filter(!B_G_DENS %in% test_out$B_G_DENS)

# good | fair cutoff at 80,282
g.f.pln <- quantile(test_filter$B_G_DENS, 0.75)

# fair | poor cutoff at 258,390
f.p.pln <- quantile(test_filter$B_G_DENS, 0.95)


# WMTNS

west_habs <- habs |>
  filter(AG_ECO3 == 'WMTNS')

test_out <- west_habs |> identify_outliers(B_G_DENS)

test_filter <- west_habs |>
  filter(!B_G_DENS %in% test_out$B_G_DENS)

# good | fair cutoff at 20,800
g.f.west <- quantile(test_filter$B_G_DENS, 0.75)

# fair | poor cutoff at 54,598
f.p.west <- quantile(test_filter$B_G_DENS, 0.95)

PredData <- PredData |>
  mutate(cyano_ecocond = factor(case_when(AG_ECO3 == "EHIGH" & cyano_transform_fit < f.p.ehigh ~ 'Low',
                                          AG_ECO3 == "EHIGH" & cyano_transform_fit >= f.p.ehigh ~ 'High',

                                          AG_ECO3 == "PLNLOW" & cyano_transform_fit < f.p.pln ~ 'Low',
                                          AG_ECO3 == "PLNLOW" & cyano_transform_fit >= f.p.pln ~ 'High',

                                          AG_ECO3 == "WMTNS" & cyano_transform_fit < f.p.west ~ 'Low',
                                          AG_ECO3 == "WMTNS" & cyano_transform_fit >= f.p.west ~ 'High'),
                                levels = c('High','Low'))) |>
  arrange(cyano_ecocond)


# get ecoregional totals
# total_table <- PredData |>
#   st_drop_geometry() |>
#   dplyr::select(AG_ECO3, cyano_ecocond) |>
#   group_by(cyano_ecocond, AG_ECO3) |>
#   mutate(total = n()) |>
#   distinct()

# WITH MICROCYSTIN

micx_grp <- habs |>
  st_drop_geometry() |>
  drop_na(MICX_DET) |>
  group_by(AG_ECO3) |>
  count(MICX_DET) |>
  mutate(total = n()) |>
  mutate(prop = n / sum(n))

PredData <- PredData |>
  mutate(micx_ecocond = factor(case_when(AG_ECO3 == "EHIGH" & pred_micx_fit <= 0.220 ~ 'Below',
                                         AG_ECO3 == "EHIGH" & pred_micx_fit > 0.220 ~ 'Above',

                                         AG_ECO3 == "PLNLOW" & pred_micx_fit <= 0.472 ~ 'Below',
                                         AG_ECO3 == "PLNLOW" & pred_micx_fit > 0.472 ~ 'Above',

                                         AG_ECO3 == "WMTNS" & pred_micx_fit <= 0.125 ~ 'Below',
                                         AG_ECO3 == "WMTNS" & pred_micx_fit > 0.125 ~ 'Above'),
                               levels = c("Above","Below" ))) |>
  arrange(micx_ecocond)

# total_micx <- PredData |>
#   st_drop_geometry() |>
#   dplyr::select(AG_ECO3, micx_ecocond) |>
#   group_by(micx_ecocond, AG_ECO3) |>
#   mutate(total = n()) |>
#   distinct()

# PredData <- PredData |>
#   rowwise() |>
#   mutate(tn_inputs = (n_farm_inputs + n_dev_inputs),
#          tp_inputs = (p_farm_inputs + p_dev_inputs))

# PredData <- PredData |>
#   mutate(cyano_cat = factor(case_when(
#     (n_farm_inputs >= 10 | p_dev_inputs >= 4) & cyano_ecocond == 'High' ~ 'HNHC',
#     (n_farm_inputs < 10 | p_dev_inputs < 4) & cyano_ecocond == 'High' ~ 'LNHC',
#     (n_farm_inputs >= 10 | p_dev_inputs >= 4) & cyano_ecocond == 'Low' ~ 'HNLC',
#     (n_farm_inputs < 10 | p_dev_inputs < 4) & cyano_ecocond == 'Low' ~ 'LNLC',
#     TRUE ~ 'OTHER'),
#     levels = c('HNHC','HNLC','LNHC','LNLC'))) |>
#   arrange(cyano_cat) |>
#   mutate(micx_cat = factor(case_when(
#     (n_farm_inputs >= 10 | p_dev_inputs >= 4) & micx_ecocond == 'Above' ~ 'HNHM',
#     (n_farm_inputs < 10 | p_dev_inputs < 4) & micx_ecocond == 'Above' ~ 'LNHM',
#     (n_farm_inputs >= 10 | p_dev_inputs >= 4) & micx_ecocond == 'Below' ~ 'HNLM',
#     (n_farm_inputs < 10 | p_dev_inputs < 4) & micx_ecocond == 'Below' ~ 'LNLM',
#     TRUE ~ 'OTHER'),
#     levels = c('HNHM','HNLM','LNHM','LNLM'))) |>
#   arrange(micx_cat)

# with model input values
PredData <- PredData |>
  mutate(cyano_cat = factor(case_when(
    (n_farm_inputs >= 10 | p_dev_inputs >= 4) & cyano_ecocond == 'High' ~ 'HNHC',
    (n_farm_inputs < 10 | p_dev_inputs < 4) & cyano_ecocond == 'High' ~ 'LNHC',
    (n_farm_inputs >= 10 | p_dev_inputs >= 4) & cyano_ecocond == 'Low' ~ 'HNLC',
    (n_farm_inputs < 10 | p_dev_inputs < 4) & cyano_ecocond == 'Low' ~ 'LNLC',
    TRUE ~ 'OTHER'),
    levels = c('HNHC','HNLC','LNHC','LNLC'))) |>
  arrange(cyano_cat) |>
  mutate(micx_cat = factor(case_when(
    (n_dev_inputs >= 10 | p_farm_inputs >= 4) & micx_ecocond == 'Above' ~ 'HNHM',
    (n_dev_inputs < 10 | p_farm_inputs < 4) & micx_ecocond == 'Above' ~ 'LNHM',
    (n_dev_inputs >= 10 | p_farm_inputs >= 4) & micx_ecocond == 'Below' ~ 'HNLM',
    (n_dev_inputs < 10 | p_farm_inputs < 4) & micx_ecocond == 'Below' ~ 'LNLM',
    TRUE ~ 'OTHER'),
    levels = c('HNHM','HNLM','LNHM','LNLM'))) |>
  arrange(micx_cat)


# analysis

# PredData |>
#   st_drop_geometry() |>
#   group_by(cyano_ecocond) |>
#   get_summary_stats(Precip8110Ws, type = 'mean_sd')

test <- PredData |>
  filter(MAXDEPTH < 1)

# check all
PredData |>
  st_drop_geometry() |>
  tbl_summary(by = micx_cat,
              statistic = list(all_continuous() ~ "{mean}")) |>
  gtsummary::add_p()

hnlm <- HNLM |>
  dplyr::filter(MAXDEPTH >0)

# ad_ratio
modelPredData <- PredData |>
  dplyr::filter(MAXDEPTH >0)

bf_model <- splm(BFIWs ~ cyano_cat, PredData, spcov_type = "exponential")
anova(bf_model)

ad_model <- splm((log10(ad_ratio)) ~  cyano_cat, PredData, spcov_type = "exponential")
anova(ad_model)

precip_model <- splm((log10(Precip8110Ws)) ~ cyano_cat, PredData, spcov_type = "exponential")
anova(precip_model)

PredData |>
  dplyr::filter(MAXDEPTH >0) |>
  ggplot(aes(x=(log10(ad_ratio) + 1), y= cyano_cat)) +
  ggridges::geom_density_ridges(aes(fill = cyano_cat),
                                scale = 2,
                                alpha = 0.85,
                                quantile_lines = TRUE, quantiles = 2) +
  scale_fill_manual(values = c("#9c0082","#4e8562", "#cc6de4", "#8bd1a5"),
                    labels = micxcat_labels) +
  xlim(0,100) +
  labs(x = "ad_ratio", y = "Density Distribution",  fill = 'Class',
       title = 'Cyanobacteria') +
  theme(axis.title.y=element_blank()) +
  guides(color = guide_legend(ncol=2, override.aes = list(size=4, shape = 15)))

ggplot() +
  geom_density(data = pred_filter, aes((x = log(cyano_transform_fit)))) +
  geom_density(data = habs, (aes(x = log(B_G_DENS))))


# thresholds w/ PredData  ======================================================

library(rstatix)
# load_all()
# PredData <- PredData |> drop_na(cyano_transform_fit)
#
# # EHIGH
# ehigh_PredData <- PredData |>
#   filter(AG_ECO3 == 'EHIGH')
#
# test_out <- ehigh_PredData |> identify_outliers(cyano_transform_fit)
#
# test_filter <- ehigh_PredData |>
#   filter(!cyano_transform_fit %in% test_out$cyano_transform_fit)
#
# # good | fair cutoff at 125,377
# g.f.ehigh <- quantile(test_filter$cyano_transform_fit, 0.75)
#
# # fair | poor cutoff at 242,242
# f.p.ehigh <- quantile(test_filter$cyano_transform_fit, 0.95)
#
#
# # PLNLOW
#
# pln_PredData <- PredData |>
#   filter(AG_ECO3 == 'PLNLOW')
#
# test_out <- pln_PredData |> identify_outliers(cyano_transform_fit)
#
# test_filter <- pln_PredData |>
#   filter(!cyano_transform_fit %in% test_out$cyano_transform_fit)
#
# # good | fair cutoff at 212,240
# g.f.pln <- quantile(test_filter$cyano_transform_fit, 0.75)
#
# # fair | poor cutoff at 338,178
# f.p.pln <- quantile(test_filter$cyano_transform_fit, 0.95)
#
#
# # WMTNS
#
# west_PredData <- PredData |>
#   filter(AG_ECO3 == 'WMTNS')
#
# test_out <- west_PredData |> identify_outliers(cyano_transform_fit)
#
# test_filter <- west_PredData |>
#   filter(!cyano_transform_fit %in% test_out$cyano_transform_fit)
#
# # good | fair cutoff at 77,413
# g.f.west <- quantile(test_filter$cyano_transform_fit, 0.75)
#
# # fair | poor cutoff at 136,937
# f.p.west <- quantile(test_filter$cyano_transform_fit, 0.95)
#
# PredData <- PredData |>
#   mutate(cyano_ecocond = factor(case_when(AG_ECO3 == "EHIGH" & cyano_transform_fit < f.p.ehigh ~ 'Low',
#                                           AG_ECO3 == "EHIGH" & cyano_transform_fit >= f.p.ehigh ~ 'High',
#
#                                           AG_ECO3 == "PLNLOW" & cyano_transform_fit < f.p.pln ~ 'Low',
#                                           AG_ECO3 == "PLNLOW" & cyano_transform_fit >= f.p.pln ~ 'High',
#
#                                           AG_ECO3 == "WMTNS" & cyano_transform_fit < f.p.west ~ 'Low',
#                                           AG_ECO3 == "WMTNS" & cyano_transform_fit >= f.p.west ~ 'High'),
#                                 levels = c('High','Low'))) |>
#   arrange(cyano_ecocond)
#
#
# # get ecoregional totals
# total_table <- PredData |>
#   st_drop_geometry() |>
#   dplyr::select(AG_ECO3, cyano_ecocond) |>
#   group_by(cyano_ecocond, AG_ECO3) |>
#   mutate(total = n()) |>
#   distinct()


# nutrient HABs risk analysis with HABs thresholds 5/11/26 ------------------

PredData <- PredData |>
  mutate(cyano_cat = factor(case_when(
    (n_farm_inputs >= 10 | p_dev_inputs >= 4) & cyano_ecocond == 'High' ~ 'HNHC',
    (n_farm_inputs < 10 | p_dev_inputs < 4) & cyano_ecocond == 'High' ~ 'LNHC',
    (n_farm_inputs >= 10 | p_dev_inputs >= 4) & cyano_ecocond == 'Low' ~ 'HNLC',
    (n_farm_inputs < 10 | p_dev_inputs < 4) & cyano_ecocond == 'Low' ~ 'LNLC',
    TRUE ~ 'OTHER'),
    levels = c('LNLC','HNHC','HNLC','LNHC'))) |>
  arrange(cyano_cat) |>
  mutate(micx_cat = factor(case_when(
    (n_farm_inputs >= 10 | p_dev_inputs >= 4) & micx_ecocond == 'Above' ~ 'HNHM',
    (n_farm_inputs < 10 | p_dev_inputs < 4) & micx_ecocond == 'Above' ~ 'LNHM',
    (n_farm_inputs >= 10 | p_dev_inputs >= 4) & micx_ecocond == 'Below' ~ 'HNLM',
    (n_farm_inputs < 10 | p_dev_inputs < 4) & micx_ecocond == 'Below' ~ 'LNLM',
    TRUE ~ 'OTHER'),
    levels = c('LNLM','HNHM','HNLM','LNHM'))) |>
  arrange(micx_cat)

# mapping these ----------------------------------------------------------------

BFIW_col <- RColorBrewer::brewer.pal(4, "YlGnBu")

nurisk = c('HNHC' = "#9c0082",
           'HNLC' = "#cc6de4",
           'LNHC' = "#4e8562",
           'LNLC' = "#8bd1a5")

nurisk_m = c('HNHM' = "#9c0082",
           'HNLM' = "#cc6de4",
           'LNHM' = "#4e8562",
           'LNLM' = "#8bd1a5")

ggplot(PredData, aes(color = cyano_cat)) +
  geom_sf(size = .25) +
  scale_color_manual(values = nurisk,
                     name = "Category") +
  labs(title = "Cyanobacteria Nutrient / Risk Combined Category") +
  geom_sf(data = states, fill = NA, color = "black", lwd = 0.1) +
  theme(plot.title = element_text(size = 12)) +
  guides(colour = guide_legend(override.aes = list(size=4)))

ggplot(PredData, aes(color = micx_cat)) +
  geom_sf(size = .25) +
  scale_color_manual(values = nurisk_m,
                     name = "Category") +
  labs(title = "Microcystin Nutrient / Risk Combined Category") +
  geom_sf(data = states, fill = NA, color = "black", lwd = 0.1) +
  theme(plot.title = element_text(size = 12)) +
  guides(colour = guide_legend(override.aes = list(size=4)))

ggsave("risk_micx.jpeg", width = 12, height = 8, device = 'jpeg', dpi = 500)


# final historgrams 2026 -------------------------------------------------------

test <- PredData |>
  st_drop_geometry() |>
  ungroup()

test <- dplyr::sample_n(test, size = 1500, replace = FALSE)

shapiro.test(test$ad_ratio)
kruskal.test(ad_ratio ~ cyano_cat, data = PredData)
kruskal.test(ad_ratio ~ micx_cat, data = PredData)

shapiro.test(test$drain_ratio)
kruskal.test(drain_ratio ~ cyano_cat, data = PredData)
kruskal.test(drain_ratio ~ micx_cat, data = PredData)

shapiro.test(test$BFIWs) # normal distribution, non-transformed ANOVA works

dr_model_cy <- splm(log(drain_ratio) ~ cyano_cat, PredData, spcov_type = "exponential")
anova(dr_model_cy)

dr_model_mx <- splm(log(drain_ratio) ~ micx_cat, PredData, spcov_type = "exponential")
anova(dr_model_mx)

bf_model_cy <- splm(BFIWs ~ cyano_cat, PredData, spcov_type = "exponential")
anova(bf_model_cy)

bf_model_mx <- splm(BFIWs ~ micx_cat, PredData, spcov_type = "exponential")
anova(bf_model_mx)

ad_model_cy <- splm(log(ad_ratio) ~ cyano_cat, PredData, spcov_type = "exponential")
anova(ad_model_cy)

ad_model_mx <- splm(log(ad_ratio) ~ micx_cat, PredData, spcov_type = "exponential")
anova(ad_model_mx)

saveRDS(dr_model_mx,
        file = 'C:/Users/mreyno04/OneDrive - Environmental Protection Agency (EPA)/Profile/REPOS/HABsDrivers/inst/model_objects/dr_model_mx26.rds')

ggsave("qqplot_cyano_ad_ratio.jpeg", width = 8, height = 6, device = 'jpeg', dpi = 500)

# ggplot(PredData, aes(drain_ratio, pred_cyano_fit)) +
#   geom_point(aes(colour = cyano_cat)) +
#   stat_poly_line() +
#   stat_poly_eq(use_label(c("eq", "P", "adj.R2"))) +
#   scale_y_continuous(trans = "log10") +
#   scale_x_continuous(trans = "log10")

sum_table <- PredData |>
  st_drop_geometry() |>
  group_by(AG_ECO3, cyano_cat) |>
  summarize(mean = mean(drain_ratio, na.rm = TRUE),
            median = median(drain_ratio, na.rm = TRUE))

minitab <- sum_table |>
  st_drop_geometry() |>
  dplyr::select(AG_ECO3, cyano_cat, mean, median)

summ_table <- PredData |>
  st_drop_geometry() |>
  group_by(AG_ECO3, micx_cat) |>
  summarize(mean = mean(drain_ratio, na.rm = TRUE),
            median = median(drain_ratio, na.rm = TRUE))


mminitab <- summ_table |>
  st_drop_geometry() |>
  dplyr::select(micx_cat, BFIWs, drain_ratio, ad_ratio)

dataset_list <- list("cyanos" = sum_table, "micx" = summ_table)
openxlsx::write.xlsx(dataset_list, file = "risk-cat-allvars.xlsx")

# models

dr_model_cy <- splm(log10(drain_ratio) ~ cyano_cat, PredData, spcov_type = "exponential")
anova(dr_model_cy)

dr_model_mx <- splm(log10(drain_ratio) ~ micx_cat, PredData, spcov_type = "exponential")
anova(dr_model_mx)

bf_model_cy <- splm(BFIWs ~ cyano_cat, PredData, spcov_type = "exponential")
anova(bf_model_cy)

bf_model_mx <- splm(BFIWs ~ micx_cat, PredData, spcov_type = "exponential")
anova(bf_model_mx)

ad_model_cy <- splm(log10(ad_ratio) ~ cyano_cat, PredData, spcov_type = "exponential")
anova(ad_model_cy)

ad_model_mx <- splm(log10(ad_ratio) ~ micx_cat, PredData, spcov_type = "exponential")
anova(ad_model_mx)

saveRDS(ad_model_cy,
        file = 'C:/Users/mreyno04/OneDrive - Environmental Protection Agency (EPA)/Profile/REPOS/HABsDrivers/inst/model_objects/ad_model_cy6_26.rds')


# percents

pred_pct <- PredData |>
  count(micx_cat) |>
  mutate(percentage = (n / sum(n)) * 100)

cyano_pct <- PredData |>
  count(cyano_cat) |>
  mutate(percentage = (n / sum(n)) * 100)

mod <- aov(drain_ratio ~ AG_ECO3 * micx_cat,
           data = PredData
)


cymod <- aov(drain_ratio ~ AG_ECO3 * cyano_cat,
           data = PredData
)

ad_mod <- aov(ad_ratio ~ AG_ECO3 * micx_cat,
           data = PredData
)
basemod <- aov(BFIWs ~ AG_ECO3 * micx_cat,
               data = PredData
)

# lake volume ratio

PredData <- PredData |>
  mutate(MAXDEPTH = replace(MAXDEPTH, MAXDEPTH == 0, 0.0001))

PredData <- PredData |>
  rowwise() |>
  mutate(volume = lake_area * (MAXDEPTH * (1/3)),
         WALV = WSAREASQKM / volume)

em11 <- emmeans(dr_model_cy, ~ cyano_cat)
emad <- emmeans(dr_model_mx, ~ micx_cat)

drain_mod <- emmeans(dr_model_cy, ~ cyano_cat)
pairs(drain_mod)
plot(pairs(drain_mod))

cymod <- aov(drain_ratio ~ AG_ECO3 * cyano_cat,
             data = PredData
)
cymod
drain_cyano <- emmeans(cymod, ~ cyano_cat, by = 'AG_ECO3')

micxmod <- aov(drain_ratio ~ AG_ECO3 * micx_cat,
             data = PredData
)
micxmod
drain_micx <- emmeans(micxmod, ~ micx_cat, by = 'AG_ECO3')

# drainage ratio with HABs dataset ---------------------------------------------

no_geo <- PredData |> st_drop_geometry()

testset <- habs |>
  st_drop_geometry() |>
  dplyr::select(COMID, DOC, PTL, NTL, B_G_DENS, MICX, EVAP_INFL) |>
  left_join(no_geo |> dplyr::select(COMID, drain_ratio, AG_ECO3, cyano_cat, micx_cat), by = 'COMID') |>
  drop_na(drain_ratio)

ggplot(testset, aes(x = drain_ratio, y = PTL)) +
  #facet_wrap(~AG_ECO3) +
  geom_point(color = "blue", size = 2) +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  stat_poly_line() +
  stat_poly_eq(use_label(c("eq", "P", "adj.R2"))) +
  scale_y_continuous(trans = "log10") +
  scale_x_continuous(trans = "log10")

test_table <- testset |>
  group_by(AG_ECO3, cyano_cat) |>
  summarize(mean.PTL = mean(PTL, na.rm = TRUE),
            median.PTL = median(PTL, na.rm = TRUE),
            mean.DR = mean(drain_ratio, na.rm = TRUE),
            median.DR = median(drain_ratio, na.rm = TRUE))

testtab <- sum_table |>
  st_drop_geometry() |>
  dplyr::select(AG_ECO3, cyano_cat, mean, median)

PredData <- PredData |>
  mutate(hydrolake = if_else(drain_ratio >= 10, "drainage", "seepage"))

PredData |>
  st_drop_geometry() |>
  #group_by(state) |>
  count(cyano_ecocond) |>
  mutate(percent = (n / sum(n)) * 100) |>
  View()


PredData <- PredData |>
  mutate(old_cutoff = if_else(pred_micx_fit >= 0.50, "above 50%", "below 50%"))

PredData$LAGOSLakeDepth <- replace(PredData$LAGOSLakeDepth, which(PredData$LAGOSLakeDepth <= 0), NA)
PredData$NHDLakeDepth <- replace(PredData$NHDLakeDepth, which(PredData$NHDLakeDepth <= 0), NA)
PredData$MaxDepth <- replace(PredData$MaxDepth, which(PredData$MaxDepth <= 0), NA)


df_clean <- PredData %>%
  mutate(
    # List columns in order of preference (most preferred to least preferred)
    combined_data = coalesce(LAGOSLakeDepth, NHDLakeDepth, depth),

    # Create a new column indicating which column provided the data
    data_source = pmap_chr(list(LAGOSLakeDepth, NHDLakeDepth, depth), function(...) {
      sources <- c("LAGOS", "NHD", "my depth")
      values <- c(...)

      # Return the name of the column containing the first non-NA value
      if (all(is.na(values))) return(NA_character_)
      sources[min(which(!is.na(values)))]
    })
  )

print(df_clean)
df_copy <- df_clean

df_copy <- df_copy |>
  distinct() |>
  filter(COMID %in% PredData$COMID)

df_copy <- df_copy %>%
  mutate(data_source = ifelse(data_source == "my depth", "Lakemorpho", data_source)) |>
  rename(depth_source = data_source)

copied <- df_copy |>
  dplyr::select(COMID, depth_source)

PredData <- PredData |>
  relocate(depth_source ,.after = MAXDEPTH)

drain_stats_all <- drain_stats_all |>
  clean_names()

saveRDS(drain_stats_all,
        file = 'C:/Users/mreyno04/OneDrive - Environmental Protection Agency (EPA)/Profile/REPOS/HABsDrivers/inst/drain_stats_all.rds')
