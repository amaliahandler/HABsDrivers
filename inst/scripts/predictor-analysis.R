
# Micx/Cyano Predictors comparison analysis
# 11-21-2024
# must load all pred results and packages

# Microcystin ==================================================================

var_pred <- st_join(wbd_copy, micx_pred_df)

pred_cols <- PredDataMini |>
  select(c(COMID, Runoff.Str, wet_ws, agr_ws, LakeVolume, drain_ratio, Tot_Sdep_2007, ag_eco9,
           ClayWs, SandWs, AgKffactWs))

comp_micx <- left_join(var_pred, pred_cols, by = 'COMID')

# Nutrients --------------------------------------------------------------------
comp_micx$nutr_all <- comp_micx$p_farm_inputs + comp_micx$n_dev_inputs

comp_micx <- comp_micx %>%
  mutate(nutr_class = factor(case_when(nutr_all >= 10 & pred_micx <= 0.75 ~ 'HNLM',
                                       nutr_all <= 10 & pred_micx >= 0.75 ~ 'LNHM',
                                       nutr_all >= 10 & pred_micx >= 0.75 ~ 'HNHM',
                                       nutr_all <= 10 & pred_micx <= 0.75 ~ 'LNLM')))
comp_micx_filter <- comp_micx |>
  filter(!is.na(nutr_class))

comp_micx_filter$Shape <- st_point_on_surface(comp_micx_filter$Shape)|>
  st_transform(crs=5072)

# HNLM ~ high nutrients, low microcystin

HNLM <- comp_micx |>
  filter(nutr_class == 'HNLM')

HNHM <- comp_micx |>
  filter(nutr_class == 'HNHM')

LNLM <- comp_micx |>
  filter(nutr_class == 'LNLM')

LNHM <- comp_micx |>
  filter(nutr_class == 'LNHM')

ggplot(HNLM, aes(color = ag_eco9)) +
  geom_sf(size = 1)

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

ggsave("eco_75_micx.jpeg", width = 12, height = 8, device = 'jpeg', dpi = 500)

# 9 ecoregions -----------------------------------------------------------------

ggplot(comp_micx_filter, aes(color = ag_eco9)) +
  geom_sf(size = 0.5) +
  facet_wrap(~nutr_class) +
  # scale_color_manual(values = BFIW_col,
  #                    labels = BFIW_labels,
  #                    name = "Sand % in Soil") +
  labs(title = "Ecoregion with 75% Micx threshold") +
  geom_sf(data = states, fill = NA, color = "black", lwd = 0.1) +
  theme(plot.title = element_text(size = 12)) +
  guides(colour = guide_legend(override.aes = list(size=4)))

# Temp / Precip ----------------------------------------------------------------

ggplot(comp_micx_filter, aes(color = Tmean8110Ws)) +
  geom_sf(size = 0.5) +
  facet_wrap(~nutr_class) +
  # scale_color_manual(values = BFIW_col,
  #                    labels = BFIW_labels,
  #                    name = "Sand % in Soil") +
  labs(title = "Temperature Avg. with 75% Micx threshold") +
  geom_sf(data = states, fill = NA, color = "black", lwd = 0.1) +
  theme(plot.title = element_text(size = 12))



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
  geom_sf(size = 0.75) +
  facet_wrap(~nutr_class) +
  scale_color_manual(values = fst_cols,
                     labels = fst_labels,
                     name = "Cover (%)") +
  labs(title = "Forested Land Cover- Micx @ 75%") +
  geom_sf(data = states, fill = NA, color = "black", lwd = 0.1) +
  theme(plot.title = element_text(size = 12)) +
  guides(colour = guide_legend(override.aes = list(size=4)))

ggsave("fst_75_micx.jpeg", width = 12, height = 8, device = 'jpeg', dpi = 500)


#  -------------------------------------------------------------------

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
  labs(y = "Density", x = "30 Year Temp Average", fill = 'Class',
       title = "Temperature")

ggplot(comp_micx, aes(x=Precip8110Ws, fill = nutr_class)) +
  geom_density(size = 0.75, alpha = 0.5) +
  labs(y = "Density", x = "30 Year Precip Average", fill = 'Class',
       title = 'Precipitation')

ggplot(comp_micx, aes(x=BFIWs, fill = nutr_class)) +
  geom_density(size = 0.75, alpha = 0.5) +
  labs(y = "Density", x = "% of Flow that is Base Flow", fill = 'Class',
       title = "Base Flow")

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

ggsave("micx_sand.jpeg", width = 12, height = 8, device = 'jpeg', dpi = 500)


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

HNLC <- comp_cyano |>
  filter(nutr_class == 'HNLC')

HNHC <- comp_cyano |>
  filter(nutr_class == 'HNHC')

LNLC <- comp_cyano |>
  filter(nutr_class == 'LNLC')

LNHC <- comp_cyano |>
  filter(nutr_class == 'LNHC')

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

# agricultural erodability -----------------------------------------------------

ggplot(comp_cyano_filter, aes(color = AgKffactWs)) +
  geom_sf(size = 0.5) +
  facet_wrap(~nutr_class) +
  # scale_color_manual(values = BFIW_col,
  #                    labels = BFIW_labels,
  #                    name = "Sand % in Soil") +
  labs(title = "Ecoregion with 100k Cyano threshold") +
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


