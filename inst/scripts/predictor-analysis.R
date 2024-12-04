
# Micx/Cyano Predictors comparison analysis
# 11-21-2024
# must load all pred results and packages

# Microcystin ==================================================================

var_pred <- st_join(wbd_copy, micx_pred_df)

comp_micx <- PredDataMini |>
  select(c(COMID, Runoff.Str, wet_ws, agr_ws, LakeVolume, drain_ratio)) |>
  merge(var_pred, by = 'COMID')


# Nutrients --------------------------------------------------------------------
comp_micx$nutr_all <- comp_micx$p_farm_inputs + comp_micx$n_dev_inputs

comp_micx <- comp_micx %>%
  mutate(nutr_class = factor(case_when(nutr_all >= 10 & pred_micx <= 0.277 ~ 'HNLM',
                                       nutr_all <= 10 & pred_micx >= 0.277 ~ 'LNHM',
                                       nutr_all >= 10 & pred_micx >= 0.277 ~ 'HNHM',
                                       nutr_all <= 10 & pred_micx <= 0.277 ~ 'LNLM')))
comp_micx_filter <- comp_micx |>
  filter(!is.na(nutr_class))

comp_micx_filter$Shape <- st_point_on_surface(comp_micx_filter$Shape)

# HNLM ~ high nutrients, low microcystin

HNLM <- comp_micx |>
  filter(nutr_class == 'HNLM')

HNHM <- comp_micx |>
  filter(nutr_class == 'HNHM')

LNLM <- comp_micx |>
  filter(nutr_class == 'LNLM')

LN <- comp_micx |>
  filter(nutr_all == 'LNHM' | nutr_all == 'LNLM')


HNLM <- HNLM %>%
  mutate(depth_class = factor(case_when(MAXDEPTH <= 1 ~ 'D1',
                                        MAXDEPTH >= 1 & MAXDEPTH < 10 ~ 'D2',
                                        MAXDEPTH >= 10 & MAXDEPTH < 50 ~ 'D3',
                                        MAXDEPTH >= 50 ~ 'D4')))

dep_labels <- c('< 1','1-10', '10-50','> 50')
dep_col <- RColorBrewer::brewer.pal(4, "YlGnBu")

ggplot(HNLM, aes(color = depth_class)) +
  geom_sf(size = 1) +
  scale_color_manual(values = dep_col,
                     labels = dep_labels,
                     name = "Meters") +
  labs(title = "Lake Depths (High Nutrients, Low Microcystin)") +
  geom_sf(data = states, fill = NA, color = "black", lwd = 0.1) +
  theme(plot.title = element_text(size = 12)) +
  guides(colour = guide_legend(override.aes = list(size=4)))

ggsave("HNLM_depth.jpeg", width = 12, height = 8, device = 'jpeg', dpi = 500)


# LNHM ~ low nutrients, high microcystin

LNHM <- comp_micx |>
  filter(nutr_class == 'LNHM')

LNHM <- LNHM %>%
  mutate(depth_class = factor(case_when(MAXDEPTH <= 1 ~ 'D1',
                                        MAXDEPTH >= 1 & MAXDEPTH < 10 ~ 'D2',
                                        MAXDEPTH >= 10 & MAXDEPTH < 50 ~ 'D3',
                                        MAXDEPTH >= 50 ~ 'D4')))

ggplot(LNHM, aes(color = depth_class)) +
  geom_sf(size = 1) +
  scale_color_manual(values = dep_col,
                     labels = dep_labels,
                     name = "Meters") +
  labs(title = "Lake Depths (Low Nutrients, High Microcystin)") +
  geom_sf(data = states, fill = NA, color = "black", lwd = 0.1) +
  theme(plot.title = element_text(size = 12)) +
  guides(colour = guide_legend(override.aes = list(size=4)))

ggsave("eco_micx.jpeg", width = 12, height = 8, device = 'jpeg', dpi = 500)


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

ggplot(comp_micx_filter, aes(x=wet_ws, fill = nutr_class)) +
  geom_density(size = 0.75, alpha = 0.5) +
  #xlim(0,50000) +
  labs(y = "Density", x = "lake volume", fill = 'Class',
       title = "lake volume")


ggplot(comp_micx_filter, aes(x=AG_ECO3, fill = nutr_class)) +
  geom_histogram(position="dodge", stat = "count") +
  labs(y = "Count", x = "Ecoregion", fill = 'Class',
       title = "Ecoregions - Micx")

# comp_micx_filter <- comp_micx_filter |>
#   mutate(depth_log <- transform(comp_micx_filter$MAXDEPTH, method = 'log'))

#names(comp_micx_filter)[names(comp_micx_filter) == "_data"] <- "log_depth"

labels = c("0-25%", "25-50%", "50-75%", "75-100%")
breaks <- c(0.25,0.50,0.75,1.0)
cols <- c("#2B83BA","#ABDDA4", "#FDAE61", "#D7191C")

ggplot(comp_micx_filter, aes(color = Runoff.Str)) +
  geom_sf(size = 0.4) +
  scale_color_stepsn(colors = cols,
                     breaks = breaks,
                     labels = labels,
                     name = "Probability (%)") +
  labs(title = "Microcystin Detection at or above 0.1 μg/L") +
  geom_sf(data = states, fill = NA, color = "black", lwd = 0.1) +
  theme(plot.title = element_text(size = 12)) +
  guides(colour = guide_legend(override.aes = list(size=4)))



# log transform depth and fetch data to make easier for density mapping

table(LNLM$AG_ECO3)

# Cyanobacteria ================================================================

# Nutrients --------------------------------------------------------------------


comp_cyano <- pred_df
comp_cyano$nutr_all <- comp_cyano$n_farm_inputs + comp_cyano$p_dev_inputs

comp_cyano_filter <- comp_cyano %>%
  mutate(nutr_class = factor(case_when(nutr_all >= 10 & pred_cyano <= 5.1034 ~ 'HNLC',
                                       nutr_all <= 10 & pred_cyano >= 5.1034 ~ 'LNHC')))
                                       #nutr_all >= 10 & pred_cyano >= 5.1034 ~ 'HNHC',
                                       #nutr_all <= 10 & pred_cyano <= 5.1034 ~ 'LNLC')))
comp_cyano_filter <- comp_cyano_filter |>
  filter(!is.na(nutr_class))

# HNLC ~ high nutrients, low cyanobacteria

HNLC <- comp_cyano |>
  filter(nutr_class == 'HNLC')

HNHC <- comp_cyano |>
  filter(nutr_class == 'HNHC')

LNLC <- comp_cyano |>
  filter(nutr_class == 'LNLC')

LNHC <- comp_cyano |>
  filter(nutr_class == 'LNHC')

HNLC <- HNLC %>%
  mutate(depth_class = factor(case_when(MAXDEPTH <= 1 ~ 'D1',
                                        MAXDEPTH >= 1 & MAXDEPTH < 10 ~ 'D2',
                                        MAXDEPTH >= 10 & MAXDEPTH < 50 ~ 'D3',
                                        MAXDEPTH >= 50 ~ 'D4')))

dep_labels <- c('< 1','1-10', '10-50','> 50')
dep_col <- RColorBrewer::brewer.pal(4, "YlGnBu")

ggplot(HNLC, aes(color = depth_class)) +
  geom_sf(size = 1) +
  scale_color_manual(values = dep_col,
                     labels = dep_labels,
                     name = "Meters") +
  labs(title = "Lake Depths (High Nutrients, Low Cyanobacteria)") +
  geom_sf(data = states, fill = NA, color = "black", lwd = 0.1) +
  theme(plot.title = element_text(size = 12)) +
  guides(colour = guide_legend(override.aes = list(size=4)))

ggsave("HNLC_depth.jpeg", width = 12, height = 8, device = 'jpeg', dpi = 500)


# LNHM ~ low nutrients, high microcystin


LNHC <- LNHC %>%
  mutate(depth_class = factor(case_when(MAXDEPTH <= 1 ~ 'D1',
                                        MAXDEPTH >= 1 & MAXDEPTH < 10 ~ 'D2',
                                        MAXDEPTH >= 10 & MAXDEPTH < 50 ~ 'D3',
                                        MAXDEPTH >= 50 ~ 'D4')))

ggplot(LNHC, aes(color = depth_class)) +
  geom_sf(size = 1) +
  scale_color_manual(values = dep_col,
                     labels = dep_labels,
                     name = "Meters") +
  labs(title = "Lake Depths (Low Nutrients, High Cyanobacteria)") +
  geom_sf(data = states, fill = NA, color = "black", lwd = 0.1) +
  theme(plot.title = element_text(size = 12)) +
  guides(colour = guide_legend(override.aes = list(size=4)))

ggsave("eco_cyano.jpeg", width = 12, height = 8, device = 'jpeg', dpi = 500)


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

# Print the result
print(result_base_R)

ggplot(comp_cyano_filter, aes(x=AG_ECO3, fill = nutr_class)) +
  geom_histogram(position="dodge", stat = "count") +
  labs(y = "Count", x = "Ecoregion", fill = 'Class',
       title = "Ecoregions - Cyano")


