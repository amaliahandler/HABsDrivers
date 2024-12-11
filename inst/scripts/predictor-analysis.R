
# Micx/Cyano Predictors comparison analysis
# 11-21-2024
# must load all pred results and packages

# Microcystin ==================================================================

# Nutrients --------------------------------------------------------------------
comp_micx <- micx_pred_df
comp_micx$nutr_all <- comp_micx$p_farm_inputs + comp_micx$n_dev_inputs

comp_micx <- comp_micx %>%
  mutate(nutr_class = factor(case_when(nutr_all > 10 & pred_micx < 0.277 ~ 'HNLM',
                                       nutr_all < 10 & pred_micx > 0.277 ~ 'LNHM')))

# HNLM ~ high nutrients, low microcystin

HNLM <- comp_micx |>
  filter(nutr_class == 'HNLM')

HNLM <- HNLM %>%
  mutate(depth_class = factor(case_when(MAXDEPTH < 1 ~ 'D1',
                                        MAXDEPTH > 1 & MAXDEPTH < 10 ~ 'D2',
                                        MAXDEPTH > 10 & MAXDEPTH < 50 ~ 'D3',
                                        MAXDEPTH > 50 ~ 'D4')))



# LNHM ~ low nutrients, high microcystin

LNHM <- comp_micx |>
  filter(nutr_class == 'LNHM')

LNHM <- LNHM %>%
  mutate(depth_class = factor(case_when(MAXDEPTH < 1 ~ 'D1',
                                        MAXDEPTH > 1 & MAXDEPTH < 10 ~ 'D2',
                                        MAXDEPTH > 10 & MAXDEPTH < 50 ~ 'D3',
                                        MAXDEPTH > 50 ~ 'D4')))


# Lake Depth -------------------------------------------------------------------






