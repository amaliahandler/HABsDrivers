
# Micx/Cyano Predictors comparison analysis
# 11-21-2024
# must load all pred results and packages

# Microcystin ==================================================================

# Nutrients --------------------------------------------------------------------
comp_micx <- micx_pred_df
comp_micx$nutr_all <- comp_micx$p_farm_inputs + comp_micx$n_dev_inputs

comp_micx <- comp_micx %>%
  mutate(class = factor(case_when(nutr_all > 10 & pred_micx < 0.277 ~ 'HNLM',
                                  nutr_all < 10 & pred_micx > 0.277 ~ 'LNHM'))) %>%
  filter(!is.na(class))

# HNLM ~ high nutrients, low microcystin
# LNHM ~ low nutrients, high microcystin

HNLM <- comp_micx |>
  filter(class == 'HNLM')

LNHM <- comp_micx |>
  filter(class == 'LNHM')




