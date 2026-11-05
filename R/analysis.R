# ============================================================
# ELECTRICITY CONSUMPTION ACROSS DANISH MUNICIPALITIES
# Reproducible analysis script
# ============================================================


# ============================================================
# 1. PACKAGES
# ============================================================

library(tidyverse)
library(lmtest)
library(sandwich)

municipality_data <- read_csv(
  "data/municipality_data.csv",
  show_col_types = FALSE
)

stopifnot(nrow(municipality_data) == 98)



# ============================================================
# 11. MAIN MODEL: MODEL 3
# ============================================================

Model3 <- lm(
  kwh_per_capita ~
    share_65plus +
    mean_disposable_income +
    log_population_density +
    ev_share +
    share_electric_heating +
    holiday_homes_per_1000,
  data = municipality_data
)


# HC3 covariance matrix for main model
Model3_HC3 <- coeftest(
  Model3,
  vcov. = vcovHC(Model3, type = "HC3")
)


# ============================================================
# 12. MODEL 3 SCALED
#
# Age, EV and electric heating:
#   percentage points
#
# Income:
#   already measured in DKK 1,000
#
# Holiday homes:
#   100 holiday homes per 1,000 permanent residents
# ============================================================

municipality_data <- municipality_data |>
  mutate(
    share_65plus_pct =
      share_65plus * 100,
    
    ev_share_pct =
      ev_share * 100,
    
    share_electric_heating_pct =
      share_electric_heating * 100,
    
    holiday_homes_100_per_1000 =
      holiday_homes_per_1000 / 100
  )


Model3_scaled <- lm(
  kwh_per_capita ~
    share_65plus_pct +
    mean_disposable_income +
    log_population_density +
    ev_share_pct +
    share_electric_heating_pct +
    holiday_homes_100_per_1000,
  data = municipality_data
)


Model3_scaled_HC3 <- coeftest(
  Model3_scaled,
  vcov. = vcovHC(
    Model3_scaled,
    type = "HC3"
  )
)


# ============================================================
# 13. MODEL 3 STANDARDIZED
#
# X variables standardized to z-scores.
# Y remains kWh per capita.
# ============================================================

municipality_data <- municipality_data |>
  mutate(
    z_share_65plus =
      as.numeric(scale(share_65plus)),
    
    z_mean_disposable_income =
      as.numeric(scale(mean_disposable_income)),
    
    z_log_population_density =
      as.numeric(scale(log_population_density)),
    
    z_ev_share =
      as.numeric(scale(ev_share)),
    
    z_share_electric_heating =
      as.numeric(scale(share_electric_heating)),
    
    z_holiday_homes_per_1000 =
      as.numeric(scale(holiday_homes_per_1000))
  )


Model3_standardized <- lm(
  kwh_per_capita ~
    z_share_65plus +
    z_mean_disposable_income +
    z_log_population_density +
    z_ev_share +
    z_share_electric_heating +
    z_holiday_homes_per_1000,
  data = municipality_data
)


Model3_standardized_HC3 <- coeftest(
  Model3_standardized,
  vcov. = vcovHC(
    Model3_standardized,
    type = "HC3"
  )
)


# ============================================================
# 14. STANDARD DEVIATIONS
# Used for interpretation of standardized coefficients
# ============================================================

standard_deviations <- municipality_data |>
  summarise(
    sd_share_65plus_pct =
      sd(share_65plus) * 100,
    
    sd_income_1000_dkk =
      sd(mean_disposable_income),
    
    sd_log_population_density =
      sd(log_population_density),
    
    sd_ev_share_pct =
      sd(ev_share) * 100,
    
    sd_electric_heating_pct =
      sd(share_electric_heating) * 100,
    
    sd_holiday_homes_per_1000 =
      sd(holiday_homes_per_1000)
  )


# ============================================================
# 15. RESIDUAL RESULTS BY MUNICIPALITY
# ============================================================

municipality_results <- municipality_data |>
  mutate(
    expected_kwh_per_capita =
      fitted(Model3),
    
    residual_kwh_per_capita =
      resid(Model3),
    
    standardized_residual =
      rstandard(Model3),
    
    leverage =
      hatvalues(Model3),
    
    cooks_distance =
      cooks.distance(Model3)
  )


municipalities_above_expected <- municipality_results |>
  arrange(desc(residual_kwh_per_capita)) |>
  select(
    Municipality,
    kwh_per_capita,
    expected_kwh_per_capita,
    residual_kwh_per_capita,
    standardized_residual
  ) |>
  slice_head(n = 10)


municipalities_below_expected <- municipality_results |>
  arrange(residual_kwh_per_capita) |>
  select(
    Municipality,
    kwh_per_capita,
    expected_kwh_per_capita,
    residual_kwh_per_capita,
    standardized_residual
  ) |>
  slice_head(n = 10)


#diverse
municipality_data |>
  mutate(
    std_residual = rstandard(Model3_scaled),
    cooks_distance = cooks.distance(Model3_scaled)
  ) |>
  arrange(std_residual) |>
  select(Municipality, std_residual, cooks_distance) |>
  head(5)