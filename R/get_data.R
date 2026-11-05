# ============================================================
# ELECTRICITY CONSUMPTION ACROSS DANISH MUNICIPALITIES
# Reproducible analysis script
# ============================================================


# ============================================================
# 1. PACKAGES
# ============================================================

library(httr2)
library(tidyverse)
library(lubridate)
library(lmtest)
library(sandwich)


# Hjælpefunktion til Statbank-metadata
show_values <- function(variable) {
  variable$values |>
    lapply(function(x) {
      c(
        id = x$id,
        name = x$text
      )
    })
}


# ============================================================
# 2. ENERGY DATA
# Energi Data Service, 2024
# ============================================================

energy_request <- request(
  "https://api.energidataservice.dk/dataset/PrivateConsumptionHeatingMonth"
) |>
  req_url_query(
    start = "2024-01-01",
    end = "2025-01-01",
    limit = 0
  )

energy_response <- energy_request |>
  req_perform()

all_energy_data <- energy_response |>
  resp_body_json() |>
  (\(x) bind_rows(x$records))() |>
  mutate(
    Month = ymd_hms(Month),
    MunicipalityCode = as.character(MunicipalityCode)
  )


# Kommunekoder, som bruges i Statbank-requests
municipality_codes <- all_energy_data |>
  distinct(MunicipalityCode) |>
  pull(MunicipalityCode) |>
  as.character()


# Årligt elforbrug 2024
electricity_2024 <- all_energy_data |>
  filter(year(Month) == 2024) |>
  group_by(MunicipalityCode) |>
  summarise(
    annual_consumption_kwh = sum(ConsumptionkWh),
    .groups = "drop"
  )


# ============================================================
# 3. POPULATION AND AGE 65+
# Statbank FOLK1AM
# ============================================================

population_metadata_request <- request(
  "https://api.statbank.dk/v1/tableinfo/FOLK1AM"
) |>
  req_url_query(lang = "da")

population_metadata <- population_metadata_request |>
  req_perform() |>
  resp_body_json(check_type = FALSE)


# Stabil kommunekode -> kommunenavn-nøgle
municipality_key <- population_metadata$variables[[1]]$values |>
  lapply(function(x) {
    tibble(
      MunicipalityCode = as.character(x$id),
      Municipality = x$text
    )
  }) |>
  bind_rows() |>
  filter(MunicipalityCode %in% municipality_codes)


age_codes <- as.character(0:125)
time_codes <- paste0("2024M", sprintf("%02d", 1:12))


population_request <- request(
  "https://api.statbank.dk/v1/data"
) |>
  req_method("POST") |>
  req_body_json(
    list(
      table = "FOLK1AM",
      format = "CSV",
      lang = "da",
      variables = list(
        list(
          code = "OMRÅDE",
          values = as.list(municipality_codes)
        ),
        list(
          code = "KØN",
          values = list("TOT")
        ),
        list(
          code = "ALDER",
          values = as.list(age_codes)
        ),
        list(
          code = "Tid",
          values = as.list(time_codes)
        )
      )
    )
  )

population_data <- population_request |>
  req_perform() |>
  resp_body_string() |>
  read_csv2(show_col_types = FALSE) |>
  mutate(
    ALDER = parse_number(ALDER),
    is_65plus = if_else(ALDER >= 65, 1, 0)
  )


population_monthly <- population_data |>
  group_by(OMRÅDE, TID) |>
  summarise(
    population = sum(INDHOLD),
    population_65plus = sum(INDHOLD * is_65plus),
    .groups = "drop"
  )


population_2024 <- population_monthly |>
  group_by(OMRÅDE) |>
  summarise(
    population = mean(population),
    population_65plus = mean(population_65plus),
    .groups = "drop"
  ) |>
  mutate(
    share_65plus = population_65plus / population
  ) |>
  transmute(
    Municipality = OMRÅDE,
    population,
    share_65plus
  )


# ============================================================
# 4. DISPOSABLE INCOME
# Statbank INDKF101
# 1,000 DKK
# ============================================================

income_request <- request(
  "https://api.statbank.dk/v1/data"
) |>
  req_method("POST") |>
  req_body_json(
    list(
      table = "INDKF101",
      format = "CSV",
      lang = "da",
      variables = list(
        list(
          code = "OMRÅDE",
          values = as.list(municipality_codes)
        ),
        list(
          code = "ENHED",
          values = list("115")
        ),
        list(
          code = "BOLIGARTUD",
          values = list("TOT")
        ),
        list(
          code = "INDKOMSTTYPE",
          values = list("100")
        ),
        list(
          code = "Tid",
          values = list("2024")
        )
      )
    )
  )

income_2024 <- income_request |>
  req_perform() |>
  resp_body_string() |>
  read_csv2(show_col_types = FALSE) |>
  transmute(
    Municipality = OMRÅDE,
    mean_disposable_income = INDHOLD / 1000
  )


# ============================================================
# 5. MUNICIPAL AREA AND POPULATION DENSITY
# Statbank ARE207
# ============================================================

area_request <- request(
  "https://api.statbank.dk/v1/data"
) |>
  req_method("POST") |>
  req_body_json(
    list(
      table = "ARE207",
      format = "CSV",
      lang = "da",
      variables = list(
        list(
          code = "OMRÅDE",
          values = as.list(municipality_codes)
        ),
        list(
          code = "Tid",
          values = list("2024")
        )
      )
    )
  )

area_2024 <- area_request |>
  req_perform() |>
  resp_body_string() |>
  read_csv2(show_col_types = FALSE) |>
  transmute(
    Municipality = OMRÅDE,
    area_km2 = INDHOLD
  )


# ============================================================
# 6. ELECTRIC VEHICLES
# Statbank BIL54
# ============================================================

ev_request <- request(
  "https://api.statbank.dk/v1/data"
) |>
  req_method("POST") |>
  req_body_json(
    list(
      table = "BIL54",
      format = "CSV",
      lang = "da",
      variables = list(
        list(
          code = "OMRÅDE",
          values = as.list(municipality_codes)
        ),
        list(
          code = "BILTYPE",
          values = list("4000101002")
        ),
        list(
          code = "BRUG",
          values = list("1100")
        ),
        list(
          code = "DRIV",
          values = list("20200", "20225")
        ),
        list(
          code = "Tid",
          values = as.list(time_codes)
        )
      )
    )
  )

ev_data <- ev_request |>
  req_perform() |>
  resp_body_string() |>
  read_csv2(show_col_types = FALSE)


ev_2024 <- ev_data |>
  group_by(OMRÅDE, DRIV) |>
  summarise(
    mean_cars = mean(INDHOLD),
    .groups = "drop"
  ) |>
  pivot_wider(
    names_from = DRIV,
    values_from = mean_cars
  ) |>
  transmute(
    Municipality = OMRÅDE,
    ev_share = El / `Drivmidler i alt`
  )


# ============================================================
# 7. ELECTRIC HEATING
# Statbank BOL102
# Occupied ordinary dwellings
# ============================================================

anvendelse_codes <- c("125", "130", "140")


heating_request <- request(
  "https://api.statbank.dk/v1/data"
) |>
  req_method("POST") |>
  req_body_json(
    list(
      table = "BOL102",
      format = "CSV",
      lang = "da",
      variables = list(
        list(
          code = "AMT",
          values = as.list(municipality_codes)
        ),
        list(
          code = "BEBO",
          values = list("1000")
        ),
        list(
          code = "ANVENDELSE",
          values = as.list(anvendelse_codes)
        ),
        list(
          code = "OPVARMNING",
          values = list("*")
        ),
        list(
          code = "Tid",
          values = list("2024")
        )
      )
    )
  )

heating_data <- heating_request |>
  req_perform() |>
  resp_body_string() |>
  read_csv2(show_col_types = FALSE)


heating_2024 <- heating_data |>
  group_by(AMT) |>
  summarise(
    total_dwellings = sum(INDHOLD),
    unknown_heating =
      sum(INDHOLD * (OPVARMNING == "Uoplyst")),
    electric_heating =
      sum(
        INDHOLD *
          (OPVARMNING %in% c("Varmepumpe", "Elvarme"))
      ),
    .groups = "drop"
  ) |>
  mutate(
    known_heating =
      total_dwellings - unknown_heating,
    share_electric_heating =
      electric_heating / known_heating
  ) |>
  transmute(
    Municipality = AMT,
    share_electric_heating
  )


# ============================================================
# 8. HOLIDAY HOMES
# Statbank BOL102
# Holiday homes without registered permanent residents
# ============================================================

holiday_request <- request(
  "https://api.statbank.dk/v1/data"
) |>
  req_method("POST") |>
  req_body_json(
    list(
      table = "BOL102",
      format = "CSV",
      lang = "da",
      variables = list(
        list(
          code = "AMT",
          values = as.list(municipality_codes)
        ),
        list(
          code = "BEBO",
          values = list("5000")
        ),
        list(
          code = "ANVENDELSE",
          values = list("565")
        ),
        list(
          code = "Tid",
          values = list("2024")
        )
      )
    )
  )

holiday_2024 <- holiday_request |>
  req_perform() |>
  resp_body_string() |>
  read_csv2(show_col_types = FALSE) |>
  group_by(AMT) |>
  summarise(
    holiday_homes = sum(INDHOLD),
    .groups = "drop"
  ) |>
  rename(Municipality = AMT) |>
  left_join(
    municipality_key,
    by = "Municipality"
  ) |>
  select(
    MunicipalityCode,
    holiday_homes
  )


# ============================================================
# 9. BUILD FINAL MUNICIPALITY DATASET
# ============================================================

municipality_data <- population_2024 |>
  left_join(
    municipality_key,
    by = "Municipality"
  ) |>
  left_join(
    area_2024,
    by = "Municipality"
  ) |>
  left_join(
    income_2024,
    by = "Municipality"
  ) |>
  left_join(
    ev_2024,
    by = "Municipality"
  ) |>
  left_join(
    heating_2024,
    by = "Municipality"
  ) |>
  left_join(
    holiday_2024,
    by = "MunicipalityCode"
  ) |>
  left_join(
    electricity_2024,
    by = "MunicipalityCode"
  ) |>
  mutate(
    population_density =
      population / area_km2,
    
    log_population_density =
      log(population_density),
    
    holiday_homes_per_1000 =
      holiday_homes / population * 1000,
    
    kwh_per_capita =
      annual_consumption_kwh / population
  )


# ============================================================
# 10. DATA VALIDATION
# ============================================================

stopifnot(
  nrow(municipality_data) == 98,
  n_distinct(municipality_data$MunicipalityCode) == 98
)

model_variables <- municipality_data |>
  select(
    kwh_per_capita,
    share_65plus,
    mean_disposable_income,
    log_population_density,
    ev_share,
    share_electric_heating,
    holiday_homes_per_1000
  )

stopifnot(
  sum(is.na(model_variables)) == 0
)

# ------------------------------------------------------------
# Save final analysis dataset
# ------------------------------------------------------------

write_csv(
  municipality_data,
  "data/municipality_data.csv"
)

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