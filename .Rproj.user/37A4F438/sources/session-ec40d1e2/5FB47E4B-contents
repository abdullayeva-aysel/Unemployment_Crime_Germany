library(readr)
library(dplyr)
library(fixest)

df <- read_csv("~/Applied_Economics/Data/Panel/final_panel_dataset_common_crimes.csv", show_col_types=FALSE) |>
  mutate(district_key = as.factor(district_key)) |>
  filter(crime_code == "674000") |>
  filter(!is.na(crime_rate), crime_rate > 0) |>
  mutate(log_crime_rate = log(crime_rate)) |>
  arrange(district_key, year) |>
  group_by(district_key) |>
  mutate(unemp_lag1 = lag(unemployment_rate, 1)) |>
  ungroup() |>
  filter(complete.cases(log_crime_rate, unemp_lag1, gdp_per_capita, share_youth_18_29, share_males, share_males_within_youth, school_leavers_no_qual))

cat("\n=========================================\n")
cat("Crime Model: Property damage\n")
cat("=========================================\n")

mod1 <- feols(log_crime_rate ~ log(unemp_lag1) | district_key + year, data = df, cluster = ~ district_key)
mod2 <- feols(log_crime_rate ~ log(unemp_lag1) + gdp_per_capita | district_key + year, data = df, cluster = ~ district_key)
mod3 <- feols(log_crime_rate ~ log(unemp_lag1) + gdp_per_capita + share_youth_18_29 | district_key + year, data = df, cluster = ~ district_key)
mod4 <- feols(log_crime_rate ~ log(unemp_lag1) + gdp_per_capita + share_youth_18_29 + share_males | district_key + year, data = df, cluster = ~ district_key)
mod5 <- feols(log_crime_rate ~ log(unemp_lag1) + gdp_per_capita + share_youth_18_29 + share_males + share_males_within_youth + school_leavers_no_qual | district_key + year, data = df, cluster = ~ district_key)

print(etable(mod1, mod2, mod3, mod4, mod5, se="cluster", cluster="district_key"))
