# ==========================================================
# Crime & Unemployment (Germany, Panel 2003-2022) - Per Crime Type
# ==========================================================

library(readr)
library(dplyr)
library(fixest)

# 1) Load final panel --------------------------------------
panel <- read_csv(
  "Data/Panel/final_panel_dataset_common_crimes.csv",
  show_col_types = FALSE
) %>%
  mutate(
    district_key = as.factor(district_key),
    crime_code = as.factor(crime_code),
    crime_type_en = case_when(
      as.character(crime_code) == "------" ~ "Total recorded crime",
      as.character(crime_code) == "*50*00" ~ "Theft from/out of motor vehicles",
      as.character(crime_code) == "435*00" ~ "Residential burglary",
      as.character(crime_code) == "674000" ~ "Property damage",
      as.character(crime_code) == "730000" ~ "Drug offenses",
      as.character(crime_code) == "899000" ~ "Street crime",
      TRUE ~ as.character(crime_code)
    ),
    year = as.integer(year)
  ) %>%
  filter(year %in% 2003:2024)

# 2) Create log dependent variable -------------------------
panel <- panel %>%
  filter(!is.na(crime_rate)) %>%
  mutate(log_crime_rate = log(crime_rate + 1))

# 3) Create 1-year lag of unemployment (within district-crime unit)
panel <- panel %>%
  arrange(district_key, crime_code, year) %>%
  group_by(district_key, crime_code) %>%
  mutate(unemp_lag1 = lag(unemployment_rate, 1)) %>%
  ungroup()

# 4) Vars to check for complete cases ----------------------
vars_full <- c(
  "log_crime_rate",
  "unemp_lag1",
  "gdp_per_capita",
  "share_youth_18_29",
  "share_males",
  "share_males_within_youth",
  "school_leavers_no_qual",
  "district_key",
  "year"
)

out_dir <- "Code/Analysis/Models_Per_Crime"
if (!dir.exists(out_dir)) {
  dir.create(out_dir, recursive = TRUE)
}

crime_types <- unique(panel$crime_code)

for (ctype in crime_types) {
  cname <- unique(panel$crime_type_en[panel$crime_code == ctype])[1]
  cname_clean <- gsub("[^A-Za-z0-9]", "_", tolower(cname))

  panel_sub <- panel %>%
    filter(crime_code == ctype) %>%
    filter(if_all(all_of(vars_full), ~ !is.na(.)))

  if (nrow(panel_sub) == 0) {
    cat("Skipping", cname, "- no complete cases.\n")
    next
  }

  mod1 <- feols(
    log_crime_rate ~ log(unemp_lag1) | district_key + year,
    data = panel_sub,
    cluster = ~district_key
  )

  mod2 <- feols(
    log_crime_rate ~ log(unemp_lag1) + gdp_per_capita | district_key + year,
    data = panel_sub,
    cluster = ~district_key
  )

  mod3 <- feols(
    log_crime_rate ~ log(unemp_lag1) +
      gdp_per_capita +
      share_youth_18_29 |
      district_key + year,
    data = panel_sub,
    cluster = ~district_key
  )

  mod4 <- feols(
    log_crime_rate ~ log(unemp_lag1) +
      gdp_per_capita +
      share_youth_18_29 +
      share_males |
      district_key + year,
    data = panel_sub,
    cluster = ~district_key
  )

  mod5 <- feols(
    log_crime_rate ~ log(unemp_lag1) +
      gdp_per_capita +
      share_youth_18_29 +
      share_males +
      share_males_within_youth +
      school_leavers_no_qual |
      district_key + year,
    data = panel_sub,
    cluster = ~district_key
  )

  out_file <- file.path(out_dir, paste0("model_", cname_clean, ".txt"))

  sink(out_file)
  cat("=========================================\n")
  cat("Summary for Crime Code:", as.character(ctype), "(", cname, ")\n")
  cat("=========================================\n")
  cat("N in common sample:", nrow(panel_sub), "\n")
  cat("Number of districts:", n_distinct(panel_sub$district_key), "\n")
  cat("Year range:", min(panel_sub$year), "-", max(panel_sub$year), "\n\n")

  print(etable(
    mod1,
    mod2,
    mod3,
    mod4,
    mod5,
    se = "cluster",
    cluster = "district_key",
    signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
    notes = "* p < 0.1, ** p < 0.05, *** p < 0.01"
  ))
  sink()

  cat("Written summary to", out_file, "\n")

  # Create a standalone runner script per crime type
  script_file <- file.path(out_dir, paste0("run_", cname_clean, ".R"))
  script_code <- sprintf(
    '
library(readr)
library(dplyr)
library(fixest)

df <- read_csv("../../Data/Panel/final_panel_dataset_common_crimes.csv", show_col_types=FALSE) |>
  mutate(district_key = as.factor(district_key)) |>
  filter(crime_code == "%s") |>
  filter(!is.na(crime_rate), crime_rate > 0) |>
  mutate(log_crime_rate = log(crime_rate)) |>
  arrange(district_key, year) |>
  group_by(district_key) |>
  mutate(unemp_lag1 = lag(unemployment_rate, 1)) |>
  ungroup() |>
  filter(complete.cases(log_crime_rate, unemp_lag1, gdp_per_capita, share_youth_18_29, share_males, share_males_within_youth, school_leavers_no_qual))

cat("\\n=========================================\\n")
cat("Crime Model: %s\\n")
cat("=========================================\\n")

mod1 <- feols(log_crime_rate ~ log(unemp_lag1) | district_key + year, data = df, cluster = ~ district_key)
mod2 <- feols(log_crime_rate ~ log(unemp_lag1) + gdp_per_capita | district_key + year, data = df, cluster = ~ district_key)
mod3 <- feols(log_crime_rate ~ log(unemp_lag1) + gdp_per_capita + share_youth_18_29 | district_key + year, data = df, cluster = ~ district_key)
mod4 <- feols(log_crime_rate ~ log(unemp_lag1) + gdp_per_capita + share_youth_18_29 + share_males | district_key + year, data = df, cluster = ~ district_key)
mod5 <- feols(log_crime_rate ~ log(unemp_lag1) + gdp_per_capita + share_youth_18_29 + share_males + share_males_within_youth + school_leavers_no_qual | district_key + year, data = df, cluster = ~ district_key)

print(etable(mod1, mod2, mod3, mod4, mod5, se="cluster", cluster="district_key"))
  ',
    as.character(ctype),
    cname
  )

  writeLines(trimws(script_code), script_file)
}
