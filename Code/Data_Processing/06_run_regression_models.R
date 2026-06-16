# ==========================================================
# Pipeline - Crime & Unemployment (Germany, Panel 2003-2022)
# Automatic Model Generation (Two-way FE regressions)
# ==========================================================

library(readr)
library(dplyr)
library(fixest)
library(gt)

# Requested analysis window (displayed in outputs)
analysis_year_start <- 2003L
analysis_year_end <- 2023L


if (dir.exists("Data") && dir.exists("Code")) {
  project_root <- "."
} else if (dir.exists("../../Data") && dir.exists("../../Code")) {
  project_root <- "../.."
} else {
  stop("Project root not found. Run from project root or Code/Data_Processing.")
}

# 1) Setup Outputs  ----------------------------------------
# Ensure directories exist
out_dir <- file.path(project_root, "Data", "Results", "Model_Tables")
if (!dir.exists(out_dir)) {
  dir.create(out_dir, recursive = TRUE)
}

# Clean prior HTML model outputs so renamed files don't leave stale duplicates
existing_html <- list.files(out_dir, pattern = "^model_.*\\.html$", full.names = TRUE)
if (length(existing_html) > 0) {
  file.remove(existing_html)
  message(sprintf("Removed %s previous HTML model table(s) from: %s", length(existing_html), out_dir))
}

# Clean legacy text outputs from older script versions to avoid confusion
legacy_out_dir <- file.path(project_root, "Data", "Results", "Models")
if (dir.exists(legacy_out_dir)) {
  legacy_txt <- list.files(legacy_out_dir, pattern = "\\.txt$", full.names = TRUE)
  if (length(legacy_txt) > 0) {
    file.remove(legacy_txt)
    message(sprintf("Removed %s legacy .txt model output(s) from: %s", length(legacy_txt), legacy_out_dir))
  }
}

# 2) Load final panel -------------------------
message("Loading Final Panel Dataset from Phase 1 Pipeline...")
panel <- read_csv(
  file.path(project_root, "Data", "Panel", "final_panel_dataset_common_crimes.csv"),
  show_col_types = FALSE
) %>%
  mutate(
    district_key = as.factor(district_key),
    crime_code = as.factor(crime_code),
    crime_type_en = case_when(
      as.character(crime_code) == "------" ~ "Total Recorded Crime",
      as.character(crime_code) == "220000" ~ "Bodily Injury (Total)",
      as.character(crime_code) == "222000" ~ "Aggravated / Serious Bodily Injury",
      as.character(crime_code) == "*50*00" ~ "Theft in/from Motor Vehicles",
      as.character(crime_code) == "435*00" ~ "Residential Burglary",
      as.character(crime_code) == "674000" ~ "Property Damage",
      as.character(crime_code) == "730000" ~ "Drug Offenses",
      as.character(crime_code) == "899000" ~ "Street Crime",
      TRUE ~ as.character(crime_code)
    ),
    year = as.integer(year)
  ) %>%
  filter(year >= analysis_year_start, year <= analysis_year_end)

# 3) Variable Setup & Lag generation -----------------------
# Create log dependent variable
panel <- panel %>%
  filter(!is.na(crime_rate)) %>%
  mutate(log_crime_rate = log(crime_rate + 1))

# Create 1-year lag of unemployment (within district-crime unit)
panel <- panel %>%
  arrange(district_key, crime_code, year) %>%
  group_by(district_key, crime_code) %>%
  mutate(unemp_lag1 = lag(unemployment_rate, 1)) %>%
  ungroup()

# 4) Enforce Uniform Sample across Models ------------------
# Define requirement for complete cases across all variables used
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

# 5) Generate Models & Out Files ---------------------------
crime_types <- unique(panel$crime_code)

save_etable_html <- function(models, out_file, title, subtitle = NULL) {
  tbl <- etable(
    models[[1]], models[[2]], models[[3]], models[[4]], models[[5]],
    signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10)
  )

  tbl_df <- as.data.frame(tbl, stringsAsFactors = FALSE)
  names(tbl_df)[1] <- "Label"
  names(tbl_df)[2:6] <- c(
    "Model 1: Baseline",
    "Model 2: + GDP",
    "Model 3: + Youth Share",
    "Model 4: + Male Share",
    "Model 5: Full Controls"
  )

  label_map <- c(
    "Dependent Var.:" = "Dependent Variable:",
    "log_crime_rate" = "Log Crime Rate",
    "log(unemp_lag1)" = "Log Unemployment Rate (t-1)",
    "gdp_per_capita" = "GDP per Capita",
    "share_youth_18_29" = "Share of Youth (18-29)",
    "share_males" = "Share of Males",
    "share_males_within_youth" = "Male Share Among Youth",
    "school_leavers_no_qual" = "Share of School Leavers (No Qualification)",
    "district_key" = "District Fixed Effects",
    "year" = "Year Fixed Effects"
  )

  for (j in seq_along(tbl_df)) {
    tbl_df[[j]] <- ifelse(tbl_df[[j]] %in% names(label_map), label_map[tbl_df[[j]]], tbl_df[[j]])
  }

  # Remove redundant etable metadata rows from the rendered HTML table.
  drop_rows <- tbl_df$Label %in% c("Fixed-Effects:", "S.E.: Clustered")
  separator_rows <- grepl("^_+$", trimws(tbl_df$Label))
  tbl_df <- tbl_df[!(drop_rows | separator_rows), , drop = FALSE]

  # Match preferred presentation in exported HTML tables.
  tbl_df <- tbl_df[tbl_df$Label != "R2", , drop = FALSE]

  fe_labels <- c("District Fixed Effects", "Year Fixed Effects")
  fe_rows <- tbl_df[tbl_df$Label %in% fe_labels, , drop = FALSE]
  tbl_df <- tbl_df[!(tbl_df$Label %in% fe_labels), , drop = FALSE]

  within_idx <- match("Within R2", tbl_df$Label)
  if (!is.na(within_idx) && nrow(fe_rows) > 0) {
    before_rows <- tbl_df[seq_len(within_idx), , drop = FALSE]
    after_rows <- if (within_idx < nrow(tbl_df)) tbl_df[(within_idx + 1):nrow(tbl_df), , drop = FALSE] else tbl_df[0, , drop = FALSE]
    tbl_df <- rbind(before_rows, fe_rows, after_rows)
  }

  obs_idx <- match("Observations", tbl_df$Label)
  if (!is.na(obs_idx)) {
    spacer <- as.data.frame(as.list(rep(" ", ncol(tbl_df))), stringsAsFactors = FALSE)
    names(spacer) <- names(tbl_df)
    before_obs <- if (obs_idx > 1) tbl_df[seq_len(obs_idx - 1), , drop = FALSE] else tbl_df[0, , drop = FALSE]
    from_obs <- tbl_df[obs_idx:nrow(tbl_df), , drop = FALSE]
    tbl_df <- rbind(before_obs, spacer, from_obs)
  }

  gt_tbl <- gt(tbl_df) %>%
    tab_header(
      title = md(paste0("**", title, "**")),
      subtitle = subtitle
    ) %>%
    tab_options(
      table.font.size = px(13),
      data_row.padding = px(3),
      heading.align = "left"
    ) %>%
    cols_label(Label = "Variable / Statistic") %>%
    opt_row_striping() %>%
    tab_source_note(md("*p<0.1, **p<0.05, ***p<0.01")) %>%
    tab_source_note(md("Clustered standard errors at district level."))

  gtsave(gt_tbl, out_file)
}

message("==== COMMENCING ITERATIVE MODELING PHASE ====")
for (ctype in crime_types) {
  cname <- unique(panel$crime_type_en[panel$crime_code == ctype])[1]
  cname_clean <- gsub("[^A-Za-z0-9]", "_", tolower(cname))
  
  # Filter sample to common cases for this crime
  panel_sub <- panel %>%
    filter(crime_code == ctype) %>%
    filter(if_all(all_of(vars_full), ~ !is.na(.)))
  
  if (nrow(panel_sub) == 0) {
    message(sprintf("- SKIPPING: %s (Insufficient data)", cname))
    next
  }
  
  message(sprintf("+ Generating models for: %s", cname))
  
  # Regressions
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
  
  # Export academic-style HTML table (gt)
  out_file <- file.path(out_dir, paste0("model_", cname_clean, ".html"))
  save_etable_html(
    models = list(mod1, mod2, mod3, mod4, mod5),
    out_file = out_file,
    title = paste0(
      "Fixed Effects Panel Regression Results: ",
      cname,
      " (",
      analysis_year_start,
      "-",
      analysis_year_end,
      ")"
    ),
    subtitle = NULL
  )
}

message("==== MODEL GENERATION COMPLETE ====")
message(sprintf("HTML tables successfully exported to: %s", out_dir))
