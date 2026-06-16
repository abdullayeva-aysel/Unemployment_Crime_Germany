library(readxl)
library(tidyverse)
library(stringr)

# -------------------------------------------------
# 1. SETUP PATHS
# -------------------------------------------------

cat("Current working directory:\n  ", getwd(), "\n\n")

# Logic to find Data folder (similar to other scripts)
candidate_data_dirs <- c(
  file.path("Data", "Raw", "INKAR"), # run from project root
  file.path("..", "..", "Data", "Raw", "INKAR") # run from Code/Data_Processing
)

inkar_dir <- NULL
for (d in candidate_data_dirs) {
  if (dir.exists(d)) {
    inkar_dir <- d
    break
  }
}

if (is.null(inkar_dir)) {
  stop("Could not find INKAR input directory.")
}

output_dir <- normalizePath(
  file.path(inkar_dir, "..", "..", "Processed", "INKAR_Processed"),
  winslash = "/"
)
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

cat("Using INKAR dir: ", normalizePath(inkar_dir, winslash = "/"), "\n")
cat("Output dir:      ", output_dir, "\n\n")


# -------------------------------------------------
# 2. HELPER FUNCTION
# -------------------------------------------------

read_inkar <- function(filename, var_name, col_pattern = NULL) {
  fpath <- file.path(inkar_dir, filename)
  if (!file.exists(fpath)) {
    warning(paste("File not found:", filename))
    return(NULL)
  }
  
  cat(paste("Processing:", filename, "->", var_name, "\n"))
  
  # Read first 10 rows to inspect structure
  raw_head <- suppressMessages(read_excel(
    fpath,
    col_names = FALSE,
    n_max = 10,
    .name_repair = "minimal"
  ))
  head_mat <- as.matrix(raw_head)
  head_mat[is.na(head_mat)] <- ""
  
  header_row_idx <- -1
  for (i in 1:nrow(head_mat)) {
    if (any(str_detect(head_mat[i, ], "Kennziffer"))) {
      header_row_idx <- i
      break
    }
  }
  
  if (header_row_idx == -1) {
    for (i in 1:nrow(head_mat)) {
      if (any(str_detect(head_mat[i, ], "Raumeinheit"))) {
        header_row_idx <- i
        break
      }
    }
  }
  
  if (header_row_idx == -1) {
    warning(paste("Could not identify header row in", filename))
    return(NULL)
  }
  
  # Year detection
  current_row_vals <- head_mat[header_row_idx, ]
  next_row_vals <- if (header_row_idx < nrow(head_mat)) {
    head_mat[header_row_idx + 1, ]
  } else {
    c()
  }
  
  has_years_current <- any(str_detect(current_row_vals, "^\\d{4}$"))
  has_years_next <- any(str_detect(next_row_vals, "^\\d{4}$"))
  
  year_row_idx <- -1
  if (has_years_current) {
    year_row_idx <- header_row_idx
  } else if (has_years_next) {
    year_row_idx <- header_row_idx + 1
  } else {
    warning(paste("No year columns found for", filename))
    return(NULL)
  }
  
  raw_data <- suppressMessages(read_excel(
    fpath,
    col_names = FALSE,
    .name_repair = "minimal"
  ))
  
  key_col_idx <- which(str_detect(
    as.character(raw_data[header_row_idx, ]),
    "Kennziffer"
  ))[1]
  if (is.na(key_col_idx)) {
    key_col_idx <- which(str_detect(
      as.character(raw_data[header_row_idx, ]),
      "Raumeinheit"
    ))[1] -
      1
    if (is.na(key_col_idx) || key_col_idx < 1) key_col_idx <- 1
  }
  
  # HEADER ROW filtering (based on col_pattern)
  col_indices_to_keep <- c(key_col_idx) # Always keep Key
  
  year_vals <- as.character(raw_data[year_row_idx, ])
  header_vals <- as.character(raw_data[header_row_idx, ]) # To check pattern
  
  for (j in 1:ncol(raw_data)) {
    val_year <- year_vals[j]
    val_head <- header_vals[j]
    
    # Check if it is a year column
    if (!is.na(val_year) && str_detect(val_year, "^\\d{4}$")) {
      # If pattern provided, check header
      if (!is.null(col_pattern)) {
        if (str_detect(val_head, col_pattern)) {
          col_indices_to_keep <- c(col_indices_to_keep, j)
        }
      } else {
        # No pattern, keep all year columns
        col_indices_to_keep <- c(col_indices_to_keep, j)
      }
    }
  }
  
  selected_data <- raw_data[
    (year_row_idx + 1):nrow(raw_data),
    col_indices_to_keep,
    drop = FALSE
  ]
  
  # Helper to get year names for the selected columns
  selected_years <- year_vals[col_indices_to_keep[-1]] # First is Key
  
  # Handle duplicates (e.g. if pattern matches multiple cols for same year - implying sum needed)
  unique_year_names <- make.unique(selected_years, sep = "___")
  
  colnames(selected_data) <- c("Kreisschluessel", unique_year_names)
  
  selected_data <- selected_data |>
    filter(!is.na(Kreisschluessel)) |>
    pivot_longer(
      cols = -Kreisschluessel,
      names_to = "Year_Raw",
      values_to = "Value_Raw"
    ) |>
    mutate(
      Kreisschluessel = str_pad(
        str_remove(Kreisschluessel, "\\s"),
        width = 5,
        pad = "0"
      ),
      Year = as.integer(str_extract(Year_Raw, "^\\d{4}")),
      Value_Clean = str_replace(Value_Raw, ",", "."),
      Value_Clean = str_remove_all(Value_Clean, "[^0-9\\.-]"),
      Value = suppressWarnings(as.numeric(Value_Clean))
    ) |>
    filter(!is.na(Year)) |>
    filter(!is.na(Value)) |>
    group_by(Kreisschluessel, Year) |>
    summarise(Value = sum(Value, na.rm = TRUE), .groups = "drop") |>
    select(Kreisschluessel, Year, !!var_name := Value) |>
    distinct() |>
    filter(str_length(Kreisschluessel) == 5)
  
  return(selected_data)
}

# -------------------------------------------------
# 3. AUTOMATION LOGIC
# -------------------------------------------------

# Helper to generate clean variable names from filenames
slugify_filename <- function(x) {
  x <- str_remove(x, "\\.xlsx?$")
  x <- str_to_lower(x)
  # Standardize diacritics: handle both NFC and NFD (common on Mac)
  x <- str_replace_all(x, "ö|ö", "o")
  x <- str_replace_all(x, "ä|ä", "a")
  x <- str_replace_all(x, "ü|ü", "u")
  x <- str_replace_all(x, "ß", "ss")
  x <- str_replace_all(x, "[^a-z0-9]", "_")
  x <- str_replace_all(x, "_+", "_")
  x <- str_remove_all(x, "^_|(_$)")
  return(x)
}

# Configuration for multi-variable files or overrides
# Using slugified filenames as keys for robustness
file_configs <- list(
  "einwohner_von_18_bis_unter_30_jahren_frauen" = list(
    list(var = "share_female_18_25", pattern = "18 bis unter 25"),
    list(var = "share_female_25_30", pattern = "25 bis unter 30")
  ),
  "arbeitslosenquote"            = list(list(var = "unemployment_rate")),
  "gdp_per_capita"               = list(list(var = "gdp_per_capita")),
  "bevolkerung_gesamt"          = list(list(var = "pop_total")),
  "bevolkerung_mannlich"       = list(list(var = "pop_male")),
  "schulabganger_ohne_abschluss" = list(list(var = "school_leavers_no_qual_raw")),
  "einwohner_von_18_bis_unter_25_jahren" = list(list(var = "share_18_25")),
  "einwohner_von_25_bis_unter_30_jahren" = list(list(var = "share_25_30"))
)

# Automation: This loop will automatically process ANY .xls or .xlsx file added to the folder.
# Files NOT in the 'file_configs' above will be generically named using the slugified filename.
all_xls_files <- list.files(inkar_dir, pattern = "\\.xlsx?$", ignore.case = TRUE)
cat("Found", length(all_xls_files), "Excel files in", inkar_dir, "\n\n")

panel_data <- NULL

for (fname in all_xls_files) {
  fslug <- slugify_filename(fname)
  configs <- file_configs[[fslug]]
  
  if (is.null(configs)) {
    cat("Note: Generic slug for", fname, "is", fslug, "\n")
    configs <- list(list(var = fslug, pattern = NULL))
  }
  
  for (cfg in configs) {
    df <- read_inkar(fname, cfg$var, cfg$pattern)
    if (!is.null(df)) {
      if (is.null(panel_data)) {
        panel_data <- df
      } else {
        panel_data <- full_join(panel_data, df, by = c("Kreisschluessel", "Year"))
      }
    }
  }
}

# -------------------------------------------------
# 4. CALCULATE DERIVED VARIABLES
# -------------------------------------------------

cat("Calculating derived variables...\n")

# Check if required columns exist before deriving
needed_for_derivation <- c("pop_male", "pop_total", "share_18_25", "share_25_30", 
                           "share_female_18_25", "share_female_25_30")
can_derive <- all(needed_for_derivation %in% colnames(panel_data))

final_panel <- panel_data

if (can_derive) {
  final_panel <- final_panel |>
    mutate(
      # Share of Males (Simple)
      share_males = pop_male / pop_total * 100,
      
      # Estimate Absolute Counts from Shares
      pop_18_25_est = pop_total * (share_18_25 / 100),
      pop_25_30_est = pop_total * (share_25_30 / 100),
      
      # Females count
      pop_fem_18_25_est = pop_18_25_est * (share_female_18_25 / 100),
      pop_fem_25_30_est = pop_25_30_est * (share_female_25_30 / 100),
      
      # Aggregates
      youth_18_29_total = pop_18_25_est + pop_25_30_est,
      youth_18_29_female = pop_fem_18_25_est + pop_fem_25_30_est,
      youth_18_29_male   = youth_18_29_total - youth_18_29_female,
      
      # Final Target Variables
      share_youth_18_29 = youth_18_29_total / pop_total * 100,
      share_males_within_youth = youth_18_29_male / youth_18_29_total * 100
    )
} else {
  missing <- needed_for_derivation[!needed_for_derivation %in% colnames(panel_data)]
  cat("Warning: Skipping derived variables. Missing:", paste(missing, collapse=", "), "\n")
}

# Standardize column naming if they were raw
if ("school_leavers_no_qual_raw" %in% colnames(final_panel)) {
  final_panel <- final_panel |> 
    mutate(school_leavers_no_qual = school_leavers_no_qual_raw) |>
    select(-school_leavers_no_qual_raw)
}

final_panel <- final_panel |>
  arrange(Kreisschluessel, Year)

# -------------------------------------------------
# 5. SAVE
# -------------------------------------------------

output_file <- file.path(output_dir, "inkar_panel.csv")
write_csv(final_panel, output_file)

cat("Saved INKAR Panel to:", output_file, "\n")
cat("Rows:", nrow(final_panel), "\n")
cat("Columns:", ncol(final_panel), "\n")
