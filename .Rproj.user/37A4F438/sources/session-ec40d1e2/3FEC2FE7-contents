
library(tidyverse)
library(pdftools)

# Suppress R CMD check NOTEs for NSE (dplyr column references)
utils::globalVariables(c(
  "x",
  "Aufklaerungsquote",
  "Kreis",
  "Kreisschluessel",
  "Straftat"
))

# Directory paths
candidate_input_dirs <- c(
  file.path("Data", "Raw", "PKS"),
  file.path("..", "Data", "Raw", "PKS"),
  file.path("..", "..", "Data", "Raw", "PKS")
)

input_dir <- NULL
for (p in candidate_input_dirs) {
  if (dir.exists(p)) {
    input_dir <- p
    break
  }
}

if (is.null(input_dir)) {
  stop(paste("Could not find PKS input directory.\nTried:", paste(candidate_input_dirs, collapse = " | ")))
}

data_root <- normalizePath(file.path(input_dir, "..", ".."), winslash = "/")
output_dir <- file.path(data_root, "Processed", "PKS_Processed", "01-converted")

# Ensure output directory exists
if (!dir.exists(output_dir)) {
  fs::dir_create(output_dir, recurse = TRUE)
}

# Get list of PDF files
pdf_files <- list.files(input_dir, pattern = "\\.pdf$", full.names = TRUE)

###########################################################################
# Helper Functions
###########################################################################

# Function to determine separator (reused from extract_table_v3.R)
get_separator <- function(text1, text2) {
  char1 <- stringr::str_sub(text1, -1)
  char2 <- stringr::str_sub(text2, 1, 1)

  is_digit1 <- stringr::str_detect(char1, "[0-9]")
  is_digit2 <- stringr::str_detect(char2, "[0-9]")

  is_lower1 <- stringr::str_detect(char1, "[a-zäöüß]")
  is_lower2 <- stringr::str_detect(char2, "[a-zäöüß]")

  if (is_digit1 && is_digit2) {
    ""
  } else if (is_lower1 && is_lower2) {
    ""
  } else {
    " "
  }
}

# Function to merge text elements based on gap (reused from extract_table_v3.R)
merge_row <- function(row_data) {
  row_data <- row_data |> dplyr::arrange(x)
  if (nrow(row_data) == 0) {
    return(NULL)
  }

  merged_texts <- list()
  current_text <- row_data$text[1]
  current_end <- row_data$x[1] + row_data$width[1]

  if (nrow(row_data) > 1) {
    for (i in 2:nrow(row_data)) {
      gap <- row_data$x[i] - current_end

      if (gap <= 3 && row_data$text[i] != "-" && current_text != "-") {
        sep <- get_separator(current_text, row_data$text[i])
        current_text <- paste0(current_text, sep, row_data$text[i])
        current_end <- row_data$x[i] + row_data$width[i]
      } else {
        merged_texts[[length(merged_texts) + 1]] <- current_text
        current_text <- row_data$text[i]
        current_end <- row_data$x[i] + row_data$width[i]
      }
    }
  }
  merged_texts[[length(merged_texts) + 1]] <- current_text
  unlist(merged_texts)
}

# Function to determine Kreis Type
get_kreis_type <- function(kreis_name) {
  if (stringr::str_detect(kreis_name, "^SK ")) {
    "SK"
  } else if (stringr::str_detect(kreis_name, "^KfS ")) {
    "KfS"
  } else if (stringr::str_detect(kreis_name, "^LK ")) {
    "LK"
  } else if (stringr::str_detect(kreis_name, "^K ")) {
    "K"
  } else if (stringr::str_detect(kreis_name, "^Region ")) {
    "Region"
  } else if (stringr::str_detect(kreis_name, "^RV ")) {
    "RV"
  } else if (stringr::str_detect(kreis_name, "\\(LK\\)")) {
    "LK"
  } else if (stringr::str_detect(kreis_name, "\\(SK\\)")) {
    "SK"
  } else {
    "Unknown"
  }
}

# Function to clean Kreis Name (remove prefix)
clean_kreis_name <- function(name, type) {
  name <- stringr::str_remove(name, "\\(LK\\)\\s*")
  name <- stringr::str_remove(name, "\\(SK\\)\\s*")

  if (type == "SK") {
    stringr::str_remove(name, "^SK\\s+")
  } else if (type == "KfS") {
    stringr::str_remove(name, "^KfS\\s+")
  } else if (type == "LK") {
    stringr::str_remove(name, "^LK\\s+")
  } else if (type == "K") {
    stringr::str_remove(name, "^K\\s+")
  } else if (type == "Region") {
    stringr::str_remove(name, "^Region\\s+")
  } else if (type == "RV") {
    stringr::str_remove(name, "^RV\\s+")
  } else {
    name
  }
}

###########################################################################
# Main Processing Loop
###########################################################################

message("\n===== PROCESSING PDF FILES (2003–2012) =====")

for (pdf_file in pdf_files) {
  filename <- basename(pdf_file)
  year <- stringr::str_extract(filename, "\\d{4}")

  if (is.na(year)) {
    message("× Could not extract year from: ", filename, " - Skipping.")
    next
  }

  message("→ Processing: ", filename, " (Year: ", year, ")")
  if (year == "2009") {
    message(
      "  ℹ Note: PDF annotation errors for 2009 are expected and benign. Data extraction is not affected."
    )
  }

  # Read PDF text to find pages with "kreis-"
  pdf_content <- pdftools::pdf_text(pdf_file)
  pages_with_kreis <- which(stringr::str_detect(
    stringr::str_to_lower(pdf_content),
    "kreis-"
  ))

  if (length(pages_with_kreis) == 0) {
    message("  × No pages with 'kreis-' found.")
    next
  }

  # Get detailed data for all pages
  all_pages_data <- pdftools::pdf_data(pdf_file)

  year_data_list <- list()
  last_y <- -100 # Initialize with a value that won't trigger merge for the first row

  for (page_num in pages_with_kreis) {
    data <- all_pages_data[[page_num]]

    # Group by Y (Row detection)
    y_values <- sort(unique(data$y))
    if (length(y_values) == 0) {
      next
    }

    row_groups <- list()
    current_group <- c(y_values[1])

    if (length(y_values) > 1) {
      for (i in 2:length(y_values)) {
        if (y_values[i] - tail(current_group, 1) < 5) {
          current_group <- c(current_group, y_values[i])
        } else {
          row_groups[[length(row_groups) + 1]] <- current_group
          current_group <- c(y_values[i])
        }
      }
    }
    row_groups[[length(row_groups) + 1]] <- current_group

    for (group in row_groups) {
      row_data <- data |> dplyr::filter(y %in% group)
      merged <- merge_row(row_data)

      if (length(merged) == 0) {
        next
      }

      # Filter for data rows: Must start with digits (Key)
      if (!stringr::str_detect(merged[1], "^\\d")) {
        # Check for multi-line Kreis name (continuation)
        current_y <- row_data$y[1]

        if (
          length(year_data_list) > 0 &&
            length(merged) == 1 &&
            !stringr::str_detect(merged[1], "\\d")
        ) {
          # Exclude footer text
          if (
            stringr::str_detect(merged[1], "_{3,}") ||
              stringr::str_detect(merged[1], "Kriminalitätsbetrachtung") ||
              stringr::str_detect(merged[1], "^Einwohner")
          ) {
            next
          }

          if (current_y - last_y < 15) {
            last_idx <- length(year_data_list)
            last_entry <- year_data_list[[last_idx]]
            current_name <- last_entry[3]
            append_text <- merged[1]

            if (stringr::str_ends(current_name, "-")) {
              new_name <- paste0(current_name, append_text)
            } else {
              new_name <- paste(current_name, append_text)
            }

            year_data_list[[last_idx]][3] <- new_name
          }
        }
        next
      }

      # Update last_y for valid rows
      last_y <- row_data$y[1]

      # Handle Index column (specific to 2003/2004)
      if (
        length(merged) > 2 &&
          stringr::str_detect(merged[1], "^\\d+$") &&
          stringr::str_length(merged[1]) <= 3 &&
          stringr::str_detect(merged[2], "^\\d+$") &&
          stringr::str_length(merged[2]) >= 4
      ) {
        merged <- merged[-1]
      }

      # Find numbers start index
      first_num_idx <- -1
      for (i in 2:length(merged)) {
        if (is.na(merged[i])) {
          next
        }
        if (
          stringr::str_detect(merged[i], "[0-9]") &&
            !stringr::str_detect(merged[i], "[a-zA-Z]")
        ) {
          first_num_idx <- i
          break
        }
      }

      if (first_num_idx == -1) {
        next
      }

      key <- stringr::str_remove_all(merged[1], " ")
      name_parts <- merged[2:(first_num_idx - 1)]
      name <- paste(name_parts, collapse = " ")
      numbers <- merged[first_num_idx:length(merged)]

      # Enforce 15 number columns
      if (length(numbers) > 15) {
        numbers <- numbers[1:15]
      }
      if (length(numbers) < 15) {
        next
      }

      # Clean numbers
      numbers <- stringr::str_remove_all(numbers, " ")

      # Determine Kreis Type and Clean Name
      kreis_type <- get_kreis_type(name)
      clean_name <- clean_kreis_name(name, kreis_type)

      # Add to list
      year_data_list[[length(year_data_list) + 1]] <- c(
        key,
        kreis_type,
        clean_name,
        numbers
      )
    }
  }

  if (length(year_data_list) > 0) {
    # Build Data Frame
    col_names <- c(
      "Schluessel",
      "Kreis_Type",
      "Kreis",
      "Einwohner",
      "Straftaten_HZ",
      "Straftaten_AQ",
      "Koerperverletzung_HZ",
      "Koerperverletzung_AQ",
      "Wohnungseinbruch_HZ",
      "Wohnungseinbruch_AQ",
      "DiebstahlKfz_HZ",
      "DiebstahlKfz_AQ",
      "Sachbeschaedigung_HZ",
      "Sachbeschaedigung_AQ",
      "Rauschgift_HZ",
      "Rauschgift_AQ",
      "Strassenkriminalitaet_HZ",
      "Strassenkriminalitaet_AQ"
    )

    df_clean <- do.call(rbind, year_data_list) |>
      as.data.frame(stringsAsFactors = FALSE)

    if (ncol(df_clean) == length(col_names)) {
      colnames(df_clean) <- col_names

      # Specific corrections for 2008 (Sachsen-Anhalt)
      if (year == "2008") {
        # SK corrections
        df_clean$Kreis_Type[df_clean$Schluessel == "15001"] <- "SK"
        df_clean$Kreis[df_clean$Schluessel == "15001"] <- "Dessau-Roßlau"
        df_clean$Kreis_Type[df_clean$Schluessel == "15002"] <- "SK"
        df_clean$Kreis[df_clean$Schluessel == "15002"] <- "Halle (Saale)"
        df_clean$Kreis_Type[df_clean$Schluessel == "15003"] <- "SK"
        df_clean$Kreis[df_clean$Schluessel == "15003"] <- "Magdeburg"

        # LK corrections
        df_clean$Kreis_Type[df_clean$Schluessel == "15081"] <- "LK"
        df_clean$Kreis[
          df_clean$Schluessel == "15081"
        ] <- "Altmarkkreis Salzwedel"
        df_clean$Kreis_Type[df_clean$Schluessel == "15082"] <- "LK"
        df_clean$Kreis[df_clean$Schluessel == "15082"] <- "Anhalt-Bitterfeld"
        df_clean$Kreis_Type[df_clean$Schluessel == "15083"] <- "LK"
        df_clean$Kreis[df_clean$Schluessel == "15083"] <- "Bördekreis"
        df_clean$Kreis_Type[df_clean$Schluessel == "15084"] <- "LK"
        df_clean$Kreis[df_clean$Schluessel == "15084"] <- "Burgenlandkreis"
        df_clean$Kreis_Type[df_clean$Schluessel == "15085"] <- "LK"
        df_clean$Kreis[df_clean$Schluessel == "15085"] <- "Harz"
        df_clean$Kreis_Type[df_clean$Schluessel == "15086"] <- "LK"
        df_clean$Kreis[df_clean$Schluessel == "15086"] <- "Jerichower Land"
        df_clean$Kreis_Type[df_clean$Schluessel == "15087"] <- "LK"
        df_clean$Kreis[df_clean$Schluessel == "15087"] <- "Mansfeld-Südharz"
        df_clean$Kreis_Type[df_clean$Schluessel == "15088"] <- "LK"
        df_clean$Kreis[df_clean$Schluessel == "15088"] <- "Saalekreis"
        df_clean$Kreis_Type[df_clean$Schluessel == "15089"] <- "LK"
        df_clean$Kreis[df_clean$Schluessel == "15089"] <- "Salzlandkreis"
        df_clean$Kreis_Type[df_clean$Schluessel == "15090"] <- "LK"
        df_clean$Kreis[df_clean$Schluessel == "15090"] <- "Stendal"
        df_clean$Kreis_Type[df_clean$Schluessel == "15091"] <- "LK"
        df_clean$Kreis[df_clean$Schluessel == "15091"] <- "Wittenberg"
      }

      # 5. Process and Standardize Data (Long Format)

      # Clean numeric columns 
      clean_numeric <- function(x) {
        # Replace "-" (missing data marker in PDFs) with NA
        x <- ifelse(stringr::str_trim(x) == "-", NA_character_, x)
        x <- stringr::str_remove_all(x, "\\.")
        x <- stringr::str_replace_all(x, ",", ".")
        suppressWarnings(as.numeric(x))
      }

      # Convert basic columns
      df_final <- df_clean |>
        mutate(
          Kreisschluessel = stringr::str_pad(
            as.character(Schluessel),
            5,
            pad = "0"
          ),
          Einwohner = clean_numeric(Einwohner)
        ) |>
        rename(Kreisart = Kreis_Type) |>
        filter(!is.na(Kreisschluessel))

      # Pivot to Long Format (Harmonized Schema)
      df_long <- df_final |>
        tidyr::pivot_longer(
          cols = matches("_(HZ|AQ)$"),
          names_to = c("Straftat_Typ", ".value"),
          names_pattern = "(.*)_(HZ|AQ)"
        ) |>
        rename(
          Straftaten_HZ = HZ,
          Aufklaerungsquote = AQ
        ) |>
        mutate(
          Straftaten_HZ = clean_numeric(Straftaten_HZ),
          Aufklaerungsquote = clean_numeric(Aufklaerungsquote)
        )

      # Map Crime Types to Official Keys (Schluesselzahl)
      df_long <- df_long |>
        mutate(
          Schluesselzahl = dplyr::case_when(
            Straftat_Typ == "Straftaten" ~ "------",
            Straftat_Typ == "Koerperverletzung" ~ "220000",
            Straftat_Typ == "Wohnungseinbruch" ~ "435*00",
            Straftat_Typ == "DiebstahlKfz" ~ "*50*00",
            Straftat_Typ == "Sachbeschaedigung" ~ "674000",
            Straftat_Typ == "Rauschgift" ~ "730000",
            Straftat_Typ == "Strassenkriminalitaet" ~ "899000",
            TRUE ~ "LEGACY_UNK"
          ),
          Straftat = case_when(
            Straftat_Typ == "Straftaten" ~ "Straftaten insgesamt",
            TRUE ~ Straftat_Typ
          )
        )

      # Calculate Absolute Counts (Cases) from Rates
      df_long <- df_long |>
        mutate(
          Erfasste_Faelle = round((Straftaten_HZ * Einwohner) / 100000),
          Aufklaerung_Faelle = round(
            Erfasste_Faelle * (Aufklaerungsquote / 100)
          )
        )

      # Add missing columns with NA to match modern schema
      df_long <- df_long |>
        mutate(
          Versuche_Anzahl = NA_real_,
          Versuche_Anteil_Prozent = NA_real_,
          Mit_Schusswaffe_gedroht = NA_real_,
          Mit_Schusswaffe_geschossen = NA_real_,
          Tatverdaechtige_Insgesamt = NA_real_,
          Tatverdaechtige_Maennlich = NA_real_,
          Tatverdaechtige_Weiblich = NA_real_,
          Nichtdeutsche_Tatverdaechtige_Anzahl = NA_real_,
          Nichtdeutsche_Tatverdaechtige_Prozent = NA_real_
        )

      # Final Selection and Ordering
      df_long <- df_long |>
        dplyr::select(
          Schluesselzahl,
          Straftat,
          Kreisschluessel,
          Kreisart,
          Kreis,
          Einwohner,
          Erfasste_Faelle,
          Straftaten_HZ,
          Versuche_Anzahl,
          Versuche_Anteil_Prozent,
          Mit_Schusswaffe_gedroht,
          Mit_Schusswaffe_geschossen,
          Aufklaerung_Faelle,
          Aufklaerungsquote,
          Tatverdaechtige_Insgesamt,
          Tatverdaechtige_Maennlich,
          Tatverdaechtige_Weiblich,
          Nichtdeutsche_Tatverdaechtige_Anzahl,
          Nichtdeutsche_Tatverdaechtige_Prozent
        ) |>
        arrange(Schluesselzahl, Kreisschluessel)

      # Detect districts with missing data (for 2010 reporting)
      if (year == "2010") {
        na_rows <- df_long |>
          filter(is.na(Aufklaerungsquote)) |>
          select(Kreisschluessel, Kreis, Straftat)

        if (nrow(na_rows) > 0) {
          # Get unique districts
          na_districts <- na_rows |>
            distinct(Kreis, Kreisschluessel)

          # Build academic message
          district_info <- paste0(
            na_districts$Kreis,
            " (ID: ",
            na_districts$Kreisschluessel,
            ")"
          )

          crime_types <- unique(na_rows$Straftat)

          message(
            "  ℹ Note: The original PDF source document contains missing data markers ('-') ",
            "for the 'Aufklaerungsquote' (clearance rate) variable in district ",
            paste(district_info, collapse = ", "),
            " for crime type(s): ",
            paste(crime_types, collapse = ", "),
            ". These values are coded as NA in the processed dataset."
          )
        }
      }

      # Save to CSV
      base_name <- stringr::str_remove(filename, "\\.pdf$")
      output_filename <- paste0(base_name, ".csv")
      output_file <- file.path(output_dir, output_filename)
      readr::write_csv(df_long, output_file)
      message(
        "  ✓ Saved: ",
        basename(output_file),
        " (",
        nrow(df_long),
        " rows)"
      )
    } else {
      message(
        "  × Error: Column count mismatch. Expected ",
        length(col_names),
        ", got ",
        ncol(df_clean)
      )
    }
  } else {
    message("  × No valid data extracted for ", year)
  }
}

message("\n===== PDF PROCESSING COMPLETED =====\n")
