
library(tidyverse)
library(knitr)

# 1. Load and filter the dataset
data <- read_csv("~/Applied_Economics/Data/Panel/final_panel_dataset_common_crimes.csv") %>%
  filter(crime_code == "------") %>%
  filter(year >= 2003, year <= 2022)

# 2. Define variables and professional labels
vars_of_interest <- c(
  "crime_rate", 
  "unemployment_rate", 
  "gdp_per_capita", 
  "share_youth_18_29",
  "share_males",
  "share_males_within_youth",
  "school_leavers_no_qual"
)

variable_labels <- c(
  "Crime Rate (per 100k)",
  "Unemployment Rate (%)",
  "GDP per Capita (€)",
  "Share of youth (18–29, %)",
  "Share of males (%)",
  "Male share among youth (%)",
  "Share of school leavers without qualifications (%)"
)

# 3. Process the data
summary_stats <- data %>%
  select(all_of(vars_of_interest)) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Value") %>%
  group_by(Variable) %>%
  summarise(
    Mean = mean(Value, na.rm = TRUE),
    SD = sd(Value, na.rm = TRUE),
    Min = min(Value, na.rm = TRUE),
    Max = max(Value, na.rm = TRUE),
    Obs = sum(!is.na(Value))
  ) %>%
  mutate(Variable = factor(Variable, 
                          levels = vars_of_interest, 
                          labels = variable_labels)) %>%
  arrange(Variable)

# 4. Generate Text Output
# You can use format = "pipe" for Markdown style or "simple" for a clean look
text_table <- kable(
  summary_stats,
  format = "pipe", 
  digits = 2,
  caption = "Summary Statistics of Key Indicators"
)

# Print to console
print(text_table)

# Save to a .txt file
writeLines(as.character(text_table), "~/Applied_Economics/Data/Results/summary_statistics.txt")