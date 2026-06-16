library(tidyverse)

# Prepare data
df_plot <- df_total %>%
  mutate(log_crime = log(crime_rate + 1)) %>%
  select(crime_rate, log_crime) %>%
  pivot_longer(
    cols = c(crime_rate, log_crime),
    names_to = "variable",
    values_to = "value"
  ) %>%
  mutate(
    variable = factor(variable,
                      levels = c("crime_rate", "log_crime"),
                      labels = c("Raw crime rate", "Log crime rate"))
  )

# Plot
fig1_combined <- ggplot(df_plot, aes(x = value)) +
  geom_histogram(
    bins = 35,                     # fewer bins → thicker bars
    fill = "#4C72B0",
    color = "white",
    linewidth = 0.3                # slightly thicker edges
  ) +
  
  facet_wrap(~variable, scales = "free") +
  
  labs(
    title = "Distribution of Crime Rates: Raw vs Log-Transformed",
    x = "",
    y = "Frequency"
  ) +
  
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

fig1_combined

# -------------------------
# FIGURE 2
# Time trends
# -------------------------
df_year <- df_total %>%
  group_by(year) %>%
  summarise(
    mean_crime = mean(crime_rate, na.rm = TRUE),
    mean_unemp = mean(unemployment_rate, na.rm = TRUE),
    .groups = "drop"
  )

fig2 <- ggplot(df_year, aes(x = year)) +
  geom_line(aes(y = mean_crime, color = "Crime rate"), linewidth = 1) +
  geom_line(aes(y = mean_unemp * 1000, color = "Unemployment rate"), linewidth = 1, linetype = "dashed") +
  
  scale_color_manual(
    values = c(
      "Crime rate" = "#4C72B0",
      "Unemployment rate" = "#C44E52"
    )
  ) +
  
  labs(
    title = "Time Trends in Crime and Unemployment (2003–2023)",
    x = "Year",
    y = "Crime rate (solid) / Unemployment rate scaled",
    color = ""
  ) +
  
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

fig2


# -------------------------
# FIGURE 3
# Scatter plot
# -------------------------
df_scatter <- df_total %>%
  filter(!is.na(unemployment_rate))

fig3 <- ggplot(df_scatter, aes(x = unemployment_rate, y = crime_rate)) +
  geom_point(alpha = 0.2, color = "#4C72B0") +
  geom_smooth(method = "lm", se = FALSE, color = "#C44E52", linewidth = 1) +
  
  coord_cartesian(ylim = c(0, 25000)) +   # 👈 zoom only
  
  labs(
    title = "Unemployment and Crime (2003–2023)",
    x = "Unemployment rate (%)",
    y = "Crime rate per 100,000 inhabitants"
  ) +
  
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.grid.minor = element_blank()
  )

fig3

# -------------------------
# Save figures
# -------------------------
ggsave("figure1_raw_crime.png", fig1_raw, width = 7, height = 5, dpi = 300)
ggsave("figure1_log_crime.png", fig1_log, width = 7, height = 5, dpi = 300)
ggsave("figure2_time_trends.png", fig2, width = 7, height = 5, dpi = 300)
ggsave("figure3_scatter.png", fig3, width = 7, height = 5, dpi = 300)