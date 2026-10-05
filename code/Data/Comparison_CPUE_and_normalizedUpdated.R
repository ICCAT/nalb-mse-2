# ============================================================
# Script: Comparison_CPUE_and_normalizedUpdated.R
#
# Purpose:
#   Compare the previous and updated standardized CPUE series,
#   normalize the updated series using the mean up to 2021,
#   and generate the CPUE file used in the 2025 advice analyses.
#
# Inputs:
#   - Data/CPUE_inputs.xlsx
#
# Outputs:
#   - Data/cpue_normalized_2025.xlsx
#   - Output/Figures/Standardization_2025_cpue_comparison.png
#
# Author: AZTI
# ============================================================


# ============================================================
# 1. Load packages
# ============================================================

library(readxl)
library(dplyr)
library(ggplot2)
library(writexl)
library(here)


# ============================================================
# 2. Set project and SharePoint directories
# ============================================================

project_dir <- here::here()
setwd(project_dir)

source("sharepoint_path.R")

setwd(shrpoint_path)


# ============================================================
# 3. Read CPUE input data
# ============================================================

cpue_new <- read_excel(
  "Data/CPUE_inputs.xlsx",
  sheet = "NEW",
  na = "NA"
)

cpue_previous <- read_excel(
  "Data/CPUE_inputs.xlsx",
  sheet = "PREV_normalized",
  na = "NA"
)


# ============================================================
# 4. Define CPUE indicator mapping
# ============================================================

indicator_map <- data.frame(
  new_column = c(
    "BB_new", "JP_LL_N_new", "JP_LL_S_new",
    "TAI_LL_N_new", "TAI_LL_S_new",
    "US_LL_N_new", "US_LL_S_new",
    "VEN_LL_new"
  ),
  previous_column = c(
    "BB_prev", "JP_LL_N_prev", "JP_LL_S_prev",
    "TAI_LL_N_prev", "TAI_LL_S_prev",
    "US_LL_N_prev", "US_LL_S_prev",
    "VEN_LL"
  ),
  indicator_name = c(
    "BB", "JP_LL_N", "JP_LL_S",
    "TAI_LL_N", "TAI_LL_S",
    "US_LL_N", "US_LL_S",
    "VEN_LL"
  ),
  stringsAsFactors = FALSE
)


# ============================================================
# 5. Normalize updated CPUE series
# ============================================================

cpue_normalized <- cpue_new

for (i in seq_along(indicator_map$new_column)) {
  
  column_name <- indicator_map$new_column[i]
  
  mean_reference <- mean(
    cpue_new[[column_name]][cpue_new$Year <= 2021],
    na.rm = TRUE
  )
  
  cpue_normalized[[paste0(column_name, "_normalized")]] <-
    cpue_new[[column_name]] / mean_reference
}


# ============================================================
# 6. Summarize normalization factors
# ============================================================

normalization_factors <- data.frame(
  indicator = indicator_map$new_column,
  mean_up_to_2021 = sapply(
    indicator_map$new_column,
    function(column_name) {
      
      mean(
        cpue_new[[column_name]][cpue_new$Year <= 2021],
        na.rm = TRUE
      )
      
    }
  )
)

print("Means used for normalization (years <= 2021):")
print(normalization_factors)


# ============================================================
# 7. Create plotting dataset
# ============================================================

plot_data <- data.frame()

for (i in seq_along(indicator_map$new_column)) {
  
  normalized_column <- paste0(
    indicator_map$new_column[i],
    "_normalized"
  )
  
  previous_column <- indicator_map$previous_column[i]
  
  indicator_name <- indicator_map$indicator_name[i]
  
  previous_series <- cpue_previous %>%
    select(Year, all_of(previous_column)) %>%
    rename(Value = all_of(previous_column)) %>%
    filter(Year <= 2021) %>%
    mutate(
      Series = "CPUE 2021",
      Indicator = indicator_name
    )
  
  updated_series <- cpue_normalized %>%
    select(Year, all_of(normalized_column)) %>%
    rename(Value = all_of(normalized_column)) %>%
    mutate(
      Series = "CPUE 2025",
      Indicator = indicator_name
    )
  
  plot_data <- bind_rows(
    plot_data,
    previous_series,
    updated_series
  )
  
}


# ============================================================
# 8. Create CPUE comparison plot
# ============================================================

cpue_comparison_plot <- ggplot(
  plot_data,
  aes(
    x = Year,
    y = Value,
    colour = Series
  )
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_vline(
    xintercept = 2021,
    linetype = "dashed",
    colour = "grey40"
  ) +
  facet_wrap(
    ~ Indicator,
    ncol = 4,
    scales = "free_y"
  ) +
  scale_color_manual(
    values = c(
      "CPUE 2021" = "#e66101",
      "CPUE 2025" = "#1f78b4"
    )
  ) +
  theme_bw(base_size = 22) +
  theme(
    axis.text.x = element_text(
      size = 18,
      angle = 45,
      hjust = 1
    ),
    axis.text.y = element_text(size = 18),
    axis.title = element_text(size = 20),
    strip.text = element_text(
      size = 20,
      face = "bold"
    ),
    legend.text = element_text(size = 18),
    legend.title = element_text(size = 20),
    plot.title = element_text(
      size = 22,
      face = "bold"
    ),
    plot.subtitle = element_text(size = 18),
    legend.position = "bottom"
  ) +
  labs(
    title = paste(
      "Comparison of standardized CPUE series",
      "(2021 vs 2025 update)"
    ),
    x = "Year",
    y = "Normalized value",
    colour = "Series"
  )


print(cpue_comparison_plot)


# ============================================================
# 9. Save CPUE comparison figure
# ============================================================

ggsave(
  filename = "Data/Standardization_2025_cpue_comparison.png",
  plot = cpue_comparison_plot,
  width = 28,
  height = 16,
  dpi = 300
)


# ============================================================
# 10. Export normalized CPUE series
# ============================================================

cpue_normalized_output <- cpue_normalized %>%
  select(
    Year,
    ends_with("_normalized")
  )

write_xlsx(
  cpue_normalized_output,
  "Data/cpue_normalized_2025.xlsx"
)

message(
  "Normalized CPUE series successfully saved to ",
  "Data/cpue_normalized_2025.xlsx"
)
