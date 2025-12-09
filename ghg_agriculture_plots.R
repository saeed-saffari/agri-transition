# ===============================================================
# Script Summary
# Data used: EN_Annex10_GHG_Econ_Canada.xlsx (Sheet 5)
#
# Plots produced:
#   1. Agriculture share of national GHG emissions
#   2. Subsector shares within agriculture (farm fuel, crops, animal)
# ===============================================================

library(tidyverse)
library(readxl)
library(ggplot2)

# ---------------------------------------------------------------
#  SECTION 1: Load and clean Excel data (matches Python structure)
# ---------------------------------------------------------------

df_raw <- read_excel(
  "EN_Annex10_GHG_Econ_Canada.xlsx",
  sheet = 5,
  col_names = FALSE
)

# Identify the row containing years (Row 3)
raw_years <- df_raw[3, 2:ncol(df_raw)] %>%
  unlist() %>%
  as.character()

# Extract only numeric year columns
year_vec <- suppressWarnings(as.numeric(raw_years))
valid_cols <- which(!is.na(year_vec))
years <- year_vec[valid_cols]

# Extract sector rows from Row 5 onward
data_raw <- df_raw[5:nrow(df_raw), c(1, valid_cols + 1)]

# Assign final column names
colnames(data_raw) <- c("Sector", years)

# Clean rows
data <- data_raw %>%
  filter(!is.na(Sector), Sector != "")


# ---------------------------------------------------------------
#  PLOT 1: Agriculture share of total national GHG emissions
# ---------------------------------------------------------------

# Extract required rows
agriculture <- data %>% filter(Sector == "Agriculture")
total <- data %>% filter(Sector == "NATIONAL GHG TOTAL")

# Convert to numeric vectors
ag_vec <- as.numeric(agriculture[1, -1])
tot_vec <- as.numeric(total[1, -1])

# Compute agriculture share of total emissions
share_vec <- ag_vec / tot_vec * 100

# Build dataframe
share_df <- tibble(
  Year = years,
  Share = share_vec
)

# -----------------------
# PLOT 1
# -----------------------
ggplot(share_df, aes(x = Year, y = Share)) +
  geom_line(color = "darkgreen", linewidth = 1.25) +
  geom_point(color = "darkgreen", size = 2.5) +
  labs(
    title = "Agriculture Share of Total GHG Emissions in Canada",
    x = "Year",
    y = "Share of National GHG (%)"
  ) +
  theme_classic(base_family = "Times New Roman") +
  theme(
    plot.title = element_text(size = 14, hjust = 0.5),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 10)
  ) +
  theme(panel.grid.major = element_line(color = "grey80", linetype = "dashed")) +
  theme(aspect.ratio = 6/12)



# ---------------------------------------------------------------
#  PLOT 2: Subsector shares within agriculture
# ---------------------------------------------------------------

# Extract subsectors
farm <- data %>% filter(Sector == "On Farm Fuel Use")
crop <- data %>% filter(Sector == "Crop Production")
animal <- data %>% filter(Sector == "Animal Production")

# Combine subsectors
sub_ag <- bind_rows(farm, crop, animal)

# Convert values to numeric
sub_ag_numeric <- sub_ag %>%
  mutate(across(-Sector, ~ as.numeric(.)))

ag_vec <- agriculture %>%
  mutate(across(-Sector, ~ as.numeric(.))) %>%
  select(-Sector) %>%
  unlist(use.names = FALSE)

# Matrix for share calculation
sub_mat <- sub_ag_numeric %>%
  select(-Sector) %>%
  as.matrix()

share_mat <- sweep(sub_mat, 2, ag_vec, FUN = "/") * 100
rownames(share_mat) <- sub_ag$Sector

# Convert to long format
share_long <- share_mat %>%
  as.data.frame() %>%
  mutate(Sector = rownames(.)) %>%
  pivot_longer(
    cols = -Sector,
    names_to = "Year",
    values_to = "Share"
  ) %>%
  mutate(Year = as.numeric(Year))

# -----------------------
# PLOT 2
# -----------------------
ggplot(share_long, aes(x = Year, y = Share, fill = Sector)) +
  geom_area(alpha = 0.9) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Shares of Agriculture GHG Emissions by Subsector",
    x = "Year",
    y = "Share of Agriculture (%)",
    fill = "Subsector"
  ) +
  theme_classic(base_family = "Times New Roman") +
  theme(
    plot.title = element_text(size = 14, hjust = 0.5),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 10),
    legend.position = "right",
    panel.grid.major = element_line(color = "grey85", linetype = "dashed")
  ) +
  theme(aspect.ratio = 6/12)
