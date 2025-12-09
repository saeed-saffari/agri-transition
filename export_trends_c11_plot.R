# ===============================================================
# Script Summary
# Data used: 1210017201_databaseLoadingData.csv
#
# Plots produced:
#   1. Agricultural exports to the U.S. (Farm, Fishing & Intermediate Food Products)
#   2. Subsector composition of agricultural exports (stacked area)
#   3. Agricultural export composition pie chart for 2024
# ===============================================================

# Set working directory for reading and saving project files
setwd("~/Library/CloudStorage/OneDrive-SharedLibraries-DalhousieUniversity/Kathleen-Talan-Saeed Project - General/data and graphs")

# Load required packages
library(tidyverse)
library(readr)
library(ggplot2)

# Load the trade dataset
df <- read_csv("1210017201_databaseLoadingData.csv")

# Keep only export records
trade <- df %>% 
  filter(Trade == "Export")

# Define the main agricultural root category (C11)
ag_root <- "Farm, fishing and intermediate food products [C11]"

# Define agricultural subcategories
ag_sub <- c(
  "Live animals [111]",
  "Wheat [112]",
  "Canola (including rapeseed) [113]",
  "Fresh fruit, nuts and vegetables, and pulse crops [114]",
  "Other crop products [115]",
  "Other animal products [116]",
  "Fish, crustaceans, shellfish and other fishery products [121]",
  "Animal feed [181]",
  "Intermediate food products [182]"
)

# Filter dataset to agricultural categories
agri <- trade %>%
  filter(`North American Product Classification System (NAPCS)` %in% c(ag_root, ag_sub))

# Convert agricultural data into wide format
pivot <- agri %>%
  select(REF_DATE, `North American Product Classification System (NAPCS)`, VALUE) %>%
  mutate(VALUE = VALUE / 1e6) %>%
  pivot_wider(
    names_from = `North American Product Classification System (NAPCS)`,
    values_from = VALUE
  )

# Extract root agriculture category (C11)
farm <- trade %>%
  filter(`North American Product Classification System (NAPCS)` == ag_root)

# Build wide pivot table for farm exports
farm_pivot <- farm %>%
  select(REF_DATE, Trade, VALUE) %>%
  mutate(VALUE = VALUE / 1e6) %>%
  pivot_wider(names_from = Trade, values_from = VALUE)


# ---------------------------------------------------------------
#  PLOT 1: Agricultural Exports to the U.S. (Root Category C11)
# ---------------------------------------------------------------

ggplot(farm_pivot, aes(x = REF_DATE, y = Export)) +
  geom_line(color = "darkred", linewidth = 1.2) +
  geom_point(color = "darkred", size = 3) +
  scale_x_continuous(breaks = 2020:2024) +
  labs(
    title = "Canada’s Agricultural Exports to the U.S. (Farm, Fishing & Intermediate Food Products)",
    x = "Year",
    y = "Value (billion CAD)"
  ) +
  theme_classic(base_family = "Times New Roman") +
  theme(
    plot.title = element_text(size = 14, hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12)
  ) +
  theme(panel.grid.major = element_line(color = "grey70", linetype = "dashed")) +
  theme(aspect.ratio = 6/12)

# Optional save
# ggsave("farm_export_us_2020_2024.png", width = 12, height = 6, dpi = 500)




# ---------------------------------------------------------------
#  PLOT 2: Composition of Agricultural Exports to the U.S. (Stacked Area)
# ---------------------------------------------------------------

# Prepare subsectors only
subs <- trade %>%
  filter(`North American Product Classification System (NAPCS)` %in% ag_sub)

# Wide format
pivot_subs <- subs %>%
  select(REF_DATE, `North American Product Classification System (NAPCS)`, VALUE) %>%
  mutate(VALUE = VALUE / 1e6) %>%
  pivot_wider(
    names_from = `North American Product Classification System (NAPCS)`,
    values_from = VALUE
  )

# Long format and compute shares
shares_long <- pivot_subs %>%
  mutate(total = rowSums(select(., -REF_DATE))) %>%
  pivot_longer(-c(REF_DATE, total), names_to = "Product", values_to = "Value") %>%
  mutate(Share = (Value / total) * 100)

# Stacked area chart
ggplot(shares_long, aes(x = REF_DATE, y = Share, fill = Product)) +
  geom_area(alpha = 0.9) +
  scale_fill_brewer(palette = "RdYlBu") +
  scale_x_continuous(breaks = 2020:2024) +
  labs(
    title = "Composition of Agricultural Exports to the U.S.\n(Subsectors, 2020–2024)",
    x = "Year",
    y = "Share of agri exports (%)",
    fill = "Product"
  ) +
  theme_classic(base_family = "Times New Roman") +
  theme(
    plot.title = element_text(size = 12, hjust = 0.5),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 10),
    legend.position = "right"
  ) +
  theme(aspect.ratio = 8/18)



# ---------------------------------------------------------------
#  PLOT 3: Pie Chart of Agricultural Export Shares (2024)
# ---------------------------------------------------------------

library(ggrepel)

# Prepare 2024 data
last_year <- pivot_subs %>%
  filter(REF_DATE == 2024) %>%
  select(-REF_DATE) %>%
  pivot_longer(everything(), names_to = "Product", values_to = "Value") %>%
  mutate(
    Percentage = Value / sum(Value) * 100,
    Label = paste0(Product, "\n", sprintf("%.1f%%", Percentage))
  )

# Label positions
last_year <- last_year %>%
  arrange(desc(Product)) %>%
  mutate(
    ymax = cumsum(Value),
    ymin = ymax - Value,
    labelPosition = (ymax + ymin) / 2
  )

ggplot(last_year, aes(x = 1, y = Value, fill = Product)) +
  geom_col(color = "white") +
  coord_polar(theta = "y") +
  
  geom_label_repel(
    aes(label = sprintf("%.1f%%", Percentage), y = labelPosition),
    label.size = 0,
    nudge_x = 1,
    show.legend = FALSE,
    size = 4,
    family = "Times New Roman",
    direction = "y",
    segment.size = 0.4
  ) +
  
  scale_fill_brewer(palette = "RdYlBu") +
  labs(
    title = "Agricultural Exports to the U.S. by Product (2024)",
    fill = "Product"
  ) +
  theme_void(base_family = "Times New Roman") +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5),
    legend.position = "right"
  )
