# ===============================================================
# Script Summary
# Data used: 3210015601_databaseLoadingData.csv
# Plots produced:
#   1. Distribution of farm sizes in Canada over time (stacked area plot)
#   2. Total number of farms by province, 1976 to 2021 (line plot)
# ===============================================================

library(tidyverse)
library(readr)
library(ggplot2)

# Load dataset
df <- read_csv("3210015601_databaseLoadingData.csv")

# Filter Canada only, exclude "Total number of farms"
canada <- df %>%
  filter(
    GEO == "Canada",
    `Total farm area distribution` != "Total number of farms"
  )

# Define ordered farm-size categories
order <- c(
  "Under 10.00 acres",
  "10.00 to 69.99 acres",
  "70.00 to 129.99 acres",
  "130.00 to 179.99 acres",
  "180.00 to 239.99 acres",
  "240.00 to 399.99 acres",
  "400.00 to 559.99 acres",
  "560.00 to 759.99 acres",
  "760.00 to 1,119.99 acres",
  "1,120.00 to 1,599.99 acres",
  "1,600.00 to 2,239.99 acres",
  "2,240.00 to 2,879.99 acres",
  "2,880.00 to 3,519.99 acres",
  "3,520.00 acres and over"
)

# Pivot to wide format
canada_pivot <- canada %>%
  select(REF_DATE, `Total farm area distribution`, VALUE) %>%
  pivot_wider(names_from = `Total farm area distribution`, values_from = VALUE) %>%
  select(REF_DATE, all_of(order))

# Compute percentage shares
canada_share <- canada_pivot %>%
  mutate(total = rowSums(select(., -REF_DATE))) %>%
  pivot_longer(-c(REF_DATE, total), names_to = "FarmSize", values_to = "Value") %>%
  mutate(Share = Value / total * 100)


library(viridis)

# ---------------------------------------------------------------
#  PLOT 1: Distribution of farm sizes in Canada over time
# ---------------------------------------------------------------
ggplot(canada_share, aes(x = REF_DATE, y = Share, fill = FarmSize)) +
  geom_area(alpha = 0.9) +
  scale_fill_viridis_d(option = "turbo", direction = -1) +
  labs(
    title = "Distribution of Farms by Total Area (Canada)",
    x = "Year",
    y = "Share of farms (%)",
    fill = "Farm size"
  ) +
  theme_classic(base_family = "Times New Roman") +
  theme(
    plot.title = element_text(size = 14, hjust = 0.5),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 10),
    legend.position = "right"
  ) +
  theme(aspect.ratio = 8/12)



# ---------------------------------------------------------------
#  PLOT 2: Total number of farms by province, 1976 to 2021
# ---------------------------------------------------------------

# Filter only "Total number of farms"
totals <- df %>%
  filter(`Total farm area distribution` == "Total number of farms")

# Pivot: provinces as columns
prov_totals <- totals %>%
  select(REF_DATE, GEO, VALUE) %>%
  pivot_wider(names_from = GEO, values_from = VALUE) %>%
  select(-Canada)   # drop national total

# Convert to long format for ggplot
prov_long <- prov_totals %>%
  pivot_longer(-REF_DATE, names_to = "Province", values_to = "Farms")

# Plot: total number of farms by province
ggplot(prov_long, aes(x = REF_DATE, y = Farms, color = Province)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  labs(
    title = "Total Number of Farms by Province (1976 to 2021)",
    x = "Year",
    y = "Number of farms",
    color = "Province"
  ) +
  theme_classic(base_family = "Times New Roman") +
  theme(
    plot.title = element_text(size = 12, hjust = 0.5),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 10),
    legend.position = "right"
  ) +
  theme(panel.grid.major = element_line(color = "grey80", linetype = "dashed")) +
  theme(aspect.ratio = 6/12)
