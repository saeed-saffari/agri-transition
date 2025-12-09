# ===============================================================
# Script Summary
# Data used: 3210015301_databaseLoadingData.csv
#
# Plots produced:
#   1. Total farm area by province (1971–2021)
#   2. Composition of farmland by use in Canada (1971–2021)
#   3. Provincial farmland shares (NA-cleaned, facet-wrapped)
# ===============================================================


# Load required packages
library(tidyverse)
library(readr)
library(ggplot2)
library(patchwork)


# Read dataset
df <- read_csv("3210015301_databaseLoadingData.csv")

# Filter total farm area in acres, years 1971+
area <- df %>%
  filter(
    `Land use` == "Total area of farms",
    `Unit of measure` == "Acres",
    REF_DATE >= 1971
  )

# Convert to wide format: rows = years, columns = provinces
prov_area <- area %>%
  select(REF_DATE, GEO, VALUE) %>%
  pivot_wider(names_from = GEO, values_from = VALUE)

# Drop Canada to keep only provinces
prov_area <- prov_area %>%
  select(-Canada)

# Convert to long format for ggplot
prov_long <- prov_area %>%
  pivot_longer(-REF_DATE, names_to = "Province", values_to = "Area")


# ---------------------------------------------------------------
#  PLOT 1: Total Farm Area by Province (1971–2021)
# ---------------------------------------------------------------
ggplot(prov_long, aes(x = REF_DATE, y = Area, color = Province)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  labs(
    title = "Total Farm Area by Province (1971–2021)",
    x = "Year",
    y = "Farm area (acres)",
    color = "Province"
  ) +
  theme_classic(base_family = "Times New Roman") +
  theme(
    plot.title = element_text(size = 14, hjust = 0.5),
    axis.title = element_text(size = 11),
    axis.text = element_text(size = 11),
    legend.position = "right"
  ) +
  theme(panel.grid.major = element_line(color = "grey80", linetype = "dashed")) +
  theme(aspect.ratio = 5/12)



# ---------------------------------------------------------------
#  PLOT 2: Composition of Farmland by Use in Canada (1971–2021)
# ---------------------------------------------------------------

# Filter Canada and relevant land use categories
canada_land <- df %>%
  filter(
    GEO == "Canada",
    REF_DATE >= 1971,
    `Unit of measure` == "Acres",
    `Land use` %in% c(
      "Land in crops",
      "Summerfallow land",
      "Tame or seeded pasture",
      "All other land"
    )
  )

# Convert to wide
land_pivot <- canada_land %>%
  select(REF_DATE, `Land use`, VALUE) %>%
  pivot_wider(names_from = `Land use`, values_from = VALUE)

# Convert to long format and compute shares
land_share <- land_pivot %>%
  mutate(total = rowSums(select(., -REF_DATE))) %>%
  pivot_longer(-c(REF_DATE, total), names_to = "LandUse", values_to = "Value") %>%
  mutate(Share = (Value / total) * 100)

# Stacked area plot
ggplot(land_share, aes(x = REF_DATE, y = Share, fill = LandUse)) +
  geom_area(alpha = 0.9) +
  scale_fill_brewer(palette = "YlGnBu") +
  labs(
    title = "Composition of Farm Land by Use (Canada, 1971–2021)",
    x = "Year",
    y = "Share of farm land (%)",
    fill = "Land use"
  ) +
  theme_classic(base_family = "Times New Roman") +
  theme(
    plot.title = element_text(size = 14, hjust = 0.5),
    axis.title = element_text(size = 11),
    axis.text = element_text(size = 11),
    legend.position = "right"
  ) +
  theme(aspect.ratio = 6/12)





# ---------------------------------------------------------------
#  PLOT 3 (previously Plot 4): Provincial farmland shares, NA-cleaned
# ---------------------------------------------------------------

library(tidyverse)
library(ggplot2)

# Filter relevant categories for provinces
land_use <- df %>%
  filter(
    `Unit of measure` == "Acres",
    REF_DATE >= 1971,
    `Land use` %in% c(
      "Land in crops",
      "Summerfallow land",
      "Tame or seeded pasture",
      "All other land"
    ),
    GEO != "Canada"
  )

# Pivot wide to detect NA rows
wide_land <- land_use %>%
  select(REF_DATE, GEO, `Land use`, VALUE) %>%
  pivot_wider(names_from = `Land use`, values_from = VALUE)

# Remove rows with missing values
wide_land_clean <- wide_land %>%
  filter(if_all(-c(REF_DATE, GEO), ~ !is.na(.x)))

# Long format and compute shares
province_share <- wide_land_clean %>%
  pivot_longer(
    cols = -c(REF_DATE, GEO),
    names_to = "Land use",
    values_to = "VALUE"
  ) %>%
  group_by(GEO, REF_DATE) %>%
  mutate(Share = VALUE / sum(VALUE) * 100) %>%
  ungroup()

# Faceted plot
ggplot(province_share, aes(x = REF_DATE, y = Share, fill = `Land use`)) +
  geom_area(alpha = 0.85) +
  facet_wrap(~ GEO, ncol = 3) +
  scale_fill_brewer(palette = "YlGnBu") +
  labs(
    title = "Shares of Farmland by Use, Provinces (1971–2021)",
    x = "Year",
    y = "Share (%)",
    fill = "Land use"
  ) +
  theme_classic(base_family = "Times New Roman") +
  theme(
    plot.title = element_text(size = 14, hjust = 0.5),
    strip.text = element_text(size = 10, face = "bold"),
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 10),
    legend.position = "right"
  )
