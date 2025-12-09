# ===============================================================
# Script Summary
# Data used: 3210038101_databaseLoadingData.csv
#
# Plots produced:
#   1. Age distribution of farm operators by province (2021)
#   2. Sex distribution of farm operators by province (2021)
#   3. National age distribution pie chart (2021)
# ===============================================================

library(tidyverse)
library(readr)
library(ggplot2)

df <- read_csv("3210038101_databaseLoadingData.csv")

# Province order for x-axis labels (same as Python)
prov_order <- c(
  "Alberta",
  "British Columbia",
  "Manitoba",
  "New Brunswick",
  "Newfoundland and Labrador",
  "Nova Scotia",
  "Ontario",
  "Prince Edward Island",
  "Quebec",
  "Saskatchewan"
)

# ---------------------------------------------------------------
#  PLOT 1: Age Distribution of Farm Operators by Province (2021)
# ---------------------------------------------------------------

# 1) Filter exactly like Python
df_age <- df %>%
  filter(
    `Farms according to the number of operators reported` == "All farms",
    Characteristics %in% c(
      "Age - under 35 years",
      "Age - 35 to 54 years",
      "Age - 55 years and over"
    ),
    !str_detect(GEO, "Canada")
  )

# 2) Compute within-province shares (row-wise in Python)
age_long <- df_age %>%
  group_by(GEO) %>%
  mutate(Share = VALUE / sum(VALUE) * 100) %>%
  ungroup() %>%
  mutate(
    Province = sub(" \\[.*", "", GEO),
    Province = factor(Province, levels = prov_order),
    AgeGroup = factor(
      Characteristics,
      levels = c(
        "Age - under 35 years",
        "Age - 35 to 54 years",
        "Age - 55 years and over"
      )
    )
  ) %>%
  select(Province, AgeGroup, Share)

# Coolwarm-style colors
coolwarm_3 <- c("#3B4CC0", "#d6d2d2", "#B40426")

ggplot(age_long, aes(x = Province, y = Share, fill = AgeGroup)) +
  geom_col() +
  scale_fill_manual(
    values = coolwarm_3,
    name = "Age Group",
    labels = c(
      "Under 35 years",
      "35 to 54 years",
      "55 years and over"
    )
  ) +
  labs(
    title = "Age Distribution of Farm Operators by Province (2021)",
    x = "",
    y = "Share (%)"
  ) +
  theme_classic(base_family = "Times New Roman") +
  theme(
    plot.title = element_text(size = 12, hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y = element_text(size = 10),
    legend.position = c(1.02, 0.6),
    legend.justification = "left"
  ) +
  theme(legend.position = "right") +
  theme(aspect.ratio = 6/12)



# ---------------------------------------------------------------
#  PLOT 2: Sex Distribution of Farm Operators by Province (2021)
# ---------------------------------------------------------------

df_sex <- df %>%
  filter(
    `Farms according to the number of operators reported` == "All farms",
    Characteristics %in% c("Sex - male", "Sex - female"),
    !str_detect(GEO, "Canada")
  ) %>%
  mutate(
    Province = sub(" \\[.*", "", GEO)
  )

sex_long <- df_sex %>%
  group_by(GEO) %>%
  mutate(Share = VALUE / sum(VALUE) * 100) %>%
  ungroup() %>%
  mutate(
    Province = factor(Province, levels = prov_order),
    Sex = factor(Characteristics, levels = c("Sex - male", "Sex - female"))
  ) %>%
  select(Province, Sex, Share)

coolwarm_2 <- c("#3B4CC0", "#B40426")

ggplot(sex_long, aes(x = Province, y = Share, fill = Sex)) +
  geom_col() +
  scale_fill_manual(
    values = coolwarm_2,
    name = "Sex",
    labels = c("Male", "Female")
  ) +
  labs(
    title = "Sex Distribution of Farm Operators by Province (2021)",
    x = "",
    y = "Share (%)"
  ) +
  theme_classic(base_family = "Times New Roman") +
  theme(
    plot.title = element_text(size = 12, hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y = element_text(size = 10),
    legend.position = c(1.02, 0.6),
    legend.justification = "left"
  ) +
  theme(legend.position = "right") +
  theme(aspect.ratio = 6/12)



# ---------------------------------------------------------------
#  PLOT 3: National Age Distribution (Pie Chart, 2021)
# ---------------------------------------------------------------

df_nat_age <- df %>%
  filter(
    GEO == "Canada [000000000]",
    `Farms according to the number of operators reported` == "All farms",
    Characteristics %in% c(
      "Age - under 35 years",
      "Age - 35 to 54 years",
      "Age - 55 years and over"
    )
  )

nat_age <- df_nat_age %>%
  select(Characteristics, VALUE) %>%
  mutate(
    Percentage = VALUE / sum(VALUE) * 100,
    AgeGroup = factor(
      Characteristics,
      levels = c(
        "Age - under 35 years",
        "Age - 35 to 54 years",
        "Age - 55 years and over"
      )
    )
  )

coolwarm_3 <- c("#3B4CC0", "#d6d2d2", "#B40426")

ggplot(nat_age, aes(x = "", y = Percentage, fill = AgeGroup)) +
  geom_col(color = "white") +
  coord_polar(theta = "y") +
  scale_fill_manual(
    values = coolwarm_3,
    name = "Age Group",
    labels = c("Under 35 years", "35 to 54 years", "55 years and over"),
    guide = guide_legend(override.aes = list(label = NULL))
  ) +
  geom_text(
    aes(label = sprintf("%.1f%%", Percentage)),
    position = position_stack(vjust = 0.5),
    size = 5,
    family = "Times New Roman",
    color = "black"
  ) +
  labs(
    title = "National Age Distribution of Farm Operators (2021)"
  ) +
  theme_void(base_family = "Times New Roman") +
  theme(
    plot.title = element_text(size = 14, hjust = 0.5),
    legend.position = "right",
    legend.key = element_rect(fill = NA)
  ) +
  theme(aspect.ratio = 6/6)
