# ===============================================================
# Script Summary
# Data used: Agri_Share_Total_GHG_by_Province.csv
#
# Plot produced:
#   1. Agriculture share of total GHG emissions by province (1990–2023),
#      arranged in a multi-panel grid.
# ===============================================================

library(tidyverse)
library(ggplot2)
library(ggpubr)

share_total <- read.csv("Agri_Share_Total_GHG_by_Province.csv")
share_total$Year <- as.numeric(share_total$Year)

prov_order <- c("NL","PE","NS","NB","QC","ON","MB","SK","AB","BC")
share_total$Province <- factor(share_total$Province, levels = prov_order)


# ---------------------------------------------------------------
#  PLOT: Agriculture share of total GHG emissions by province
# ---------------------------------------------------------------

p_list <- share_total %>%
  group_by(Province) %>%
  group_map(~{
    ggplot(.x, aes(x = Year, y = Share_Ag_in_Total_GHG)) +
      geom_line(size = 0.75, color = "darkblue") +
      geom_point(size = 1.25, color = "darkblue") +
      labs(
        title = unique(.x$Province),
        x = "Year",
        y = "Share of GHG (%)"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 12),
        axis.text = element_text(size = 5),
        axis.title = element_text(size = 4),
        panel.grid.major = element_line(linetype = "dashed", alpha = 0.4)
      )
  })

fig <- ggarrange(
  plotlist = p_list,
  nrow = 4,
  ncol = 3,
  common.legend = FALSE
)

annotate_figure(
  fig,
  top = text_grob(
    "Agriculture Share of Total GHG Emissions by Province/Territory (1990–2023)\n",
    size = 12,
    hjust = 0.5
  )
)
