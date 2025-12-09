# Agricultural Transitions, Emissions, and Trade Visualization in Canada

This repository contains data analysis and visualization scripts for examining agricultural transitions, greenhouse gas emissions, land use, demographics, and trade patterns in Canada. All visualizations were generated in R, with some Python-to-R conversions for consistency across the project.

The project uses datasets from **Statistics Canada** and **Environment Canada** to create publication-ready figures stored in the `graphs` folder. Raw datasets used in the analysis are stored in the `data` folder.

---

## Data Sources

All datasets originate from the **Statistics Canada** website.

### **Data files used in this repository**
- `3210015601_databaseLoadingData.csv`
- `3210015301_databaseLoadingData.csv`
- `3210038101_databaseLoadingData.csv`
- `Agri_Share_Total_GHG_by_Province.csv`
- `1210017201_databaseLoadingData.csv`
- `EN_Annex10_GHG_Econ_Canada.xlsx` (Sheet 5)

These datasets include information related to:

- Farm size and land use (1971–2021)  
- Demographics of farm operators  
- Agricultural GHG emissions nationally and by province  
- Subsector GHG contributions  
- Agricultural export flows from Canada to the United States  

---

## Scripts and Code Files

All code for cleaning, processing, and visualizing data is included in this repository. Scripts generate figures such as:

### **Agriculture and Land Use**
- Distribution of farm sizes in Canada  
- Total farm area trends by province  
- Shares of farmland allocated to crops, pasture, summerfallow, and other land  

### **Demographics of Farm Operators**
- Age distribution by province (2021)  
- Sex distribution by province (2021)  
- National age distribution (pie chart for 2021)  

### **Agricultural Emissions**
- Agriculture’s share of total national GHG emissions  
- Subsector-level emissions (crop, livestock, on-farm fuel)  
- Provincial GHG emissions shares (multi-panel arrangement)  

### **Trade Analysis**
- Agricultural exports to the United States (C11 category)  
- Subsector composition of agricultural exports (2020–2024)  
- Export composition pie chart for 2024  

All generated graphics are stored in the **graphs/** folder.

