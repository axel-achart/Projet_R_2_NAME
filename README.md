# Bac France Dashboard

**A data story of French Baccalaureate results, by Path and Academy**

---

## Overview

This project analyzes and visualizes the French Baccalaureate (bac) dataset using **R** and **Shiny**.  
The goal is to provide clear, interactive insights into success rates and student distributions across different **filieres** (Paths), **academies**, and **years**.

https://www.kaggle.com/datasets/yanisstentzel/french-baccalaureate-2021-2024

---

## Why R?

- **Statistical power**: Native support for summary statistics, groupings, and correlations.  
- **Visualization**: Advanced and customizable plots with `ggplot2`, plus interactive dashboards via `shiny`.  
- **Data wrangling**: Efficient data manipulation with `dplyr` and simple import/export workflows.

---

## Workflow

1. **Load & Explore**  
   Import the dataset, check its structure and data consistency.  

2. **Filter & Prepare**  
   Subset data by year, academy, or Path as needed.  

3. **Transform & Summarize**  
   Calculate totals and success rates for each Path.  

4. **Visualize**  
   Display key metrics and trends with dynamic barplots, pie charts, and tables.

---

## Key Insights

- The **"Général"** Path consistently shows the highest success rates.  
- The **"Professionnel"** Path exhibits more variability and generally lower success rates.  
- Interactive filters enable comparison across academies, years, and specialties.

---

## Usage

- Run `app.R` (or your main script) to launch the dashboard.  
- Use sidebar filters to explore data by session, academy, and Path.  
- View dynamic key statistics, success rate visualizations, and mention distributions.

---

## Files

- `app.R` – Main Shiny application  
- `data/raw/baccalaureate_by_academy_france.csv` – Main dataset

---

## Contact

For questions, suggestions, or bug reports, please contact:  
**[your email here]**

