Bac France Dashboard
A data story of French baccalaureate results, by Path and Academy
Overview
This project analyzes and visualizes the French Baccalaureate (bac) dataset using R and Shiny. The goal is to provide clear, interactive insights into success rates and student distributions across different filières (Paths), academies, and years.
Why R?
Statistical power: Native support for summary stats, groupings, and correlations.
Visualization: Advanced, customizable plots (ggplot2) and interactive dashboards (shiny).
Data wrangling: Efficient manipulation with dplyr and easy data import/export.
Workflow
Load & Explore: Import the dataset, check structure and consistency.
Filter & Prepare: Subset by year, academy, or Path as needed.
Transform & Summarize: Compute totals and success rates for each Path.
Visualize: Show key metrics and trends with dynamic barplots, pie charts, and tables.
Key Insights
The "Général" Path consistently has the highest success rate.
The "Professionnel" Path shows more variability and generally lower success rates.
Interactive filters allow comparison across academies, years, and specialties.
Usage
Run app.R (or your main script) to launch the dashboard.
Use sidebar filters to explore the data by session, academy, and Path.
View dynamic key stats, success rate visualizations, and mention distributions.
Files:
app.R – Main Shiny app
data/raw/baccalaureate_by_academy_france.csv – Main dataset
Contact:
For questions, improvements, or bug reports, please contact [your email here].