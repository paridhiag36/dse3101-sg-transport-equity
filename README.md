# dse3101-sg-transport-equity
This repository is a cleaned and reorganized version of our original project repository. The original had too many commits and a disorganized file structure, making it difficult to maintain. This version provides a fresh start with a clear file hierarchy and updated documentation.

📁 Repository Structure
1. datasets/
Contains both raw and processed datasets used throughout the project.
The original datasets we start with are:
- t1-9.xls – from LTA DataMall
- SG Public Hospitals & Polyclinics.xlsx – manually curated dataset
- subzones.ts – from a public GitHub source
- ResidentPopulationData.csv – from the source cited in our documentation
- raw_subzones.geojson – geo-spatial data for Singapore subzones (source cited in documentation)
And the processed datasets (outputs from various data preparation scripts) are also stored here.

2. dataprep_r/
Contains all R Markdown scripts used for preparing and cleaning data from multiple sources.
Scripts (in execution order):
- Data_Prep1.Rmd
Parses t1-9.xls from LTA DataMall to extract Singapore's Planning Areas and Subzones.
Outputs: PA_SZ.csv
- Data_Prep2.Rmd
Uses regex to extract Planning Area and Subzone divisions (with lat/lon) from subzones.ts.
Outputs: subzones_cleaned.csv
- StartingPoint_DataPrep.Rmd
Merges PA_SZ.csv and subzones_cleaned.csv after validation.
Outputs: starting_point.csv
- Subzone_Boundaries_Dataset_Prep.Rmd
Cross-checks starting_point.csv with raw_subzones.geojson, adds missing values manually.
Outputs: starting_point_final.csv, updated subzone_boundaries.geojson
- Dist_Heuristic.Rmd
Calculates straight-line distances from each subzone to all healthcare facilities.
Outputs: heuristic_distance.csv, top5_healthcare.csv
- PlanningAreaDemographics.Rmd
Processes ResidentPopulationData.csv to get age distributions across planning areas.
Outputs: planning_area_demographics.csv
- CombineHealthCareQueries.Rmd
Combines and normalizes healthcare query outputs from the onemap_query/ folder.
Outputs: healthcareUpdatedQuery_onemap.csv, healthcareUpdatedQueryNormalised_onemap.csv

3. onemap_query/
Contains:
query-code.py: Python script for running OneMap API queries for healthcare facilities.
Multiple CSV output files, later combined using CombineHealthCareQueries.Rmd.

4. simulation_query/
Contains:
query.py: Python script used for simulation-related queries.
full-simulation.csv: Result of the simulation query.

5. prototype_final/
Final files used to run the Shiny prototype app:-
app.R and global.R: The Shiny application code.
Final datasets:
healthcare-final.csv
full-simulation.csv
subzone_boundaries.geojson
planning_area_demographics.csv