# UpperWarrenSEM

This project takes capture-recapture data on four mammal species in the Upper Warren region of south-western Australia and makes predictions of abundance over space and time using state-space models. These predictions are then fed into a Structural Equation Modelling framework to identify the key drivers of population change for these four mammal species. Outputs from the SEMs are then used to undertake scenario analysis of potential drivers of future population change. 

## Scripts
There are five key scripts included in this repository that outline the full analysis. 
### 1_Create capture histories for State Space Models.R 
This script takes the capture history data for each mammal species and converts it into a data format appropriate for modelling in JAGS. 
The output data is located in Data_Clean/UW_uniquecapdata_transect_inputforSSM_16072021.Rdata

### 2_Run State Space Models in JAGS.R
This script fits a series of state-space models in JAGS for each of the four species. 
The output data is located in Data_Clean/transect_besstmodels_abundancesforSEM_23072021.Rdata

### 3_Compile environmental covariates for SEM.Rmd
This script extracts spatial and temporal environmental covariates used for modelling in the SEM. This script will not work as there is a large amount of spatial data required at this stage. 
The output data is located in Data_Clean/transect.sem.data.Nov2021.csv

### 4_Build and fit Structural Equation Models.Rmd
This script fits a series of Structural Equation Models and uses model selection to identify the best fitting SEM. 
The best fitting SEM is saved at Data_Clean/best.sem.fit.Rdata

### 5_Run SEM Scenario Analysis.Rmd
This script takes the best fitting SEM and uses it to undertake scenario analysis for a range of fire and rainfall scenarios, and saves the outputs as a plot. 