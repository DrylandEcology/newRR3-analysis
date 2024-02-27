#!/bin/sh


#------ Import external objects ------
# -> data-raw/
Rscript --verbose Script00_newRR3_Imports.R


#------ Create core data items ------

# -> prepare `"rSW2metrics"` extraction as 19 predictors
Rscript --verbose Script01_RR2022predictors_PrepareData.R


#------ Create derived data items ------

# -> calculate predictor novelty
Rscript --verbose Script02_RR2022predictors_Novelty.R

# -> predict `"R&R"` indicators
Rscript --verbose Script03_RR2022_Prediction.R

# -> calculate random forest prediction uncertainty
Rscript --verbose Script04_RR2022_RF-Uncertainty.R

# -> calculate most impactful predictors of change in `"R&R"`
Rscript --verbose Script05_RR2022_MostImportantPredictors.R

# -> calculate deltas
Rscript --verbose Script11_newRR3_DeltasIndividualModels.R

# -> calculate summaries across climate model projections
Rscript --verbose Script12_newRR3_SummaryValuesAcrossModels.R

# -> calculate deltas of across-model summaries
Rscript --verbose Script13_newRR3_DeltasAcrossModels.R

# -> calculate agreement/consistency and robustness of deltas
Rscript --verbose Script14_newRR3_AgreementDeltasAcrossModels.R

# -> identify robust across-model deltas of `"R&R"`
Rscript --verbose Script15_newRR3_RobustifyCategoricalDeltasAcrossModels.R

# -> simplify across-model deltas of `"R&R"`
Rscript --verbose Script16_newRR3_SimplifyCategoricalDeltasAcrossModels.R


#------ Create multi-/single-band GeoTIFFs/COGs from RATs ------

# -> results/newRR3_GeoTIFFs-sim/
#Rscript --verbose Script20_newRR3_DataReleaseGeoTIFFsFromRATs.R

# -> results/newRR3_COGs-sim/
#Rscript --verbose Script21_newRR3_DataReleaseResaveRenameGeoTIFFsToCOGs.R


#------ Tabulate spatial summaries ------

# -> download and prepare geographical units for spatial intersections
Rscript --verbose Script30_newRR3_PrepareGeographicalUnits.R

# -> calculate summaries (tabulations) across geographic units
Rscript --verbose Script31_newRR3_SummaryAcrossGeographicalUnits.R


#------ Create display items ------

# -> create overview display items
Rscript --verbose Script50_newRR3_DisplayOverviewItems.R

# -> create maps for manuscript and appendix
Rscript --verbose Script51_newRR3_DisplayMaps.R

# -> create stacked bar plots of proportion of areas
Rscript --verbose Script52_newRR3_DisplaySpatialStackedBarPlots.R

# -> create tables and in-text values of spatial summaries
Rscript --verbose Script53_newRR3_DisplayAndResultTables.R
