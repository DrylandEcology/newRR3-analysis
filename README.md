[newRR3-Rpackage]: https://github.com/DrylandEcology/newRR3-Rpackage
[newRR3-analysis]: https://github.com/DrylandEcology/newRR3-analysis



# Analyses for the `"newRR"` Project
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.8310208.svg)](https://doi.org/10.5281/zenodo.8310208)

Contact: Daniel R Schlaepfer [ORCiD](https://orcid.org/0000-0001-9973-2065)


This repository contains code developed for the manuscript

**Declining ecological resilience and resistance indicators under climate change
in the sagebrush region, United States**

by Daniel R Schlaepfer, Jeanne C Chambers, Alexandra K Urza,
Brice B Hanberry, Jessi L Brown, David I Board, Steven B Campbell,
Karen J Clause, Brice B Hanberry, Alexandra K Urza, Michele R Crist,
and John B Bradford


Code to replicate analyses by Schlaepfer et al. requires
  * the R package `"newRR3"`
    (available from the repository [newRR3-Rpackage][])
  * R scripts utilizing functionality from `"newRR3"`
    (this repository)


The objective of this study was to understand where and why climate change
will alter the distribution of ecological resilience and resistance
in the sagebrush biome throughout the 21st century.
To accomplish this, the study pursued five specific objectives:
  1. Estimate the new R&R indicators under future climate conditions and
     quantified changes from historical conditions.
  1. Develop a continuous R&R index that integrates probability information
     from the underlying predictive R&R models.
  1. Assess (i) the robustness of projected changes in R&R to
     uncertainty in future climate conditions;
     (ii) certainty of the predictive R&R model; and
     (iii) novelty among predictors.
  1. Identify causes of projected changes in R&R.
  1. Examined how future projected R&R indicators relate to
     recently-defined geographic patterns of ecological integrity
     across the sagebrush biome.


<br>

## Table of contents

1. [Setup](#setup)
1. [Organization](#organization)
1. [Data](#data)
1. [Example code](#example)
1. [Folder and file name conventions](#naming)
1. [Workflow overview](#overview)


<br>
<a name="setup"></a>

## Setup and replication of analysis

Replication of results and display items by Schlaepfer et al.
requires the following steps

1. Install the R package `"newRR3"` (and its dependencies)
  ```{r}
  remotes::install_github("DrylandEcology/newRR3-Rpackage")
  ```

2. Download/clone R scripts and associated metadata (this repository)
  ```{sh}
  git clone https://github.com/DrylandEcology/newRR3-analysis.git
  ```

3. Download data from this project, unzip, and move to appropriate folder
  Data are available as `"csv"`-spreadsheets and a base `"GeoTIFF"`
  from the USGS ScienceBase-Catalog
  ([DOI 10.5066/XXX](https://doi.org/10.5066/XXX))
  ```{r}
  cd newRR3-analysis/
  mkdir -p data-raw/gsu/
  mkdir -p results/newRR3_RATs-combined/

  XXX
  ```

4. Run the script that carries out the entire workflow (see code comments)
  ```{sh}
  cd scripts/
  ./run_entire_workflow.sh
  ```



<br>
<a name="organization"></a>

## Organization of `"newRR3-analysis"`

This repository contains the following elements

  * `data-raw/`
    contains the "raw" (input) data necessary to run the workflow
    * `gsu/`
      (manual data release download, see step `"3"` of section [Setup](#setup))
      contains the base `"GeoTIFF"` files that map the c. 100,000 study units
      to the base 30-meter raster; used for creating maps
    * `mrrrf/`
      (data downloaded by scripts)
      contains names of predictor variables and script will download
      the predictive R&R models from a data release
      ([Chambers et al. 2023](https://doi.org/10.5061/DRYAD.H18931ZPB))
    * `prepared_manually/` contains spreadsheet with variable names
    * `rsw2metrics/`
      (folder generated by scripts) not required with
      manual data release download (see step `"3"` of section [Setup](#setup))
    * `scens/`
      contains spreadsheet with climate conditions
    * `spatialunits/`
      (folder generated by scripts; data downloaded by scripts)
      contains spatial data downloaded by scripts;
      used for spatial intersections with R&R results

  * `data/`
    (folder, sub-folders, and content generated by scripts)
    contains intermediate data objects

  * `examples/` (see also section [Example code](#example))
    example R code that illustrates on how to combine the base `"GeoTIFF"` and
    a spreadsheet from `newRR3_RATs-combined/` into
      * a new `"GeoTIFF"` with raster attribute table (RAT) /
        value attribute table (VAT); or
      * a new `"GeoTIFF"` containing values of one variable

  * `README.md`
    this file

  * `results/`
    contains output formatted as `"csv"`-spreadsheets and `"GeoTIFF"`s
    generated by scripts
    * `newRR3_GeoTIFFs-sim/`
      contains `"GeoTIFF"` generated by scripts
      (alternatively, manual data release download, see section [Data](#data))
    * `newRR3_RATs-combined/`
      (manual data release download, see step `"3"` of section [Setup](#setup))
    * `SpatialUnits_Tabulations/`
      (folder, sub-folders, and content generated by scripts)

  * `displayitems/`
    (folder, sub-folders, and content generated by scripts)
    contains figures and tables generated by scripts

  * `scripts/`
    contains a shell script and R scripts that run the analysis
    (see section [Workflow overview](#overview))


<br>
<a name="data"></a>

## Data

  This analysis produced results which were released in two separate
  data releases

  * Schlaepfer et al. XXX is available from the USGS ScienceBase-Catalog
    ([DOI 10.5066/XXX](https://doi.org/10.5066/XXX)) and consists of
    `"GeoTIFF"`s containing `"csv"`-spreadsheets and a base `"GeoTIFF"`
    (see step `"3"` of section [Setup](#setup)).
      * The base `"GeoTIFF"` are inputs to the scripts of this repository and
        are located in `data-raw/gsu/`.
      * The `"csv"`-spreadsheets are located in `results/newRR3_RATs-combined/`;
        the predictor values are inputs to the scripts and all the others are
        outputs of these scripts.

  * Schlaepfer et al. XXX is available from the USGS ScienceBase-Catalog
    ([DOI 10.5066/XXX](https://doi.org/10.5066/XXX)) and consists of
    `"GeoTIFF"`s containing resulting `"R&R"` variables.
    These correspond to what this repository's scripts output in
    `results/newRR3_GeoTIFFs-sim/`.


  The workflow requires additional data which are downloaded and installed
  by the scripts during runtime including

  * Chambers et al. (2023) data archive from Dryad
    (https://doi.org/10.5061/DRYAD.H18931ZPB)
  * Sagebrush ecological integrity (Doherty et al., 2022) data release from
    ScienceBase-Catalog (https://doi.org/10.5066/P94Y5CDV)


<br>
<a name="example"></a>

## Example R code

Example R code (`"examples/"`) illustrates on how to use the spreadsheets
from the data release (`"results/newRR3_RATs-combined/"`)
  * create a new `"GeoTIFF"` with raster attribute table (RAT) /
    value attribute table (VAT) and extract values at locations; or
  * create a new single-band `"GeoTIFF"` containing values of one variable

The code examples assumes that
  * the R working directory is at `"examples/"`
  * the data release has been downloaded
    (see step `"3"` of section [Setup](#setup))
    * base/template `"GeoTIFFs` from the data release have been moved to
      `"raw-data/gsu/"`
    * spreadsheets from the data release have been moved to
      `"results/newRR3_RATs-combined/"`

The script `"scripts/Script20_newRR3_DataReleaseGeoTIFFsFromRATs.R"`
provides additional details.



<br>
<a name="naming"></a>

## Folder and file name conventions

Folder and file names follow specific templates with defined terminology that
provide sufficient information to interpret the data contained in the
folders and files.

This approach is inspired by `"CMIP6"`, e.g., their document on
[`"CMIP6 Global Attributes, DRS, Filenames, Directory Structure, and CV’s"`](https://goo.gl/v1drZl).

All strings appearing in the folder and file name segments are constructed
using only the following characters:  a-z, A-Z, 0-9, and the hyphen ("-").
Underscores are prohibited throughout except as shown in the templates
(and underscores, one at a time, may appear in `<variable-name>`,
e.g., `"Tmean_mean"`, `"CWD_mon_corr_temp_mean"`).


### Vocabulary of name segments

See also `?newRR3::output_names` and
`meta <- newRR3::get_project_description(...)`.


  * `<experiment>` represents the `CMIP5` climate experiment/emission scenario
    (or ambient climate conditions;
    see also `meta[["simexps"]][c("tag_scen1", "list")]`);
    * possible values: `"ambient"`, `"historical"`, `"RCP45"`, `"RCP85"`

  * `<scenario-id>_<GCM>` represents our scenario identification number and
    global climate model name
    (see also `meta[["simexps"]][["tag_scen"]]`);
    * possible values:
      * `<scenario-id>`: `sc1..sc41`
      * `<GCM>`: `"bcc-csm1-1"`, `"bcc-csm1-1-m"`, `"BNU-ESM"`, `"CanESM2"`,
        `"CCSM4"`, `"CNRM-CM5"`, `"CSIRO-Mk3-6-0"`, `"GFDL-ESM2G"`,
        `"GFDL-ESM2M"`, `"HadGEM2-CC365"`, `"HadGEM2-ES365"`, `"inmcm4"`,
        `"IPSL-CM5A-LR"`, `"IPSL-CM5A-MR"`, `"IPSL-CM5B-LR"`, `"MIROC-ESM"`,
        `"MIROC-ESM-CHEM"`, `"MIROC5"`, `"MRI-CGCM3"`, `"NorESM1-M"`

  * `<climate-desc>` represents the following template
    `[<scenario-id>_<GCM>_]<experiment>`
    where the sub-segment `<scenario-id>_<GCM>` is used only when appropriate,
    i.e., the unit refers to individual model runs
    (see `<output-set-dir>` and `<output-set-file>`).

  * `<time>` represents the time period (first to last calendar year) and
    type of temporal data (climatology across years `"clim"` or time series)
    of the form `YYYY-YYYY(-clim)`
    (see also `meta[["simexps"]][c("tag_scen1", "tag_acrmod")]`);
    * possible values: `"1980-2020-clim"`, `"1950-2005-clim"`, `"2029-2064-clim"`,
    `"2064-2099-clim"`

  * `<output-set-dir>` indicates whether a subfolder contains files for
    individual model runs (`"IndRuns"`) or
    for across-model summaries (`"AcrMod"`);
    * possible values: `"IndRuns"`, `"AcrMod"`

  * `<output-set-file>` indicates whether a file contains
    individual model runs (`"sim"`) or across-model summaries (`"acrmod"`);
    * possible values: `"sim"`, `"acrmod"`

  * `<value-type-dir>` indicates whether a subfolder contains files with
    variables that represent the indicated conditions (`"Values"`) or
    variables that represent change between indicated conditions and the
    historical reference states (`"Deltas"`);
    * possible values: `"Values"`, `"Deltas"`

  * `<value-type-file>` indicates whether a file contains
    variables that represent the indicated conditions (`"value"`) or
    variables that represent one of the types of change between
    indicated conditions and the historical reference states (`"*delta*"`);
    * possible values:
      * `"value"`
      * `"delta"`
      * `"robust090delta"`: delta with a robust signal at 90% consistency among
        climate projections
      * `"simpledelta"`: delta simplified to three categories corresponding to
        an increase, no change, or a decrease
      * `"robust090simpledelta"`: robust simplified deltas

  * `<variable-set>` represents sets of related variables
    (see also `meta[["varsets"]][["tags"]]`);
    * possible values:
      * `"RR"` (ecological resilience and resistance)
      * `"Preds19"` (19 predictors of `"R&R"`)
      * `"Preds19-Novelty"` (novelty among predictors)
      * `"RR-RFcertainty"` (un/certainty in predictive `"R&R"` random forest models)
      * `"MIRRP"` (most impactful `"R&R"` predictors)

  * `<acrmod-summary>` represents the summary applied across-models
    (see also `meta[["acrmod"]]`);
    * possible values:
      * `"low"`, `"med"`, `"high"`: 10%, 50%, and 90% percentile across models
      * `"agree"`: fraction of models that agree with the sign of median change
      * `"robust090"`: > 90% of models agree with the sign of median change

  * `<method-description>` is short for the following template
    `<value-type-file>-<output-set-file>[-<acrmod-summary>]`
    where the sub-segment `<acrmod-summary>` is used only when appropriate,
    i.e., the method utilized an across-model summary.
    * utilized values:
      * `"(value)|(delta)-sim"`
      * `"(value)|(delta)|(simpledelta)-acrmod-(low)|(med)|(high)"`
      * `"robust090delta-acrmod-med"`

  * `<acrgeo-summary>` represents the summary applied across-geographic space;
    * possible values: `"argeo-<spatial unit>"`
      where `<spatial unit>` is the name of the spatial unit across which
      the summary was calculated

  * `<variable-name>` represents variable names as listed in spreadsheets at
    `"data-raw/prepared_manually/Description_<variable-set>.csv"`
    (see also `meta[["varsets"]]`)

  * `<var-cat-level>` represents a level of a categorical variable
    (see also `?newRR3::get_levels` and `?newRR3::is_categorical`)



### Name template of subfolders and files in `"results/"`

* The names of subfolders in
  `"results/newRR3_GeoTIFFs-sim/"`,
  `"results/newRR3_RATs-combined/"`, and
  `"results/SpatialUnits_Tabulations"`
  are constructed consistent with the following template:

      `"<experiment>_<time>_<output-set-dir>_<value-type-dir>_<variable-set>/"`


* The names of files in `"results/newRR3_GeoTIFFs-sim/"` and
  `"results/newRR3_RATs-combined/"`
  are constructed consistent with the following template:

      `"<variable-set>__<climate-desc>_<time>__<method-description>[__<variable-name>].(tif)|(csv)"`

  where the segment `<variable-name>` is only used when appropriate, e.g.,
  single-band `"GeoTIFF"`s.


* The names of files in `"results/SpatialUnits_Tabulations/"`
  are constructed consistent with the following template:

      `"<variable-set>__<climate-desc>_<time>__<method-description>__<acrgeo-summary>__<variable-name>.csv"`


* Examples
  * `"ambient_1980-2020-clim_IndRuns_Values_RR/"`
  * `"RCP45_2029-2064-clim_AcrMod_Deltas_Preds19/"`
  * `"RR__sc1_NA_ambient_1980-2020-clim__value-sim.csv"`
  * `"Preds19__RCP45_2064-2099-clim__delta-acrmod-med.csv"`
  * `"RR__RCP85_2029-2064-clim__robust090delta-acrmod-med__Resilience_response.tif"`
  * `"Tabulation__RR__RCP45_2029-2064-clim__delta-acrmod-high__acrgeo-ecotype__Resistance_index.csv"`
  * `"Tabulation__RR__RCP45_2029-2064-clim__delta-acrmod-high__acrgeo-ecotype__Resilience_prob.pred_L.csv"`



<br>
<a name="overview"></a>

## Workflow overview

  * `"Script00_newRR3_Imports.R"`
    * Purpose: import external objects
    * output: `"data-raw/"`


  * `"Script01_RR2022predictors_PrepareData.R"`
    * Purpose: prepare `"rSW2metrics"` extraction as 19 predictors

    * Note: skipped if output is obtained from data release
      (see step `"3"` of section [Setup](#setup))

    * Input
      * rSFSW2 simulation description: `"data-raw/scens/"`
      * Variable descriptions: `"data-raw/prepared_manually/"`
      * Template GeoTIFF: `"data-raw/gsu/"`
      * rSW2metrics output: `"data-raw/rsw2metrics/"`

    * Output
      * Core data for 19 predictors (`"Preds19"`):
        spreadsheets with values for individual runs (`"IndRuns"`):
          n = 101 = 1 (ambient) + 5 (time periods x RCPs) * 20 (climate models)
      * Output
        * `results/newRR3_RATs-combined/`
          `"[experiment]_[time]_IndRuns_Values_Preds19"/`
          `"Preds19__[scenario-id]_[GCM]_[experiment]_[time]__value-sim.csv"`
        * Variables: as is


  * `"Script02_RR2022predictors_Novelty.R"`
    * Purpose: calculate predictor novelty

    * Input
      * Core spreadsheets with 19 predictors
        (created by `"Script01_RR2022predictors_PrepareDataRelease.R"`):
        `"results/newRR3_RATs-combined/*/Preds19__*.csv"`
      * Training data used to fit random forest models that predict RR:
        `"data-raw/mrrrf/"`

    * Output
      * Data with predictor novelty (`"Preds19-Novelty"`):
        spreadsheets with values for individual runs (`"IndRuns"`):
          n = 101 = 1 (ambient) + 5 (time periods x RCPs) * 20 (climate models)
      * Output
        * `results/newRR3_RATs-combined/`
          `"[experiment]_[time]_IndRuns_Values_Preds19-Novelty"/`
          `"Preds19-Novelty__[scenario-id]_[GCM]_[experiment]_[time]__value-sim.csv"`
        * Variables: as is


  * `"Script03_RR2022_Prediction.R"`
    * Purpose: predict `"R&R"` indicators

    * Input
      * Core spreadsheets with 19 predictors
        (created by `"Script01_RR2022predictors_PrepareDataRelease.R"`):
        `"results/newRR3_RATs-combined/*/Preds19__*.csv"`
      * Random forest models that predict RR: `"data-raw/mrrrf/"`

    * Output
      * Data with predicted R&R (`"RR"`):
        spreadsheets with values for individual runs (`"IndRuns"`):
          n = 101 = 1 (ambient) + 5 (time periods x RCPs) * 20 (climate models)
      * Output
        * `results/newRR3_RATs-combined/`
          `"[experiment]_[time]_IndRuns_Values_RR"/`
          `"RR__[scenario-id]_[GCM]_[experiment]_[time]__value-sim.csv"`
        * Variables: as is


  * `"Script04_RR2022_RF-Uncertainty.R"`
    * Purpose: calculate random forest prediction uncertainty

    * Input
      * Core spreadsheets with 19 predictors
        (created by `"Script01_RR2022predictors_PrepareDataRelease.R"`):
        `"results/newRR3_RATs-combined/*/Preds19__*.csv"`
      * Spreadsheets with predicted R&R probabilities
        (created by `"Script03_RR2022_Prediction.R"`):
        `"results/newRR3_RATs-combined/*/RR__*.csv"`

    * Output
      * Data with prediction uncertainty (`"RR-RFcertainty"`):
        spreadsheets with values for individual runs (`"IndRuns"`):
          n = 101 = 1 (ambient) + 5 (time periods x RCPs) * 20 (climate models)
      * Output
        * `results/newRR3_RATs-combined/`
          `"[experiment]_[time]_IndRuns_Values_RR-RFcertainty"/`
          `"RR-RFcertainty__[scenario-id]_[GCM]_[experiment]_[time]__value-sim.csv"`
        * Variables: as is


  * `"Script05_RR2022_MostImportantPredictors.R"`
    * Purpose: calculate most impactful predictors of change in `"R&R"`

    * Input
      * Core spreadsheets with 19 predictors
        (created by `"Script01_RR2022predictors_PrepareDataRelease.R"`):
        `"results/newRR3_RATs-combined/*/Preds19__*.csv"`
      * Random forest models that predict RR: `"data-raw/mrrrf/"`

    * Output
      * Data with most-important RR-predictor (`"MIRRP"`)
        against historical reference:
        spreadsheets with values for individual runs (`"IndRuns"`):
          n = 80 = 4 (time periods x RCPs) * 20 (climate models)
      * Output
        * `results/newRR3_RATs-combined/`
          `"[experiment]_[time]_IndRuns_Values_MIRRP"/`
          `"MIRRP__[scenario-id]_[GCM]_[experiment]_[time]__value-sim.csv"`
        * Variables: `"Resilience_mirrp"`, `"Resistance_mirrp"`


  * `"Script11_newRR3_DeltasIndividualModels.R"`
    * Purpose: calculate deltas, i.e.,
      change between future and historical climate projections

    * Input
      * Data created by scripts
        `"Script01_RR2022predictors_PrepareDataRelease.R"`, ...,
        `"Script04_RR2022_RF-Uncertainty.R"`:
        `"results/newRR3_RATs-combined/*_IndRuns_Values_*/"`

    * Output
      * Derived data for deltas:
        spreadsheets with deltas for individual runs (`"IndRuns"`):
          n = 80 = 4 (future time periods x RCPs) * 20 (GCMs)
      * Output
        * `results/newRR3_RATs-combined/`
          `"[experiment]_[time]_IndRuns_Deltas_[variable-set]"/`
          `"[variable-set]__[scenario-id]_[GCM]_[experiment]_[time]__delta-sim.csv"`
        * Variable names: `"*_delta"`


  * `"Script12_newRR3_SummaryValuesAcrossModels.R"`
    * Purpose: calculate summaries (low, median, and high values)
      across climate model projections

    * Input
      * Data created by scripts
        `"Script01_RR2022predictors_PrepareDataRelease.R"`, ...,
        `"Script04_RR2022_RF-Uncertainty.R"`:
        `"results/newRR3_RATs-combined/*_IndRuns_Values_*/"`
      * Deltas are excluded (see `"Script13_newRR3_DeltasAcrossModels.R"`)

    * Output
      * Derived data for summaries/ensembles across climate models:
        low, median, high values
      * Output
        * `results/newRR3_RATs-combined/`
          `"[experiment]_[time]_AcrMod_Values_[variable-set]"/`
          `"[variable-set]__[experiment]_[time]__value-acrmod-(low)|(med)|(high).csv"`
        * Variable names: `"*_acrmoddistr"`


  * `"Script13_newRR3_DeltasAcrossModels.R"`
    * Purpose: calculate deltas, i.e.,
      change between future and historical climate projections,
      of across-model summaries (low, median, and high values)

    * Input
      * Data created by `"Script12_newRR3_SummaryValuesAcrossModels.R"`:
        `"results/newRR3_RATs-combined/*_AcrMod_Values_*"`

    * Output
      * Derived data for summaries/ensembles across climate models:
        low, median, high values
      * Output
        * `results/newRR3_RATs-combined/`
          `"[experiment]_[time]_AcrMod_Deltas_[variable-set]"/`
          `"[variable-set]__[experiment]_[time]__delta-acrmod-[acrmod-summary].csv"`
        * Variable names: `"*_delta_acrmoddistr"`


  * `"Script14_newRR3_AgreementDeltasAcrossModels.R"`
    * Purpose: calculate agreement/consistency and robustness of deltas

    * Input
      * Data created by scripts
        `"Script11_newRR3_DeltasIndividualModels.R"` and
        `"Script13_newRR3_DeltasAcrossModels.R"`:
        `"results/newRR3_RATs-combined/*_Deltas_*/"`

    * Output
      * Derived data for robustness and agreement of deltas of
        summaries/ensembles across climate models at median levels
      * Output
        * `"results/newRR3_RATs-combined/"`
          `"[experiment]_[time]_AcrMod_Deltas_[variable-set]"/`
          `"[variable-set]__[experiment]_[time]__delta-acrmod-(agree)|(robust090).csv"`
        * Variable names: `"*_delta_acrmod(agree)|(robust)"`


  * `"Script15_newRR3_RobustifyCategoricalDeltasAcrossModels.R"`
    * Purpose: identify robust across-model deltas of `"R&R"`

    * Input
      * Delta `"R&R"` across-model median created by
        `"Script12_newRR3_SummaryAcrossModels.R"`:
        `"results/newRR3_RATs-combined/*/RR__*__delta-acrmod-med.csv"`
      * Robustness of delta `"R&R"` across-model median created by
        `"Script14_newRR3_AgreementDeltasAcrossModels.R"`:
        `"results/newRR3_RATs-combined/*/RR__*__delta-acrmod-robust090.csv"`

    * Output
      * Derived data for robust delta summaries/ensembles across climate models:
        `"robust*delta"`

      * Input
        * Across-model deltas `"R&R"` created by
          `"Script12_newRR3_SummaryAcrossModels.R"`:
          `"results/newRR3_RATs-combined/*/RR__*__delta-acrmod-(low)|(med)|(high).csv"`
        * Robustified across-model deltas `"R&R"` created by
          `"Script15_newRR3_RobustifyCategoricalDeltasAcrossModels.R"`:
          `"results/newRR3_RATs-combined/*/RR__*__robust090delta-acrmod-med.csv"`

      * Output
        * Derived data for simplified delta summaries/ensembles across climate models:
          `"*simpledelta"`
        * Output
          * `"results/newRR3_RATs-combined/"`
            `"[experiment]_[time]_AcrMod_Deltas_RR"/`
            `"RR__[experiment]_[time]__*delta-acrmod-(low)|(med)|(high).csv"`
          * Variable names: `"*_*simpledelta_acrmoddistr"`


  * `"Script16_newRR3_SimplifyCategoricalDeltasAcrossModels.R"`
    * Purpose: simplify across-model deltas of `"R&R"`
      (increase, stable, decrease)

    * Input
      * Across-model deltas `"R&R"` created by
        `"Script12_newRR3_SummaryAcrossModels.R"`:
        `"results/newRR3_RATs-combined/*/RR__*__delta-acrmod-(low)|(med)|(high).csv"`
      * Robustified across-model deltas `"R&R"` created by
        `"Script15_newRR3_RobustifyCategoricalDeltasAcrossModels.R"`:
        `"results/newRR3_RATs-combined/*/RR__*__robust090delta-acrmod-med.csv"`

    * Output
      * Derived data for simplified delta summaries/ensembles across climate models:
        `"*simpledelta"`
      * Output
        * `"results/newRR3_RATs-combined/"`
          `"[experiment]_[time]_AcrMod_Deltas_RR"/`
          `"RR__[experiment]_[time]__*delta-acrmod-(low)|(med)|(high).csv"`
        * Variable names: `"*_*simpledelta_acrmoddistr"`


  * `"Script20_newRR3_DataReleaseGeoTIFFsFromRATs.R"`
    * Purpose: create `"GeoTIFFs"` from spreadsheets

    * Input
      * Any spreadsheet (used as raster attribute table) created by any of the
        scripts: `"results/newRR3_RATs-combined/*/*.csv"`

    * Output
      * `"results/newRR3_GeoTIFFs-combined/"`
        `"[experiment]_[time]_[output-set-dir]_[value-type-dir]_[variable-set]/"`
        `"[variable-set]__[climate-desc]_[time]__[method-description][__[variable-name]].tif"`


  * `"Script30_newRR3_PrepareGeographicalUnits.R"`
    * Purpose: download and prepare geographical units for spatial intersections

    * Input
      * Template GeoTIFF: `"data-raw/gsu/"`
      * Download and import spatial units of interest
        (requested by specifying elements of `list_spatialunits` and
        `list_spatialunits_combinations`): `"data-raw/spatialunits/"`

    * Output
      * Geopackage containing all spatial intersections:
        `"data/SpatialUnits_Tabulations/*.gpkg"`
      * Tables of pixel counts for each spatial intersection:
        `"data/SpatialUnits_Tabulations/"`


  * `"Script31_newRR3_SummaryAcrossGeographicalUnits.R"`
    * Purpose: calculate summaries (tabulations) across geographic units

    * Input
      * All data created by scripts
        `"Script01_RR2022predictors_PrepareDataRelease.R"`,
        ...,
        `"Script16_newRR3_SimplifyCategoricalDeltasAcrossModels.R"`:
        `"results/newRR3_RATs-combined/*/*.csv"`
      * Tables of pixel counts for each spatial intersection,
       created by `"Script21_newRR3_PrepareGeographicalUnits.R"`

    * Output
      * Spreadsheets with summary values across geographical units
        * `"results/SpatialUnits_Tabulations/Tabulations_bs-rangelands/"`
          `"[experiment]_[time]_[output-set-dir]_[value-type-dir]_[variable-set]/"`
          `"[variable-set]__[climate-desc]_[time]__[method-description]__[acrgeo-summary]__[variable-name].csv"`
        * Variable names
          * continuous (numeric): low = `"X.5."`, median = `"X50."`, high = `"X95."`
          * categorical: counts for each levels, e.g., `"L"`, `"ML"`, `"M"`, `"H+MH"`


  * `"Script50_newRR3_DisplayOverviewItems.R"`
    * Purpose: create overview display items

    * Output: `"displayitems/Overview_supplementary_*/"`


  * `"Script51_newRR3_DisplayMaps.R"`
    * Purpose: create maps for manuscript and appendix

    * Output:
      * `"displayitems/Maps_manuscript_*/"`
      * `"displayitems/Maps_supplementary_*/"`


  * `"Script52_newRR3_DisplaySpatialStackedBarPlots.R"`
    * Purpose: create stacked bar plots of proportion of areas

    * Output: `"displayitems/StackedBars-SpatialUnitTabulations/"`


  * `"Script53_newRR3_DisplayAndResultTables.R"`
    * Purpose: create tables and in-text values of spatial summaries

    * Output: `"displayitems/Tables-SpatialUnitTabulations/"`
