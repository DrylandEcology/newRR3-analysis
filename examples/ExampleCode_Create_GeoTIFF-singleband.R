#------ . ------
#------ EXAMPLE CODE TO CREATE CUSTOM SINGLEBAND GEOTIFF ------
#------ . ------

# The example code assumes
#   * working directory is at `"newRR3-analysis/examples/"`
#   * the data release has been downloaded
#   * spreadsheets from the data release have been moved to
#     `"newRR3-analysis/results/newRR3_RATs-combined/"`
#   * base/template `"GeoTIFFs` from the data release have been moved to
#     `"newRR3-analysis/raw-data/gsu/"`
#   * the associated R package `"newRR3"` (and dependencies) is installed
#     (install with `remotes::install_github("DrylandEcology/newRR3-Rpackage")`)

# Script `"scripts/Script20_newRR3_DataReleaseGeoTIFFsFromRATs.R"`
# provides additional details (see https://doi.org/10.5281/zenodo.8310208).

# Expected output: masked, singleband `"GeoTIFF"`

# Note: The code to create a catalyzed `"GeoTIFF"` may take many hours.


#------ User input ------

#--- User selects spreadsheet with variable to convert to singleband GeoTIFF
#   * here, the example is for R&R categories under RCP4.5 for 2029-2064
#   * replace with your file name
user_fname_input_csv <- file.path(
  "..",
  "results",
  "newRR3_RATs-combined",
  "RCP45_2029-2064-clim_AcrMod_Values_RR",
  "RR__RCP45_2029-2064-clim__value-acrmod-med.csv"
)

#--- User selects variable (column in spreadsheet)
#   * here, the example is for resilience categories
user_varname <- "Resilience_response_acrmoddistr"


#--- User selects mask
#   * possible values
#       * `"sim"` (all simulated areas, i.e., no mask)
#       * `"rangelands"` (masked to areas as in Schlaepfer et al.)
user_mask <- "rangelands"

#------ End of user input ------



#--- Paths ------
dir_here <- "."

dir_gtbase <- file.path(dir_here, "..", "data-raw", "gsu")

dir_tmp <- file.path(dir_here, "tmp_examples")
dir.create(dir_tmp, recursive = TRUE, showWarnings = FALSE)

user_fname_out_tif <- file.path(
  dir_tmp,
  sub(
    pattern = ".csv$",
    replacement = paste0("__", user_varname, ".tif"),
    x = basename(user_fname_input_csv)
  )
)


if (file.exists(user_fname_out_tif)) {
  stop("User requested GeoTIFF already exists: ", shQuote(user_fname_out_tif))
}


#----- Set up ------
stopifnot(
  requireNamespace("newRR3"),
  requireNamespace("terra"),
  file.exists(dir_gtbase),
  user_mask %in% c("sim", "rangelands")
)


#------ Load template GeoTIFF ------
template_GeoTIFF <- terra::rast(
  file.path(dir_gtbase, "gsu_masked_v20220314.tif")
)


#------ Load requested spreadsheet ------
if (!file.exists(user_fname_input_csv)) {
  stop("User provided file does not exist: ", shQuote(user_fname_input_csv))
}

x <- utils::read.csv(file = user_fname_input_csv)


#------ Create RAT (raster/value attribute table) ------
if (!user_varname %in% colnames(x)) {
  stop("User provided variable does not exist: ", shQuote(user_varname))
}

#--- Remove units outside of selected mask
imask <- grep(paste0("count_", user_mask), colnames(x), fixed = TRUE)
irows <- x[[imask]] > 0 & !is.na(x[[imask]])

#--- Select value and variable columns
icols <- c("value", user_varname)

rat <- x[irows, icols, drop = FALSE]



#------ Create numerical (catalyzed) GeoTIFF ------
message("Creating ", shQuote(user_fname_out_tif))
message("Creating this catalyzed GeoTIFF may take many hours ...")

newRR3::make_geotiff_catalyzed(
  rast_template = template_GeoTIFF,
  rat = rat,
  fname = user_fname_out_tif,
  names_rat = newRR3::extract_fname_element(user_fname_input_csv, pos = 1L)
)
