#------ . ------
#------ EXAMPLE CODE TO CREATE CUSTOM ATTRIBUTE-TABLE ENHANCED GEOTIFF ------
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

# Expected output
#   * `"GeoTIFF"` with associated (raster/value) attribute table
#   * Values extracted at a geographic location

# Note: The code to create a `"GeoTIFF"` with RAT may take several minutes.


#------ User input ------

#--- User selects spreadsheet to convert to GeoTIFF + RAT/VAT
#   * here, the example is for R&R under RCP4.5 for 2029-2064
#   * replace with your file name
user_fname_input_csv <- file.path(
  "..",
  "results",
  "newRR3_RATs-combined",
  "RCP45_2029-2064-clim_AcrMod_Values_RR",
  "RR__RCP45_2029-2064-clim__value-acrmod-med.csv"
)


#--- User selects mask
#   * possible values
#       * `"sim"` (all simulated areas, i.e., no mask)
#       * `"rangelands"` (masked to areas as in Schlaepfer et al.)
user_mask <- "rangelands"


#--- User selects geographic locations (to illustrate value extraction)
user_sites_wgs84 <- data.frame(
  longitude = c(-117.809, -115.398, -107.921),
  latitude = c(43.482, 41.660, 41.893)
)

#--- User selects variable (column in spreadsheet)
#   * here, the example is for continuous resilience
user_varname <- "Resilience_index_acrmoddistr"


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
    replacement = ".tif",
    x = basename(user_fname_input_csv)
  )
)


if (file.exists(user_fname_out_tif)) {
  warning(
    "User requested GeoTIFF already exists: ",
    shQuote(user_fname_out_tif),
    ". Skipping creation of GeoTIFF."
  )
}


#----- Set up ------
stopifnot(
  requireNamespace("newRR3"),
  requireNamespace("terra"),
  requireNamespace("sf"),
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

#--- Remove units outside of selected mask
imask <- grep(paste0("count_", user_mask), colnames(x), fixed = TRUE)
irows <- x[[imask]] > 0 & !is.na(x[[imask]])

rat <- x[irows, , drop = FALSE]

#--- Rename mask column and delete unused mask columns
colnames(rat)[imask] <- "count"

irmask <- grep("count_", colnames(rat), fixed = TRUE)
if (length(irmask) > 0) {
  rat <- rat[, -irmask, drop = FALSE]
}




#------ Create GeoTIFF with associated RAT/VAT ------
message("Creating a GeoTIFF with RAT may take several minutes ...")

x <- newRR3::make_geotiff_with_rat(
  rast_template = template_GeoTIFF,
  rat = rat,
  fname = user_fname_out_tif,
  names_rat = newRR3::extract_fname_element(user_fname_input_csv, pos = 1L)
)


#------ Extract values from locations ------
xloc <- sf::st_as_sf(
  x = user_sites_wgs84,
  coords = c("longitude", "latitude"),
  crs = "OGC:CRS84"
)

xlocp <- sf::st_transform(xloc, sf::st_crs(x))

# Extract values from the currently "active" variable
xloc_vals1 <- terra::extract(x, xlocp)
print(xloc_vals1)

# Change "active" variable and extract values
terra::activeCat(x) <- user_varname
xloc_vals2 <- terra::extract(x, xlocp)
print(xloc_vals2)

# Extract values for all available variables in RAT
xrat <- terra::cats(x)[[1L]]

x0 <- x
levels(x0) <- NULL
xloc_suid <- terra::extract(x0, xlocp)

ids <- match(xloc_suid[[2L]], xrat[["value"]], nomatch = 0L)

xloc_vals3 <- xrat[ids, , drop = FALSE]
print(xloc_vals3)

