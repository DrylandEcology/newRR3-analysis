#----- . ------
#----- IMPORT EXTERNAL OBJECTS ------
#
#   * Variable descriptions: -> data-raw/prepared_manually/
#   * rSFSW2 simulation description: -> data-raw/scens/
#   * rSW2metrics output: -> data-raw/rsw2metrics/
#   * Random forest models that predict RR: `"data-raw/mrrrf/"`
#   * Template GeoTIFF (with simulation units as "value"): -> data-raw/gsu/
#
#----- . ------


#----- SET UP ------
stopifnot(requireNamespace("newRR3")) # e.g., devtools::load_all()


#--- Settings ------
req_meta_version <- "3.2.0"
method_to_import <- "copy" # c("symlink", "copy")


# 2023-Mar-22: combination based on simulated extent and rangeland mask as attribute
mask_sets <- c("sim", "rangelands")



#--- Paths (part 1) ------
dir_prj <- ".."
dir_wp <- file.path(dir_prj, "..", "..")

dir_dataraw <- file.path(dir_prj, "data-raw")
dir.create(dir_dataraw, recursive = TRUE, showWarnings = FALSE)

dir_data <- file.path(dir_prj, "data")
dir.create(dir_data, recursive = TRUE, showWarnings = FALSE)



#--- Check manual raw data ------
#  * Description tables for each variable set
stopifnot(dir.exists(file.path(dir_dataraw, "prepared_manually")))


#--- Import external data (part 1) ------
newRR3::import_scenarios_rsfsw2(
  dir_dataraw = dir_dataraw,
  dir_simexps = file.path(dir_wp, "1_SOILWAT2_Simulations"),
  tag_simexp = newRR3::get_project_description()[["simexps"]][["tags"]],
  method_to_import = method_to_import
)

newRR3::import_predictors_variablenames(
  dir_dataraw = dir_dataraw,
  dir_plotsim = file.path(dir_wp, "..", "Prod043b_newRR_PlotSimulations"),
  method_to_import = method_to_import
)


#------ Load project metadata ------
meta <- newRR3::get_project_description(
  include_variables = TRUE,
  include_scenarios = TRUE,
  dir_data = dir_dataraw
)
stopifnot(!is.null(meta) && meta[["v"]] == req_meta_version)


#------ . ------

#------ Import external data (part 2) ------

#--- * Import metrics ------
fnames_predictors_rsw2metrics <- newRR3::import_predictors_rsw2metrics(
  dir_dataraw = dir_dataraw,
  dir_extractmetrics = file.path(dir_wp, "2_CalculateDerivedMetrics"),
  metric_name = "RR2022predictors",
  list_simexps = meta[["simexps"]][["list"]],
  version_simdata = meta[["version_simdata"]],
  tag_simexp0 = meta[["simexps"]][["tag0"]],
  method_to_import = method_to_import
)


#--- * Import spatial masks ------
fnames_gsu_template <- lapply(
  mask_sets,
  function(mask) {
    newRR3::import_gsu_template(
      dir_dataraw = dir_dataraw,
      dir_mappingdata = file.path(dir_wp, "0_Inputs", "Data_for_Mapping"),
      mask_set = mask,
      version_inputs = meta[["version_inputs"]],
      method_to_import = method_to_import
    )
  }
) |>
  stats::setNames(nm = mask_sets)


#--- * Combine spatial masks ------
# Use full/simulated extent and provide other masks as attributes

fnames_gsu_template[["combined"]] <- gsub(
  pattern = "gsu_",
  replacement = "gsu_combined_",
  fnames_gsu_template[["sim"]]
)

for (kf in 1:2) {
  if (!file.exists(fnames_gsu_template[["combined"]][[kf]])) {
    file.copy(
      from = fnames_gsu_template[["sim"]][[kf]],
      to = fnames_gsu_template[["combined"]][[kf]],
      copy.mode = TRUE,
      copy.date = TRUE
    )
  }
}

if (!file.exists(fnames_gsu_template[["combined"]][[3L]])) {
  rat_mask_combined <- utils::read.csv(
    file = fnames_gsu_template[["sim"]][[3L]]
  )
  suids <- rat_mask_combined[["value"]]

  for (km in seq_along(mask_sets)[-1L]) {
    tmp <- utils::read.csv(file = fnames_gsu_template[[mask_sets[[km]]]][[3L]])
    rat_mask_combined <- merge(
      rat_mask_combined,
      tmp,
      by = "value",
      all.x = TRUE,
      suffixes = paste0("_", mask_sets[c(1L, km)])
    )

    stopifnot(rat_mask_combined[["value"]] == suids)
  }

  write.csv(
    rat_mask_combined,
    file = fnames_gsu_template[["combined"]][[3L]],
    row.names = FALSE
  )
}



#--- * Import RR random forests ------
# Required by
#   * Script02_RR2022predictors_Novelty.R
#   * Script03_RR2022_Prediction.R
fnames_mrrrf <- newRR3::import_predictrr_rfworkflows(
  dir_dataraw = dir_dataraw,
  dir_plotsim = file.path(dir_wp, "..", "Prod043b_newRR_PlotSimulations"),
  version_rf = meta[["version_rf"]],
  method_to_import = method_to_import
)



print(summary(warnings()))

#------. ------
#------. ------
