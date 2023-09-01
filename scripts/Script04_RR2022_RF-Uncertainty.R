#----- . ------
#----- RANDOM-FOREST PREDICTION UNCERTAINTY ------
#
# * Input
#   * Core spreadsheets with 19 predictors
#     (created by `"Script01_RR2022predictors_PrepareDataRelease.R"`):
#     `"results/newRR3_RATs-combined/*/Preds19__*.csv"`
#   * Spreadsheets with predicted R&R probabilities
#     (created by `"Script03_RR2022_Prediction.R"`):
#     `"results/newRR3_RATs-combined/*/RR__*.csv"`
#
# * Output
#   * Data with prediction uncertainty (`"RR-RFcertainty"`):
#     spreadsheets with values for individual runs (`"IndRuns"`):
#       n = 101 = 1 (ambient) + 5 (time periods x RCPs) * 20 (climate models)
#   * Output
#     * `results/newRR3_RATs-combined/`
#       `"[experiment]_[time]_IndRuns_Values_RR-RFcertainty"/`
#       `"RR-RFcertainty__[scenario-id]_[GCM]_[experiment]_[time]__value-sim.csv"`
#     * Variables: as is
#
#----- . ------


#----- SET UP ------
stopifnot(requireNamespace("newRR3"))


#--- Settings ------
req_meta_version <- "3.2.0"

vtag <- "rfcertainty"
vtag_input <- "rr"

# 2023-Mar-22: full/simulated extent: data; rangeland mask: as attribute and for analysis
mask_sets <- c("sim", "rangelands", "combined")[[3L]]

list_uncertainty_metrics <- c("excessp", "pmajority")


#--- Paths (part 1) ------
dir_prj <- ".."

dir_dataraw <- file.path(dir_prj, "data-raw")
stopifnot(dir.exists(dir_dataraw))

dir_res <- file.path(dir_prj, "results")
stopifnot(dir.exists(dir_res))




#------ Load project metadata ------
meta <- newRR3::get_project_description(
  include_variables = TRUE,
  include_scenarios = TRUE,
  dir_data = dir_dataraw
)
stopifnot(!is.null(meta) && meta[["v"]] == req_meta_version)

stopifnot(
  vtag %in% names(meta[["varsets"]][["tags"]]),
  vtag_input %in% names(meta[["varsets"]][["tags"]])
)


#------ . ------
#------ LOOP OVER SPATIAL MASKS ------
for (km in seq_along(mask_sets)) {
  #--- Paths (part 2) ------
  dir_data_rat <- file.path(
    dir_res,
    paste0("newRR3_RATs-", mask_sets[[km]])
  )
  stopifnot(file.exists(dir_data_rat))


  #----- . ------
  #--- SPREADSHEET RATs OF UNCERTAINTY ------

  #--- Input file names of predicted RR as RAT spreadsheets
  fns_in_ratcsv <- lapply(
    seq_along(meta[["simexps"]][["list"]]),
    function(k0) {
      file.path(
        dir_data_rat,
        newRR3::name_output_folder(
          meta[["simexps"]][["tag_scen_path"]][[k0]],
          "IndRuns",
          "Values",
          meta[["varsets"]][["tags"]][[vtag_input]]
        ),
        newRR3::name1_output_ratcsv(
          meta[["varsets"]][["tags"]][[vtag_input]],
          meta[["simexps"]][["tag_scen"]][[k0]],
          "value-sim"
        )
      )
    }
  )

  #--- Create output file names of RF-uncertainty as RAT spreadsheets
  fns_out_ratcsv <- lapply(
    seq_along(meta[["simexps"]][["list"]]),
    function(k0) {
      file.path(
        dir_data_rat,
        newRR3::name_output_folder(
          meta[["simexps"]][["tag_scen_path"]][[k0]],
          "IndRuns",
          "Values",
          meta[["varsets"]][["tags"]][[vtag]]
        ),
        newRR3::name1_output_ratcsv(
          meta[["varsets"]][["tags"]][[vtag]],
          meta[["simexps"]][["tag_scen"]][[k0]],
          "value-sim"
        )
      )
    }
  )


  pb <- utils::txtProgressBar(
    max = length(unlist(fns_out_ratcsv)),
    style = 3
  )
  kpb <- 0


  #--- There is work to be done ------
  if (!all(file.exists(unlist(fns_out_ratcsv)))) {

    #--- Create directories for results ------
    tmp <- lapply(
      unique(dirname(unlist(fns_out_ratcsv))),
      dir.create,
      recursive = TRUE,
      showWarnings = FALSE
    )


    #------ Loop over simulation parts (RCPs) ------
    for (k0 in seq_along(meta[["simexps"]][["list"]])) {

      if (!all(file.exists(fns_out_ratcsv[[k0]]))) {

        #--- * Loop over scenarios (GCMs x time periods) ------
        for (k1 in seq_along(meta[["simexps"]][["tag_scen"]][[k0]])) {

          # Check that we work on correct scenario and with correct files
          stopifnot(
            grepl(
              meta[["simexps"]][["tag_scen"]][[k0]][[k1]],
              basename(fns_in_ratcsv[[k0]][[k1]])
            ),
            grepl(
              meta[["simexps"]][["tag_scen"]][[k0]][[k1]],
              basename(fns_out_ratcsv[[k0]][[k1]])
            )
          )


          if (!file.exists(fns_out_ratcsv[[k0]][[k1]])) {

            #--- ** Load predicted RR for scenario ------
            xV <- utils::read.csv(fns_in_ratcsv[[k0]][[k1]])

            stopifnot(
              inherits(xV, "data.frame"),
              meta[["varsets"]][[vtag_input]][["varname"]] %in% colnames(xV)
            )


            #--- ** Calculate RF-uncertainty ------
            rat <- xV[, meta[["rat_header"]][[mask_sets[[km]]]], drop = FALSE]

            # Loop over resilience and resistance
            for (kr in seq_along(meta[["varsets"]][["rr"]][["rrs"]])) {
              rtag <- meta[["varsets"]][["rr"]][["rrs"]][[kr]]

              # Identify columns that contain probabilities
              tmp_vars <- grep(paste0(rtag, "_prob"), colnames(xV))

              rat[[paste0(rtag, "_pfirst")]] <- newRR3::calc_pfirst(
                xV[, tmp_vars, drop = FALSE]
              )

              rat[[paste0(rtag, "_excessp")]] <- newRR3::calc_excessp(
                xV[, tmp_vars, drop = FALSE]
              )
            }

            stopifnot(
              identical(nrow(rat), nrow(xV)),
              identical(
                rat[, meta[["rat_header"]][[mask_sets[[km]]]]],
                xV[, meta[["rat_header"]][[mask_sets[[km]]]]]
              ),
              meta[["varsets"]][[vtag]][["varname"]] %in% colnames(rat)
            )


            #--- ** Write RAT as spreadsheet ------
            write.csv(
              rat,
              file = fns_out_ratcsv[[k0]][[k1]],
              row.names = FALSE
            )
          }

          kpb <- kpb + 1
          utils::setTxtProgressBar(pb, kpb)
        }

      } else {
        kpb <- kpb + length(fns_out_ratcsv[[k0]])
        utils::setTxtProgressBar(pb, kpb)
      }
    }
  }

  close(pb)
}


print(summary(warnings()))

#------. ------
#------. ------
