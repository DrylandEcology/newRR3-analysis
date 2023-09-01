#----- . ------
#----- PREPARE rSW2metrics EXTRACTION AS 19 PREDICTORS OF NEWRR ------
#
# * Input
#   * rSFSW2 simulation description: `"data-raw/scens/"`
#   * Variable descriptions: `"data-raw/prepared_manually/"`
#   * Template GeoTIFF: `"data-raw/gsu/"`
#   * rSW2metrics output: `"data-raw/rsw2metrics/"`
#
# * Output
#   * Core data for 19 predictors (`"Preds19"`):
#     spreadsheets with values for individual runs (`"IndRuns"`):
#       n = 101 = 1 (ambient) + 5 (time periods x RCPs) * 20 (climate models)
#   * Output (see `?newRR3::output_names`)
#     * `"results/newRR3_RATs-combined/"`
#        `"[experiment]_[time]_IndRuns_Values_Preds19"/`
#        `"Preds19__[scenario-id]_[GCM]_[experiment]_[time]__value-sim.csv"`
#     * Variables: as is
#
#----- . ------


#----- SET UP ------
stopifnot(requireNamespace("newRR3")) # e.g., devtools::load_all()


#--- Settings ------
req_meta_version <- "3.2.0"

vtag <- "preds"

# 2023-Mar-22: full/simulated extent: data; rangeland mask: as attribute and for analysis
mask_sets <- c("sim", "rangelands", "combined")[[3L]]



#--- Paths (part 1) ------
dir_prj <- ".."

dir_dataraw <- file.path(dir_prj, "data-raw")
dir.create(dir_dataraw, recursive = TRUE, showWarnings = FALSE)

dir_res <- file.path(dir_prj, "results")
dir.create(dir_res, recursive = TRUE, showWarnings = FALSE)




#------ Load project metadata ------
meta <- newRR3::get_project_description(
  include_variables = TRUE,
  include_scenarios = TRUE,
  dir_data = dir_dataraw
)
stopifnot(!is.null(meta) && meta[["v"]] == req_meta_version)

stopifnot(vtag %in% names(meta[["varsets"]][["tags"]]))


#------ . ------
#------ LOOP OVER SPATIAL MASKS ------
for (km in seq_along(mask_sets)) {
  #--- Paths (part 2) ------
  dir_data_rat <- file.path(
    dir_res,
    paste0("newRR3_RATs-", mask_sets[[km]])
  )


  #----- . ------
  #--- SPREADSHEET RATs OF 19 PREDICTORS OF RR ------

  #--- Create output file names of predictors as RAT spreadsheets
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

    #--- Link to imported external data ------
    fnames_predictors_rsw2metrics <- newRR3::import_predictors_rsw2metrics(
      dir_dataraw = dir_dataraw,
      list_simexps = meta[["simexps"]][["list"]]
    )

    fnames_gsu_template <- newRR3::import_gsu_template(
      dir_dataraw = dir_dataraw,
      mask_set = mask_sets[[km]],
      version_inputs = meta[["version_inputs"]]
    )


    #--- Create directories for results ------
    tmp <- lapply(
      unique(dirname(unlist(fns_out_ratcsv))),
      dir.create,
      recursive = TRUE,
      showWarnings = FALSE
    )


    #--- Load templates of GeoTIFF and RAT ------
    # Note: `read.csv()` reads "value" as "numeric" instead of "integer" by default
    gsu_suids <- utils::read.csv(
      grep(".csv$", fnames_gsu_template, value = TRUE)
    )



    #------ Loop over simulation parts (RCPs) ------
    for (k0 in seq_along(meta[["simexps"]][["list"]])) {

      if (!all(file.exists(fns_out_ratcsv[[k0]]))) {

        #--- * Load rSW2metrics extraction object ------
        xV <- readRDS(fnames_predictors_rsw2metrics[[k0]])

        # remove columns with only NAs
        # (i.e., scenario x time-period combinations that don't exist)
        ids_allna <- apply(is.na(xV), 2, all)
        xV <- xV[, !ids_allna, drop = FALSE]

        vars_scen <- grep(
          "\\<sc[[:digit:]]+_[[:alnum:]]+\\>",
          colnames(xV),
          value = TRUE
        )

        stopifnot(
          inherits(xV, "data.frame"),
          meta[["varsets"]][[vtag]][["varname"]] %in% unique(xV[, "group"]),
          length(vars_scen) == length(meta[["simexps"]][["tag_scen"]][[k0]])
        )


        #--- * Loop over scenarios (GCMs x time periods) ------
        for (k1 in seq_along(meta[["simexps"]][["tag_scen"]][[k0]])) {

          # Check that we work on correct scenario and with correct files
          tmp1 <- strsplit(vars_scen[[k1]], split = "_", fixed = TRUE)
          tmp2 <- strsplit(
            meta[["simexps"]][["tag_scen"]][[k0]][[k1]],
            split = "_",
            fixed = TRUE
          )
          stopifnot(
            identical(tmp1[[1L]][[1L]], tmp2[[1L]][[1L]]),
            grepl(
              meta[["simexps"]][["tag_scen"]][[k0]][[k1]],
              basename(fns_out_ratcsv[[k0]][[k1]])
            )
          )


          if (!file.exists(fns_out_ratcsv[[k0]][[k1]])) {

            #--- ** Prepare RAT ------
            rat <- newRR3::prepare_rat(
              x = xV,
              scen = vars_scen[[k1]],
              req_factors = NULL,
              vars = meta[["varsets"]][[vtag]][["varname"]],
              template = gsu_suids
            )

            stopifnot(
              identical(nrow(rat), nrow(gsu_suids)),
              identical(
                rat[, meta[["rat_header"]][[mask_sets[[km]]]]],
                gsu_suids[, meta[["rat_header"]][[mask_sets[[km]]]]]
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
