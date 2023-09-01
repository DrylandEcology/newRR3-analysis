#----- . ------
#----- DELTAS BETWEEN FUTURE AND HISTORICAL RCPs ------
#
# https://climate.northwestknowledge.net/MACA/MACAfaq.php
# https://climate.northwestknowledge.net/MACA/MACAanalysis.php
# quoted:
# > General Steps in Analysis
# > The following order for an analysis of a change in metric is advised:
# >   - Define a metric
# >   - Calculate the metric in the historical period (first average over all
#       56 years [1950-2005 which have the same statistics as the actual years
#       1979-2009 (from the training data)],
#       then average over all pixels in the study area)
# >   - Calculate the metric for the future period (first average over a 30 year
#       period, then average over all pixels in the study area)
# >   - Calculate the change in the metric between future and historical periods
# >   - Calculate statistics of the average changes on the set of models
#       (multi-model mean gives signal, multi-model standard deviation gives
#       measure of uncertainty)
#
# * Input
#   * Data created by scripts
#     `"Script01_RR2022predictors_PrepareDataRelease.R"`, ...,
#     `"Script04_RR2022_RF-Uncertainty.R"`:
#     `"results/newRR3_RATs-combined/*_IndRuns_Values_*/"`
#
# * Output
#   * Derived data for deltas:
#     spreadsheets with deltas for individual runs (`"IndRuns"`):
#       n = 80 = 4 (future time periods x RCPs) * 20 (GCMs)
#   * Output
#     * `results/newRR3_RATs-combined/`
#       `"[experiment]_[time]_IndRuns_Deltas_[variable-set]"/`
#       `"[variable-set]__[scenario-id]_[GCM]_[experiment]_[time]__delta-sim.csv"`
#     * Variable names: `"*_delta"`
#
#----- . ------


#----- SET UP ------
stopifnot(requireNamespace("newRR3"))


#--- Settings ------
req_meta_version <- "3.2.0"

vtag <- c("preds", "rr", "rfcertainty") # exclude: "mirrp", "novelty"
vtag_input <- vtag

# 2023-Mar-22: full/simulated extent: data; rangeland mask: as attribute and for analysis
mask_sets <- c("sim", "rangelands", "combined")[[3L]]



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
  include_deltas = TRUE,
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
  #--- SPREADSHEET RATs OF DELTAs ------

  #--- Input file names of RAT spreadsheets for individual runs
  # exclude ambient
  fns_in_ratcsv <- lapply(
    seq_along(meta[["simexps"]][["list"]]),
    function(k0) {
      lapply(
        meta[["varsets"]][["tags"]][vtag_input],
        function(ft) {
          tmp <- as.list(file.path(
            dir_data_rat,
            newRR3::name_output_folder(
              meta[["simexps"]][["tag_scen_path"]][[k0]],
              "IndRuns",
              "Values",
              ft
            ),
            newRR3::name1_output_ratcsv(
              ft,
              meta[["simexps"]][["tag_scen"]][[k0]],
              "value-sim"
            )
          ))
          tmp[[1L]] <- list() # remove ambient
          tmp
        }
      )
    }
  )


  #--- Create output file names of deltas as RAT spreadsheets
  # (for future time periods x RCPs, i.e., not historical and not ambient)
  fns_out_ratcsv <- lapply(
    seq_along(meta[["simexps"]][["list"]]),
    function(k0) {
      lapply(
        meta[["varsets"]][["tags"]][vtag],
        function(ft) {
          tmp <- as.list(file.path(
            dir_data_rat,
            newRR3::name_output_folder(
              meta[["simexps"]][["tag_scen_path"]][[k0]],
              "IndRuns",
              "Deltas",
              ft
            ),
            newRR3::name1_output_ratcsv(
              ft,
              meta[["simexps"]][["tag_scen"]][[k0]],
              "delta-sim"
            )
          ))
          tmp[[1L]] <- list() # remove ambient
          tmp
        }
      )
    }
  )
  fns_out_ratcsv[[1L]] <- list() # remove historical (historical is base)


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

      #--- * exclude historical (historical is base) ------
      if (k0 == 1L) next

      #------ * Loop over vtags ------
      for (kt in seq_along(vtag)) {

        if (!all(file.exists(unlist(fns_out_ratcsv[[k0]][[kt]])))) {

          #--- * Loop over scenarios (GCMs x time periods) ------
          for (k1 in seq_along(meta[["simexps"]][["tag_scen"]][[k0]])) {

            #--- ** exclude ambient (deltas only for RCP time periods) ------
            if (k1 == 1L) next

            if (!file.exists(fns_out_ratcsv[[k0]][[kt]][[k1]])) {
              #--- ** Identify historical run under GCM[k1] ------
              kh <- which(
                meta[["simexps"]][["tag_scen"]][[1L]] ==
                  meta[["simexps"]][["tag_scen_histref"]][[k0]][[k1]]
              )

              # Check that we work on correct scenario and with correct files
              stopifnot(
                identical(
                  meta[["simexps"]][["meta_scen"]][[1L]][["Model"]][[kh]],
                  meta[["simexps"]][["meta_scen"]][[k0]][["Model"]][[k1]]
                ),
                grepl(
                  meta[["simexps"]][["tag_scen"]][[1L]][[kh]],
                  basename(fns_in_ratcsv[[1L]][[kt]][[kh]])
                ),
                grepl(
                  meta[["simexps"]][["tag_scen"]][[k0]][[k1]],
                  basename(fns_in_ratcsv[[k0]][[kt]][[k1]])
                ),
                grepl(
                  meta[["simexps"]][["tag_scen"]][[k0]][[k1]],
                  basename(fns_out_ratcsv[[k0]][[kt]][[k1]])
                )
              )


              #--- ** Load values ------
              xV <- utils::read.csv(fns_in_ratcsv[[k0]][[kt]][[k1]])
              xHist <- utils::read.csv(fns_in_ratcsv[[1L]][[kt]][[kh]])

              stopifnot(
                inherits(xV, "data.frame"),
                inherits(xHist, "data.frame"),
                identical(colnames(xV), colnames(xHist))
              )


              #--- ** Calculate deltas ------
              vars <- setdiff(
                colnames(xV),
                meta[["rat_header"]][[mask_sets[[km]]]]
              )

              stopifnot(
                identical(meta[["varsets"]][[vtag[[kt]]]][["varname"]], vars)
              )

              rat <- xV
              rat[, vars] <- NA


              #--- Check if variables are numeric or categorical
              # if categorical, then they need special care for deltas
              fun_deltas <- lapply(
                vars,
                newRR3::select_function,
                options = list(
                  RR_categories = list(
                    vars = c("Resistance_response", "Resilience_response"),
                    fun = function(x, y) {
                      newRR3::convolve_levels(
                        to = meta[["varsets"]][["rr"]][["levels"]][x],
                        from = meta[["varsets"]][["rr"]][["levels"]][y],
                        clevels = meta[["delta"]][["rr"]][["full"]][["levels"]]
                      )
                    }
                  )
                ),
                fun_default = function(x, y) x - y
              )


              #--- Apply delta functions for each variable
              for (kv in seq_along(vars)) {
                rat[[vars[[kv]]]] <- fun_deltas[[kv]](
                  x = xV[[vars[[kv]]]],
                  y = xHist[[vars[[kv]]]]
                )
              }


              stopifnot(
                identical(nrow(rat), nrow(xV)),
                identical(
                  rat[, meta[["rat_header"]][[mask_sets[[km]]]]],
                  xV[, meta[["rat_header"]][[mask_sets[[km]]]]]
                ),
                meta[["varsets"]][[vtag[[kt]]]][["varname"]] %in% colnames(rat),
                identical(colnames(rat), colnames(xV))
              )


              #--- ** Update variable names ------
              ids <- which(colnames(rat) %in% vars)
              colnames(rat)[ids] <- paste0(colnames(rat)[ids], "_delta")


              #--- ** Write RAT as spreadsheet ------
              write.csv(
                rat,
                file = fns_out_ratcsv[[k0]][[kt]][[k1]],
                row.names = FALSE
              )
            }

            kpb <- kpb + 1
            utils::setTxtProgressBar(pb, kpb)
          }

        } else {
          kpb <- kpb + length(fns_out_ratcsv[[k0]][[kt]])
          utils::setTxtProgressBar(pb, kpb)
        }
      }
    }
  }

  close(pb)
}


print(summary(warnings()))

#------. ------
#------. ------
