#----- . ------
#----- ATTRIBUTION OF CHANGE TO RF-PREDICTORS: IDENTIFY MOST IMPORTANT PREDICTORS OF RR ------
#
# * Input
#   * Core spreadsheets with 19 predictors
#     (created by `"Script01_RR2022predictors_PrepareDataRelease.R"`):
#     `"results/newRR3_RATs-combined/*/Preds19__*.csv"`
#   * Random forest models that predict RR: `"data-raw/mrrrf/"`
#
# * Output
#   * Data with most-important RR-predictor (`"MIRRP"`)
#     against historical reference:
#     spreadsheets with values for individual runs (`"IndRuns"`):
#       n = 80 = 4 (time periods x RCPs) * 20 (climate models)
#   * Output
#     * `results/newRR3_RATs-combined/`
#       `"[experiment]_[time]_IndRuns_Values_MIRRP"/`
#       `"MIRRP__[scenario-id]_[GCM]_[experiment]_[time]__value-sim.csv"`
#     * Variables: `"Resilience_mirrp"`, `"Resistance_mirrp"`
#
#
# * Method
#
# We have: 1 RF with 2 predictions (4 probabilities per R/R) for each pixel
#          under 2 different climate conditions (hist vs. rcp x time)
#         -> rf_hist() and rf_fut()
#
# Question: Which of the 19 predictors was most influential for making the
#           future prediction?
#
# Quantification
#   * Create 19 additional predictions using data frames with predictor values
#     under RCP x time conditions except for one predictor at a time which is
#     held under historical condition
#     -> rf_mod_k(pred_1(fut), ..., pred_k(hist), ..., pred_19(fut)); k in {1, 19}
#   * Calculate the 19 Euclidean distances (in 4-dim space of probabilities)
#     between original prediction under RCP x time
#     conditions (all predictors have those values) and one of the 19
#     additional predictions at a time where one predictor value was held
#     under historical condition
#     -> dL2_k(rf_mod_k, rf_fut); k in {1, 19}
#   * Largest distances indicates the most influential predictor
#    -> argmax{dL2_k}; k in {1, 19}
#
#----- . ------



#----- SET UP ------
stopifnot(requireNamespace("newRR3"))



#--- Settings ------
req_meta_version <- "3.2.0"

vtag <- "mirrp" # MIRRP = most important RR predictors
vtag_input <- c("preds", "rr")

# 2023-Mar-22: full/simulated extent: data; rangeland mask: as attribute and for analysis
mask_sets <- c("sim", "rangelands", "combined")[[3L]]

seed_resolve_ties <- 54001


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
  #--- SPREADSHEET RATs OF PREDICTED RR ------

  #--- Input file names of predictors and of predicted RR as RAT spreadsheets
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

  #--- Create output file names of MIRRPs as RAT spreadsheets
  # (for future time periods x RCPs, i.e., not historical and not ambient)
  fns_out_ratcsv <- lapply(
    seq_along(meta[["simexps"]][["list"]]),
    function(k0) {
      tmp <- as.list(file.path(
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
      ))
      tmp[[1L]] <- list() # remove ambient
      tmp
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


    #--- Link to imported external data ------
    fnames_mrrrf <- newRR3::import_predictrr_rfworkflows(
      dir_dataraw = dir_dataraw
    )


    #------ Load RF prediction objects ------
    #--- Resilience and resistance fitted model workflow
    #  (Jessi Brown, 2022-Feb-08)
    resil.wf <- readRDS(
      fnames_mrrrf[grep("Resilience", basename(fnames_mrrrf), fixed = TRUE)]
    )

    resist.wf <- readRDS(
      fnames_mrrrf[grep("Resistance", basename(fnames_mrrrf), fixed = TRUE)]
    )


    #------ Loop over simulation parts (RCPs) ------
    for (k0 in seq_along(meta[["simexps"]][["list"]])) {

      #--- * exclude historical (historical is base) ------
      if (k0 == 1L) next

      if (!all(file.exists(unlist(fns_out_ratcsv[[k0]])))) {

        #--- * Loop over scenarios (GCMs x time periods) ------
        for (k1 in seq_along(meta[["simexps"]][["tag_scen"]][[k0]])) {

          #--- ** exclude ambient (MIRRPs only for RCP time periods) ------
          if (k1 == 1L) next

          if (!file.exists(fns_out_ratcsv[[k0]][[k1]])) {
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
                basename(fns_in_ratcsv[[1L]][["preds"]][[kh]])
              ),
              grepl(
                meta[["simexps"]][["tag_scen"]][[k0]][[k1]],
                basename(fns_in_ratcsv[[k0]][["preds"]][[k1]])
              ),
              grepl(
                meta[["simexps"]][["tag_scen"]][[k0]][[k1]],
                basename(fns_out_ratcsv[[k0]][[k1]])
              )
            )



            #--- ** Load 19 Predictors ------
            xV <- utils::read.csv(fns_in_ratcsv[[k0]][["preds"]][[k1]])
            xHist <- utils::read.csv(fns_in_ratcsv[[1L]][["preds"]][[kh]])

            stopifnot(
              inherits(xV, "data.frame"),
              inherits(xHist, "data.frame"),
              identical(colnames(xV), colnames(xHist))
            )


            #--- ** Load predicted RR ------
            xrrV <- utils::read.csv(fns_in_ratcsv[[k0]][["rr"]][[k1]])
            xrrHist <- utils::read.csv(fns_in_ratcsv[[1L]][["rr"]][[kh]])

            stopifnot(
              inherits(xrrV, "data.frame"),
              inherits(xrrHist, "data.frame"),
              identical(colnames(xrrV), colnames(xrrHist))
            )


            #--- ** Variable importance for predicting RR ------

            # Container for Euclidean distances
            des <- array(
              dim = c(
                nrow(xHist),
                length(meta[["varsets"]][["preds"]][["varname"]]),
                length(meta[["varsets"]][["rr"]][["rrs"]])
              ),
              dimnames = list(
                NULL,
                meta[["varsets"]][["preds"]][["varname"]],
                meta[["varsets"]][["rr"]][["rrs"]]
              )
            )


            #--- **** Predict RR where predictor kv is under historical condition ------
            # while all other predictors are under future condition k0
            for (kv in seq_along(meta[["varsets"]][["preds"]][["varname"]])) {
              ikv <- grep(
                paste0(
                  "^",
                  meta[["varsets"]][["preds"]][["varname"]][[kv]],
                  "(_|$)"
                ),
                colnames(xV)
              )
              stopifnot(length(ikv) == 1L)

              # Prepare predictors
              # predictor k under hist/k1 and all others under k0/k1 conditions
              tmp <- xV
              tmp[[ikv]] <- xHist[[ikv]]

              ratv <- newRR3::predict_rr(
                x = tmp,
                var_predictors = meta[["varsets"]][["preds"]][["varname"]],
                rf_workflow_rst = resist.wf,
                rf_workflow_rsl = resil.wf,
                nrr = meta[["varsets"]][["rr"]][["varname"]],
                rr_levels = meta[["varsets"]][["rr"]][["levels"]]
              )

              stopifnot(
                identical(nrow(ratv), nrow(xV)),
                identical(
                  ratv[, meta[["rat_header"]][[mask_sets[[km]]]]],
                  xV[, meta[["rat_header"]][[mask_sets[[km]]]]]
                ),
                meta[["varsets"]][["rr"]][["varname"]] %in% colnames(ratv)
              )


              #--- **** Calculate Euclidean distances from future predicted RR variables ------
              # for each set of four resilience/resistance probabilities
              # based on https://stackoverflow.com/a/64577688
              for (kr in seq_along(meta[["varsets"]][["rr"]][["rrs"]])) {
                ids <- grep(
                  paste0(meta[["varsets"]][["rr"]][["rrs"]][[kr]], "_prob"),
                  colnames(ratv)
                )
                stopifnot(length(ids) == length(meta[["varsets"]][["rr"]][["levels"]]))

                des[, kv, kr] <- proxy::dist(
                  xrrV[, ids],
                  ratv[, ids],
                  method = "Euclidean",
                  by_row = TRUE,
                  pairwise = TRUE
                )
              }
            }


            #--- **** Identify predictor associated with largest, positive distance ------
            # - identify largest, positive distance
            # - don't identify largest, positive distance if 50% or more of distances are equally the largest
            # - resolve ties at random
            set.seed(seed_resolve_ties)

            mip <- apply(
              des,
              MARGIN = 3L,
              newRR3::calc_MIRRP_from_dists,
              seed = NA
            )

            #--- ** Write RAT as spreadsheet ------
            colnames(mip) <- paste0(
              meta[["varsets"]][["rr"]][["rrs"]],
              "_",
              vtag
            )

            desl <- lapply(
              1:2,
              function(k) {
                tmp <- des[, , k]
                colnames(tmp) <- paste0(
                  colnames(tmp),
                  "_dL2",
                  meta[["varsets"]][["rr"]][["rrs"]][[k]]
                )
                tmp
              }
            )

            rat <- data.frame(
              xV[, meta[["rat_header"]][[mask_sets[[km]]]]],
              mip,
              desl[[1L]],
              desl[[2L]],
              stringsAsFactors = FALSE
            )

            stopifnot(
              identical(nrow(rat), nrow(xV)),
              identical(
                rat[, meta[["rat_header"]][[mask_sets[[km]]]]],
                xV[, meta[["rat_header"]][[mask_sets[[km]]]]]
              ),
              meta[["varsets"]][[vtag]][["varname"]] %in% colnames(rat)
            )

            write.csv(rat, file = fns_out_ratcsv[[k0]][[k1]], row.names = FALSE)
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
