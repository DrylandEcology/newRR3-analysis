#----- . ------
#----- CALCULATE PREDICTOR NOVELTY ------
#
# * Input
#   * Core spreadsheets with 19 predictors
#     (created by `"Script01_RR2022predictors_PrepareDataRelease.R"`):
#     `"results/newRR3_RATs-combined/*/Preds19__*.csv"`
#   * Training data used to fit random forest models that predict RR:
#     `"data-raw/mrrrf/"`
#
# * Output
#   * Data with predictor novelty (`"Preds19-Novelty"`):
#     spreadsheets with values for individual runs (`"IndRuns"`):
#       n = 101 = 1 (ambient) + 5 (time periods x RCPs) * 20 (climate models)
#   * Output
#     * `results/newRR3_RATs-combined/`
#       `"[experiment]_[time]_IndRuns_Values_Preds19-Novelty"/`
#       `"Preds19-Novelty__[scenario-id]_[GCM]_[experiment]_[time]__value-sim.csv"`
#     * Variables: as is
#
#----- . ------


#----- SET UP ------
stopifnot(
  requireNamespace("newRR3"),
  # CAST updated calculation of the threshold with v0.7.1;
  # see `newRR3::calc_AOArdi()`
  requireNamespace("CAST"),
  getNamespaceVersion("CAST") >= "0.7.1",
  requireNamespace("parallel")
)


#--- Settings ------
req_meta_version <- "3.2.0"
n_parallel_workers <- 8L # used only if CAST < v0.7.1

vtag <- "novelty"
vtag_input <- "preds"

# 2023-Mar-22: full/simulated extent: data; rangeland mask: as attribute and for analysis
mask_sets <- c("sim", "rangelands", "combined")[[3L]]



#--- Paths (part 1) ------
dir_prj <- ".."

dir_dataraw <- file.path(dir_prj, "data-raw")
stopifnot(dir.exists(dir_dataraw))

dir_data <- file.path(dir_prj, "data")
stopifnot(dir.exists(dir_data))

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
  #--- SPREADSHEET RATs OF NOVELTY ------

  #--- Input file names of predictors as RAT spreadsheets
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

  #--- Create output file names of novelty of predictors as RAT spreadsheets
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
    fnames_mrrrf <- newRR3::import_predictrr_rfworkflows(
      dir_dataraw = dir_dataraw
    )


    #--- Create directories for results ------
    tmp <- lapply(
      unique(dirname(unlist(fns_out_ratcsv))),
      dir.create,
      recursive = TRUE,
      showWarnings = FALSE
    )

    #------ Load predictor weights ------
    # variable importance of RSL and RST models
    fname_varimp_data <- file.path(
      dir_data,
      "mrrrf",
      paste0(
        "Predictors19_variableImportance__20211214_RR_RF_model-",
        meta[["version_rf"]],
        ".rds"
      )
    )
    xvarimp <- newRR3::get_varimp_rfworkflows(
      fname_varimp_data = fname_varimp_data,
      fnames_mrrrf = fnames_mrrrf,
      dir_dataraw = dir_dataraw
    )

    stopifnot(
      vapply(
        xvarimp,
        function(vi) {
          all(meta[["varsets"]][[vtag_input]][["varname"]] %in% names(vi))
        },
        FUN.VALUE = NA
      ),
      meta[["varsets"]][["rr"]][["rrs"]] %in% names(xvarimp)
    )


    #------ Load training data ------
    fname_training_data <- file.path(
      dir_data,
      "mrrrf",
      paste0(
        "Predictors19_plots__values-training__",
        "20211214_RR_RF_model", "-", meta[["version_rf"]],
        ".rds"
      )
    )
    xtraining <- newRR3::get_training_rfworkflows(
      fname_training = fname_training_data,
      fnames_mrrrf = fnames_mrrrf
    )

    stopifnot(
      meta[["varsets"]][[vtag_input]][["varname"]] %in% colnames(xtraining)
    )

    xtrain <- xtraining[, meta[["varsets"]][[vtag_input]][["varname"]], drop = FALSE]


    #--- Calculate training data distance metrics ------
    fnames_dist_trained <- file.path(
      dirname(fname_training_data),
      vapply(
        meta[["varsets"]][["novelty"]][["varname"]],
        function(dm) {
          sub(
            ".rds$",
            paste0("__trained-", dm, ".rds"),
            basename(fname_training_data)
          )
        },
        FUN.VALUE = NA_character_
      )
    )
    has_dist_trained <- file.exists(fnames_dist_trained)

    list_dist_trained <- stats::setNames(
      vector(
        mode = "list",
        length = length(meta[["varsets"]][["novelty"]][["varname"]])
      ),
      meta[["varsets"]][["novelty"]][["varname"]]
    )


    for (k in seq_along(meta[["varsets"]][["novelty"]][["varname"]]))  {
      ttmp <- meta[["varsets"]][["novelty"]][["varname"]][[k]]

      if (grepl("_isnovel", ttmp)) next

      if (has_dist_trained[[k]]) {
        list_dist_trained[[ttmp]] <- readRDS(fnames_dist_trained[[k]])

      } else {
        # Get "version" for threshold trimming
        tmpv <- regmatches(
          ttmp,
          regexpr("[[:digit:]]{3,}", ttmp)
        )
        vmod <- if (length(tmpv) == 1L) {
          paste(strsplit(tmpv, split = "")[[1L]], collapse = ".")
        }


        #--- * Train (modified) NT1 ------
        if (isTRUE(grepl("NT1", ttmp, fixed = TRUE))) {
          list_dist_trained[[ttmp]] <- newRR3::train_NT1(
            traindata = xtrain,
            trimmed = vmod
          )
        }


        #--- * Train (modified) NT2 ------
        if (isTRUE(grepl("NT2", ttmp, fixed = TRUE))) {
          list_dist_trained[[ttmp]] <- newRR3::train_NT2(
            traindata = xtrain,
            trimmed = vmod
          )
        }


        #--- * Train weighted AOA-rdi ------
        if (isTRUE(grepl("AOArdi", ttmp, fixed = TRUE))) {
          has_weights <- isTRUE(grepl("w", ttmp, fixed = TRUE))

          w <- if (has_weights) {
            idrr <- vapply(
              names(meta[["varsets"]][["rr"]][["rrs"]]),
              function(rr) grepl(rr, ttmp),
              FUN.VALUE = NA
            )
            idvi <- which(
              names(xvarimp) == meta[["varsets"]][["rr"]][["rrs"]][[which(idrr)]]
            )

            data.frame(t(xvarimp[[idvi]]))
          }

          list_dist_trained[[ttmp]] <- suppressMessages(
            CAST::trainDI(
              train = xtrain,
              weight = if (has_weights) w else NA
            )
          )
        }


        #--- Save data
        if (!is.null(list_dist_trained[[ttmp]])) {
          saveRDS(list_dist_trained[[ttmp]], file = fnames_dist_trained[[k]])
        }
      }
    }



    #------ Set up parallel cluster for `CAST::aoa()` ------
    cl <- if (getNamespaceVersion("CAST") < "0.7.1") {
      parallel::makeCluster(as.integer(n_parallel_workers))
    }


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

            #--- ** Load 19 Predictors ------
            xV <- utils::read.csv(fns_in_ratcsv[[k0]][[k1]])

            stopifnot(
              inherits(xV, "data.frame"),
              meta[["varsets"]][[vtag_input]][["varname"]] %in% colnames(xV)
            )

            newdata <- xV[, meta[["varsets"]][[vtag_input]][["varname"]], drop = FALSE]


            #--- ** Calculate novelty ------
            rat <- data.frame(
              xV[, meta[["rat_header"]][[mask_sets[[km]]]], drop = FALSE],
              matrix(
                ncol = length(meta[["varsets"]][["novelty"]][["varname"]]),
                dimnames = list(
                  NULL,
                  meta[["varsets"]][["novelty"]][["varname"]]
                )
              ),
              stringsAsFactors = FALSE
            )


            #--- *** Calculate novelty metrics ------
            for (k in seq_along(meta[["varsets"]][["novelty"]][["varname"]]))  {
              ttmp <- meta[["varsets"]][["novelty"]][["varname"]][[k]]

              if (grepl("_isnovel", ttmp)) next

              # Get "version" for threshold trimming
              tmpv <- regmatches(
                ttmp,
                regexpr("[[:digit:]]{3,}", ttmp)
              )
              vmod <- if (length(tmpv) == 1L) {
                paste(strsplit(tmpv, split = "")[[1L]], collapse = ".")
              }


              #--- **** Calculate (modified) NT1 ------
              if (isTRUE(grepl("NT1", ttmp, fixed = TRUE))) {
                rat[[ttmp]] <- newRR3::calc_NT1(
                  newdata = newdata,
                  trained = list_dist_trained[[ttmp]],
                  trimmed = vmod
                )
              }

              #--- **** Calculate (modified) NT2 ------
              if (isTRUE(grepl("NT2", ttmp, fixed = TRUE))) {
                rat[[ttmp]] <- newRR3::calc_NT2(
                  newdata = newdata,
                  trained = list_dist_trained[[ttmp]],
                  trimmed = vmod
                )
              }

              #--- **** Calculate versioned (and weighted) AOA-rdi ------
              if (isTRUE(grepl("AOArdi", ttmp, fixed = TRUE))) {
                rat[[ttmp]] <- newRR3::calc_AOArdi(
                  newdata = newdata,
                  trained = list_dist_trained[[ttmp]],
                  trimmed = vmod,
                  cl = cl
                )
              }

            }


            #--- *** Derive novelty status ------
            for (k in seq_along(meta[["varsets"]][["novelty"]][["varname"]]))  {
              ttmp <- meta[["varsets"]][["novelty"]][["varname"]][[k]]

              if (!grepl("_isnovel", ttmp)) next

              ttmp_base <- sub("_isnovel", "", ttmp, fixed = TRUE)
              stopifnot(ttmp_base %in% names(rat))

              #--- categorized novelty status
              # level labels: meta[["varsets"]][["novelty"]][["levels"]]
              # e.g., `factor(rat[[ttmp]], labels = meta[["varsets"]][["novelty"]][["levels"]])`
              # 1, "novel"; 2, "notnovel"
              rat[[ttmp]] <- newRR3::is_novel_lvls(rat[[ttmp_base]])
            }


            #--- ** Write RAT as spreadsheet ------
            stopifnot(
              identical(nrow(rat), nrow(xV)),
              identical(
                rat[, meta[["rat_header"]][[mask_sets[[km]]]]],
                xV[, meta[["rat_header"]][[mask_sets[[km]]]]]
              ),
              meta[["varsets"]][[vtag]][["varname"]] %in% colnames(rat)
            )


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


    #------ Clean up parallel cluster ------
    if (inherits(cl, "cluster")) parallel::stopCluster(cl)
  }

  close(pb)
}


print(summary(warnings()))

#------. ------
#------. ------
