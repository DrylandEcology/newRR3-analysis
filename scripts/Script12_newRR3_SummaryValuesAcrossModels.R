#----- . ------
#----- SUMMARIES ACROSS CLIMATE MODEL VALUES (GCMs WITHIN RCP x TIMEPERIODS) ------
#
# * Input
#   * Data created by scripts
#     `"Script01_RR2022predictors_PrepareDataRelease.R"`, ...,
#     `"Script04_RR2022_RF-Uncertainty.R"`:
#     `"results/newRR3_RATs-combined/*_IndRuns_Values_*/"`
#   * Deltas are excluded (see `"Script13_newRR3_DeltasAcrossModels.R"`)
#
# * Output
#   * Derived data for summaries/ensembles across climate models:
#     low, median, high values
#   * Output
#     * `results/newRR3_RATs-combined/`
#       `"[experiment]_[time]_AcrMod_Values_[variable-set]"/`
#       `"[variable-set]__[experiment]_[time]__value-acrmod-(low)|(med)|(high).csv"`
#     * Variable names: `"*_acrmoddistr"`
#
#----- . ------


#----- SET UP ------
stopifnot(requireNamespace("newRR3"))
options(warn = 2L) # convert warnings to errors

#--- Settings ------
req_meta_version <- "3.2.0"

# note: no deltas: "mirrp", "novelty"
vtag <- c("preds", "novelty", "rr", "rfcertainty", "mirrp")
vtag_input <- vtag

# 2023-Mar-22: full/simulated extent: data; rangeland mask: as attribute and for analysis
mask_sets <- c("sim", "rangelands", "combined")[[3L]]


# Method to calculate across-model summaries of most-likely R&R category
method_acrmod_rr <- c(
  # Nearest even order quantiles of (ordered) R&R categories
  "rr_response_from_quantile3",
  # Model picked from ranked weighted sum of most-likely R&R categories
  "rr_from_index", # pick probabilities + most-likely category from selected model
  "rr_response_from_index",  # pick most-likely category from selected model
  # Re-determine most-likely category from summarized probabilities
  "rr_response_from_probs",
  "rr_response_from_probs_mod" # flip low/high for p(L) and p(ML)
)[2L]


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
  include_summaries = TRUE,
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
  #--- SPREADSHEET RATs OF ACROSS-MODEL SUMMARIES ------


  #--- Input data folders (sets of `IndRuns`) over which content to summarize
  # expect that each folder contain a set of files one for each climate model (GCM),
  # i.e., exclude ambient
  # note: "mirrp" has no historical nor deltas
  dns_in_ratcsv <- lapply(
    seq_along(meta[["simexps"]][["list"]]),
    function(k0) {
      lapply(
        meta[["varsets"]][["tags"]][vtag_input],
        function(ft) {
          tmp1 <- if (!(k0 == 1 && ft == "MIRRP")) {
            # no historical for "MIRRP"
            as.list(
              file.path(
                dir_data_rat,
                newRR3::name_output_folder(
                  unique(meta[["simexps"]][["tag_scen_path"]][[k0]]),
                  "IndRuns",
                  "Values",
                  ft
                )
              )
            )
          }
          tmp1[[1L]] <- list() # remove ambient
          tmp1
        }
      )
    }
  )

  # Simplify to vector (because we don't loop explicitly over simexps)
  dns_in_ratcsv <- unlist(dns_in_ratcsv)


  #--- Create output data folders for across-model summaries
  dns_out_ratcsv <- file.path(
    dirname(dns_in_ratcsv),
    gsub("_IndRuns_", "_AcrMod_", basename(dns_in_ratcsv))
  )


  #--- Create output file paths for across-model summaries
  tmp_out <- dns_in_ratcsv |>
    lapply(function(path) list.files(path)[1L]) |>
    unlist() |>
    gsub(pattern = "-sim", replacement = "-acrmod", x = _) |>
    gsub(pattern = ".csv$", replacement = "", x = _) |>
    strsplit(split = "_")

  stopifnot(length(table(lengths(tmp_out))) == 1L)

  tmp_out <- tmp_out |>
    # remove "scX" and "climate model" elements
    lapply(function(x) x[-(3:4)]) |>
    # put file name back together
    lapply(function(x) paste(x, collapse = "_")) |>
    unlist()

  stopifnot(length(tmp_out) == length(dns_out_ratcsv))

  fns_out_ratcsv <- list(
    signal = mapply(
      function(path, x) {
        file.path(
          path,
          paste0(x, "-", meta[["acrmod"]][["signal"]][["tag"]], ".csv")
        )
      },
      dns_out_ratcsv,
      tmp_out,
      SIMPLIFY = FALSE
    )
  )

  stopifnot(
    length(dns_out_ratcsv) == length(fns_out_ratcsv[["signal"]]),
    lengths(fns_out_ratcsv[["signal"]]) == length(meta[["acrmod"]][["signal"]][["tag"]])
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
      dns_out_ratcsv,
      dir.create,
      recursive = TRUE,
      showWarnings = FALSE
    )


    #------ Loop over sets of `IndRuns` ------
    for (ki in seq_along(dns_in_ratcsv)) {

      tmp_fns <- fns_out_ratcsv[["signal"]][[ki]]

      if (!all(file.exists(tmp_fns))) {

        #--- * Identify simulation part (RCP) ------
        k0 <- which(
          vapply(
            meta[["simexps"]][["list"]],
            function(x) {
              grepl(x, x = basename(dns_in_ratcsv[[ki]]), ignore.case = TRUE)
            },
            FUN.VALUE = NA
          )
        )


        #--- * Identify files belonging to set of `IndRuns` ------
        fns_tmp <- list.files(path = dns_in_ratcsv[[ki]], full.names = TRUE)

        # Check that all members of set of `IndRuns` exists and have been found
        tmp_unique_ClimMods <- unique(
          meta[["simexps"]][["meta_scen"]][[k0]][-1L, "Model"] # ignore ambient
        )

        stopifnot(
          vapply(
            tmp_unique_ClimMods,
            function(x) {
              sum(grepl(paste0("_", x, "_"), x = basename(fns_tmp))) == 1L
            },
            FUN.VALUE = NA
          ),
          identical(
            length(tmp_unique_ClimMods),
            length(fns_tmp)
          )
        )


        #--- * Load values ------
        xV <- lapply(fns_tmp, read.csv)

        tmp <- colnames(xV[[1L]])
        stopifnot(
          vapply(xV, function(x) inherits(x, "data.frame"), FUN.VALUE = NA),
          vapply(xV, function(x) identical(colnames(x), tmp), FUN.VALUE = NA)
        )


        #--- Identify variables and numeric/categorical type
        vars <- setdiff(
          colnames(xV[[1L]]),
          meta[["rat_header"]][[mask_sets[[km]]]]
        )

        ids_vars_cat <- which(newRR3::is_categorical(vars))
        ids_vars_num <- setdiff(seq_along(vars), ids_vars_cat)


        #--- RAT output template
        rat_template <- xV[[1L]]
        rat_template[, vars] <- NA


        #--- Reformat to array
        mV <- array(
          data = unlist(lapply(xV, function(x) x[, vars, drop = FALSE])),
          dim = c(nrow(xV[[1L]]), length(vars), length(xV)),
          dimnames = list(NULL, vars, NULL)
        )

        if (FALSE) {
          stopifnot(
            vapply(
              seq_along(xV),
              function(k) identical(mV[, , k], as.matrix(xV[[k]][, vars])),
              FUN.VALUE = NA
            )
          )
        }


        #--- * ------
        #--- * Calculate summaries across climate models ------
        if (!all(file.exists(fns_out_ratcsv[["signal"]][[ki]]))) {

          tmp_acrmod <- array(
            dim = c(
              length(meta[["acrmod"]][["signal"]][["tag"]]),
              dim(mV)[1:2]
            ),
            dimnames = c(
              list(meta[["acrmod"]][["signal"]][["tag"]]),
              dimnames(mV)[1:2]
            )
          )

          #--- ** Numerical summaries across climate models ------
          if (length(ids_vars_num) > 0L) {
            tmp_acrmod[, , ids_vars_num] <- apply(
              mV[, ids_vars_num, , drop = FALSE],
              MARGIN = 1:2,
              FUN = meta[["acrmod"]][["signal"]][["fun_numeric"]]
            )
          }


          #--- ** Categorical summaries across climate models ------
          if (length(ids_vars_cat) > 0L) {
            for (kf in seq_along(meta[["acrmod"]][["signal"]][["tag"]])) {
              for (kv in ids_vars_cat) {

                kv_done <- FALSE

                if (grepl("resistance|resilience", vars[[kv]])) {
                  rr <- strsplit(
                    vars[[kv]],
                    split = "_",
                    fixed = TRUE
                  )[[1L]][[1L]]

                  kvs_probs <- paste0(
                    rr,
                    "_prob.pred_",
                    gsub(
                      "+",
                      ".",
                      meta[["varsets"]][["rr"]][["levels"]],
                      fixed = TRUE
                    )
                  )
                }

                #--- *** Across-model summary of RR-values ------
                if (
                  vars[[kv]] %in% c("Resistance_response", "Resilience_response")
                ) {

                  if (method_acrmod_rr == "rr_response_from_quantile3") {
                    #--- **** Approach: Nearest even order quantiles of (ordered) R&R categories ------
                    # Advantage: most-likely categories are correctly sorted for low/median/high
                    # Problem: most-likely categories are not linked to summarized probabilities
                    # Problem: set of summarized probabilities are not consistent

                    tmp_acrmod[kf, , kv] <- apply(
                      mV[, kv, , drop = FALSE],
                      MARGIN = 1:2,
                      FUN = stats::quantile,
                      probs = meta[["acrmod"]][["signal"]][["probs"]][[kf]],
                      type = 3L,
                      na.rm = TRUE
                    )

                    kv_done <- TRUE

                  } else if (
                    method_acrmod_rr %in% c("rr_response_from_index", "rr_from_index")
                  ) {
                    #--- **** Approach: Model picked from ranked index ------
                    # R_response (and R_probs) picked from model that
                    # is nearest to level `kf` of numerical across-model quantile
                    # of rsl/rst-index
                    # Advantage: most-likely categories are linked to summarized probabilities
                    # Advantage: set of summarized probabilities are consistent
                    # Problem: most-likely categories (1-2% of units)
                    # and summarized probabilities (5-45% of units) are not
                    # correctly sorted for low/median/high

                    kri <- paste0(rr, "_index")

                    # Identify which individual model output (in `mV`)
                    # corresponds to the across-model quantile level `kf`
                    tmpi <- newRR3::calc_nearest_column(
                      x = tmp_acrmod[kf, , kri],
                      mat = mV[, kri, ],
                      seed = 12367L
                    )

                    used_kvs <- if (method_acrmod_rr == "rr_response_from_index") {
                      kv
                    } else if (method_acrmod_rr == "rr_from_index") {
                      c(
                        kv,
                        vapply(
                          kvs_probs,
                          function(v) grep(paste0("^", v, "$"), vars),
                          FUN.VALUE = NA_integer_
                        )
                      )
                    }

                    # Construct index matrix for sub-setting `mV` by `tmpi`
                    # in the third dimension
                    tmpim <- cbind(
                      dim1 = rep(seq_len(nrow(mV)), times = length(used_kvs)),
                      dim2 = rep(used_kvs, each = nrow(mV)),
                      dim3 = rep(tmpi, times = length(used_kvs))
                    )

                    # Pick identified individual model to be across-model summary
                    tmp_acrmod[kf, , used_kvs] <- mV[tmpim]

                    kv_done <- TRUE

                  } else if (method_acrmod_rr == "rr_response_from_probs") {
                    #--- **** Approach: Re-determined R&R from summarized probabilities ------
                    # Advantage: most-likely categories are linked to summarized probabilities
                    # Problem: most-likely categories (1-4% of units) are not
                    # correctly sorted for low/median/high
                    # Problem: set of summarized probabilities are not consistent

                    tmp_acrmod[kf, , kv] <- newRR3::calc_RF_response_from_probs(
                      x = tmp_acrmod[kf, , kvs_probs],
                      seed = 12367L
                    )

                    kv_done <- TRUE

                  } else if (method_acrmod_rr == "rr_response_from_probs_mod") {
                    #--- **** Approach: Re-determined R&R from summarized probabilities ------
                    # flip low/high for p(L) and p(ML) -- low -> high p(L); high -> low p(L)
                    # Advantage: most-likely categories are linked to summarized probabilities
                    # Advantage: most-likely categories are correctly sorted for low/median/high
                    # Problem: set of summarized probabilities are not consistent

                    tmp_probs <- cbind(
                      tmp_acrmod[if (kf == 1) 3 else 1, , kvs_probs[1:2]],
                      tmp_acrmod[kf, , kvs_probs[3:4]]
                    )

                    # normalize probs so that sum(probs) = 1
                    tmp_probs <- sweep(
                      tmp_probs,
                      MARGIN = 1,
                      STATS = rowSums(tmp_probs),
                      FUN = "/"
                    )

                    tmp_acrmod[kf, , kv] <- newRR3::calc_RF_response_from_probs(
                      x = tmp_probs,
                      seed = 12367L
                    )

                    kv_done <- TRUE

                  } else {
                    stop(method_acrmod_rr, " not implemented.")
                  }
                }


                #--- *** Re-calculate MIRRP from summarized distances ------
                if (
                  vars[[kv]] %in% c("Resistance_mirrp", "Resilience_mirrp")
                ) {
                  kvs_dist <- grep(
                    pattern = paste0(
                      "_dL2",
                      strsplit(vars[[kv]], split = "_", fixed = TRUE)[[1L]][[1L]]
                    ),
                    x = meta[["varsets"]][["mirrp"]][["varname"]],
                    value = TRUE
                  )

                  tmp_acrmod[kf, , kv] <- newRR3::calc_MIRRP_from_dists(
                    x = tmp_acrmod[kf, , kvs_dist],
                    seed = 54001
                  )

                  kv_done <- TRUE
                }




                #--- *** Re-calculate `_isnovel` from summarized novelty ------
                if (
                  grepl("_isnovel", vars[[kv]])
                ) {
                  ttmp_base <- sub("_isnovel", "", vars[[kv]], fixed = TRUE)
                  stopifnot(ttmp_base %in% vars)

                  #--- categorized novelty status
                  # level labels: meta[["varsets"]][["novelty"]][["levels"]]
                  # e.g., `factor(rat[[ttmp]], labels = meta[["varsets"]][["novelty"]][["levels"]])`
                  # 0, "notnovel; 1, "novel"
                  tmp_acrmod[kf, , kv] <- newRR3::is_novel_lvls(
                    tmp_acrmod[kf, , ttmp_base]
                  )

                  kv_done <- TRUE
                }


                if (!kv_done) {
                  stop(
                    "Summary across climate models is not implemented for ",
                    vars[[kv]]
                  )
                }

              }
            }
          }


          #--- ** Write RATs as spreadsheet ------
          for (kf in seq_along(meta[["acrmod"]][["signal"]][["tag"]])) {
            rat <- rat_template
            rat[, vars] <- tmp_acrmod[kf, , ]

            stopifnot(
              identical(nrow(rat), nrow(xV[[1L]])),
              identical(
                rat[, meta[["rat_header"]][[mask_sets[[km]]]]],
                xV[[1L]][, meta[["rat_header"]][[mask_sets[[km]]]]]
              ),
              identical(colnames(rat), colnames(xV[[1L]]))
            )

            #--- *** Update variable names ------
            ids <- which(colnames(rat) %in% vars)
            colnames(rat)[ids] <- paste0(colnames(rat)[ids], "_acrmoddistr")

            write.csv(
              rat,
              file = fns_out_ratcsv[["signal"]][[ki]][[kf]],
              row.names = FALSE
            )
          }

        }

        kpb <- kpb + length(fns_out_ratcsv[["signal"]][[ki]])
        utils::setTxtProgressBar(pb, kpb)

      } else {
        kpb <- kpb + length(tmp_fns)
        utils::setTxtProgressBar(pb, kpb)
      }
    }

  } else {
    kpb <- kpb + length(unlist(fns_out_ratcsv))
    utils::setTxtProgressBar(pb, kpb)
  }

  close(pb)
}


print(summary(warnings()))

#------. ------
#------. ------
