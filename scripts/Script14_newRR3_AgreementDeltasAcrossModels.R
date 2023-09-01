#----- . ------
#----- AGREEMENT AND ROBUSTNESS OF DELTAS OF ACROSS-CLIMATE MODELS SUMMARIES (GCMs WITHIN RCP x TIMEPERIODS) ------
#
# * Input
#   * Data created by scripts
#     `"Script11_newRR3_DeltasIndividualModels.R"` and
#     `"Script13_newRR3_DeltasAcrossModels.R"`:
#     `"results/newRR3_RATs-combined/*_Deltas_*/"`
#
# * Output
#   * Derived data for robustness and agreement of deltas of
#     summaries/ensembles across climate models at median levels
#   * Output
#     * `"results/newRR3_RATs-combined/"`
#       `"[experiment]_[time]_AcrMod_Deltas_[variable-set]"/`
#       `"[variable-set]__[experiment]_[time]__delta-acrmod-(agree)|(robust090).csv"`
#     * Variable names: `"*_delta_acrmod(agree)|(robust)"`
#
#----- . ------


#----- SET UP ------
stopifnot(requireNamespace("newRR3"))
options(warn = 2L) # convert warnings to errors

#--- Settings ------
req_meta_version <- "3.2.0"

# note: no deltas: "mirrp", "novelty"
vtag <- c("preds", "rr", "rfcertainty")
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


  nh <- length(meta[["rat_header"]][[mask_sets[[km]]]])


  #----- . ------
  #--- SPREADSHEET RATs OF DELTAs OF ACROSS-MODEL SUMMARIES ------


  #--- Input data folders (deltas of `AcrMod`) for which to calculate agreement
  # i.e., exclude ambient; exclude historical (historical is base)
  # note: "mirrp" has no historical nor deltas
  dns_in_ratcsv <- lapply(
    seq_along(meta[["simexps"]][["list"]])[-1L], # exclude historical
    function(k0) {
      lapply(
        meta[["varsets"]][["tags"]][vtag_input],
        function(ft) {
          tmp1 <- if (!(ft == "MIRRP")) {
            # don't calculate deltas for "MIRRP"
            as.list(
              file.path(
                dir_data_rat,
                newRR3::name_output_folder(
                  unique(meta[["simexps"]][["tag_scen_path"]][[k0]]),
                  "AcrMod",
                  "Deltas",
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

  stopifnot(file.exists(dns_in_ratcsv))


  #--- Create output data folders for across-model summaries
  # identical to inputs (agreement, robustness stored together with deltas)
  dns_out_ratcsv <- dns_in_ratcsv


  #--- Create output file paths for agreement and robustness
  tmp_out <- dns_in_ratcsv |>
    lapply(function(path) list.files(path, pattern = "-med")[1L]) |>
    unlist() |>
    gsub(pattern = "-med", replacement = "", x = _) |>
    gsub(pattern = ".csv$", replacement = "", x = _) |>
    strsplit(split = "_")

  stopifnot(length(table(lengths(tmp_out))) == 1L)

  tmp_out <- tmp_out |>
    # put file name back together
    lapply(function(x) paste(x, collapse = "_")) |>
    unlist()

  stopifnot(length(tmp_out) == length(dns_out_ratcsv))

  fns_out_ratcsv <- list(
    robust = mapply(
      function(path, x) {
        # agreement and robustness only makes sense for deltas
        file.path(
          path,
          paste0(x, "-", meta[["acrmod"]][["robust"]][["tag"]], ".csv")
        )
      },
      dns_out_ratcsv,
      tmp_out,
      SIMPLIFY = FALSE
    )
  )

  stopifnot(
    lengths(fns_out_ratcsv[["robust"]]) %in% c(0L, length(meta[["acrmod"]][["robust"]][["tag"]])),
    any(lengths(fns_out_ratcsv[["robust"]]) > 0L)
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


    #------ Loop over sets of `AcrMod` ------
    for (ki in seq_along(dns_in_ratcsv)) {

      if (!all(file.exists(fns_out_ratcsv[["robust"]][[ki]]))) {

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

        #--- * exclude historical (historical is base) ------
        if (k0 == 1L) next


        #--- . ------

        tag_central <- "med"
        kc <- which(tag_central == meta[["acrmod"]][["signal"]][["tag"]])
        stopifnot(length(kc) == 1L)

        #--- * Load delta of central summary across climate models ------

        tmp <- fns_out_ratcsv[["robust"]][[ki]][[1L]]
        fname_med <- file.path(
          dirname(tmp),
          sub(
            pattern = "delta-acrmod-agree",
            replacement = paste0("delta-acrmod-", tag_central),
            x = basename(tmp)
          )
        )

        xDmed <- utils::read.csv(fname_med)


        #--- Identify variables and numeric/categorical type
        vars_Dmed <- setdiff(
          colnames(xDmed),
          meta[["rat_header"]][[mask_sets[[km]]]]
        )

        ids_vars_cat <- which(newRR3::is_categorical(vars_Dmed))
        ids_vars_num <- setdiff(seq_along(vars_Dmed), ids_vars_cat)


        #--- * RAT output template ------
        rat_template <- xDmed
        rat_template[, vars_Dmed] <- NA


        #--- . ------
        #--- * Load deltas from individual climate models to calculate agreement ------

        #--- ** Identify files belonging to set of `IndRuns` ------
        tmp <- fns_out_ratcsv[["robust"]][[ki]][[1L]]

        fns_tmp <- list.files(
          path = sub(
            pattern = "AcrMod_Deltas",
            replacement = "IndRuns_Deltas",
            dirname(tmp)
          ),
          pattern = "delta-sim",
          full.names = TRUE
        )

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


        #--- ** Load values ------
        xDind <- lapply(fns_tmp, read.csv)

        tmp_vars_Dind <- colnames(xDind[[1L]])
        stopifnot(
          vapply(xDind, function(x) inherits(x, "data.frame"), FUN.VALUE = NA),
          vapply(
            xDind,
            function(x) identical(colnames(x), tmp_vars_Dind),
            FUN.VALUE = NA
          )
        )

        #--- Identify variables
        ids <- match(
          newRR3::simplify_variables_names(vars_Dmed),
          newRR3::simplify_variables_names(tmp_vars_Dind),
          nomatch = 0L
        )
        vars_Dind <- tmp_vars_Dind[ids]


        #--- Reformat to array
        mDind <- array(
          data = unlist(lapply(xDind, function(x) x[, vars_Dind, drop = FALSE])),
          dim = c(nrow(xDind[[1L]]), length(vars_Dind), length(xDind)),
          dimnames = list(NULL, vars_Dind, NULL)
        )

        if (FALSE) {
          stopifnot(
            vapply(
              seq_along(xDind),
              function(k) {
                identical(mDind[, , k], as.matrix(xDind[[k]][, vars_Dind]))
              },
              FUN.VALUE = NA
            )
          )
        }



        #--- . ------
        #--- * Calculate agreement and robustness of delta summaries across climate models ------

        #--- ** Agreement of summaries across climate models ------
        ka <- grep("acrmod-agree", fns_out_ratcsv[["robust"]][[ki]])
        stopifnot(length(ka) == 1L)

        if (!file.exists(fns_out_ratcsv[["robust"]][[ki]][[ka]])) {

          #--- *** Numerical agreement across climate models ------
          rat_agree <- rat_template

          for (kv in ids_vars_num) {
            rat_agree[, nh + kv] <- newRR3::agreement_frequency(
              x = mDind[, kv, ],
              x_central = xDmed[, nh + kv]
            )
          }


          #--- *** Categorical agreement across climate models ------
          for (kv in ids_vars_cat) {
            if (
              vars_Dmed[[kv]] %in%
                c(
                  "Resistance_response_delta_acrmoddistr",
                  "Resilience_response_delta_acrmoddistr"
                )
            ) {
              # Replace (delta) categories with associated sign (direction of change)
              # then, calculate agreement based on sign (of signs)
              meta[["delta"]][["rr"]][["full"]][["sign"]]

              rat_agree[, nh + kv] <- newRR3::agreement_frequency(
                x = array(
                  data = meta[["delta"]][["rr"]][["full"]][["sign"]][
                    as.vector(mDind[, kv, ])
                  ],
                  dim = dim(mDind[, kv, ])
                ),
                x_central = meta[["delta"]][["rr"]][["full"]][["sign"]][
                  xDmed[, nh + kv]
                ]
              )

            } else {
              stop(
                "Agreement across climate models is not implemented for ",
                vars_Dmed[[kv]]
              )
            }
          }


          #--- *** Write RAT as spreadsheet ------
          stopifnot(
            identical(nrow(rat_agree), nrow(xDmed)),
            identical(
              rat_agree[, meta[["rat_header"]][[mask_sets[[km]]]]],
              xDmed[, meta[["rat_header"]][[mask_sets[[km]]]]]
            ),
            identical(colnames(rat_agree), colnames(xDmed))
          )


          #--- **** Update variable names ------
          ids <- which(colnames(rat_agree) %in% vars_Dmed)
          colnames(rat_agree)[ids] <- gsub(
            pattern = "_delta_acrmoddistr",
            replacement = "_delta_acrmodagree",
            x = colnames(rat_agree)[ids],
            fixed = TRUE
          )

          write.csv(
            rat_agree,
            file = fns_out_ratcsv[["robust"]][[ki]][[ka]],
            row.names = FALSE
          )

        } else {
          rat_agree <- utils::read.csv(fns_out_ratcsv[["robust"]][[ki]][[ka]])
        }

        kpb <- kpb + 1L
        utils::setTxtProgressBar(pb, kpb)



        #--- .. ------
        #--- ** Calculate robustness of agreement across climate models ------
        kr <- grep("acrmod-robust", fns_out_ratcsv[["robust"]][[ki]])
        stopifnot(length(kr) == 1L)

        if (!file.exists(fns_out_ratcsv[["robust"]][[ki]][[kr]])) {
          #--- *** robustness of agreement across climate models ------
          rat_robust <- rat_template

          for (kv in seq_along(vars_Dmed)) {
            rat_robust[, nh + kv] <- newRR3::robustness(
              x = rat_agree[, nh + kv, drop = FALSE],
              frq_robust = meta[["acrmod"]][["robust"]][["frq"]]
            )
          }


          #--- *** Write RAT as spreadsheet ------
          stopifnot(
            identical(nrow(rat_robust), nrow(xDmed)),
            identical(
              rat_robust[, meta[["rat_header"]][[mask_sets[[km]]]]],
              xDmed[, meta[["rat_header"]][[mask_sets[[km]]]]]
            ),
            identical(colnames(rat_robust), colnames(xDmed))
          )


          #--- **** Update variable names ------
          ids <- which(colnames(rat_robust) %in% vars_Dmed)
          colnames(rat_robust)[ids] <- gsub(
            pattern = "_delta_acrmoddistr",
            replacement = "_delta_acrmodrobust",
            x = colnames(rat_robust)[ids],
            fixed = TRUE
          )

          write.csv(
            rat_robust,
            file = fns_out_ratcsv[["robust"]][[ki]][[kr]],
            row.names = FALSE
          )
        }

        kpb <- kpb + 1L
        utils::setTxtProgressBar(pb, kpb)

      } else {
        kpb <- kpb + length(fns_out_ratcsv[["robust"]][[ki]])
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
