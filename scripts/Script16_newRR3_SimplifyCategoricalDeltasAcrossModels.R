#----- . ------
#----- SIMPLIFY ACROSS-MODEL DELTAs (ROBUSTIFIED and OTHERs) ------
#
# Re-code categorical deltas (convolutions) to three levels:
# increase, stable, decrease
#
# * Input
#     * Across-model deltas `"R&R"` created by
#       `"Script12_newRR3_SummaryAcrossModels.R"`:
#       `"results/newRR3_RATs-combined/*/RR__*__delta-acrmod-(low)|(med)|(high).csv"`
#     * Robustified across-model deltas `"R&R"` created by
#       `"Script15_newRR3_RobustifyCategoricalDeltasAcrossModels.R"`:
#       `"results/newRR3_RATs-combined/*/RR__*__robust090delta-acrmod-med.csv"`
#
# * Output
#   * Derived data for simplified delta summaries/ensembles across climate models:
#     `"*simpledelta"`
#   * Output
#     * `"results/newRR3_RATs-combined/"`
#       `"[experiment]_[time]_AcrMod_Deltas_RR"/`
#       `"RR__[experiment]_[time]__*delta-acrmod-(low)|(med)|(high).csv"`
#     * Variable names: `"*_*simpledelta_acrmoddistr"`
#
#----- . ------


#----- SET UP ------
stopifnot(requireNamespace("newRR3"))
options(warn = 2L) # convert warnings to errors

#--- Settings ------
req_meta_version <- "3.2.0"

vtag <- c("rr") # only "rr" has categorical variables that need simplifying
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
  vtag_input %in% names(meta[["varsets"]][["tags"]]),
  grepl("robust", meta[["acrmod"]][["robust"]][["tag"]][[2L]])
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
  #--- SPREADSHEET RATs OF SIMPLIFIED DELTA ACROSS-MODEL SUMMARIES ------
  pattern_acrmoddeltas <- paste0(
    "__",
    c("", meta[["acrmod"]][["robust"]][["tag"]][[2L]]),
    "delta-acrmod"
  )
  pattern_acrmodsimpledeltas <- gsub(
    pattern = "delta",
    replacement = "simpledelta",
    x = pattern_acrmoddeltas
  )


  #--- Input files: across-model (robust) deltas that need simplifying
  tmp <- expand.grid(
    fts = vtag_input,
    sts = meta[["acrmod"]][["signal"]][["tag"]],
    stringsAsFactors = FALSE
  )

  #--- List with elements for deltas and for robustdeltas
  tmpf_in_ratcsv <- lapply(
    pattern_acrmoddeltas,
    function(pd) {
      mapply(
        function(ft, st) {
          list.files(
            path = dir_data_rat,
            pattern = paste0(
              meta[["varsets"]][["tags"]][[ft]],
              "__(.+)",
              pd, "-",
              st
            ),
            full.names = FALSE,
            recursive = TRUE
          )
        },
        tmp[["fts"]],
        tmp[["sts"]],
        SIMPLIFY = FALSE
      ) |>
        unlist()
    }
  )

  fns_in_ratcsv <- lapply(
    tmpf_in_ratcsv,
    function(x) file.path(dir_data_rat, x)
  )


  #--- Create output file paths for simplified (robust) delta across-model summaries
  tmp <- mapply( # nolint: undesirable_function_linter.
    function(tmpf, pd) {
      vapply(
        tmpf,
        function(x) {
          file.path(
            dirname(x),
            sub(
              pattern = pd,
              replacement = sub("delta", "simpledelta", x = pd),
              basename(x)
            )
          )
        },
        FUN.VALUE = NA_character_
      )
    },
    tmpf_in_ratcsv,
    pattern_acrmoddeltas,
    SIMPLIFY = FALSE
  )

  fns_out_ratcsv <- lapply(
    tmp,
    function(x) file.path(dir_data_rat, x)
  )


  pb <- utils::txtProgressBar(
    max = length(unlist(fns_out_ratcsv)),
    style = 3
  )
  kpb <- 0


  #--- There is work to be done ------
  if (!all(file.exists(unlist(fns_out_ratcsv)))) {

    #------ Loop over {delta, robustdelta} ------
    for (ki in seq_along(fns_out_ratcsv)) {

      if (!all(file.exists(fns_out_ratcsv[[ki]]))) {

        #------ Loop over files with deltas that need simplifying ------
        for (kf in seq_along(fns_out_ratcsv[[ki]])) {

          #--- Check that we have required input files
          tmp_fin <- fns_in_ratcsv[[ki]][[kf]]
          stopifnot(file.exists(tmp_fin))


          #--- * Load values ------
          xV <- lapply(tmp_fin, read.csv)

          tmp <- colnames(xV[[1L]])
          stopifnot(
            vapply(xV, function(x) inherits(x, "data.frame"), FUN.VALUE = NA)
          )


          #--- Identify variables and numeric/categorical type
          vars <- setdiff(
            colnames(xV[[1L]]),
            meta[["rat_header"]][[mask_sets[[km]]]]
          )

          ids_vars_cat <- which(newRR3::is_categorical(vars))
          ids_vars_num <- setdiff(seq_along(vars), ids_vars_cat)

          vtagk <- newRR3::standardize_vtags(
            newRR3::extract_fname_element(tmp_fin[[1L]], pos = 1L),
            tags = meta[["varsets"]][["tags"]]
          )

          is_robust <- grepl(
            "robust",
            x = newRR3::extract_fname_element(tmp_fin[[1L]], pos = 3L)
          )


          #--- RAT output: copy numerical data
          rat <- xV[[1L]]
          rat[, vars[ids_vars_cat]] <- NA


          #--- * Simplify categorical deltas ------
          for (kc in ids_vars_cat) {
            rat[[vars[[kc]]]] <- if (is_robust) {
              newRR3::simplify_robustdelta(
                x = xV[[1L]][[vars[[kc]]]],
                x_conv = meta[["delta"]][[vtagk]][["robust"]],
                simple_conv = meta[["delta"]][["basic"]][["robust"]]
              )
            } else {
              newRR3::simplify_delta(
                x = xV[[1L]][[vars[[kc]]]],
                x_conv = meta[["delta"]][[vtagk]][["full"]],
                simple_conv = meta[["delta"]][["basic"]][["full"]]
              )
            }
          }


          #--- * Write RATs as spreadsheet ------
          stopifnot(
            identical(nrow(rat), nrow(xV[[1L]])),
            identical(
              rat[, meta[["rat_header"]][[mask_sets[[km]]]]],
              xV[[1L]][, meta[["rat_header"]][[mask_sets[[km]]]]]
            ),
            identical(colnames(rat), colnames(xV[[1L]]))
          )


          #--- * Update variable names ------
          ids <- which(colnames(rat) %in% vars)
          colnames(rat)[ids] <- gsub(
            pattern = "delta",
            replacement = "simpledelta",
            x = colnames(rat)[ids]
          )

          write.csv(
            rat,
            file = fns_out_ratcsv[[ki]][[kf]],
            row.names = FALSE
          )

          kpb <- kpb + 1L
          utils::setTxtProgressBar(pb, kpb)
        }

      } else {
        kpb <- kpb + length(fns_out_ratcsv[[ki]])
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
