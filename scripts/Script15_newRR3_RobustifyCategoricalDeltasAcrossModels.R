#----- . ------
#----- IDENTIFY ROBUST ACROSS-MODEL DELTAs of R&R ------
#
# Re-code categorical deltas (convolutions) to robust and non-robust variants
#
# * Input
#   * Delta `"R&R"` across-model median created by
#     `"Script12_newRR3_SummaryAcrossModels.R"`:
#     `"results/newRR3_RATs-combined/*/RR__*__delta-acrmod-med.csv"`
#   * Robustness of delta `"R&R"` across-model median created by
#     `"Script14_newRR3_AgreementDeltasAcrossModels.R"`:
#     `"results/newRR3_RATs-combined/*/RR__*__delta-acrmod-robust090.csv"`
#
# * Output
#   * Derived data for robust delta summaries/ensembles across climate models:
#     `"robust*delta"`
#   * Output
#     * `"results/newRR3_RATs-combined/"`
#       `"[experiment]_[time]_AcrMod_Deltas_RR"/`
#       `"RR__[experiment]_[time]__robust090delta-acrmod-med.csv"`
#     * Variable names: `"*_robustdelta_acrmoddistr"`
#
#----- . ------


#----- SET UP ------
stopifnot(requireNamespace("newRR3"))
options(warn = 2L) # convert warnings to errors

#--- Settings ------
req_meta_version <- "3.2.0"

vtag <- c("rr") # only "rr" has categorical variables that need newRR3::robustifying
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
  "med" %in% meta[["acrmod"]][["signal"]][["tag"]],
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
  #--- SPREADSHEET RATs OF ROBUSTIFIED ACROSS-MODEL SUMMARIES ------
  pattern_acrmoddeltas <- "__delta-acrmod-med"
  pattern_acrmodrobustdeltas <- sub(
    pattern = "delta",
    replacement = paste0(meta[["acrmod"]][["robust"]][["tag"]][[2L]], "delta"),
    x = pattern_acrmoddeltas
  )


  #--- Input files 1: median across-model deltas that need robustifying
  tmpf_in_ratcsv <- lapply(
    vtag_input,
    function(ft) {
      list.files(
        path = dir_data_rat,
        pattern = paste0(
          meta[["varsets"]][["tags"]][[ft]],
          "__(.+)",
          pattern_acrmoddeltas
        ),
        full.names = FALSE,
        recursive = TRUE
      )
    }
  )

  fns_in_ratcsv <- lapply(
    tmpf_in_ratcsv,
    function(x) file.path(dir_data_rat, x)
  )


  #--- Input files 2: across-model robustness for each fns_in_ratcsv
  tmp <- lapply(
    tmpf_in_ratcsv,
    function(tmpf) {
      vapply(
        tmpf,
        function(x) {
          file.path(
            dirname(x),
            sub(
              pattern = pattern_acrmoddeltas,
              replacement = paste0(
                "__delta-acrmod-",
                meta[["acrmod"]][["robust"]][["tag"]][[2L]]
              ),
              basename(x)
            )
          )
        },
        FUN.VALUE = NA_character_
      )
    }
  )

  fns_in2_ratcsv <- lapply(
    tmp,
    function(x) file.path(dir_data_rat, x)
  )


  #--- Create output file paths for robusitified median across-model summaries
  tmp <- lapply(
    tmpf_in_ratcsv,
    function(tmpf) {
      vapply(
        tmpf,
        function(x) {
          file.path(
            dirname(x),
            sub(
              pattern = pattern_acrmoddeltas,
              replacement = pattern_acrmodrobustdeltas,
              basename(x)
            )
          )
        },
        FUN.VALUE = NA_character_
      )
    }
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

    #------ Loop over vtags ------
    for (ki in seq_along(fns_out_ratcsv)) {

      if (!all(file.exists(fns_out_ratcsv[[ki]]))) {

        #------ Loop over files with deltas that need newRR3::robustifying ------
        for (kf in seq_along(fns_out_ratcsv[[ki]])) {

          #--- Check that we have required input files
          tmp_fin <- c(
            fns_in_ratcsv[[ki]][[kf]],
            fns_in2_ratcsv[[ki]][[kf]]
          )

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
          vars2 <- setdiff(
            colnames(xV[[2L]]),
            meta[["rat_header"]][[mask_sets[[km]]]]
          )

          stopifnot(
            identical(
              newRR3::simplify_variables_names(vars),
              newRR3::simplify_variables_names(vars2)
            )
          )


          ids_vars_cat <- which(newRR3::is_categorical(vars))
          ids_vars_num <- setdiff(seq_along(vars), ids_vars_cat)


          #--- RAT output: copy numerical data
          rat <- xV[[1L]]
          rat[, vars[ids_vars_cat]] <- NA


          #--- * newRR3::robustify categorical columns ------
          for (kc in ids_vars_cat) {
            rat[, vars[[kc]]] <- newRR3::robustify(
              x = xV[[1L]][[vars[[kc]]]],
              xrobust = xV[[2L]][[vars2[[kc]]]],
              x_conv = meta[["delta"]][[vtag[[ki]]]][["full"]],
              robust_conv = meta[["delta"]][[vtag[[ki]]]][["robust"]],
              robust_levels = meta[["acrmod"]][["robust"]][["levels"]]
            )
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
            pattern = "_delta",
            replacement = "_robustdelta",
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
