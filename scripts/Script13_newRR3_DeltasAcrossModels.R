#----- . ------
#----- DELTAS OF ACROSS-CLIMATE MODELS SUMMARIES (GCMs WITHIN RCP x TIMEPERIODS) ------
#
# * Input
#   * Data created by `"Script12_newRR3_SummaryValuesAcrossModels.R"`:
#     `"results/newRR3_RATs-combined/*_AcrMod_Values_*"`
#
# * Output
#   * Derived data for summaries/ensembles across climate models:
#     low, median, high values
#   * Output
#     * `results/newRR3_RATs-combined/`
#       `"[experiment]_[time]_AcrMod_Deltas_[variable-set]"/`
#       `"[variable-set]__[experiment]_[time]__delta-acrmod-[acrmod-summary].csv"`
#     * Variable names: `"*_delta_acrmoddistr"`
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


  #----- . ------
  #--- SPREADSHEET RATs OF ACROSS-MODEL SUMMARY VALUES ------


  #--- Input data folders (values of `AcrMod`) for which to calculate deltas
  # expect that each folder contain a set of files one for each climate model (GCM),
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
    gsub("_AcrMod_Values_", "_AcrMod_Deltas_", basename(dns_in_ratcsv))
  )


  #--- Create output file paths for deltas of all across-model summaries
  tmp_out <- dns_in_ratcsv |>
    lapply(function(path) list.files(path, pattern = "-med")[1L]) |>
    unlist() |>
    gsub(pattern = "-med", replacement = "", x = _) |>
    gsub(pattern = "value-", replacement = "delta-", x = _) |>
    gsub(pattern = ".csv$", replacement = "", x = _) |>
    strsplit(split = "_")

  stopifnot(length(table(lengths(tmp_out))) == 1L)

  tmp_out <- tmp_out |>
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


    #------ Loop over sets of `AcrMod` ------
    for (ki in seq_along(dns_in_ratcsv)) {

      if (!all(file.exists(fns_out_ratcsv[["signal"]][[ki]]))) {

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


        #--- * ------
        #--- * Calculate deltas of across climate model summaries ------

        #--- ** Identify files with across-model value summaries ------

        # Future projected across-model value summaries
        fnames_future <-
          fns_out_ratcsv[["signal"]][[ki]] |>
          gsub(pattern = "AcrMod_Deltas", replacement = "AcrMod_Values", x = _) |>
          gsub(pattern = "delta-acrmod", replacement = "value-acrmod", x = _)

        # Historical reference projected across-model value summaries
        fnames_hist <- gsub(
          pattern = paste0(
            meta[["simexps"]][["tag_acrmod"]][[k0]],
            collapse = "|"
          ),
          replacement = meta[["simexps"]][["tag_acrmod"]][[1L]],
          x = fnames_future
        )

        stopifnot(
          file.exists(fnames_future),
          file.exists(fnames_hist),
          mapply(
            function(tag, fname) grepl(tag, fname),
            meta[["acrmod"]][["signal"]][["tag"]],
            basename(fnames_future)
          ),
          mapply(
            function(tag, fname) grepl(tag, fname),
            meta[["acrmod"]][["signal"]][["tag"]],
            basename(fnames_hist)
          )
        )


        #--- Load values
        xV <- lapply(fnames_future, read.csv)
        xHist <- lapply(fnames_hist, read.csv)

        tmp <- colnames(xV[[1L]])
        stopifnot(
          vapply(xV, function(x) inherits(x, "data.frame"), FUN.VALUE = NA),
          vapply(xV, function(x) identical(colnames(x), tmp), FUN.VALUE = NA),
          vapply(xHist, function(x) inherits(x, "data.frame"), FUN.VALUE = NA),
          vapply(xHist, function(x) identical(colnames(x), tmp), FUN.VALUE = NA)
        )


        #--- Identify variables and numeric/categorical type
        vars <- setdiff(
          colnames(xV[[1L]]),
          meta[["rat_header"]][[mask_sets[[km]]]]
        )

        stopifnot(grepl("_acrmoddistr", vars, fixed = TRUE))

        ids_vars_cat <- which(newRR3::is_categorical(vars))
        ids_vars_num <- setdiff(seq_along(vars), ids_vars_cat)


        #--- Reformat to array
        mV <- array(
          data = unlist(lapply(xV, function(x) x[, vars, drop = FALSE])),
          dim = c(nrow(xV[[1L]]), length(vars), length(xV)),
          dimnames = list(NULL, vars, meta[["acrmod"]][["signal"]][["tag"]])
        )
        mHist <- array(
          data = unlist(lapply(xHist, function(x) x[, vars, drop = FALSE])),
          dim = c(nrow(xHist[[1L]]), length(vars), length(xHist)),
          dimnames = list(NULL, vars, meta[["acrmod"]][["signal"]][["tag"]])
        )

        stopifnot(dim(mV) == dim(mHist))


        #--- ** Calculate deltas ------
        tmp_acrmod <- array(
          dim = dim(mV),
          dimnames = dimnames(mV)
        )


        #--- *** Numerical deltas ------
        if (length(ids_vars_num) > 0L) {
          tmp_acrmod[, ids_vars_num, ] <-
            mV[, ids_vars_num, , drop = FALSE] -
            mHist[, ids_vars_num, , drop = FALSE]
        }


        #--- *** Categorical deltas ------
        for (kv in ids_vars_cat) {
          #--- **** Delta RR: calculate convoluted R&R from across-model value summaries ------
          if (
            vars[[kv]] %in%
              c("Resistance_response_acrmoddistr", "Resilience_response_acrmoddistr")
          ) {
            for (kf in seq_along(meta[["acrmod"]][["signal"]][["tag"]])) {
              rv <- mV[, kv, kf, drop = TRUE]
              rh <- mHist[, kv, kf, drop = TRUE]

              stopifnot(
                max(rv, na.rm = TRUE) <= length(meta[["varsets"]][["rr"]][["levels"]]),
                max(rh, na.rm = TRUE) <= length(meta[["varsets"]][["rr"]][["levels"]])
              )

              # calculate convolution
              tmp_acrmod[, kv, kf] <- newRR3::convolve_levels(
                to = meta[["varsets"]][["rr"]][["levels"]][rv],
                from = meta[["varsets"]][["rr"]][["levels"]][rh],
                clevels = meta[["delta"]][["rr"]][["full"]][["levels"]]
              )
            }

          } else {
            stop(
              "Deltas for climate model value summaries are not implemented for ",
              vars[[kv]]
            )
          }
        }


        #--- *** Update variable names ------
        vars_delta <- gsub(
          pattern = "_acrmoddistr",
          replacement = "_delta_acrmoddistr",
          x = vars,
          fixed = TRUE
        )
        colnames(tmp_acrmod) <- vars_delta


        #--- RAT output template
        rat_template <- xV[[1L]]
        ids <- match(vars, colnames(rat_template))
        colnames(rat_template)[ids] <- vars_delta
        rat_template[, vars_delta] <- NA


        #--- ** Write RATs as spreadsheet ------
        for (kf in seq_along(meta[["acrmod"]][["signal"]][["tag"]])) {
          rat <- rat_template
          rat[, vars_delta] <- tmp_acrmod[, , kf]

          stopifnot(
            identical(dim(rat), dim(xV[[1L]])),
            identical(
              rat[, meta[["rat_header"]][[mask_sets[[km]]]]],
              xV[[1L]][, meta[["rat_header"]][[mask_sets[[km]]]]]
            )
          )

          write.csv(
            rat,
            file = fns_out_ratcsv[["signal"]][[ki]][[kf]],
            row.names = FALSE
          )

          kpb <- kpb + 1L
          utils::setTxtProgressBar(pb, kpb)
        }

      } else {
        kpb <- kpb + length(fns_out_ratcsv[["signal"]][[ki]])
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
