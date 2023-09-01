#----- . ------
#----- SUMMARIES ACROSS GEOGRAPHICAL UNITS ------
#
# * Input
#   * All data created by scripts
#     `"Script01_RR2022predictors_PrepareDataRelease.R"`,
#     ...,
#     `"Script16_newRR3_SimplifyCategoricalDeltasAcrossModels.R"`:
#     `"results/newRR3_RATs-combined/*/*.csv"`
#   * Tables of pixel counts for each spatial intersection,
#    created by `"Script21_newRR3_PrepareGeographicalUnits.R"`
#
# * Output
#   * Spreadsheets with summary values across geographical units
#     * `"results/SpatialUnits_Tabulations/Tabulations_bs-rangelands/"`
#       `"[experiment]_[time]_[output-set-dir]_[value-type-dir]_[variable-set]/"`
#       `"[variable-set]__[climate-desc]_[time]__[method-description]__[acrgeo-summary]__[variable-name].csv"`
#     * Variable names
#       * continuous (numeric): low = `"X.5."`, median = `"X50."`, high = `"X95."`
#       * categorical: counts for each levels, e.g., `"L"`, `"ML"`, `"M"`, `"H+MH"`
#
#----- . ------


#----- SET UP ------
stopifnot(requireNamespace("newRR3"))
library("doFuture")
library("foreach")
library("progressr")

n_workers <- 12L


#--- Settings ------
req_meta_version <- "3.2.0"

vtag <- c("preds", "novelty", "rr", "rfcertainty", "mirrp")
vtag_input <- vtag

# 2023-Mar-22: full/simulated extent: data; rangeland mask: as attribute and for analysis
mask_sets <- c("sim", "rangelands", "combined")[[3L]]
data_mask_sets <- c("sim", "rangelands", "combined")[[2L]]



#--- Paths (part 1) ------
dir_prj <- ".."

dir_dataraw <- file.path(dir_prj, "data-raw")
stopifnot(dir.exists(dir_dataraw))

dir_data <- file.path(dir_prj, "data")
stopifnot(dir.exists(dir_data))

dir_res <- file.path(dir_prj, "results")
stopifnot(dir.exists(dir_res))


#--- Paths (part 2) ------
dir_data_tu <- file.path(dir_data, "SpatialUnits_Tabulations")
dir.create(dir_data_tu, recursive = TRUE, showWarnings = FALSE)

dir_res_tu <- file.path(dir_res, "SpatialUnits_Tabulations")
dir.create(dir_res_tu, recursive = TRUE, showWarnings = FALSE)



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
  #----- . ------
  #----- SPATIAL TABULATION UNITS ------
  tag_dir_tab_x_mask <- paste0("TabulationUnits_", data_mask_sets[[km]])

  fname_ctus <- list.files(
    path = file.path(dir_data_tu, tag_dir_tab_x_mask),
    pattern = paste0(tag_dir_tab_x_mask, "[[:alnum:]_-]+\\.rds"),
    full.names = TRUE
  )

  list_tus <- vapply(
    strsplit(
      basename(fname_ctus),
      split = "__",
      fixed = TRUE
    ),
    function(x) gsub(".rds", "", x[[2L]], fixed = TRUE),
    FUN.VALUE = NA_character_
  )

  list_ctus_gsu <- lapply(fname_ctus, readRDS)
  names(list_ctus_gsu) <- list_tus

  stopifnot(length(list_ctus_gsu) > 0L)


  #----- . ------
  #--- SPREADSHEETS OF TABULATED RATs AND SPATIAL UNITS ------

  #--- Paths (part 2) ------
  dir_data_rat <- file.path(
    dir_res,
    paste0("newRR3_RATs-", mask_sets[[km]])
  )
  stopifnot(file.exists(dir_data_rat))


  #--- Input spreadsheets over which content to summarize
  tag_in_ratcsv <- list.files(
    path = dir_data_rat,
    pattern = ".csv$",
    recursive = TRUE,
    full.names = FALSE
  )

  fns_in_ratcsv <- file.path(dir_data_rat, tag_in_ratcsv)


  # Extract associated vtags
  vtags_fns_in <- newRR3::standardize_vtags(
    newRR3::extract_fname_element(tag_in_ratcsv, pos = 1L),
    tags = meta[["varsets"]][["tags"]]
  )

  stopifnot(
    length(vtags_fns_in) == length(fns_in_ratcsv),
    !anyNA(vtags_fns_in)
  )


  #--- Create output file paths for summaries across spatial units
  # will need to append variable name to finalize file names
  fns_out_ratcsv <- mapply(
    function(fn, vartag) {
      list_tabs_x_varnames <- expand.grid(
        tabulation = names(list_ctus_gsu),
        variable = meta[["varsets"]][[vartag]][["varname"]],
        KEEP.OUT.ATTRS = FALSE,
        stringsAsFactors = FALSE
      )
      file.path(
        dir_res_tu,
        paste0("Tabulations_bs-", data_mask_sets[[km]]),
        dirname(fn),
        paste0(
          "Tabulation__",
          sub(".csv$", "", basename(fn)),
          "__acrgeo-", list_tabs_x_varnames[["tabulation"]],
          "__", list_tabs_x_varnames[["variable"]],
          ".csv"
        )
      )
    },
    tag_in_ratcsv,
    vtags_fns_in
  )

  stopifnot(
    length(fns_out_ratcsv) == length(fns_in_ratcsv),
    lengths(fns_out_ratcsv) > 0L
  )



  #--- There is work to be done ------
  if (!all(file.exists(unlist(fns_out_ratcsv)))) {

    #--- Create directories for results ------
    tmp <- lapply(
      unique(dirname(unlist(fns_out_ratcsv))),
      dir.create,
      recursive = TRUE,
      showWarnings = FALSE
    )


    #------ Start parallel loop over input files ------
    if (n_workers > 1L && !interactive()) {
      future::plan(strategy = "future::multisession", workers = n_workers)
    } else {
      future::plan(strategy = "future::sequential")
    }
    doFuture::registerDoFuture()
    doRNG::registerDoRNG(12345) # Initialize parallel RNG stream

    progressr::handlers(global = TRUE)
    progressr::handlers("txtprogressbar")
    prev_op <- options(progressr.enable = TRUE)

    progressr::with_progress({
      N_infiles <- length(fns_in_ratcsv)
      p_nthstep <- max(1L, floor(N_infiles / 100))
      p <- progressr::progressor(
        steps = max(1L, floor(N_infiles / p_nthstep))
      )

      ki <- NULL

      foreach(
        ki = seq_len(N_infiles),
        .combine = c,
        .errorhandling = "stop",
        .inorder = FALSE
      ) %dopar% {

        stopifnot(file.exists(fns_in_ratcsv[[ki]]))


        if (!all(file.exists(unlist(fns_out_ratcsv[[ki]])))) {

          #--- Identify vtag and associated set of variable names
          vtagk <- newRR3::standardize_vtags(
            newRR3::extract_fname_element(fns_in_ratcsv[[ki]], pos = 1L),
            tags = meta[["varsets"]][["tags"]]
          )

          vars <- meta[["varsets"]][[vtagk]][["varname"]]
          ids_vars_cat <- which(newRR3::is_categorical(vars))
          ids_vars_num <- setdiff(seq_along(vars), ids_vars_cat)


          #------ Load input ------
          xV <- utils::read.csv(fns_in_ratcsv[[ki]])


          #--- Subset to current data mask
          if (mask_sets[[km]] == "combined") {
            tmpv <- paste0("count_", data_mask_sets[[km]])
            ids <- !is.na(xV[[tmpv]]) & xV[[tmpv]] > 0
            stopifnot(sum(ids) > 0L)
            xV <- xV[ids, , drop = FALSE]
          }


          #------ Loop over tabulation outputs derived from current input file ------
          for (kf in seq_along(fns_out_ratcsv[[ki]])) {

            if (!file.exists(fns_out_ratcsv[[ki]][[kf]])) {

              #--- * Identify variable and spatial unit set for tabulations ------
              e5 <- newRR3::extract_fname_element(
                fns_out_ratcsv[[ki]][[kf]],
                pos = 5L
              )
              stopifnot(grepl("acrgeo-", e5))
              ctukt <- sub("acrgeo-", "", e5)

              # Locate actually used variable name
              varkt <- newRR3::extract_fname_element(
                fns_out_ratcsv[[ki]][[kf]],
                pos = 6L
              )
              stopifnot(!is.null(varkt))

              varkt_used <- grep(
                pattern = paste0("(^", varkt, "$)|(^", varkt, "_)"),
                x = colnames(xV),
                value = TRUE
              )
              stopifnot(length(varkt_used) == 1L)
              is_var_categorical <- newRR3::is_categorical(varkt_used)


              var_levels <- if (is_var_categorical) {
                newRR3::get_levels(
                  vtag = vtagk,
                  vdesc = newRR3::extract_fname_element(
                    fns_out_ratcsv[[ki]][[kf]],
                    pos = 4L
                  ),
                  meta = meta,
                  return_labels = FALSE
                )
              }

              #--- * Calculate summaries across geographical units ------
              # Note: we ignore here the "count" contained in `xV` because
              # the correct count for the spatial intersection is provided by
              # `list_ctus_gsu[[ctukt]]`

              rat <- newRR3::tabulate_acrgeo(
                x = xV[, c("value", varkt_used), drop = FALSE],
                varname = varkt_used,
                is_var_categorical = is_var_categorical,
                ctus = list_ctus_gsu[[ctukt]],
                fun = if (is_var_categorical) {
                  newRR3::fun_acrossgeo_categorical
                } else {
                  newRR3::fun_acrossgeo_numeric
                },
                levels = var_levels
              )


              if (is_var_categorical) {
                #--- Fix column names: use labels instead of `X1`, ...
                var_labels <- newRR3::get_levels(
                  vtag = vtagk,
                  vdesc = newRR3::extract_fname_element(
                    fns_out_ratcsv[[ki]][[kf]],
                    pos = 4L
                  ),
                  meta = meta,
                  return_labels = TRUE
                )

                ids <- grep("^X[[:digit:]]+$", colnames(rat))
                stopifnot(
                  length(ids) == length(var_levels),
                  length(ids) == length(var_labels)
                )
                colnames(rat)[ids] <- var_labels
              }


              #--- * Write RATs as spreadsheet ------
              write.csv(
                rat,
                file = fns_out_ratcsv[[ki]][[kf]],
                row.names = FALSE
              )
            }
          }
        }

        # only update progress every n-th step (or at the very end)
        if (ki %% p_nthstep == 0 || ki == N_infiles) p() # progressr object

        NULL
      }
    })


    #------ Clean up parallel loop ------
    future::plan(strategy = "future::sequential")
    progressr::handlers(global = FALSE)
    options(prev_op)
  }

}


print(summary(warnings()))

#------. ------
#------. ------
