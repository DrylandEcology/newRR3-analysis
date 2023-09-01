#----- . ------
#----- CREATE GEOTIFFs FROM RATs ------
#
# * Input
#   * Any spreadsheet (used as raster attribute table) created by any of the
#     scripts: `"results/newRR3_RATs-combined/*/*.csv"`
#
# * Output
#   * `"results/newRR3_GeoTIFFs-combined/"`
#     `"[experiment]_[time]_[output-set-dir]_[value-type-dir]_[variable-set]/"`
#     `"[variable-set]__[climate-desc]_[time]__[method-description][__[variable-name]].tif"`
#
#----- . ------



#----- SET UP ------
stopifnot(requireNamespace("newRR3"))
library("doFuture")
library("foreach")
library("progressr")

n_workers <- 8L

# Select sets of RATs to convert ot GeoTIFFs
#   * "all_singleband": convert all available RAT spreadsheet to single-band GeoTIFFs
#   * "all_multiband": convert all available RAT spreadsheet to multi-band GeoTIFFs
#   * "set20230403_singleband": convert R&R RATs to single-band GeoTIFFs
#     Output: n = 88 GeoTIFFs
#       * R&R (continuous index, response categories, probabilities)
#           * ambient: n = 2 * 6 = 12
#           * Median-GCM and time x RCP: 2 * 6 * 5 = 60
#       * R&R (continuous index, response categories)
#           * Robust delta R&R for median-GCM and future time x RCP: 2 * 2 * 4 = 16
requested_geotiffs <- "set20230403_singleband"


#--- Settings ------
req_meta_version <- "3.2.0"

vtag_input <- c("preds", "novelty", "rr", "rfcertainty", "mirrp")

# 2023-Mar-22: full/simulated extent: data; rangeland mask: as attribute and for analysis
# 2023-Apr-03: create single-band GeoTIFFs with rangeland mask
# 2023-Jun-05: create single-band GeoTIFFs with full mask
mask_sets <- c("sim", "rangelands", "combined")[[1L]]
data_mask_sets <- c("sim", "rangelands", "combined")[[3L]]



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
  vtag_input %in% names(meta[["varsets"]][["tags"]])
)


#------ . ------
#------ LOOP OVER SPATIAL MASKS ------
for (km in seq_along(mask_sets)) {
  #----- . ------
  #--- SPREADSHEETS OF TABULATED RATs ------

  #--- Paths (part 2) ------
  dir_data_gt <- file.path(
    dir_res,
    paste0("newRR3_GeoTIFFs-", mask_sets[[km]])
  )

  dir_data_rat <- file.path(
    dir_res,
    paste0("newRR3_RATs-", data_mask_sets[[km]])
  )
  stopifnot(file.exists(dir_data_rat))


  #--- Input spreadsheets over which content to summarize ------
  tag_in_ratcsv <- list.files(
    path = dir_data_rat,
    pattern = ".csv$",
    recursive = TRUE,
    full.names = FALSE
  )


  #--- * Subset inputs to request ------
  if (grepl("^all_", requested_geotiffs)) {
    # keep all inputs
    used_tag_in_ratcsv <- tag_in_ratcsv

  } else if (grepl("^set20230403_", requested_geotiffs)) {
    #     Output: n = 88 GeoTIFFs
    used_vtag_input <- "rr"
    tmpv <- meta[["varsets"]][["tags"]][used_vtag_input]

    used_tag_in_ratcsv <- c(
      # Ambient values
      grep(
        pattern = paste0(tmpv, "__sc1_NA_ambient_1980-2020-clim__value-sim"),
        x = tag_in_ratcsv,
        value = TRUE
      ),
      # Median-GCM values for each time x scenario
      grep(
        pattern = paste0(tmpv, "__(.+)__value-acrmod-med"),
        x = tag_in_ratcsv,
        value = TRUE
      ),
      # Robust median-GCM deltas for each future time x scenario
      grep(
        pattern = paste0(tmpv, "__(.+)__robust090delta-acrmod-med"),
        x = tag_in_ratcsv,
        value = TRUE
      )
    )

    stopifnot(length(used_tag_in_ratcsv) == 10L)

  } else {
    stop(shQuote(requested_geotiffs), " not implemented.")
  }



  # Extract associated vtags
  vtags_fns_in <- newRR3::standardize_vtags(
    newRR3::extract_fname_element(used_tag_in_ratcsv, pos = 1L),
    tags = meta[["varsets"]][["tags"]]
  )

  stopifnot(
    length(vtags_fns_in) == length(used_tag_in_ratcsv),
    !anyNA(vtags_fns_in)
  )


  #----- . ------
  #--- Create tasks for outputs of GeoTIFFs ------

  tmp <- mapply( # nolint: undesirable_function_linter.
    function(fn, vartag) {
      if (grepl("multiband", requested_geotiffs)) {
        data.frame(
          fin = file.path(dir_data_rat, fn),
          fout = file.path(dir_data_gt, gsub(".csv$", ".tif", fn)),
          stringsAsFactors = FALSE
        )

      } else if (grepl("singleband", requested_geotiffs)) {
        vns <- meta[["varsets"]][[vartag]][["varname"]]

        data.frame(
          fin = file.path(dir_data_rat, fn),
          fout = file.path(
            dir_data_gt,
            vapply(
              vns,
              function(vn) gsub(".csv$", paste0("__", vn, ".tif"), fn),
              FUN.VALUE = NA_character_
            )
          ),
          var = vns,
          stringsAsFactors = FALSE
        )

      } else {
        stop(shQuote(requested_geotiffs), " not implemented.")
      }
    },
    used_tag_in_ratcsv,
    vtags_fns_in,
    SIMPLIFY = FALSE
  )

  list_out_gt <- do.call(rbind, tmp)


  #--- * Subset tasks to request ------
  if (grepl("^set20230403_", requested_geotiffs)) {
    #--- exclude probability deltas (but include probability values, etc.)
    ids <-
      grepl(
        "_prob.pred_",
        list_out_gt[["var"]]
      ) &
      grepl(
        paste0(
          meta[["varsets"]][["tags"]][["rr"]],
          "__(.+)__robust090delta-acrmod-med"
        ),
        list_out_gt[["fin"]]
      )

    if (sum(ids) > 0L) {
      list_out_gt <- list_out_gt[!ids, , drop = FALSE]
    }
  }

  rownames(list_out_gt) <- NULL


  # Subset to those that still need to be completed
  ids_todo <- !file.exists(list_out_gt[["fout"]])



  #--- There is work to be done ------
  if (any(ids_todo)) {

    #--- Link to imported external data ------
    fnames_gsu_template <- newRR3::import_gsu_template(
      dir_dataraw = dir_dataraw,
      mask_set = mask_sets[[km]],
      version_inputs = meta[["version_inputs"]]
    )


    #--- Create directories for results ------
    tmp <- lapply(
      unique(dirname(list_out_gt[["fout"]])),
      dir.create,
      recursive = TRUE,
      showWarnings = FALSE
    )


    #----- . ------
    #------ Start parallel loop over input files ------
    if (n_workers > 1L && !interactive()) {
      n_workers <- min(n_workers, sum(ids_todo))
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
      N_tasks <- nrow(list_out_gt)
      N_todo <- sum(ids_todo)
      p_nthstep <- max(1L, floor(N_tasks / N_tasks))
      p_nsteps <- max(1L, floor(N_tasks / p_nthstep))
      p_nstepstodo <- floor(N_todo / p_nthstep)
      p <- progressr::progressor(steps = p_nsteps)

      #--- update progressor with already completed tasks
      for (ki in seq_len(p_nsteps - p_nstepstodo)) {
        # only update progress every n-th step (or at the very end)
        if (ki %% p_nthstep == 0 || ki == N_tasks) p() # progressr object
      }

      #--- loop over tasks that need processing
      ki <- NULL

      foreach(
        ki = which(ids_todo),
        .combine = c,
        .errorhandling = "stop",
        .inorder = FALSE
      ) %dopar% {

        stopifnot(file.exists(list_out_gt[["fin"]][[ki]]))


        if (!file.exists(list_out_gt[["fout"]][[ki]])) {

          #--- Identify vtag and associated set of variable names
          vtagk <- newRR3::standardize_vtags(
            newRR3::extract_fname_element(list_out_gt[["fin"]][[ki]], pos = 1L),
            tags = meta[["varsets"]][["tags"]]
          )


          #--- * Load RAT input ------
          xV <- utils::read.csv(list_out_gt[["fin"]][[ki]])


          #--- * Apply masks ------
          if (grepl("multiband", requested_geotiffs)) {
            #--- Convert "count*" columns into "mask" indicators
            icol_counts <- grep("count", meta[["rat_header"]][[data_mask_sets[[km]]]])

            for (kic in icol_counts) {
              # Convert any x > 0 into 1s and retain NAs
              xV[[kic]] <- as.integer(as.logical(xV[[kic]]))
            }

            colnames(xV)[icol_counts] <- gsub(
              pattern = "count",
              replacement = "mask",
              x = colnames(xV)[icol_counts]
            )

            #--- Drop uninformative masks
            for (kic in icol_counts) {
              if (!anyNA(xV[[kic]])) {
                xV[[kic]] <- NULL
              }
            }

          } else {
            # Remove units outside of current mask
            imask <- grep(paste0("count_", mask_sets[[km]]), colnames(xV))
            xV <- xV[xV[[imask]] > 0 & !is.na(xV[[imask]]), , drop = FALSE]

            # Remove mask/count columns
            imasks <- grep("^count_", colnames(xV))
            if (length(imasks) > 0L) {
              xV <- xV[, -imasks, drop = FALSE]
            }
          }


          #--- * Select variable if single-band requests
          if (grepl("singleband", requested_geotiffs)) {
            used_var <- grep(
              paste0("^", list_out_gt[["var"]][[ki]], "(_|$)"),
              colnames(xV),
              value = TRUE
            )
            xV <- xV[, c("value", used_var), drop = FALSE]

            rat2 <- if (newRR3::is_categorical(used_var)) {
              lvls <- newRR3::get_levels(
                vtag = vtagk,
                vdesc = newRR3::extract_fname_element(
                  list_out_gt[["fout"]][[ki]],
                  pos = 3L
                ),
                meta = meta,
                return_labels = TRUE
              )

              sid10 <- match(
                newRR3::simplify_variables_names(used_var),
                meta[["varsets"]][[vtagk]][["varname"]]
              )

              stopifnot(!anyNA(sid10))

              stats::setNames(
                data.frame(seq_along(lvls), lvls),
                c("VALUE", meta[["varsets"]][[vtagk]][["short10"]][[sid10]])
              )
            }

          } else {
            rat2 <- NULL
          }


          #--- * Create and write GeoTIFF to disk ------
          newRR3::make_geotiff_catalyzed(
            rast_template = grep(".tif$", fnames_gsu_template, value = TRUE),
            rat = xV,
            fname = list_out_gt[["fout"]][[ki]],
            names_rat = vtagk,
            rat2 = rat2,
            overwrite = FALSE
          )
        }

        # only update progress every n-th step (or at the very end)
        if (ki %% p_nthstep == 0 || ki == N_tasks) p() # progressr object

        NULL
      }
    })


    #----- . ------
    #------ Clean up parallel loop ------
    future::plan(strategy = "future::sequential")
    progressr::handlers(global = FALSE)
    options(prev_op)
  }
}


print(summary(warnings()))

#------. ------
#------. ------
