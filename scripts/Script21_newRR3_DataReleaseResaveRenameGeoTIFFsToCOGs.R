#----- . ------
#----- RESAVE GEOTIFFs AS COGs AND RENAME FOR DATA RELEASE ------
#
# * Input
#   * `"results/newRR3_GeoTIFFs-combined/"`
#     `"[experiment]_[time]_[output-set-dir]_[value-type-dir]_[variable-set]/"`
#     `"[variable-set]__[climate-desc]_[time]__[method-description][__[variable-name]].tif"`
#
# * Output
#   * `"results/newRR3_COGs-combined/"`
#     `"[experiment]_[time]_[output-set-dir]_[value-type-dir]_[variable-set]/"`
#     `"[variable-set]__[climate-desc]_[time]__[method-description][__[variable-name]].tif"`
#
#----- . ------



#----- SET UP ------
stopifnot(requireNamespace("newRR3"))
library("doFuture")
library("foreach")
library("progressr")

filetype <- "COG" # c("GTiff", "COG")

n_workers <- 2L


#--- Settings ------
req_meta_version <- "3.2.0"

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


#------ Download COG validation script ------
# see https://github.com/cogeotiff/cog-spec/blob/master/spec.md
fname_validcog <- file.path(
  dir_dataraw,
  "validate_cloud_optimized_geotiff.py"
)

if (!file.exists(fname_validcog)) {
  res <- download.file(
    url = "https://raw.githubusercontent.com/OSGeo/gdal/master/swig/python/gdal-utils/osgeo_utils/samples/validate_cloud_optimized_geotiff.py",
    destfile = fname_validcog
  )

  if (res != 0L) {
    stop("Error downloading 'validate_cloud_optimized_geotiff.py'.")
  }
}



#------ Load project metadata ------
meta <- newRR3::get_project_description(
  include_variables = TRUE,
  include_scenarios = TRUE,
  include_deltas = TRUE,
  include_summaries = TRUE,
  dir_data = dir_dataraw
)
stopifnot(!is.null(meta) && meta[["v"]] == req_meta_version)


#------ . ------
#------ LOOP OVER SPATIAL MASKS ------
for (km in seq_along(mask_sets)) {
  #----- . ------
  #--- SPREADSHEETS OF TABULATED RATs ------

  #--- Paths (part 2) ------
  dir_data_gtin <- file.path(
    dir_res,
    paste0("newRR3_GeoTIFFs-", mask_sets[[km]])
  )
  stopifnot(file.exists(dir_data_gtin))

  dir_data_gtout <- file.path(
    dir_res,
    paste0("newRR3_COGs-", mask_sets[[km]])
  )
  dir.create(dir_data_gtout, recursive = TRUE, showWarnings = FALSE)


  #--- GeoTIFFs to convert to COGs ------
  fin_gtiffs <- list.files(dir_data_gtin, pattern = ".tif$", recursive = TRUE)


  #--- Convert GeoTIFFs to COGs and rename ------
  fname_new <- file.path(dir_data_gtout, "renamedFiles_dataRelease.csv")

  if (file.exists(fname_new)) {
    tmp <- read.csv(fname_new)
    fin_gtiffs <- tmp[["originalName"]]
    fout_cogs <- tmp[["dataReleaseName"]]

  } else {

    dtmp <- dirname(fin_gtiffs) |>
      strsplit(split = "_", fixed = TRUE) |>
      vapply(
        FUN = function(x) {
          x1 <- paste0(toupper(substr(x[[1L]], 1L, 1L)), substring(x[[1L]], 2L))
          if (length(x) == 5L) {
            paste(x[[5L]], x1, x[[4L]], sub("-clim", "", x[[2L]]), sep = "_")
          } else {
            x1
          }
        },
        FUN.VALUE = NA_character_
      )

    ftmp <- gsub(".tif$", "", basename(fin_gtiffs)) |>
      strsplit(split = "__", fixed = TRUE) |>
      vapply(
        FUN = function(x) {
          tmp <- if (length(x) == 4L) {
            x4 <- strsplit(x[[4L]], split = "_", fixed = TRUE)[[1L]]
            x2 <- strsplit(x[[2L]], split = "_", fixed = TRUE)[[1L]]
            paste(
              switch(
                EXPR = substr(x4[[1L]], 1L, 6L),
                Resili = "Resil",
                Resist = "Resist"
              ),
              switch(
                EXPR = substr(x4[[2L]], 1L, 3L),
                res = "Resp",
                ind = "Index",
                pro = paste0("ProbPred_", sub(".", "", x4[[3L]], fixed = TRUE))
              ),
              sub("-clim$", "", x2[[length(x2)]]),
              sep = "_"
            )
          } else {
            "Mask"
          }
          paste0(tmp, ".tif")
        },
        FUN.VALUE = NA_character_
      )

    fout_cogs <- file.path(dtmp, ftmp)

    utils::write.csv(
      x = data.frame(
        originalName = fin_gtiffs,
        dataReleaseName = fout_cogs
      ),
      file = fname_new
    )
  }

  dfin_gtiffs <- file.path(dir_data_gtin, fin_gtiffs)
  dfout_cogs <- file.path(dir_data_gtout, fout_cogs)



  #--- Create directories for results ------
  tmp <- lapply(
    unique(dirname(dfout_cogs)),
    dir.create,
    recursive = TRUE,
    showWarnings = FALSE
  )


  #------ . ------
  #------ Start parallel loop over input files ------
  ids_todo <- file.exists(dfin_gtiffs) & !file.exists(dfout_cogs)

  if (any(ids_todo)) {
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
      N_tasks <- length(dfout_cogs)
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

        stopifnot(file.exists(dfin_gtiffs[[ki]]))

        if (!file.exists(dfout_cogs[[ki]])) {

          #--- * Resave GeoTIFF as COG ------
          terra::writeRaster(
            x = terra::rast(dfin_gtiffs[[ki]]),
            filename = dfout_cogs[[ki]],
            overwrite = FALSE,
            filetype = "COG",
            wopt = list(gdal = "COMPRESS=DEFLATE")
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


  #------ . ------
  #------ Check for valid COGs ------
  checked_cogs <- vapply(
    dfout_cogs,
    function(fname_cog) {
      system2(
        command = "python",
        args = c(fname_validcog, fname_cog),
        stdout = TRUE
      ) |>
      grepl(
        pattern = "is a valid cloud optimized GeoTIFF",
        x = _
      ) |>
      any()
    },
    FUN.VALUE = NA
  )

  if (any(!checked_cogs)) {
    stop(
      "Some files are not valid COGs: ",
      toString(shQuote(basename(dfout_cogs[!checked_cogs])))
    )
  }
}


print(summary(warnings()))

#------. ------
#------. ------
