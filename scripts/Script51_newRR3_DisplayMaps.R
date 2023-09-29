#----- . ------
#----- MAPS ------
#
#    * Output:
#      * `"displayitems/Maps_manuscript_*/"`
#      * `"displayitems/Maps_supplementary_*/"`
#
#----- . ------



#----- SET UP ------
stopifnot(requireNamespace("newRR3"))
options(warn = 1L) # 2L, convert warnings to errors
sf::sf_use_s2(TRUE)
verbose <- TRUE # TRUE, print file names as they are processed


#--- Settings ------
req_meta_version <- "3.2.0"

vtag_input <- c("rr", "rfcertainty", "novelty", "preds", "mirrp")

# 2023-Mar-22: full/simulated extent: data; rangeland mask: as attribute and for analysis
mask_sets <- c("sim", "rangelands", "combined")[[3L]]
data_mask_sets <- c("sim", "rangelands", "combined")[[2L]]


# Figure maps
style_figures <- "ESA" # ESA, e.g., "(a)"; other, e.g., "A)"

downsample_raster <- 20L
crs_map <- 6350
add_nonrobust_crosshatching <- TRUE # add crosshatching indicating areas of nonrobust change

do_exploratory_maps <- FALSE # produce maps not included in manuscript/appendix
do_manuscript_maps <- TRUE # produce maps included as figures in manuscript
do_appendix_maps <- FALSE # produce maps included as figures in appendix



#--- Paths (part 1) ------
dir_prj <- ".."

dir_dataraw <- file.path(dir_prj, "data-raw")
stopifnot(dir.exists(dir_dataraw))

dir_res <- file.path(dir_prj, "results")
stopifnot(dir.exists(dir_res))

dir_dataraw_tu <- file.path(dir_dataraw, "spatialunits")
stopifnot(dir.exists(dir_dataraw_tu))


dir_outitems <- file.path(dir_prj, "displayitems")
dir.create(dir_outitems, recursive = TRUE, showWarnings = FALSE)




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
  vtag_input %in% names(meta[["varsets"]][["tags"]]),
  grepl("robust", meta[["acrmod"]][["robust"]][["tag"]][[2L]])
)




#------ . ------
#------ LOOP OVER SPATIAL MASKS ------
for (km in seq_along(mask_sets)) {

  #------ Load spatial data ------

  #--- * suid geotiff ------
  list_gsu <-
    newRR3::import_gsu_template(
      dir_dataraw = dir_dataraw,
      mask_set = data_mask_sets[[km]],
      version_inputs = meta[["version_inputs"]]
    ) |>
    grep(".tif$", x = _, value = TRUE) |>
    newRR3::process_raster(
      downsample = downsample_raster
    )


  #--- * polygons ------
  st_sagebiome <- newRR3::load_sagebiome_polygon(dir_dataraw_tu, crs_map)

  st_NEadmin1 <- newRR3::load_NEadmin1_polygon(dir_dataraw_tu, crs_map)

  st_ecoregionL3 <- newRR3::load_ecoregions_polygon(dir_dataraw_tu, crs_map)



  #------ Paths (part 2) ------
  dir_data_rat <- file.path(
    dir_res,
    paste0("newRR3_RATs-", mask_sets[[km]])
  )
  stopifnot(file.exists(dir_data_rat))



  #------ . ------
  #------ Map settings ------
  map_bbox <- sf::st_bbox(st_sagebiome)
  # See `ggplot2::coord_sf()$aspect` to figure out the aspect ratio:
  # sf::st_is_longlat(sf::st_crs(crs_map)) is FALSE => ratio = 1
  asp <- (map_bbox[["ymax"]] - map_bbox[["ymin"]]) / (map_bbox[["xmax"]] - map_bbox[["xmin"]])

  n_models <- length(
    unique(meta[["simexps"]][["meta_scen"]][["hist"]][["Model"]][-1L])
  )

  res_bitmap <- switch(EXPR = style_figures, ESA = 600, 300)
  fun_dev_bitmap <- switch(
    EXPR = style_figures,
    ESA = grDevices::tiff,
    grDevices::png
  )
  fig_ext <- switch(EXPR = style_figures, ESA = "tif", "png")
  tag_style <- paste(
    "style",
    switch(EXPR = style_figures, ESA = "esa", "other"),
    fig_ext,
    sep = "-"
  )


  #------ . ------
  #--- Maps v2 ------

  #------ ** Define figures ------
  size_panel_in <- switch(
    EXPR = style_figures,
    # ESA author guidelines
    #   * single-column width sizing of 8.5 cm
    #   * maximum size is 18 cm wide by 24 cm tall
    ESA = 18 / 2 / 2.54,
    4 # 4 x 4 inch
  )

  lvls_robustconsistency <- c("Other", "Robust")

  used_vtag_input <- c(
    "rr", "rrplow",
    "rfcertaintyp1st", "rfcertaintydpe",
    "noveltyAll", "novelty2v"
  )

  implemented_vtags <- c(
    used_vtag_input,
    "rslp4", "rstp4",
    "rragreeFrq", "rragreeCnt",
    "rrindex",
    "rrindexagreeFrq", "rrindexagreeCnt",
    "preds1to5", "preds6to10", "preds11to15", "preds16to19",
    "mirrp"
  )


  get_vars_to_map <- function(vtagk) {
    stopifnot(vtagk %in% implemented_vtags)

    switch(
      EXPR = vtagk,
      rr = c(
        Resilience = "Resilience_response",
        Resistance = "Resistance_response"
      ),
      rrindex = c(
        Resilience = "Resilience_index",
        Resistance = "Resistance_index"
      ),
      rrplow = c(
        `p(L-resilience)` = "Resilience_prob.pred_L",
        `p(L-resistance)` = "Resistance_prob.pred_L"
      ),
      rslp4 = c(
        `p(L-resilience)` = "Resilience_prob.pred_L",
        `p(ML-resilience)` = "Resilience_prob.pred_ML",
        `p(M-resilience)` = "Resilience_prob.pred_M",
        `p(H+MH-resilience)` = "Resilience_prob.pred_H.MH"
      ),
      rstp4 = c(
        `p(L-resistance)` = "Resistance_prob.pred_L",
        `p(ML-resistance)` = "Resistance_prob.pred_ML",
        `p(M-resistance)` = "Resistance_prob.pred_M",
        `p(H+MH-resistance)` = "Resistance_prob.pred_H.MH"
      ),
      rragreeFrq = , rragreeCnt = c(
        `Climate projection consistency (resilience)` = "Resilience_response_delta_acrmodagree",
        `Climate projection consistency (resistance)` = "Resistance_response_delta_acrmodagree"
      ),
      rrindexagreeFrq = , rrindexagreeCnt = c(
        `Climate projection consistency (resilience)` = "Resilience_index_delta_acrmodagree",
        `Climate projection consistency (resistance)` = "Resistance_index_delta_acrmodagree"
      ),
      rfcertaintyp1st = c(
        `p(1st-resilience)` = "Resilience_pfirst",
        `p(1st-resistance)` = "Resistance_pfirst"
      ),
      rfcertaintydpe = c(
        `RF-certainty (resilience)` = "Resilience_excessp",
        `RF-certainty (resistance)` = "Resistance_excessp"
      ),
      noveltyAll = c(
        `Novelty (NT1)` = "NT1",
        `Novelty (NT1-mod070)` = "NT1mod070",
        `Novelty (NT1-mod071)` = "NT1mod071",
        `Novelty (NT2)` = "NT2",
        `Novelty (NT2-mod070)` = "NT2mod070",
        `Novelty (NT2-mod071)` = "NT2mod071",
        `Novelty (AOA070-rdi)` = "AOArdi070",
        `Novelty (AOA071-rdi)` = "AOArdi071",
        `Novelty (AOA070-rdi-wRSL)` = "AOArdi070wRSL",
        `Novelty (AOA071-rdi-wRSL)` = "AOArdi071wRSL",
        `Novelty (AOA070-rdi-wRST)` = "AOArdi070wRST",
        `Novelty (AOA071-rdi-wRST)` = "AOArdi071wRST"
      ),
      novelty2v = c(
        `Novelty (NT1)` = "NT1",
        `Novelty (DI-ratio)` = "AOArdi071"
      ),
      preds1to5 = c(
        `Mean Tmean (C)` = "Tmean_mean",
        `Mean Diurnal Trange (C)` = "Trange_diurnal_mean",
        `Mean Coldest Month Tmean (C)` = "Tmean_coldestmonth_mean",
        `SD Coldest Month Tmean (C)` = "Tmean_coldestmonth_sd",
        `SD Hottest Month Tmean (C)` = "Tmean_hottestmonth_sd"
      ),
      preds6to10 = c(
        `Mean PPT (mm)` = "PPT_mean",
        `CV PPT (mm/mm)` = "PPT_cv",
        `Mean Rain (mm)` = "Rain_mean",
        `Mean Jul-Aug PPT (mm)` = "PPTinJAS_mean",
        `Mean Driest Month PPT (mm)` = "PPT_driestmonth_mean"
      ),
      preds11to15 = c(
        `CV PET (mm/mm)` = "PET_cv",
        `CV ET (mm/mm)` = "ET_cv",
        `Mean CWD (mm)` = "CWD_mean",
        `Mean Seasonal Timing CWD (-)` = "CWD_mon_corr_temp_mean",
        `Mean Seasonal Variation CWD (mm/mm)` = "CWD_mon_cv_mean"
      ),
      preds16to19 = c(
        `Mean DDD (C x day)` = "DDD_mean",
        `Mean Dry Soil Duration (day)` = "DSI_duration_mean",
        `Mean Seasonal Timing PPT (-)` = "CorTempPPT_mean",
        `Diffuse Recharge (mm)` = "DeepDrainage_mean"
      ),
      mirrp = c(
        `MIP (RSL)` = "Resilience_mirrp",
        `MIP (RST)` = "Resistance_mirrp"
      ),
      stop(vtagk, " not implemented but should be (see `implemented_vtags`).")
    )
  }

  get_vtag_input <- function(vtagk, tags = meta[["varsets"]][["tags"]]) {
    stopifnot(vtagk %in% implemented_vtags)

    if (vtagk %in% names(tags)) {
      tags[vtagk]
    } else {
      switch(
        EXPR = vtagk,
        rrplow = , rragreeFrq = , rragreeCnt = , rrindexagreeFrq = , rrindexagreeCnt = , rslp4 = , rstp4 = , rrindex = tags[["rr"]],
        novelty2v = tags[["novelty"]],
        noveltyAll = tags[["novelty"]],
        rfcertaintyp1st = tags[["rfcertainty"]],
        rfcertaintydpe = tags[["rfcertainty"]],
        preds1to5 = , preds6to10 = , preds11to15 = , preds16to19 = tags[["preds"]],
        stop(vtagk, " not implemented but should be (see `implemented_vtags`).")
      )
    }
  }


  #--- *** List of figure definitions: ------
  list_def_maps <- c(
    #--- **** Fig: 2 rows (vals): ambient + one of {RCP x time} ------
    {
      tmp <- expand.grid(
        vtagk = c(used_vtag_input, "rrindex"),
        acrmod = unlist(meta[["simexps"]][["tag_acrmod"]]),
        stringsAsFactors = FALSE,
        KEEP.OUT.ATTRS = FALSE
      )
      mapply(
        function(vtagk, acrmod) {
          list(
            inp = data.frame(
              vtag = vtagk,
              xtag = c(meta[["simexps"]][["tag_scen1"]], acrmod),
              ftag = c("value", "value"),
              dtag = c("IndRuns_Values", "AcrMod_Values"),
              otag = c("value-sim", "value-acrmod-med"),
              stringsAsFactors = FALSE
            ),
            type = "v2ap",
            fix_limits_within_ftag = FALSE
          )
        },
        tmp[["vtagk"]],
        tmp[["acrmod"]],
        SIMPLIFY = FALSE
      )
    },
    #--- **** Fig: 2 rows (vals): future RCP: both time periods ------
    {
      tmp <- expand.grid(
        vtagk = c(used_vtag_input, "mirrp"),
        acrmod = meta[["simexps"]][["tag_acrmod"]][-1L],
        stringsAsFactors = FALSE,
        KEEP.OUT.ATTRS = FALSE
      )
      mapply(
        function(vtagk, acrmod) {
          list(
            inp = data.frame(
              vtag = vtagk,
              xtag = c(acrmod[[1L]], acrmod[[2L]]),
              ftag = c("value", "value"),
              dtag = c("AcrMod_Values", "AcrMod_Values"),
              otag = c("value-acrmod-med", "value-acrmod-med"),
              stringsAsFactors = FALSE
            ),
            type = "v2pp",
            fix_limits_within_ftag = FALSE
          )
        },
        tmp[["vtagk"]],
        tmp[["acrmod"]],
        SIMPLIFY = FALSE
      )
    },
    #--- **** Fig: 2 rows (vals, deltas): ambient + one of {future RCP x time} ------
    {
      tmp <- expand.grid(
        vtagk = c(used_vtag_input, "rrindex"),
        acrmod = unlist(meta[["simexps"]][["tag_acrmod"]][-1L]),
        stringsAsFactors = FALSE,
        KEEP.OUT.ATTRS = FALSE
      )
      mapply(
        function(vtagk, acrmod) {
          list(
            inp = data.frame(
              vtag = vtagk,
              xtag = c(meta[["simexps"]][["tag_scen1"]], acrmod),
              ftag = c("value", "delta"),
              dtag = c("IndRuns_Values", "AcrMod_Deltas"),
              otag = c(
                "value-sim",
                if (vtagk == "rr") {
                  "robust090simpledelta-acrmod-med"
                } else {
                  "delta-acrmod-med"
                }
              ),
              stringsAsFactors = FALSE
            ),
            type = "vd2ap",
            fix_limits_within_ftag = FALSE
          )
        },
        tmp[["vtagk"]],
        tmp[["acrmod"]],
        SIMPLIFY = FALSE
      )
    },
    #--- **** Fig: 2 rows (deltas): future RCP: both time periods ------
    {
      tmp <- expand.grid(
        vtagk = used_vtag_input,
        acrmod = meta[["simexps"]][["tag_acrmod"]][-1L],
        stringsAsFactors = FALSE,
        KEEP.OUT.ATTRS = FALSE
      )
      mapply(
        function(vtagk, acrmod) {
          list(
            inp = data.frame(
              vtag = vtagk,
              xtag = c(acrmod[[1L]], acrmod[[2L]]),
              ftag = c("delta", "delta"),
              dtag = c("AcrMod_Deltas", "AcrMod_Deltas"),
              otag = if (vtagk == "rr") {
                c("robust090simpledelta-acrmod-med", "robust090simpledelta-acrmod-med")
              } else {
                c("delta-acrmod-med", "delta-acrmod-med")
              },
              stringsAsFactors = FALSE
            ),
            type = "d2pp",
            fix_limits_within_ftag = FALSE
          )
        },
        tmp[["vtagk"]],
        tmp[["acrmod"]],
        SIMPLIFY = FALSE
      )
    },
    #--- **** Fig: 2 rows (vals, deltas): (vals, delta) of one of {future RCP x time} ------
    {
      tmp <- expand.grid(
        vtagk = c(used_vtag_input, "rrindex"),
        acrmod = unlist(meta[["simexps"]][["tag_acrmod"]][-1L]),
        stringsAsFactors = FALSE,
        KEEP.OUT.ATTRS = FALSE
      )
      mapply(
        function(vtagk, acrmod) {
          list(
            inp = data.frame(
              vtag = vtagk,
              xtag = c(acrmod, acrmod),
              ftag = c("value", "delta"),
              dtag = c("AcrMod_Values", "AcrMod_Deltas"),
              otag = c(
                "value-acrmod-med",
                if (vtagk == "rr") {
                  "robust090simpledelta-acrmod-med"
                } else {
                  "delta-acrmod-med"
                }
              ),
              stringsAsFactors = FALSE
            ),
            type = "vd2pp",
            fix_limits_within_ftag = FALSE
          )
        },
        tmp[["vtagk"]],
        tmp[["acrmod"]],
        SIMPLIFY = FALSE
      )
    },
    #--- **** Fig: 2 rows (deltas, agree): one of {future RCP x time} ------
    # 1 row: robust simple delta RR
    # 2 row: agreement (frequency) in RR
    {
      lapply(
        unlist(meta[["simexps"]][["tag_acrmod"]][-1L]),
        function(acrmod) {
          list(
            inp = data.frame(
              vtag = c("rr", "rragreeFrq"),
              xtag = c(acrmod, acrmod),
              ftag = c("delta", "agree"),
              dtag = c("AcrMod_Deltas", "AcrMod_Deltas"),
              otag = c("robust090simpledelta-acrmod-med", "delta-acrmod-agree"),
              stringsAsFactors = FALSE
            ),
            type = "da2pp",
            fix_limits_within_ftag = FALSE
          )
        }
      )
    },
    {
      lapply(
        unlist(meta[["simexps"]][["tag_acrmod"]][-1L]),
        function(acrmod) {
          list(
            inp = data.frame(
              vtag = c("rrindex", "rrindexagreeFrq"),
              xtag = c(acrmod, acrmod),
              ftag = c("delta", "agree"),
              dtag = c("AcrMod_Deltas", "AcrMod_Deltas"),
              otag = c("delta-acrmod-med", "delta-acrmod-agree"),
              stringsAsFactors = FALSE
            ),
            type = "da2pp",
            fix_limits_within_ftag = FALSE
          )
        }
      )
    },
    #--- **** Fig: 2 rows (deltas, agree-count): one of {future RCP x time} ------
    # 1 row: robust simple delta RR
    # 2 row: agreement (count) in RR
    {
      lapply(
        unlist(meta[["simexps"]][["tag_acrmod"]][-1L]),
        function(acrmod) {
          list(
            inp = data.frame(
              vtag = c("rr", "rragreeCnt"),
              xtag = c(acrmod, acrmod),
              ftag = c("delta", "agree"),
              dtag = c("AcrMod_Deltas", "AcrMod_Deltas"),
              otag = c("robust090simpledelta-acrmod-med", "delta-acrmod-agree"),
              stringsAsFactors = FALSE
            ),
            type = "da2pp",
            fix_limits_within_ftag = FALSE
          )
        }
      )
    },
    {
      lapply(
        unlist(meta[["simexps"]][["tag_acrmod"]][-1L]),
        function(acrmod) {
          list(
            inp = data.frame(
              vtag = c("rrindex", "rrindexagreeCnt"),
              xtag = c(acrmod, acrmod),
              ftag = c("delta", "agree"),
              dtag = c("AcrMod_Deltas", "AcrMod_Deltas"),
              otag = c("delta-acrmod-med", "delta-acrmod-agree"),
              stringsAsFactors = FALSE
            ),
            type = "da2pp",
            fix_limits_within_ftag = FALSE
          )
        }
      )
    },
    #--- **** Fig: 6 rows (vals): amb, hist + all {future RCP x time} ------
    # columns: resilience/resistance probabilities of each of 4 categories
    {
      lapply(
        c("rslp4", "rstp4"),
        function(vtagk) {
          nexp <- length(unlist(meta[["simexps"]][["tag_acrmod"]]))
          list(
            inp = data.frame(
              vtag = vtagk,
              xtag = c(
                meta[["simexps"]][["tag_scen1"]],
                unlist(meta[["simexps"]][["tag_acrmod"]])
              ),
              ftag = c("value", rep("value", nexp)),
              dtag = c("IndRuns_Values", rep("AcrMod_Values", nexp)),
              otag = c("value-sim", rep("value-acrmod-med", nexp)),
              stringsAsFactors = FALSE
            ),
            type = "v6ap",
            fix_limits_within_ftag = FALSE
          )
        }
      )
    },
    #--- **** Fig: 4 rows (deltas): all {future RCP x time} ------
    # 1-4 columns: delta resilience/resistance probabilities of each of 4 categories
    {
      lapply(
        c("rslp4", "rstp4"),
        function(vtagk) {
          nexp <- length(unlist(meta[["simexps"]][["tag_acrmod"]][-1L]))
          list(
            inp = data.frame(
              vtag = vtagk,
              xtag = unlist(meta[["simexps"]][["tag_acrmod"]][-1L]),
              ftag = rep("delta", nexp),
              dtag = rep("AcrMod_Deltas", nexp),
              otag = rep("delta-acrmod-med", nexp),
              stringsAsFactors = FALSE
            ),
            type = "d4p",
            fix_limits_within_ftag = FALSE
          )
        }
      )
    },
    #--- **** Fig: 6 rows (vals, vals, deltas): amb, hist, all {future RCP x time} ------
    # columns: sets of predictors
    {
      lapply(
        c("rrindex", "preds1to5", "preds6to10", "preds11to15", "preds16to19"),
        function(vtagk) {
          nexp <- length(unlist(meta[["simexps"]][["tag_acrmod"]]))
          list(
            inp = data.frame(
              vtag = vtagk,
              xtag = c(
                meta[["simexps"]][["tag_scen1"]],
                unlist(meta[["simexps"]][["tag_acrmod"]])
              ),
              ftag = c("value", "value", rep("delta", nexp - 1L)),
              dtag = c("IndRuns_Values", "AcrMod_Values", rep("AcrMod_Deltas", nexp - 1L)),
              otag = c("value-sim", "value-acrmod-med", rep("delta-acrmod-med", nexp - 1L)),
              stringsAsFactors = FALSE
            ),
            type = "vvd6ap",
            fix_limits_within_ftag = TRUE
          )
        }
      )
    }
  )


  #--- ** Figures for manuscript & supplementary materials ------
  list_ms_figs <- c(
    Fig1 = "Map_rr-v2ap_ambient_1980-2020_and_RCP45_2064-2099",
    Fig2 = "Map_rr-rragreeCnt-da2pp_RCP45_2064-2099_and_RCP45_2064-2099",
    #Fig4 = "Map_rrplow-vd2ap_ambient_1980-2020_and_RCP45_2064-2099",
    Fig4 = "Map_rrindex-vd2ap_ambient_1980-2020_and_RCP45_2064-2099",
    Fig5 = "Map_rfcertaintydpe-vd2ap_ambient_1980-2020_and_RCP45_2064-2099",
    Fig6 = "Map_novelty2v-v2ap_ambient_1980-2020_and_RCP45_2064-2099"
  )

  list_sm_figs <- c(
    `FigS1-1` = "Map_rr-v2ap_ambient_1980-2020_and_historical_1950-2005",
    `FigS1-2` = "Map_rr-v2pp_RCP45_2029-2064_and_RCP45_2064-2099",
    `FigS1-3` = "Map_rr-v2pp_RCP85_2029-2064_and_RCP85_2064-2099",

    `FigS2-1` = "Map_rr-rragreeCnt-da2pp_RCP45_2029-2064_and_RCP45_2029-2064",
    `FigS2-2` = "Map_rr-rragreeCnt-da2pp_RCP85_2029-2064_and_RCP85_2029-2064",
    `FigS2-3` = "Map_rr-rragreeCnt-da2pp_RCP85_2064-2099_and_RCP85_2064-2099",

    `FigS4-1` = "Map_rslp4-v6ap_ambient_1980-2020_and_historical_1950-2005_and_RCP45_2029-2064_and_RCP45_2064-2099_and_RCP85_2029-2064_and_RCP85_2064-2099",
    `FigS4-2` = "Map_rstp4-v6ap_ambient_1980-2020_and_historical_1950-2005_and_RCP45_2029-2064_and_RCP45_2064-2099_and_RCP85_2029-2064_and_RCP85_2064-2099",
    `FigS4-3` = "Map_rslp4-d4p_RCP45_2029-2064_and_RCP45_2064-2099_and_RCP85_2029-2064_and_RCP85_2064-2099",
    `FigS4-4` = "Map_rstp4-d4p_RCP45_2029-2064_and_RCP45_2064-2099_and_RCP85_2029-2064_and_RCP85_2064-2099",

    `FigS5-1` = "Map_rfcertaintydpe-d2pp_RCP45_2029-2064_and_RCP45_2064-2099",
    `FigS5-2` = "Map_rfcertaintydpe-d2pp_RCP85_2029-2064_and_RCP85_2064-2099",

    `FigS6-1` = "Map_novelty2v-v2ap_ambient_1980-2020_and_historical_1950-2005",
    `FigS6-2` = "Map_novelty2v-v2pp_RCP45_2029-2064_and_RCP45_2064-2099",
    `FigS6-3` = "Map_novelty2v-v2pp_RCP85_2029-2064_and_RCP85_2064-2099",

    `FigS6-4` = "Map_noveltyAll-v2ap_ambient_1980-2020_and_historical_1950-2005",
    `FigS6-5` = "Map_noveltyAll-v2pp_RCP45_2029-2064_and_RCP45_2064-2099",
    `FigS6-6` = "Map_noveltyAll-v2pp_RCP85_2029-2064_and_RCP85_2064-2099",

    `FigSX-1` = "Map_preds1to5-vvd6ap_ambient_1980-2020_and_historical_1950-2005_and_RCP45_2029-2064_and_RCP45_2064-2099_and_RCP85_2029-2064_and_RCP85_2064-2099",
    `FigSX-2` = "Map_preds6to10-vvd6ap_ambient_1980-2020_and_historical_1950-2005_and_RCP45_2029-2064_and_RCP45_2064-2099_and_RCP85_2029-2064_and_RCP85_2064-2099",
    `FigSX-3` = "Map_preds11to15-vvd6ap_ambient_1980-2020_and_historical_1950-2005_and_RCP45_2029-2064_and_RCP45_2064-2099_and_RCP85_2029-2064_and_RCP85_2064-2099",
    `FigSX-4` = "Map_preds16to19-vvd6ap_ambient_1980-2020_and_historical_1950-2005_and_RCP45_2029-2064_and_RCP45_2064-2099_and_RCP85_2029-2064_and_RCP85_2064-2099",

    `FigSY-1` = "Map_mirrp-v2pp_RCP45_2029-2064_and_RCP45_2064-2099",
    `FigSY-2` = "Map_mirrp-v2pp_RCP85_2029-2064_and_RCP85_2064-2099",

    `FigSZ-a1` = "Map_rrindex-vvd6ap_ambient_1980-2020_and_historical_1950-2005_and_RCP45_2029-2064_and_RCP45_2064-2099_and_RCP85_2029-2064_and_RCP85_2064-2099",
    `FigSZ-b1` = "Map_rrindex-v2ap_ambient_1980-2020_and_historical_1950-2005",
    `FigSZ-b2` = "Map_rrindex-vd2pp_RCP45_2029-2064_and_RCP45_2029-2064",
    `FigSZ-b3` = "Map_rrindex-vd2pp_RCP45_2064-2099_and_RCP45_2064-2099",
    `FigSZ-b4` = "Map_rrindex-vd2pp_RCP85_2029-2064_and_RCP85_2029-2064",
    `FigSZ-b5` = "Map_rrindex-vd2pp_RCP85_2064-2099_and_RCP85_2064-2099",

    `FigSZZ-1` = "Map_rrindex-rrindexagreeCnt-da2pp_RCP45_2064-2099_and_RCP45_2064-2099",
    `FigSZZ-2` = "Map_rrindex-rrindexagreeCnt-da2pp_RCP85_2064-2099_and_RCP85_2064-2099"
  )


  #--- ** Calculate additional stuff that define figures ------
  for (k0 in seq_along(list_def_maps)) {
    #--- File name of output figure
    tmpv <- unique(list_def_maps[[k0]][["inp"]][["vtag"]])
    tmpd <- vapply(tmpv, get_vtag_input, FUN.VALUE = NA_character_)
    tmp_tag <- paste0(
      "Map_",
      paste0(tmpv, collapse = "-"),
      "-", list_def_maps[[k0]][["type"]], "_",
      paste0(list_def_maps[[k0]][["inp"]][["xtag"]], collapse = "_and_")
    )

    is_ms <- if (do_manuscript_maps) {
      grepl(tmp_tag, list_ms_figs)
    } else {
      rep(FALSE, times = length(list_ms_figs))
    }
    list_def_maps[[k0]][["is_ms"]] <- any(is_ms)

    is_sm <- if (do_appendix_maps) {
      grepl(tmp_tag, list_sm_figs)
    } else {
      rep(FALSE, times = length(list_sm_figs))
    }
    list_def_maps[[k0]][["is_sm"]] <- any(is_sm)

    tmpfd <- file.path(
      dir_outitems,
      paste0(
        "Maps_",
        if (list_def_maps[[k0]][["is_ms"]]) {
          "manuscript"
        } else if (list_def_maps[[k0]][["is_sm"]]) {
          "supplementary"
        } else {
          "exploration"
        },
        "_",
        tag_style
      )
    )

    if (!list_def_maps[[k0]][["is_ms"]] && !list_def_maps[[k0]][["is_sm"]]) {
      tmpfd <- file.path(
        tmpfd,
        paste0("Maps_", paste0(unique(tmpd), collapse = "-"))
      )
    }

    list_def_maps[[k0]][["fname_fig"]] <- file.path(
      tmpfd,
      paste0(
        if (list_def_maps[[k0]][["is_ms"]]) {
          names(list_ms_figs)[is_ms]
        } else if (list_def_maps[[k0]][["is_sm"]]) {
          names(list_sm_figs)[is_sm]
        } else {
          "Fig"
        },
        "_",
        tmp_tag,
        ".", fig_ext
      )
    )

    #--- Determine variables for each row of panels
    list_def_maps[[k0]][["vars"]] <- lapply(
      list_def_maps[[k0]][["inp"]][["vtag"]],
      get_vars_to_map
    )

    #--- Count number of variables/columns
    ntmp <- unique(lengths(list_def_maps[[k0]][["vars"]]))
    stopifnot(length(ntmp) == 1L)
    list_def_maps[[k0]][["nvars"]] <- ntmp


    #--- Calculate panel matrix size (# rows: conditions; columns: variables)
    list_def_maps[[k0]][["n_panels"]] <- c(
      nrow(list_def_maps[[k0]][["inp"]]),
      list_def_maps[[k0]][["nvars"]]
    )


    #--- Determine panel tag identification letters
    list_def_maps[[k0]][["tag_id_letter"]] <- array(
      dim = list_def_maps[[k0]][["n_panels"]]
    )
    ip <- 1L
    for (kr in seq_len(list_def_maps[[k0]][["n_panels"]][[1L]])) {
      for (kc in seq_len(list_def_maps[[k0]][["n_panels"]][[2L]])) {
        tmp <- switch(
          EXPR = style_figures,
          ESA = letters,
          LETTERS
        )

        list_def_maps[[k0]][["tag_id_letter"]][kr, kc] <- paste0(
          rep(
            tmp[1L + ((ip - 1L) %% 26L)],
            1L + ((ip - 1L) %/% 26L)
          ),
          collapse = ""
        )
        ip <- ip + 1L
      }
    }


    #--- Estimate number of color legends
    list_def_maps[[k0]][["nlegends"]] <- max(
      length(tmpv),
      length(unique(list_def_maps[[k0]][["inp"]][["ftag"]]))
    )

    #--- Collect legends across panels (or keep panel+legend as is)
    list_def_maps[[k0]][["collect_legends"]] <- !any(tmpd %in% "Preds19")


    #--- Panel tags as identifier for legend titles if multiple collected legends
    # (otherwise readers have a hard time matching legends to panels)
    if (
      list_def_maps[[k0]][["collect_legends"]] &&
      list_def_maps[[k0]][["nlegends"]] > 1 &&
        # if tags can easily assigned to collected legends:
      list_def_maps[[k0]][["nlegends"]] == nrow(list_def_maps[[k0]][["inp"]])
    ) {
      tmp <- apply(
        list_def_maps[[k0]][["tag_id_letter"]],
        MARGIN = 1L,
        paste0,
        collapse = ""
      )
      list_def_maps[[k0]][["legend_name_tags"]] <- paste0(tmp, ": ")
    }
  }



  #------ ** Loop over figures ------
  pb <- utils::txtProgressBar(max = length(list_def_maps), style = 3L)

  for (k0 in seq_along(list_def_maps)) {

    if (
      !do_exploratory_maps &&
        !list_def_maps[[k0]][["is_ms"]] &&
        !list_def_maps[[k0]][["is_sm"]]
    ) {
      # skip since exploratory map
      next
    }


    if (!file.exists(list_def_maps[[k0]][["fname_fig"]])) {

      if (verbose) {
        print(basename(list_def_maps[[k0]][["fname_fig"]]))
      }

      #--- *** Prepare panels ------

      # rows: conditions; columns: variables
      list_panels <- list()
      ip <- 1L
      rr_flvls <- NULL # factor levels (if RR)


      #--- *** Load data ------
      list_data <- array(list(), dim = list_def_maps[[k0]][["n_panels"]])
      list_st_nonrobust <- array(list(), dim = list_def_maps[[k0]][["n_panels"]])

      for (kr in seq_len(list_def_maps[[k0]][["n_panels"]][[1L]])) {
        vtagk <- list_def_maps[[k0]][["inp"]][["vtag"]][[kr]]

        xfname <- list.files(
          path = file.path(
            dir_data_rat,
            paste0(
              list_def_maps[[k0]][["inp"]][["xtag"]][[kr]],
              "-clim_",
              list_def_maps[[k0]][["inp"]][["dtag"]][[kr]],
              "_",
              get_vtag_input(vtagk)
            )
          ),
          pattern = paste0("__", list_def_maps[[k0]][["inp"]][["otag"]][[kr]]),
          recursive = TRUE,
          full.names = TRUE
        )

        stopifnot(length(xfname) == 1L)

        xrat <- utils::read.csv(xfname)

        #--- Subset to current data mask
        if (mask_sets[[km]] == "combined") {
          tmpv <- paste0("count_", data_mask_sets[[km]])
          ids <- !is.na(xrat[[tmpv]]) & xrat[[tmpv]] > 0
          stopifnot(sum(ids) > 0L)
          xrat <- xrat[ids, , drop = FALSE]
        }

        #--- Identify variables
        tag_vars_used <- vapply(
          list_def_maps[[k0]][["vars"]][[kr]],
          function(tv) {
            tmp <- grep(
              # after 'tv', the variable name either ends or continues with "_"
              # (but not with "_isnovel")
              pattern = paste0("^", tv, "(_|$)"),
              x = colnames(xrat),
              value = TRUE
            )
            if (grepl("isnovel", tv, fixed = TRUE)) {
              tmp
            } else {
              grep("isnovel", tmp, fixed = TRUE, value = TRUE, invert = TRUE)
            }
          },
          FUN.VALUE = NA_character_
        )

        stopifnot(length(tag_vars_used) == list_def_maps[[k0]][["nvars"]])

        is_vars_factor <- newRR3::is_categorical(tag_vars_used)


        #--- **** Deal with cross-hatching nonrobust areas for continuous, acrmod-delta ------
        has_crosshatching <- FALSE
        tag_varsagree_used <- NULL
        xrat_agree <- NULL

        if (add_nonrobust_crosshatching) {
          # Check that variables are continuous, across-model deltas
          has_crosshatching <-
            !is_vars_factor &
            rep(
              grepl("AcrMod_Deltas", list_def_maps[[k0]][["inp"]][["dtag"]][[kr]]),
              times = list_def_maps[[k0]][["nvars"]]
            )


          if (any(has_crosshatching)) {
            # Locate agreement data associated with variable
            xfname_agree <- list.files(
              path = file.path(
                dir_data_rat,
                paste0(
                  list_def_maps[[k0]][["inp"]][["xtag"]][[kr]],
                  "-clim_",
                  list_def_maps[[k0]][["inp"]][["dtag"]][[kr]],
                  "_",
                  get_vtag_input(vtagk)
                )
              ),
              pattern = "__delta-acrmod-agree",
              recursive = TRUE,
              full.names = TRUE
            )

            stopifnot(length(xfname_agree) == 1L)

            xrat_agree <- utils::read.csv(xfname_agree)

            #--- Subset to current data mask
            if (mask_sets[[km]] == "combined") {
              tmpv <- paste0("count_", data_mask_sets[[km]])
              ids <- !is.na(xrat_agree[[tmpv]]) & xrat_agree[[tmpv]] > 0
              stopifnot(sum(ids) > 0L)
              xrat_agree <- xrat_agree[ids, , drop = FALSE]
            }

            #--- Identify variables
            tag_varsagree_used <- vapply(
              list_def_maps[[k0]][["vars"]][[kr]],
              function(tv) {
                grep(
                  # after 'tv', the variable name either ends or continues with "_"
                  pattern = paste0("^", tv, "(_|$)"),
                  x = colnames(xrat_agree),
                  value = TRUE
                )
              },
              FUN.VALUE = NA_character_
            )

            stopifnot(
              length(tag_varsagree_used) == list_def_maps[[k0]][["nvars"]]
            )
          }
        }


        #--- **** Put prepared values together ------
        for (kc in seq_len(list_def_maps[[k0]][["n_panels"]][[2L]])) {

          if (is_vars_factor[[kc]]) {
            #--- Categorical variable

            rr_flvls <- newRR3::get_levels(
              vtag = vtagk,
              vdesc = newRR3::extract_fname_element(
                xfname,
                pos = 3L
              ),
              meta = meta,
              return_labels = TRUE
            )

            if (vtagk == "mirrp") {
              # Use "short" labels for 19 rr-predictors
              rr_flvls <- meta[["varsets"]][["preds"]][["short10"]]
            }

            xtmp <- factor(
              xrat[, tag_vars_used[[kc]], drop = TRUE],
              levels = seq_along(rr_flvls),
              labels = rr_flvls
            )


          } else {
            #--- Continuous variable
            rr_flvls <- NULL
            xtmp <- xrat[, tag_vars_used[[kc]], drop = TRUE]
          }


          #--- scale values if requested
          if (vtagk %in% c("rragreeCnt", "rrindexagreeCnt")) {
            xtmp <- round(xtmp * n_models)
          }

          #--- robust consistency (if variable is continuous and delta)
          has_robustconsistency <-
            !is_vars_factor[[kc]] && isTRUE(has_crosshatching[[kc]])

          xrobustconsistency <- if (has_robustconsistency) {
            tmp <- newRR3::robustness(
              x = xrat_agree[, tag_varsagree_used[[kc]], drop = TRUE],
              frq_robust = meta[["acrmod"]][["robust"]][["frq"]]
            )
            lvls_robustconsistency[1L + tmp]
          }

          #--- store values for later
          list_data[kr, kc][[1L]] <- list(
            x = xtmp,
            rat_values = xrat[, "value", drop = TRUE],
            is_vars_factor = is_vars_factor[[kc]],
            flvls = rr_flvls,
            weights = if (!is_vars_factor[[kc]]) {
              xrat[, paste0("count_", data_mask_sets[[km]]), drop = TRUE]
            },
            consistency = xrobustconsistency,
            consistency_lvls = if (has_robustconsistency) lvls_robustconsistency
          )


          #--- create nonrobust polygon for crosshatching on map
          # (but don't do crosshatching for agreement itself)
          if (
            add_nonrobust_crosshatching &&
              isTRUE(has_crosshatching[[kc]]) &&
              isTRUE(!grepl("acrmodagree", tag_vars_used[[kc]]))
          ) {
            list_st_nonrobust[kr, kc][[1L]] <- newRR3::create_crosshatching(
              xstars = list_gsu[["xstars"]],
              x = xrat_agree[, tag_varsagree_used[[kc]], drop = TRUE],
              rat_values = xrat_agree[, "value", drop = TRUE],
              dx = units::set_units(4L, "km"),
              fun_crosshatch = function(x) {
                ifelse(
                  newRR3::robustness(
                    x = x,
                    frq_robust = meta[["acrmod"]][["robust"]][["frq"]]
                  ) > 0,
                  NA_integer_,
                  1L
                )
              }
            )
          }
        }
      }


      #--- *** Calculate limits ------
      list_limits <- array(dim = c(2L, list_def_maps[[k0]][["n_panels"]]))
      list_limits_insets <- list_limits

      sets_limits <- unique(list_def_maps[[k0]][["inp"]][["ftag"]])
      list_sets_limits <- match(
        list_def_maps[[k0]][["inp"]][["ftag"]],
        sets_limits
      )


      #--- Fix limits per column across rows within groups of 'ftag', e.g., "value" or "delta"
      if (list_def_maps[[k0]][["fix_limits_within_ftag"]]) {
        for (kc in seq_len(list_def_maps[[k0]][["n_panels"]][[2L]])) {

          for (ks in seq_along(sets_limits)) {
            ids_rows <- which(list_sets_limits == ks)

            xtmp <- unlist(
              lapply(ids_rows, function(kr) list_data[kr, kc][[1L]][["x"]])
            )
            xtmp <- suppressWarnings(as.numeric(xtmp))
            xtmp <- xtmp[is.finite(xtmp)]

            if (length(xtmp) > 0) {
              rlim <- range(xtmp)
              list_limits[, ids_rows, kc] <- rlim

              # outlier removed range (see Meyer & Pebesma 2021, `calc_AOArdi()`)
              orr <- if (
                !all(
                  unique(list_def_maps[[k0]][["inp"]][["vtag"]][ids_rows]) %in% c("rragreeCnt", "rrindexagreeCnt")
                )
              ) {
                stats::quantile(xtmp, probs = c(0.25, 0.75), style = 3L) +
                  c(-1.5, 1.5) * stats::IQR(xtmp)
              } else {
                # but not for binned scales
                rlim
              }

              list_limits_insets[, ids_rows, kc] <- c(
                max(rlim[[1L]], orr[[1L]]),
                min(rlim[[2L]], orr[[2L]])
              )
            }
          }
        }

      } else {
        #--- Fix limits across columns and rows within groups of 'ftag'
        ids_cols <- seq_len(list_def_maps[[k0]][["n_panels"]][[2L]])

        for (ks in seq_along(sets_limits)) {
          ids_rows <- which(list_sets_limits == ks)

          ids <- expand.grid(row = ids_rows, col = ids_cols)

          xtmp <- unlist(
            mapply(
              function(kr, kc) list_data[kr, kc][[1L]][["x"]],
              ids[["row"]],
              ids[["col"]]
            )
          )
          xtmp <- suppressWarnings(as.numeric(xtmp))
          xtmp <- xtmp[is.finite(xtmp)]

          if (length(xtmp) > 0) {
            rlim <- range(xtmp)

            # outlier removed range (see Meyer & Pebesma 2021, `calc_AOArdi()`)
            orr <- if (
              !all(
                unique(list_def_maps[[k0]][["inp"]][["vtag"]][ids_rows]) %in% c("rragreeCnt", "rrindexagreeCnt")
              )
            ) {
              stats::quantile(xtmp, probs = c(0.25, 0.75), style = 3L) +
                c(-1.5, 1.5) * stats::IQR(xtmp)
            } else {
              # but not for binned scales
              rlim
            }

            list_limits_insets[, ids_rows, ids_cols] <- c(
              max(rlim[[1L]], orr[[1L]]),
              min(rlim[[2L]], orr[[2L]])
            )
          }
        }
      }




      #--- *** Create panels ------
      #--- Loop over rows of panels / different data sets
      for (kr in seq_len(list_def_maps[[k0]][["n_panels"]][[1L]])) {
        vtagk <- list_def_maps[[k0]][["inp"]][["vtag"]][[kr]]


        #--- Loop over columns of panels / variables in loaded data
        for (kc in seq_len(list_def_maps[[k0]][["n_panels"]][[2L]])) {

          #--- **** Create map object ------

          #--- calculate panel decoration
          tmp0 <- list_def_maps[[k0]][["tag_id_letter"]][kr, kc]
          tmp1 <- paste0(
            switch(
              EXPR = style_figures,
              ESA = paste0("(", tmp0, ") "),
              paste0(tmp0, ") ")
            ),
            names(list_def_maps[[k0]][["vars"]][[kr]])[[kc]]
          )

          if (list_def_maps[[k0]][["inp"]][["ftag"]][[kr]] == "delta") {
            # if delta, then insert 'change' at end but before a '('"
            tmp1 <- if (grepl(" (", tmp1, fixed = TRUE)) {
              sub(" (", replacement = " change (", tmp1, fixed = TRUE)
            } else {
              paste(tmp1, "change")
            }
          }

          tmp2 <- paste0(
            "— ",
            newRR3::make_acrmod_labels(
              list_def_maps[[k0]][["inp"]][["xtag"]][[kr]]
            )
          )

          tmp_tag_panel <- if (nchar(tmp1) <= 30) {
            paste(tmp1, tmp2)
          } else {
            # Create a two-line tag with a right-adjusted second line
            list(
              tmp1,
              # Add md/html formatting for `ggtext::geom_richtext()`
              if (TRUE) {
                paste0(
                  # First line is transparent
                  "<span style='color:transparent'>", tmp2, "</span><br>",
                  tmp2
                )
              } else {
                paste0(
                  # First line is transparent
                  "<span style='color:transparent'>", tmp1, "</span><br>",
                  # Second line is printed as right-adjusted
                  # Once `ggtext` supports 'text-align' (which it does not as of v0.1.2)
                  #"<span style='text-align: right'>", tmp2, "</span>"
                  # In the meantime, hackishly add some transparent whitespace
                  "<span style='color:transparent'>",
                  substr(tmp1, 1, nchar(tmp1) - nchar(tmp2) - 3), "</span>",
                  tmp2
                )
              }
            )
          }


          #--- create panel
          list_panels[[ip]] <- newRR3::plot_map(
            xstars = newRR3::prepare_stars(
              xstars = list_gsu[["xstars"]],
              x = list_data[kr, kc][[1L]][["x"]],
              rat_values = list_data[kr, kc][[1L]][["rat_values"]]
            ),
            bbox = map_bbox,
            add_coords = TRUE,
            maintitle = NULL,
            subtitle = NULL,
            panel_tag = NULL,
            show_legend = TRUE,
            legend_xoffset = 0.,
            legend_yoffset = 0.,
            st_poly_hatched = list_st_nonrobust[kr, kc][[1L]],
            st_geom_sb = st_sagebiome,
            st_geom_state = st_NEadmin1,
            st_geom_eco = st_ecoregionL3
          ) +
            newRR3::ggplot2_map_theme()


          #--- **** Add panel tag (description) ------
          if (length(tmp_tag_panel) > 1) {
            list_panels[[ip]] <- list_panels[[ip]] +
              # Once `ggtext` supports 'text-align' (which it does not as of v0.1.2),
              # use `newRR3::add_tag_as_richlabel()`
              #newRR3::add_tag_as_richlabel(tag = tmp_tag_panel[[2L]])
              newRR3::add_tag_as_richlabel_rightadjusted(
                tag = tmp_tag_panel[[2L]],
                relative_fontsize = 0.87
              )
          }

          list_panels[[ip]] <- list_panels[[ip]] +
            newRR3::add_tag_as_label(
              tag = tmp_tag_panel[[1L]],
              relative_fontsize = 0.87
            )


          #--- **** Add color scale legend ------
          if (
            list_def_maps[[k0]][["collect_legends"]] &&
              !list_data[kr, kc][[1L]][["is_vars_factor"]]
          ) {
            # make legend wider if continuous
            list_panels[[ip]] <- list_panels[[ip]] +
              ggplot2::theme(
                # I don't understand `0.05npc` -- value determined by
                # trial and error via printing to file
                legend.key.width = grid::unit(0.05, units = "npc")
              )
          }

          #--- Make legend text smaller if more than one legend across panels
          if (
            !list_def_maps[[k0]][["collect_legends"]] ||
              list_def_maps[[k0]][["nlegends"]] > 1
          ) {
            list_panels[[ip]] <- list_panels[[ip]] +
              ggplot2::theme(
                # 1 instead of 1.25 (see `ggplot2_map_theme()`)
                legend.text = ggplot2::element_text(size = ggplot2::rel(1))
              )
          }

          #--- Move legend title (if present) to above (instead of left) of
          # scale bar/keys even if legend plotted horizontally (bottom)
          # but only if more than one legend
          tmp1 <- if (list_def_maps[[k0]][["nlegends"]] > 1) "top"
          tmp2 <- if (list_def_maps[[k0]][["nlegends"]] == 1L) 1
          tmp_cltp <- ggplot2::guide_colorbar(
            title.position = tmp1,
            title.vjust = tmp2
          )
          tmp_kltp <- ggplot2::guide_legend(
            title.position = tmp1,
            title.vjust = tmp2
          )


          #--- Select color scale based on variables
          tmp <- list_def_maps[[k0]][["inp"]][["ftag"]][[kr]]

          list_panels[[ip]] <- list_panels[[ip]] +
            if (vtagk == "rr") {
              if (tmp == "value") {
                newRR3::colorscale_rr(
                  levels = list_data[kr, kc][[1L]][["flvls"]]
                )

              } else if (tmp == "delta") {
                newRR3::colorscale_robustsimpledelta(
                  name = paste0(
                    list_def_maps[[k0]][["legend_name_tags"]][[kr]],
                    "Change in R&R"
                  ),
                  levels = list_data[kr, kc][[1L]][["flvls"]],
                  guide = tmp_kltp
                )
              }

            } else if (grepl("novelty", vtagk)) {
              if (tmp == "value") {
                if (vtagk == "noveltyAll") {
                  newRR3::colorscale_novelty_flatbinned(
                    name = paste0(
                      list_def_maps[[k0]][["legend_name_tags"]][[kr]],
                      "Novelty"
                    ),
                    limits = c(0, 10),
                    guide = tmp_cltp
                  )

                } else {
                  newRR3::colorscale_novelty_diverging(
                    name = paste0(
                      list_def_maps[[k0]][["legend_name_tags"]][[kr]],
                      "Novelty"
                    ),
                    limits = c(0, 10),
                    guide = tmp_cltp
                  )
                }

              } else if (tmp == "delta") {
                newRR3::colorscale_deltas_c(
                  name = paste0(
                    list_def_maps[[k0]][["legend_name_tags"]][[kr]],
                    "Change in novelty"
                  ),
                  limits = list_limits[, kr, kc, drop = TRUE],
                  guide = tmp_cltp
                )
              }

            } else if (grepl("rfcertainty", vtagk)) {
              if (grepl("value", tmp)) {
                newRR3::colorscale_values_c(
                  name = paste0(
                    list_def_maps[[k0]][["legend_name_tags"]][[kr]],
                    "RF-certainty"
                  ),
                  limits = c(0, 1),
                  guide = tmp_cltp
                )

              } else if (tmp == "delta") {
                newRR3::colorscale_deltas_c(
                  name = paste0(
                    list_def_maps[[k0]][["legend_name_tags"]][[kr]],
                    "Change in RF-certainty"
                  ),
                  limits = c(-1, 1),
                  guide = tmp_cltp,
                  rev = FALSE # FALSE: purple:decrease <> green:increase
                )
              }

            } else if (vtagk %in% c("rrplow", "rslp4", "rstp4")) {
              if (tmp == "value") {
                newRR3::colorscale_values_c(
                  name = paste0(
                    list_def_maps[[k0]][["legend_name_tags"]][[kr]],
                    "Probability"
                  ),
                  limits = c(0, 1),
                  guide = tmp_cltp
                )

              } else if (tmp == "delta") {
                newRR3::colorscale_deltas_c(
                  name = paste0(
                    list_def_maps[[k0]][["legend_name_tags"]][[kr]],
                    "Change in probability"
                  ),
                  limits = c(-1, 1),
                  guide = tmp_cltp
                )
              }

            } else if (vtagk == "rrindex") {
              if (tmp == "value") {
                newRR3::colorscale_values_c(
                  name = paste0(
                    list_def_maps[[k0]][["legend_name_tags"]][[kr]],
                    "Index"
                  ),
                  limits = c(-1, 1),
                  guide = tmp_cltp
                )

              } else if (tmp == "delta") {
                if (TRUE) {
                  newRR3::colorscale_deltas_c(
                    name = paste0(
                      list_def_maps[[k0]][["legend_name_tags"]][[kr]],
                      "Change in index"
                    ),
                    limits = c(-4/3, 4/3), #c(-2, 2),
                    guide = tmp_cltp,
                    rev = FALSE # FALSE: purple:decrease <> green:increase
                  )
                } else {
                  colorspace::scale_fill_binned_diverging(
                    name = paste0(
                      list_def_maps[[k0]][["legend_name_tags"]][[kr]],
                      "Change in index"
                    ),
                    limits = c(-4/3, 4/3), #c(-2, 2),
                    breaks = c(-4/3, -3/3, -2/3, -1/3, 1/3, 2/3, 3/3, 4/3),
                    labels = c(
                      "-4/3", "-1", "-2/3", "-1/3", "1/3", "2/3", "1", "4/3"
                    ),
                    guide = tmp_cltp,
                    palette = "Purple-Green",
                    rev = TRUE,
                    na.value = "gray",
                    mid = 0
                  )
                }
              }

            } else if (vtagk %in% c("rragreeFrq", "rrindexagreeFrq")) {
              newRR3::colorscale_flatcontinuous(
                name = paste0(
                  list_def_maps[[k0]][["legend_name_tags"]][[kr]],
                  "Consistency"
                ),
                aesthetics = "fill",
                limits = c(0, 1),
                threshold = 0.9,
                position_flat = 2L,
                viridis_pal_option = "E", # D = viridis; E = cividis
                reverse = TRUE,
                labels = scales::label_percent(),
                guide = tmp_cltp
              )

            } else if (vtagk %in% c("rragreeCnt", "rrindexagreeCnt")) {
              newRR3::colorscale_flatbinned(
                name = paste0(
                  list_def_maps[[k0]][["legend_name_tags"]][[kr]],
                  "Consistency (count)"
                ),
                aesthetics = "fill",
                breaks = c(0, 2 * seq_len(n_models / 2)),
                threshold = 0.9 * n_models,
                position_flat = 2L,
                viridis_pal_option = "E", # D = viridis; E = cividis
                reverse = TRUE,
                end_hue = 8 / 9, # < 1 increased contrast with "flat" color
                guide = tmp_cltp
              )

            } else if (grepl("^preds", vtagk)) {
              if (tmp == "value") {
                newRR3::colorscale_values_c(
                  limits = list_limits[, kr, kc, drop = TRUE],
                  guide = tmp_cltp
                )

              } else if (tmp == "delta") {
                newRR3::colorscale_deltas_c(
                  limits = list_limits[, kr, kc, drop = TRUE],
                  guide = tmp_cltp
                )
              }

            } else if (vtagk == "mirrp") {
              newRR3::colorscale_predictors19(
                guide = tmp_kltp
              )
            }



          #--- **** Beautify panel adornments ------
          list_panels <- newRR3::panels_remove_inner_labels(
            list_panels,
            ks_panels_do_remove = ip,
            n_panels = list_def_maps[[k0]][["n_panels"]],
            remove_xlabs = TRUE,
            remove_xticks = TRUE,
            remove_ylabs = TRUE,
            remove_yticks = TRUE
          )


          #--- **** Add inset with weighted density * count -------
          if (!list_data[kr, kc][[1L]][["is_vars_factor"]]) {
            pinset <- newRR3::inset_densitycountplot(
              x = list_data[kr, kc][[1L]][["x"]],
              limits = list_limits_insets[, kr, kc, drop = TRUE],
              x_binned = vtagk %in% c("rragreeCnt", "rrindexagreeCnt"),
              weight = list_data[kr, kc][[1L]][["weights"]],
              consistency = list_data[kr, kc][[1L]][["consistency"]],
              consistency_lvls = list_data[kr, kc][[1L]][["consistency_lvls"]],
              add_vertical0 =
                list_def_maps[[k0]][["inp"]][["ftag"]][[kr]] == "delta"
            )

            if (!is.null(pinset)) {
              list_panels[[ip]] <- list_panels[[ip]] +
                patchwork::inset_element(
                  pinset,
                  0.005, 0.005, 360 / 1133, 230 / 1236, # left, bottom, right, top in npc units
                  align_to = "panel",
                  clip = TRUE,
                  ignore_tag = TRUE
                )
            }
          }


          #--- **** Update panel ID ------
          ip <- ip + 1L
        }

      }


      #--- *** Place panels in plot ------
      tmp_plot <- patchwork::wrap_plots(
        list_panels,
        nrow = list_def_maps[[k0]][["n_panels"]][[1L]],
        ncol = list_def_maps[[k0]][["n_panels"]][[2L]],
        guides = if (list_def_maps[[k0]][["collect_legends"]]) {
          "collect"
        } else {
          "keep"
        }
      )

      if (list_def_maps[[k0]][["collect_legends"]]) {
        tmp_plot <- tmp_plot &
          ggplot2::theme(
            legend.position = "bottom"
          )
      }


      #--- *** Save map to file ------
      dir.create(
        dirname(list_def_maps[[k0]][["fname_fig"]]),
        recursive = TRUE,
        showWarnings = FALSE
      )

      # height of bottom legend relative to panel height
      size_hlegend_rel <- if (list_def_maps[[k0]][["collect_legends"]]) {
        if (!all(list_data[kr, kc][[1L]][["is_vars_factor"]])) 0.25 else 0.125
      } else {
        0
      }

      do.call(
        fun_dev_bitmap,
        args = c(
          list(
            file = list_def_maps[[k0]][["fname_fig"]],
            height =
              (list_def_maps[[k0]][["n_panels"]][[1L]] + size_hlegend_rel) * asp * size_panel_in,
            width = list_def_maps[[k0]][["n_panels"]][[2L]] * size_panel_in,
            units = "in",
            res = res_bitmap
          ),
          if (fig_ext %in% c("jpg", "tif", "png")) {
            # type "quartz" ignores "quality" and "compression"
            list(type = "cairo")
          },
          if (fig_ext == "jpg") {
            list(quality = 95)
          },
          if (fig_ext == "tif") {
            list(compression = "zip+p")
          }
        )
      )

      plot(tmp_plot)

      grDevices::dev.off()
    }

    utils::setTxtProgressBar(pb, k0)
  }

  close(pb)
}

print(summary(warnings()))

#------. ------
#------. ------
