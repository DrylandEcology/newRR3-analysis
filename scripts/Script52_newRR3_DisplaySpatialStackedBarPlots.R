#----- . ------
#------ PROPORTION OF AREA ------
#
# Figures: stacked bar plots
#
#    * Output: `"displayitems/StackedBars-SpatialUnitTabulations/"`
#
#----- . ------



#----- SET UP ------
stopifnot(requireNamespace("newRR3"))
options(warn = 2L) # convert warnings to errors

#--- Settings ------
req_meta_version <- "3.2.0"

vtag_input <- c("rr", "mirrp")

style_figures <- "ESA" # ESA, e.g., "(a)"; other, e.g., "A)"

# 2023-Mar-22: full/simulated extent: data; rangeland mask: as attribute and for analysis
mask_sets <- c("sim", "rangelands", "combined")[[3L]]
data_mask_sets <- c("sim", "rangelands", "combined")[[2L]]



#--- Paths (part 1) ------
dir_prj <- ".."

dir_dataraw <- file.path(dir_prj, "data-raw")
stopifnot(dir.exists(dir_dataraw))

dir_res <- file.path(dir_prj, "results")
stopifnot(dir.exists(dir_res))

dir_res_tu <- file.path(dir_res, "SpatialUnits_Tabulations")
stopifnot(dir.exists(dir_res_tu))

dir_dataraw_tu <- file.path(dir_dataraw, "spatialunits")
stopifnot(dir.exists(dir_dataraw_tu))


dir_outitems <- file.path(dir_prj, "displayitems")
dir.create(dir_outitems, recursive = TRUE, showWarnings = FALSE)

tag_outitems_figtu <- "StackedBars-SpatialUnitTabulations"

tag_style <- paste(
  "style",
  switch(EXPR = style_figures, ESA = "esa", "other"),
  sep = "-"
)



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


#--- Load aggregated ecoregions ------
fname_aggecoregions <- file.path(
  dir_dataraw_tu,
  "EPAecoregionL3",
  "EcoregionsL3_Simplified.csv"
)

agged_ecoregions <- if (file.exists(fname_aggecoregions)) {
  data.frame(
    utils::read.csv(fname_aggecoregions),
    stringsAsFactors = FALSE
  )
}


#------ . ------
#------ LOOP OVER SPATIAL MASKS ------
for (km in seq_along(mask_sets)) {

  #--- Paths (part 2) ------
  dir_data_rat <- file.path(
    dir_res,
    paste0("newRR3_RATs-", mask_sets[[km]])
  )
  stopifnot(file.exists(dir_data_rat))


  #--- File paths of for summaries across spatial units ------
  find_tabulation_files <- function(pattern, path1, path2, mask) {
    tmp <- lapply(
      list.files(path = path1),
      function(dn) {
        list.files(
          path = file.path(
            path2,
            paste0("Tabulations_bs-", mask),
            dn
          ),
          pattern = pattern,
          recursive = TRUE,
          full.names = TRUE
        )
      }
    )
    unlist(tmp[lengths(tmp) > 0L])
  }


  #----- . ------
  tag_ambient <- meta[["simexps"]][["tag_scen1"]]
  tag_hist <- meta[["simexps"]][["tag_acrmod"]][[1L]]
  tag_acrmods <- unlist(meta[["simexps"]][["tag_acrmod"]][-1L])

  labels_simslices <- list(
    newRR3::make_acrmod_labels(tag_ambient),
    newRR3::make_acrmod_labels(tag_hist),
    newRR3::make_acrmod_labels(tag_acrmods)
  )



  #---- Loop over vtags ------
  for (vtagk in vtag_input) {

    tag_vars <- switch(
      EXPR = vtagk,
      rr = c(RSL = "Resilience_response", RST = "Resistance_response"),
      mirrp = c(`MIRRP-RSL` = "Resilience_mirrp", `MIRRP-RST` = "Resistance_mirrp"),
      stop(vtagk, " not implemented.")
    )


    #--- Prepare tabulated input files ----
    if (vtagk == "rr") {

      fns_in_ambient <- list.files(
        path = file.path(
          dir_res_tu,
          paste0("Tabulations_bs-", data_mask_sets[[km]]),
          paste0(
            "ambient_1980-2020-clim_IndRuns_Values_",
            meta[["varsets"]][["tags"]][[vtagk]]
          )
        ),
        recursive = TRUE,
        full.names = TRUE
      )

      fns_in_historical <- list.files(
        path = file.path(
          dir_res_tu,
          paste0("Tabulations_bs-", data_mask_sets[[km]]),
          paste0(
            "historical_1950-2005-clim_AcrMod_Values_",
            meta[["varsets"]][["tags"]][[vtagk]]
          )
        ),
        pattern = "value-acrmod-med",
        recursive = TRUE,
        full.names = TRUE
      )

      fns_in_robustsimpledelta <- find_tabulation_files(
        pattern = "robust090simpledelta",
        path1 = dir_data_rat,
        path2 = dir_res_tu,
        mask = data_mask_sets[[km]]
      )

      fns_in_robustdelta <- find_tabulation_files(
        pattern = "robust090delta",
        path1 = dir_data_rat,
        path2 = dir_res_tu,
        mask = data_mask_sets[[km]]
      )

      fns_in_simpledelta <- lapply(
        meta[["acrmod"]][["signal"]][["tag"]],
        function(as) {
          tmp <- find_tabulation_files(
            pattern = paste0("__simpledelta-acrmod-", as),
            path1 = dir_data_rat,
            path2 = dir_res_tu,
            mask = data_mask_sets[[km]]
          )
          unlist(tmp[lengths(tmp) > 0L])
        }
      )

      fns_in_acrmodvalues <- lapply(
        meta[["acrmod"]][["signal"]][["tag"]],
        function(as) {
          tmp <- find_tabulation_files(
            pattern = paste0(
              meta[["varsets"]][["tags"]][[vtagk]],
              "__(.+)__value-acrmod-",
              as
            ),
            path1 = dir_data_rat,
            path2 = dir_res_tu,
            mask = data_mask_sets[[km]]
          )
          unlist(tmp[lengths(tmp) > 0L])
        }
      )

      fns_in_acrmoddeltas <- lapply(
        meta[["acrmod"]][["signal"]][["tag"]],
        function(as) {
          tmp <- find_tabulation_files(
            pattern = paste0(
              meta[["varsets"]][["tags"]][[vtagk]],
              "__(.+)__delta-acrmod-",
              as
            ),
            path1 = dir_data_rat,
            path2 = dir_res_tu,
            mask = data_mask_sets[[km]]
          )
          unlist(tmp[lengths(tmp) > 0L])
        }
      )

    } else if (vtagk == "mirrp") {

      fns_in_med_mirrpm <- find_tabulation_files(
        pattern = paste0(
          meta[["varsets"]][["tags"]][[vtagk]],
          "__(.+)",
          "__value-acrmod-med"
        ),
        path1 = dir_data_rat,
        path2 = dir_res_tu,
        mask = data_mask_sets[[km]]
      )
    }



    #----- . ------
    #--- RR ------
    #----- . ------
    if (vtagk == "rr") {

      #--- Figures RR ------

      #--- Fig: Robust-delta (overall; w/o amb, w/o hist) ------
      #   - extent: ecotype
      #   - Panels: RSL, RST
      #   - Vertical: timeperiods x RCPs
      #   - Horizontal: robust deltas (median acrmod) organized by historical

      #------ ** Define figures ------
      tmp <- expand.grid(
        incl_ambient = c(FALSE, TRUE),
        incl_hist = c(FALSE, TRUE),
        extent = "Overall",
        KEEP.OUT.ATTRS = FALSE,
        stringsAsFactors = FALSE
      )
      # we want at least either ambient or historical but not none
      tmp <- tmp[tmp[["incl_ambient"]] | tmp[["incl_hist"]], , drop = FALSE]

      list_def_figs <- lapply(
        split(tmp, seq_len(nrow(tmp))),
        function(x) list(inp = x)
      )


      #--- *** Figures for manuscript & supplementary materials ------
      list_ms_figs <- c(
        Fig3 = "StackedBars-RobustDeltasRR_Overall_by_TimeRCPsAndHistorical.pdf"
      )
      list_sm_figs <- NULL

      for (k0 in seq_along(list_def_figs)) {
        tmp_tag <- paste0(
          "StackedBars-RobustDeltasRR_",
          list_def_figs[[k0]][["inp"]][["extent"]],
          "_by_TimeRCPs",
          if (list_def_figs[[k0]][["inp"]][["incl_ambient"]]) "AndAmbient",
          if (list_def_figs[[k0]][["inp"]][["incl_hist"]]) "AndHistorical",
          ".pdf"
        )

        is_ms <- grepl(tmp_tag, list_ms_figs)
        is_sm <- grepl(tmp_tag, list_sm_figs)

        list_def_figs[[k0]][["fname"]] <- file.path(
          dir_outitems,
          tag_outitems_figtu,
          paste0(
            if (any(is_ms)) {
              "Manuscript"
            } else if (any(is_sm)) {
              "Supplementary"
            } else {
              "Exploration"
            },
            "_",
            tag_style
          ),
          paste0(
            if (any(is_ms)) {
              names(list_ms_figs)[is_ms]
            } else if (any(is_sm)) {
              names(list_sm_figs)[is_sm]
            } else {
              "Fig"
            },
            "_",
            tmp_tag
          )
        )
      }


      #--- ** Loop over figures ------
      for (k0 in seq_along(list_def_figs)) {

        if (!file.exists(list_def_figs[[k0]][["fname"]])) {
          list_extents <- "ecotype"

          amb_or_hist <-
            list_def_figs[[k0]][["inp"]][["incl_ambient"]] ||
            list_def_figs[[k0]][["inp"]][["incl_hist"]]

          list_plots <- list()
          np <- if (amb_or_hist) 2L else 1L
          ip <- 1L
          xrelHist <- NULL

          for (kp in seq_len(np)) {
            is_ambhist <- (amb_or_hist) && kp == 1L

            tmp_simslices <- if (is_ambhist) {
              c(
                if (list_def_figs[[k0]][["inp"]][["incl_ambient"]]) tag_ambient,
                if (list_def_figs[[k0]][["inp"]][["incl_hist"]]) tag_hist
              )
            } else {
              tag_acrmods
            }

            ftmp <- newRR3::locate_tabulated_files(
              filenames = if (is_ambhist) {
                c(
                  if (list_def_figs[[k0]][["inp"]][["incl_ambient"]]) fns_in_ambient,
                  if (list_def_figs[[k0]][["inp"]][["incl_hist"]]) fns_in_historical
                )
              } else {
                fns_in_robustdelta
              },
              simslices = tmp_simslices,
              extent = list_extents[[1L]],
              variables = tag_vars
            )

            xsut2 <- newRR3::prepare_df_for_stackedbars(
              filenames = ftmp,
              simslices = tmp_simslices,
              extent = list_extents[[1L]],
              variables = tag_vars
            )

            xsut2[["Response"]] <- gsub(".", "+", xsut2[["Response"]], fixed = TRUE)


            if (is_ambhist) {
              #--- RR levels as factor
              xsut2[["Response"]] <- factor(
                xsut2[["Response"]],
                levels = meta[["varsets"]][["rr"]][["levels"]],
                ordered = TRUE
              )

              xsut3 <- xsut2

              #--- Relative proportions for reference category
              xrelHist <-
                dplyr::filter(xsut3, grepl("Historical", SimSlice)) |>
                dplyr::group_by(Var, SimSlice, SpatialUnit) |>
                dplyr::mutate(
                  freq = cumsum(values / sum(values))
                )

              # Remove 1s
              tmp <- meta[["varsets"]][["rr"]][["levels"]]
              ids <- xrelHist[["Response"]] == tmp[length(tmp)]
              xrelHist <- xrelHist[!ids, , drop = FALSE]


            } else {
              #--- Sort y-axis top->down
              xsut2[["SimSlice"]] <- factor(
                xsut2[["SimSlice"]],
                levels = sort(unique(xsut2[["SimSlice"]]), decreasing = TRUE)
              )

              #--- ** Sort robust deltas by reference category (historical RR) ------
              tmp <- newRR3::sort_deltas_by_reference(
                x = xsut2[["Response"]],
                ref_lvls = meta[["varsets"]][["rr"]][["levels"]],
                x_conv = meta[["delta"]][["rr"]][["robust"]],
                simple_conv = meta[["delta"]][["basic"]][["robust"]],
                is_robust = TRUE
              )

              xsut2[["Response"]] <- tmp[["Response"]]
              xsut2[["RR_hist"]] <- tmp[["Reference"]]



              #--- *** Summarize robust deltas ------
              # (by 1) reference categories and 2) direction of delta)
              levels_robustdelta_from_ref <- paste0(
                rep(
                  meta[["delta"]][["basic"]][["robust"]][["levels"]],
                  times = length(meta[["varsets"]][["rr"]][["levels"]])
                ),
                "_from_",
                rep(
                  meta[["varsets"]][["rr"]][["levels"]],
                  each = length(meta[["delta"]][["basic"]][["robust"]][["levels"]])
                )
              )

              xsut3 <- dplyr::group_by(xsut2, dplyr::across(!values)) |>
                dplyr::summarize(
                  values = sum(values),
                  n = dplyr::n(),
                  .groups = "keep"
                )

              xsut3[["Response_Full"]] <- factor(
                paste0(xsut3[["Response"]], "_from_", xsut3[["RR_hist"]]),
                levels = levels_robustdelta_from_ref,
                ordered = TRUE
              )
            }

            #--- ** Plot ------
            #--- *** Create base plot (using robust simple delta colors) ------
            list_plots[[ip]] <- newRR3::plot_spatialtabulation_stackedbars(
              xsut3,
              var_y = "SimSlice",
              colorscale = if (is_ambhist) {
                newRR3::colorscale_rr
              } else {
                newRR3::colorscale_robustsimpledelta
              }
            ) +
              ggplot2::theme(
                plot.title = ggplot2::element_text(size = 11)
              )


            if (is_ambhist) {
              list_plots[[ip]] <-
                list_plots[[ip]] +
                ggplot2::guides(
                  fill = ggplot2::guide_legend(nrow = 2L)
                )

            } else {
              #--- *** Add robust simple deltas (sorted by ambient) ------
              list_plots[[ip]] <-
                list_plots[[ip]] +
                ggnewscale::new_scale_fill() +
                ggplot2::geom_bar(
                  ggplot2::aes(
                    x = values,
                    y = SimSlice,
                    fill = Response_Full
                  ),
                  position = ggplot2::position_fill(reverse = TRUE),
                  stat = "identity",
                  show.legend = FALSE
                ) +
                newRR3::colorscale_robustdelta_by_ref(
                  levels = levels(xsut3[["Response_Full"]])
                )
            }

            list_plots[[ip]] <-
              list_plots[[ip]] +
              ggplot2::geom_vline(
                data = xrelHist,
                ggplot2::aes(xintercept = freq)
              )

            ip <- ip + 1L
          }


          if (amb_or_hist) {
            list_plots <- newRR3::panels_remove_inner_labels(
              list_plots,
              remove_xlabs = 1L,
              remove_facetlabs = 2L,
              remove_inner_plotmargins = TRUE
            )
          }


          #--- ** Save plot ------
          dir.create(
            dirname(list_def_figs[[k0]][["fname"]]),
            recursive = TRUE,
            showWarnings = FALSE
          )

          grDevices::pdf(
            file = list_def_figs[[k0]][["fname"]],
            height = 2.8 +
              (if (list_def_figs[[k0]][["inp"]][["incl_ambient"]]) 0.8 else 0) +
              (if (list_def_figs[[k0]][["inp"]][["incl_hist"]]) 0.8 else 0),
            width = 6.5 + if (amb_or_hist) 0.2 else 0
          )
          plot(
            patchwork::wrap_plots(
              list_plots,
              ncol = 1L,
              heights = if (amb_or_hist) {
                c(
                  (if (list_def_figs[[k0]][["inp"]][["incl_ambient"]]) 1 else 0) +
                    (if (list_def_figs[[k0]][["inp"]][["incl_hist"]]) 1 else 0),
                  length(tag_acrmods)
                )
              },
              guides = "collect"
            ) &
              ggplot2::theme(legend.position = "bottom")
          )
          grDevices::dev.off()
        }
      }



      #----- . ------
      #--- * Fig: Robust-simple-delta (overall; w/o amb) ------
      #   - extent: ecotype
      #   - Panels: RSL, RST
      #   - Vertical: timeperiods x RCPs
      #   - Horizontal: robust simplified deltas (median acrmod)
      kh <- FALSE

      for (ka in c(FALSE, TRUE)) {
        figname <- file.path(
          dir_outitems,
          tag_outitems_figtu,
          paste0("Exploration_", tag_style),
          paste0(
            "Fig_StackedBars-RobustSimplifiedDeltasRR_Overall_by_TimeRCPs",
            if (ka) "AndAmbient",
            ".pdf"
          )
        )

        if (!file.exists(figname)) {
          list_extents <- "ecotype"

          list_plots <- list()
          np <- if (ka) 2L else 1L
          ip <- 1L

          for (kp in seq_len(np)) {
            is_amb <- ka && kp == 1L

            ftmp <- newRR3::locate_tabulated_files(
              filenames = if (is_amb) fns_in_ambient else fns_in_robustsimpledelta,
              simslices = if (is_amb) tag_ambient else tag_acrmods,
              extent = list_extents[[1L]],
              variables = tag_vars
            )

            xsut2 <- newRR3::prepare_df_for_stackedbars(
              filenames = ftmp,
              simslices = if (is_amb) tag_ambient else tag_acrmods,
              extent = list_extents[[1L]],
              variables = tag_vars
            )

            if (is_amb) {
              # Fix RR levels
              xsut2[["Response"]] <- factor(
                gsub(".", "+", xsut2[["Response"]], fixed = TRUE),
                levels = meta[["varsets"]][["rr"]][["levels"]],
                ordered = TRUE
              )
            } else {
              # Sort y-axis top->down
              xsut2[["SimSlice"]] <- factor(
                xsut2[["SimSlice"]],
                levels = sort(unique(xsut2[["SimSlice"]]), decreasing = TRUE)
              )
            }

            list_plots[[ip]] <- newRR3::plot_spatialtabulation_stackedbars(
              xsut2,
              var_y = "SimSlice",
              colorscale = if (is_amb) {
                newRR3::colorscale_rr
              } else {
                newRR3::colorscale_robustsimpledelta
              }
            ) +
              ggplot2::theme(
                plot.title = ggplot2::element_text(size = 11)
              )

            if (is_amb) {
              list_plots[[ip]] <- list_plots[[ip]] +
                ggplot2::guides(
                  fill = ggplot2::guide_legend(nrow = 2L)
                )
            }

            ip <- ip + 1L
          }


          if (ka) {
            list_plots <- newRR3::panels_remove_inner_labels(
              list_plots,
              remove_xlabs = 1L,
              remove_facetlabs = 2L
            )
          }


          #--- Create plot
          dir.create(dirname(figname), recursive = TRUE, showWarnings = FALSE)
          grDevices::pdf(
            file = figname,
            height = 2.8 + if (ka) 0.8 else 0,
            width = 6.5 + if (ka) 0.2 else 0
          )
          plot(
            patchwork::wrap_plots(
              list_plots,
              ncol = 1L,
              heights = if (ka) c(1L, length(tag_acrmods)),
              guides = "collect"
            ) &
              ggplot2::theme(legend.position = "bottom")
          )
          grDevices::dev.off()
        }
      }



      #--- * Fig: Simple-delta with low/high + robust-med (overall; w/o amb) ------
      #   - extent: ecotype
      #   - Panels: RSL, RST
      #   - Vertical: timeperiods x RCPs
      #   - Horizontal: simplified deltas (low, robust-median, high acrmod)
      kh <- FALSE

      for (ka in c(FALSE, TRUE)) {
        figname <- file.path(
          dir_outitems,
          tag_outitems_figtu,
          paste0("Exploration_", tag_style),
          paste0(
            "Fig_StackedBars-SimplifiedDeltasRR-WithLowRobustmedHigh_Overall_by_TimeRCPs",
            if (ka) "AndAmbient",
            ".pdf"
          )
        )

        if (!file.exists(figname)) {
          list_extents <- "ecotype"

          list_plots <- list()
          np <- if (ka) 2L else 1L
          ip <- 1L

          for (kp in seq_len(np)) {
            is_amb <- ka && kp == 1L

            if (is_amb) {
              ftmp <- newRR3::locate_tabulated_files(
                filenames = fns_in_ambient,
                simslices = tag_ambient,
                extent = list_extents[[1L]],
                variables = tag_vars
              )

              xsut2 <- newRR3::prepare_df_for_stackedbars(
                filenames = ftmp,
                simslices = tag_ambient,
                extent = list_extents[[1L]],
                variables = tag_vars
              )

              # Fix RR levels
              xsut2[["Response"]] <- factor(
                gsub(".", "+", xsut2[["Response"]], fixed = TRUE),
                levels = meta[["varsets"]][["rr"]][["levels"]],
                ordered = TRUE
              )

            } else {
              xsut2l <- NULL

              for (kl in seq_along(meta[["acrmod"]][["signal"]][["tag"]])) {
                ftmp <- newRR3::locate_tabulated_files(
                  filenames = if (meta[["acrmod"]][["signal"]][["tag"]][[kl]] == "med") {
                    fns_in_robustsimpledelta
                  } else {
                    fns_in_simpledelta[[kl]]
                  },
                  simslices = tag_acrmods,
                  extent = list_extents[[1L]],
                  variables = tag_vars
                )

                xtmp <- newRR3::prepare_df_for_stackedbars(
                  filenames = ftmp,
                  simslices = tag_acrmods,
                  extent = list_extents[[1L]],
                  variables = tag_vars
                )

                xsut2l[[kl]] <- cbind(
                  xtmp,
                  acrmod_level =  meta[["acrmod"]][["signal"]][["tag"]][[kl]]
                )
              }

              xsut2 <- do.call(rbind, xsut2l)


              # Sort y-axis top->down
              xsut2[["SimSlice"]] <- factor(
                xsut2[["SimSlice"]],
                levels = sort(unique(xsut2[["SimSlice"]]), decreasing = TRUE)
              )

              # Hack response levels to work with robust color scale
              xsut2[["Response_orig"]] <- xsut2[["Response"]]
              for (rcl in meta[["delta"]][["basic"]][["full"]][["levels"]]) {
                xsut2[["Response"]] <- gsub(
                  pattern = paste0("^", rcl, "$"),
                  replacement = paste0("nonrobust_", rcl),
                  x = xsut2[["Response"]]
                )
              }

              # Fix levels
              xsut2[["Response"]] <- factor(
                xsut2[["Response"]],
                levels = meta[["delta"]][["basic"]][["robust"]][["levels"]],
                ordered = TRUE
              )

              xsut2[["acrmod_level"]] <- factor(
                xsut2[["acrmod_level"]],
                levels = meta[["acrmod"]][["signal"]][["tag"]],
                ordered = TRUE
              )

              # Set relative bar widths
              xsut2[["width"]] <- ifelse(
                xsut2[["acrmod_level"]] == "med",
                1,
                0.3
              )
            }

            list_plots[[ip]] <- if (is_amb) {
              newRR3::plot_spatialtabulation_stackedbars(
                xsut2,
                var_y = "SimSlice",
                colorscale = newRR3::colorscale_rr
              )
            } else {
              newRR3::plot_spatialtabulation_stackedbars3(
                xsut2,
                var_y = "SimSlice",
                colorscale = newRR3::colorscale_robustsimpledelta
              )
            } +
              ggplot2::theme(
                plot.title = ggplot2::element_text(size = 11)
              )

            if (is_amb) {
              list_plots[[ip]] <- list_plots[[ip]] +
                ggplot2::guides(
                  fill = ggplot2::guide_legend(nrow = 2L)
                )
            }

            ip <- ip + 1L
          }


          if (ka) {
            list_plots <- newRR3::panels_remove_inner_labels(
              list_plots,
              remove_xlabs = 1L,
              remove_facetlabs = 2L
            )
          }


          #--- Create plot
          dir.create(dirname(figname), recursive = TRUE, showWarnings = FALSE)
          grDevices::pdf(
            file = figname,
            height = 2.8 + if (ka) 0.8 else 0,
            width = 6.5 + if (ka) 0.2 else 0
          )
          plot(
            patchwork::wrap_plots(
              list_plots,
              ncol = 1L,
              heights = if (ka) c(1L, length(tag_acrmods)),
              guides = "collect"
            ) &
              ggplot2::theme(legend.position = "bottom")
          )
          grDevices::dev.off()
        }
      }



      #----- . ------
      #--- * Fig: {ecoregions, SEI category} by time x RCP & ambient ------

      #------ ** Define figures ------
      list_extents <- c(
        ecoregionL3 = "ecoregionL3",
        ecoregionSimplified = "ecoregionL3",
        SCD1SEI2020 = "SCD1SEI2020"
      )

      tmp <- expand.grid(
        incl_ambient = TRUE,
        incl_hist = FALSE,
        extent = names(list_extents),
        KEEP.OUT.ATTRS = FALSE,
        stringsAsFactors = FALSE
      )
      tmp[["extentData"]] <- list_extents[match(list_extents, tmp[["extent"]])]
      # we want at either ambient or historical but not none or both
      tmp <- tmp[xor(tmp[["incl_ambient"]], tmp[["incl_hist"]]), , drop = FALSE]

      list_def_figs <- lapply(
        split(tmp, seq_len(nrow(tmp))),
        function(x) list(inp = x)
      )


      #--- *** Figures for manuscript & supplementary materials ------
      list_ms_figs <- c(
        Fig8 = "StackedBars-RobustSimplifiedDeltasRR_SCD1SEI2020_by_TimeRCPsAndAmbient.pdf"
      )

      list_sm_figs <- c(
        `FigS3-1` = "StackedBars-RobustSimplifiedDeltasRR_ecoregionSimplified_by_TimeRCPsAndAmbient.pdf",
        `FigS3-2` = "StackedBars-RobustSimplifiedDeltasRR_ecoregionL3_by_TimeRCPsAndAmbient.pdf"
      )

      for (k0 in seq_along(list_def_figs)) {
        tmp_tag <- paste0(
          "StackedBars-RobustSimplifiedDeltasRR_",
          list_def_figs[[k0]][["inp"]][["extent"]],
          "_by_TimeRCPs",
          if (list_def_figs[[k0]][["inp"]][["incl_ambient"]]) "AndAmbient",
          if (list_def_figs[[k0]][["inp"]][["incl_hist"]]) "AndHistorical",
          ".pdf"
        )

        is_ms <- grepl(tmp_tag, list_ms_figs)
        is_sm <- grepl(tmp_tag, list_sm_figs)

        list_def_figs[[k0]][["fname"]] <- file.path(
          dir_outitems,
          tag_outitems_figtu,
          paste0(
            if (any(is_ms)) {
              "Manuscript"
            } else if (any(is_sm)) {
              "Supplementary"
            } else {
              "Exploration"
            },
            "_",
            tag_style
          ),
          paste0(
            if (any(is_ms)) {
              names(list_ms_figs)[is_ms]
            } else if (any(is_sm)) {
              names(list_sm_figs)[is_sm]
            } else {
              "Fig"
            },
            "_",
            tmp_tag
          )
        )
      }


      #--- ** Loop over figures ------
      for (k0 in seq_along(list_def_figs)) {

        if (!file.exists(list_def_figs[[k0]][["fname"]])) {
          list_plots <- list()
          ip <- it <- 1L

          for (ka in c(TRUE, FALSE)) {
            used_slices <- if (ka) {
              tag_ambient
            } else {
              tag_acrmods
            }

            for (kp in seq_along(used_slices)) {
              ftmp <- newRR3::locate_tabulated_files(
                filenames = if (ka) fns_in_ambient else fns_in_robustsimpledelta,
                simslices = used_slices[[kp]],
                extent = list_def_figs[[k0]][["inp"]][["extentData"]],
                variables = tag_vars
              )

              xsut2 <- newRR3::prepare_df_for_stackedbars(
                filenames = ftmp,
                simslices = used_slices[[kp]],
                extent = list_def_figs[[k0]][["inp"]][["extentData"]],
                variables = tag_vars
              )

              # Remove "NSA" (non-sagebrush area) from SEI categories
              xsut2 <- xsut2[!xsut2[["SpatialUnit"]] %in% "NSA", , drop = FALSE]


              # Simplify ecoregions
              if (
                list_def_figs[[k0]][["inp"]][["extent"]] == "ecoregionSimplified" &&
                  !is.null(agged_ecoregions)
              ) {
                ids <- match(
                  xsut2[["SpatialUnit"]],
                  agged_ecoregions[["NA_L3NAME"]]
                )
                stopifnot(!anyNA(ids))

                xsut2[["SpatialUnit"]] <- agged_ecoregions[["NA_AGGBREV"]][ids]
              }

              # Sort y-axis top->down
              xsut2[["SpatialUnit"]] <- factor(
                xsut2[["SpatialUnit"]],
                levels = sort(unique(xsut2[["SpatialUnit"]]), decreasing = TRUE)
              )

              # Fix RR levels
              if (ka) {
                xsut2[["Response"]] <- factor(
                  gsub(".", "+", xsut2[["Response"]], fixed = TRUE),
                  levels = meta[["varsets"]][["rr"]][["levels"]],
                  ordered = TRUE
                )
              }


              # Create plot
              list_plots[[ip]] <- newRR3::plot_spatialtabulation_stackedbars(
                xsut2,
                var_y = "SpatialUnit",
                colorscale = if (ka) {
                  newRR3::colorscale_rr
                } else {
                  newRR3::colorscale_robustsimpledelta
                }
              ) +
                ggplot2::labs(
                  title = paste0(
                    switch(
                      EXPR = style_figures,
                      ESA = paste0("(", letters[[it]], ") "),
                      paste0(LETTERS[[it]], ") ")
                    ),
                    labels_simslices[[if (ka) 1L else if (kh) 2L else 3L]][[kp]]
                  )
                ) +
                ggplot2::theme(
                  plot.title = ggplot2::element_text(size = 11),
                  legend.position = "right"
                )

              ip <- ip + 1L
              it <- it + 1L

              # Add empty panel (used to collect the legend) on ambient "row"
              if (!ka && kp == 2L) {
                #list_plots[[ip]] <- ggplot2::ggplot() + ggplot2::theme_void()
                list_plots[[ip]] <- patchwork::guide_area()
                ip <- ip + 1L
              }
            }

          }

          list_plots <- newRR3::panels_remove_inner_labels(
            list_plots,
            remove_xlabs = c(1L, 2L, 5L),
            remove_ylabs = c(5L, 6L),
            remove_facetlabs = c(2L, 3L, 6L)
          )


          n <- length(unique(xsut2[["SpatialUnit"]]))
          ll <- switch(
            EXPR = list_def_figs[[k0]][["inp"]][["extent"]],
            ecoregionL3 = 0.5,
            ecoregionSimplified = 0.4,
            SCD1SEI2020 = 0.1,
            0.2
          )

          dir.create(
            dirname(list_def_figs[[k0]][["fname"]]),
            recursive = TRUE,
            showWarnings = FALSE
          )
          grDevices::pdf(
            file = list_def_figs[[k0]][["fname"]],
            height = 3 * (1 / 7 * n + 1.571),
            width = (2 + ll) * 5 #3 * (if (n < 20) 3.5 else 5)
          )
          plot(
            patchwork::wrap_plots(
              list_plots,
              byrow = FALSE,
              ncol = 2L,
              guides = "collect"
            ) &
              ggplot2::theme(
                legend.position = "right",
                legend.direction = "horizontal"
              )
          )
          grDevices::dev.off()
        }
      }
    }



    #----- . ------
    #--- Figures MIRRP ------
    #----- . ------
    if (vtagk == "mirrp") {
      #--- * Fig: Most Important Predictors (overall) ------
      #   - extent: ecotype
      #   - Panels: RSL, RST
      #   - Vertical: timeperiods x RCPs
      #   - Horizontal: MIPs

      used_responselabels <- "description" # "short10"

      figname <- file.path(
        dir_outitems,
        tag_outitems_figtu,
        paste0("Manuscript_", tag_style),
        paste0(
          "Fig7_StackedBars-MIRRP_Overall_by_TimeRCPs",
          ".pdf"
        )
      )

      if (!file.exists(figname)) {
        list_extents <- "ecotype"

        list_plots <- list()
        ip <- 1L

        ftmp <- newRR3::locate_tabulated_files(
          filenames = fns_in_med_mirrpm,
          simslices = tag_acrmods,
          extent = list_extents[[1L]],
          variables = tag_vars
        )

        xsut2 <- newRR3::prepare_df_for_stackedbars(
          filenames = ftmp,
          simslices = tag_acrmods,
          extent = list_extents[[1L]],
          variables = tag_vars
        )


        # Beautify labels
        xsut2[["Response"]] <- factor(
          meta[["varsets"]][["preds"]][["varname"]][xsut2[["Response"]]],
          levels = meta[["varsets"]][["preds"]][["varname"]],
          labels = meta[["varsets"]][["preds"]][[used_responselabels]]
        )

        xsut2[["Var"]] <- gsub("_mirrp", "", xsut2[["Var"]])

        xsut2[["SimSlice"]] <- gsub(
          pattern = "_",
          replacement = "\n",
          x = xsut2[["SimSlice"]],
          fixed = TRUE
        )
        substr(xsut2[["SimSlice"]], 1L, 1L) <- toupper(
          substr(xsut2[["SimSlice"]], 1L, 1L)
        )

        # Sort y-axis top->down
        xsut2[["SimSlice"]] <- factor(
          xsut2[["SimSlice"]],
          levels = sort(unique(xsut2[["SimSlice"]]), decreasing = TRUE)
        )


        tmp_plot <- newRR3::plot_spatialtabulation_stackedbars(
          data = xsut2,
          var_y = "SimSlice",
          colorscale = newRR3::colorscale_predictors19
        )

        if (used_responselabels == "description") {
          tmp_plot <- tmp_plot +
            ggplot2::guides(fill = ggplot2::guide_legend(ncol = 3L))
        }

        list_plots[[ip]] <- tmp_plot
        ip <- ip + 1L


        #--- Create plot
        dir.create(dirname(figname), recursive = TRUE, showWarnings = FALSE)
        grDevices::pdf(
          file = figname,
          height = 4 + if (used_responselabels == "description") 0.75 else 0,
          width = 6.5
        )
        plot(
          patchwork::wrap_plots(
            list_plots,
            ncol = 1L,
            guides = "collect"
          ) &
            ggplot2::theme(legend.position = "bottom")
        )
        grDevices::dev.off()
      }

    }
  }
}

print(summary(warnings()))

#------. ------
#------. ------
