#----- . ------
#----- OVERVIEW DISPLAY ITEMS ------
#
# * Output: `"displayitems/Overview_supplementary/"`
#----- . ------



#----- SET UP ------
stopifnot(requireNamespace("newRR3"))
options(warn = 2L) # convert warnings to errors
sf::sf_use_s2(TRUE)

#--- Settings ------
req_meta_version <- "3.2.0"

vtag <- c("rr") # only "rr" has categorical variables that need simplifying
vtag_input <- vtag

# 2023-Mar-22: full/simulated extent: data; rangeland mask: as attribute and for analysis
mask_sets <- c("sim", "rangelands", "combined")[[3L]]
data_mask_sets <- c("sim", "rangelands", "combined")[1:2]

# Figure maps
downsample_raster <- 20L
crs_map <- 6350



#--- Paths (part 1) ------
dir_prj <- ".."

dir_dataraw <- file.path(dir_prj, "data-raw")
stopifnot(dir.exists(dir_dataraw))

dir_res <- file.path(dir_prj, "results")
stopifnot(dir.exists(dir_res))

dir_dataraw_tu <- file.path(dir_dataraw, "spatialunits")
stopifnot(dir.exists(dir_dataraw_tu))

dir_res_tu <- file.path(dir_res, "SpatialUnits_Tabulations")
stopifnot(dir.exists(dir_res_tu))



#--- Paths (part 2) ------
dir_outitems <- file.path(dir_prj, "displayitems")

dir_outitems_ov <- file.path(dir_outitems, "Overview_supplementary")
dir.create(dir_outitems_ov, recursive = TRUE, showWarnings = FALSE)




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
#------ . ------

#------ Create table: climate model ------
fname_gcm <- file.path(
  dir_outitems_ov,
  "TableS2_MACAgridMETv2_ListClimateModel.csv"
)

if (!file.exists(fname_gcm)) {
  tmp <- data.frame(
    `Climate model` = meta[["simexps"]][["meta_scen"]][["hist"]][["Model"]][-1L],
    `historical (1950-2005)` = "x",
    `RCP 4.5 (2029-2064)` = "x",
    `RCP 4.5 (2064-2099)` = "x",
    `RCP 8.5 (2029-2064)` = "x",
    `RCP 8.5 (2064-2099)` = "x",
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  write.csv(tmp, file = fname_gcm, row.names = FALSE)
}



#------ . ------
#------ LOOP OVER SPATIAL MASKS ------
for (km in seq_along(data_mask_sets)) {

  #------ . ------
  #--- Overviews: study area map, ecoregion map and table ------
  fname_overview_studyarea_figs <- file.path(
    dir_outitems_ov,
    paste0(
      "FigS1_Map_StudyArea-",
      c("", "Lower48-"),
      data_mask_sets[[km]],
      ".png"
    )
  )

  fname_overview_ecoregions_figs <- file.path(
    dir_outitems_ov,
    paste0(
      "FigS1_Map_Ecoregions-",
      c("", "Lower48-"),
      data_mask_sets[[km]],
      ".png"
    )
  )

  fname_overview_ecoregions_table <- file.path(
    dir_outitems_ov,
    paste("TableS1_Ecoregions-", data_mask_sets[[km]], ".csv")
  )

  ftmps <- c(
    fname_overview_studyarea_figs,
    fname_overview_ecoregions_figs,
    fname_overview_ecoregions_table
  )

  if (!all(file.exists(ftmps))) {

    #--- * Load suid geotiff ------
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

    # Just use as mask
    has_value <-
      !is.na(list_gsu[["xstars"]][[1L]]) & list_gsu[["xstars"]][[1L]] > 0L
    list_gsu[["xstars"]][[1L]][has_value] <- 1L


    #--- * Load and prepare polygons ------
    # See `Script30_newRR3_PrepareGeographicalUnits.R`
    st_sagebiome <- newRR3::load_sagebiome_polygon(dir_dataraw_tu, crs_map)

    st_NEadmin1 <- newRR3::load_NEadmin1_polygon(dir_dataraw_tu, crs_map)


    if (
      (!file.exists(fname_overview_ecoregions_fig) ||
        !file.exists(fname_overview_ecoregions_table)) &&
        data_mask_sets[[km]] == "rangelands"
    ) {
      #--- * Load and prepare polygons (part 2) ------
      st_ecoregions <- newRR3::load_ecoregions_polygon(dir_dataraw_tu, crs_map)


      #--- ** Simplified ecoregions ------
      st_ecoregions <- st_ecoregions |>
        # sort alphabetically by name
        dplyr::arrange(
          !!rlang::sym("NA_AGGBREV"),
          !!rlang::sym("NA_L3NAME")
        ) |>
        dplyr::ungroup() |>
        # add row number (across all ecoregions)
        dplyr::mutate(id = dplyr::row_number()) |>
        # create labels
        dplyr::group_by(!!rlang::sym("NA_AGGBREV")) |>
        dplyr::mutate(
          labelAgg = paste0(
            !!rlang::sym("NA_AGGBREV"), " (",
            if (length(id) > 1) paste0(min(id), "-", max(id)) else id,
            ")"
          )
        )

      # calculate total area by EPA L3 ecoregion
      st_ecoregions[["area_total_l3_1e3km2"]] <-
        sf::st_area(st_ecoregions) |>
        units::set_units(1e-3 * km^2) |>
        round(digits = 1)



      # Subset ecoregions to sagebrush biome
      st_ecoL3_in_sagbrush <- suppressWarnings(
        # suppress: "attribute variables are assumed to be spatially constant throughout all geometries"
        st_ecoregions |>
          sf::st_intersection(st_sagebiome) |>
          sf::st_make_valid()
      )

      # calculate area by EPA L3 ecoregion within sagebrush biome
      st_ecoL3_in_sagbrush[["area_sbiome_l3_1e3km2"]] <-
        sf::st_area(st_ecoL3_in_sagbrush) |>
        units::set_units(1e-3 * km^2) |>
        round(digits = 1)


      #--- For each aggregated ecoregion: union, sort
      st_ecoAgg_in_sagbrush <- suppressWarnings(
        # suppress: "attribute variables are assumed to be spatially constant throughout all geometries"
        st_ecoL3_in_sagbrush |>
          # union polygons by aggregated ecoregions
          dplyr::group_by(
            !!rlang::sym("NA_AGGBREV"),
            !!rlang::sym("labelAgg")
          ) |>
          dplyr::summarize(
            .groups = "keep",
            do_union = TRUE
          ) |>
          sf::st_make_valid() |>
          # sort alphabetically by name
          dplyr::arrange(!!rlang::sym("NA_AGGBREV"))
      )
    }


    #--- * Map extents ------
    map_bbox_sb <- sf::st_bbox(st_sagebiome)

    #--- Lower48
    # Northwest Angle Inlet in Lake of the Woods, Minnesota: northernmost point in the 48 contiguous states
    #   49.384472, -95.153389
    # Ballast Key, Florida: southernmost point in the 48 contiguous states continuously above water
    #   24.520833, -81.963611
    # Sail Rock, Lubec, Maine: easternmost point in the 50 states, by direction of travel and easternmost incorporated place in the 50 states
    #   44.812556, -66.947028
    # Bodelteh Islands, offshore from Cape Alava, Washington: westernmost point in the 48 contiguous states continuously above water
    #   48.178528, -124.771694
    map_bbox_lower48 <- rbind(
      c(49.384472, -95.153389),
      c(24.520833, -81.963611),
      c(44.812556, -66.947028),
      c(48.178528, -124.771694)
    )[, 2:1] |>
      data.matrix() |>
      sf::st_multipoint() |>
      sf::st_sfc(crs = "OGC:CRS84") |>
      sf::st_cast(to = "POINT") |>
      sf::st_transform(crs = sf::st_crs(list_gsu[["xstars"]])) |>
      sf::st_bbox()


    #--- * Plots & tables ------

    #--- ** Create study area map ------
    if (!all(file.exists(fname_overview_studyarea_figs))) {

      for (k in 1:2) {
        map_bbox <- switch(k, map_bbox_sb, map_bbox_lower48)

        tmp_base <- newRR3::plot_map(
          xstars = list_gsu[["xstars"]],
          bbox = map_bbox,
          add_coords = FALSE,
          maintitle = NULL,
          subtitle = NULL,
          panel_tag = NULL,
          show_legend = FALSE,
          legend_xoffset = 0.,
          legend_yoffset = 0.,
          st_geom_sb = st_sagebiome,
          st_geom_state = st_NEadmin1,
          st_geom_eco = NULL
        ) +
          newRR3::ggplot2_map_theme() +
          ggplot2::scale_fill_gradientn(
            colors = c("white", "darkgray"),
            limits = c(0, 1),
            na.value = "white"
          )

        tmp_panel <- tmp_base +
          # Wrap up figure
          newRR3::add_coords(
            crs = crs_map,
            bbox = map_bbox,
            expand_bbox = TRUE
          ) +
          ggplot2::theme(
            legend.position = "right"
          )


        #--- Save map to file
        # See `ggplot2::coord_sf(...)$aspect()` to figure out the aspect ratio:
        asp <- (map_bbox[["ymax"]] - map_bbox[["ymin"]]) / (map_bbox[["xmax"]] - map_bbox[["xmin"]])

        png(
          file = fname_overview_studyarea_figs[[k]],
          height = 5 * asp,
          width = 5,
          units = "in",
          res = 300
        )
        plot(tmp_panel)
        dev.off()
      }
    }



    if (data_mask_sets[[km]] == "rangelands") {
      #--- ** Create ecoregion map ------
      if (!all(file.exists(fname_overview_ecoregions_figs))) {
        for (k in 1:2) {
          map_bbox <- switch(k, map_bbox_sb, map_bbox_lower48)

        tmp_base <- newRR3::plot_map(
          xstars = list_gsu[["xstars"]],
          bbox = map_bbox,
          add_coords = FALSE,
          maintitle = NULL,
          subtitle = NULL,
          panel_tag = NULL,
          show_legend = FALSE,
          legend_xoffset = 0.,
          legend_yoffset = 0.,
          st_geom_sb = st_sagebiome,
          st_geom_state = st_NEadmin1,
          st_geom_eco = st_ecoregions
        ) +
          newRR3::ggplot2_map_theme() +
          ggplot2::scale_fill_gradientn(
            colors = c("white", "darkgray"),
            limits = c(0, 1),
            na.value = "white"
          )

        tmp_panel <- tmp_base +
          # Add simplified ecoregions as colored polygons (with legend)
          ggnewscale::new_scale_fill() +
          ggplot2::geom_sf(
            data = st_ecoAgg_in_sagbrush,
            show.legend = TRUE,
            size = 2,
            alpha = 0.5,
            ggplot2::aes(fill = !!rlang::sym("labelAgg"))
          ) +
          colorspace::scale_fill_discrete_qualitative(
            name = "Ecoregions"
          ) +
          # Add ecoregions L3 as empty polygon borders + ID labels
          ggnewscale::new_scale_fill() +
          ggplot2::geom_sf(
            data = st_ecoL3_in_sagbrush,
            show.legend = FALSE,
            fill = NA
          ) +
          ggrepel::geom_label_repel(
            data = st_ecoL3_in_sagbrush,
            ggplot2::aes(
              label = !!rlang::sym("id"),
              geometry = !!rlang::sym("geometry")
            ),
            stat = "sf_coordinates",
            min.segment.length = 0, # 0, always show line between point and label
            force_pull = 10 # attraction between point and label
          ) +
          # Wrap up figure
          newRR3::add_coords(
            crs = crs_map,
            bbox = map_bbox,
            expand_bbox = TRUE
          ) +
          ggplot2::theme(
            legend.position = "right"
          )


        #--- Save map to file
        # See `ggplot2::coord_sf(...)$aspect()` to figure out the aspect ratio:
        asp <- (map_bbox[["ymax"]] - map_bbox[["ymin"]]) / (map_bbox[["xmax"]] - map_bbox[["xmin"]])

        png(
          file = fname_overview_ecoregions_figs[[k]],
          height = 5 * asp,
          width = 5 + 2.5,
          units = "in",
          res = 300
        )
        plot(tmp_panel)
        dev.off()
        }
      }



      #--- ** Create ecoregion table ------
      if (!file.exists(fname_overview_ecoregions_table)) {
        # Load simulated area by ecoregion
        tmp_sim_ecoL3 <- readRDS(
          file.path(
            "/Users/dschlaepfer/Swork/Work_Stuff/2_Research/2020_Prj043_newRR/Prod043d_newRR_Obj3_FutureWallToWall/4_Analysis_newRR3/project/data/SpatialUnits_Tabulations/TabulationUnits_rangelands/TabulationUnits_rangelands__ecoregionL3.rds"
          )
        )

        tmp_area_sim_l3_1e3km2 <- data.frame(
          # Name of L3 ecoregion
          NA_L3NAME = vapply(
            tmp_sim_ecoL3,
            function(x) x[["ctus_orig"]],
            FUN.VALUE = NA_character_
          ),
          # Simulated area by L3 ecoregion
          area_sim_l3_1e3km2 = vapply(
            tmp_sim_ecoL3,
            function(x) {
              round(
                sum(x[["dft"]][["count"]][!is.na(x[["dft"]][["suid"]])]) *
                  # 30-m pixel size -> m2
                  30 * 30 /
                  # m2 -> 1e3 km2
                  (1e3 * 1e6),
                digits = 1
              )
            },
            FUN.VALUE = NA_real_
          )
        )


        df_table_ecoregions <-
          sf::st_drop_geometry(st_ecoL3_in_sagbrush) |>
          merge(
            y = tmp_area_sim_l3_1e3km2,
            all.x = TRUE
          ) |>
          dplyr::arrange(!!rlang::sym("id"), .by_group = FALSE) |>
          dplyr::group_by(!!rlang::sym("NA_AGGBREV"), .drop = FALSE) |>
          dplyr::mutate(
            area_sbiome_agg_1e3km2 = sum(!!rlang::sym("area_sbiome_l3_1e3km2")),
            area_sim_agg_1e3km2 = sum(!!rlang::sym("area_sim_l3_1e3km2"))
          )

        tmp_out <- c(
          "NA_AGGBREV",
          "area_sbiome_agg_1e3km2", "area_sim_agg_1e3km2",
          "id", "NA_L3NAME",
          "area_total_l3_1e3km2", "area_sbiome_l3_1e3km2", "area_sim_l3_1e3km2"
        )

        write.csv(
          df_table_ecoregions[, tmp_out],
          file = fname_overview_ecoregions_table,
          row.names = FALSE
        )
      }
    }
  }



  #------ . ------
  #--- Theoretical distributions: index, RF-certainty ------
  fname_overview_continuousindex_fig <- file.path(
    dir_outitems_ov,
    "FigS2_index_theoretical.png"
  )

  fname_overview_RFcertainty_fig <- file.path(
    dir_outitems_ov,
    "FigS3_RFcertainty_theoretical.png"
  )

  if (
    !all(
      file.exists(fname_overview_continuousindex_fig),
      file.exists(fname_overview_RFcertainty_fig)
    )
  ) {

    fig_ps_vs_theoretical <- function(
    filename,
      p_grid,
      ri,
      gamma = 1,
      ylab,
      ylim
    ) {
      p <- seq(0, 1, by = 0.01)

      grDevices::png(
        filename = filename,
        res = 300,
        units = "in",
        height = 3.5,
        width = 4.5
      )

      par_prev <- graphics::par(
        mfrow = c(2L, 2L),
        mar = c(2.5, 2.5, 0.5, 0.5),
        mgp = c(1, 0, 0),
        tcl = 0.3
      )

      for (k in 1:4) {
        x <- p_grid[, k]
        graphics::plot(
          x,
          ri,
          pch = 16,
          col = "darkgray",
          xlab = paste0("Probability (", colnames(p_grid)[k], ")"),
          ylab = ylab,
          xlim = c(0, 1),
          ylim = ylim
        )

        py <- try(
          predict(
            mgcv::gam(ri ~ s(x), gamma = gamma),
            newdata = data.frame(x = p)
          ),
          silent = TRUE
        )

        if (!inherits(py, "try-error")) {
          graphics::lines(p, py, col = "red", lwd = 2)
        }
      }
      graphics::par(par_prev)
      dev.off()
    }




    #--- Create all possible probability values (on regular 0.01-grid)
    p <- seq(0, 1, by = 0.01)
    xg <- expand.grid(L = p, ML = p, M = p, `H+MH` = p)
    ids1 <- abs(rowSums(xg) - 1) < sqrt(.Machine[["double.eps"]])
    xg <- as.matrix(xg[ids1, , drop = FALSE])


    #--- ** Theoretical figure: p(.) vs index ------
    if (!file.exists(fname_overview_continuousindex_fig)) {
      fig_ps_vs_theoretical(
        filename = fname_overview_continuousindex_fig,
        p_grid = xg,
        ri = newRR3::calc_RR_index(xg),
        ylab = "Continuous index",
        ylim = c(-1, 1)
      )
    }

    #--- ** Theoretical figure: p(.) vs RF-certainty ------
    if (!file.exists(fname_overview_RFcertainty_fig)) {
      fig_ps_vs_theoretical(
        filename = fname_overview_RFcertainty_fig,
        p_grid = xg,
        ri = newRR3::calc_excessp(xg),
        ylab = "RF-certainty",
        ylim = c(0, 1)
      )
    }
  }

}


print(summary(warnings()))

#------. ------
#------. ------
