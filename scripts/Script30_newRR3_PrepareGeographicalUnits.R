#----- . ------
#----- DOWNLOAD AND PREPARE GEOGRAPHICAL UNITS ------
#
# Count 30-m pixels of template GeoTIFF for each spatial intersection of
# spatial units of interest and simulation units of this project
#
# * Input
#   * Template GeoTIFF: `"data-raw/gsu/"`
#   * Download and import spatial units of interest
#     (requested by specifying elements of `list_spatialunits` and
#     `list_spatialunits_combinations`): `"data-raw/spatialunits/"`
#
# * Output
#   * Geopackage containing all spatial intersections:
#     `"data/SpatialUnits_Tabulations/*.gpkg"`
#   * Tables of pixel counts for each spatial intersection:
#     `"data/SpatialUnits_Tabulations/"`
#
#----- . ------


#----- SET UP ------
stopifnot(requireNamespace("newRR3"))
library("dplyr") # for methods `summarize()`, `group_by()`

sf::sf_use_s2(TRUE)


#--- Settings ------
req_meta_version <- "3.2.0"

# 2023-Mar-22: full/simulated extent: data; rangeland mask: as attribute and for analysis
mask_sets <- c("sim", "rangelands", "combined")[[2L]]


#--- * List of included spatial units ------
list_spatialunits <- c(
  # area within/outside "sagebrush biome"
  sagebiome = TRUE,

  # US state boundaries
  usstate = TRUE,
  # EPA ecoregions L3
  ecoregionL3 = TRUE,

  # US federal lands
  USAFederalLands_ESRI = FALSE, # Issue: copyright does not allow use for USGS
  USAFederalLands_SMA = FALSE, # Issue: polygons are not correctly closed

  # WAFWAs Sagebrush Conservation Design (raster)
  SCD1SEI2020 = TRUE,
  SCD1SEI2020vs2001 = FALSE
)

# Specific combinations of spatial units
list_spatialunits_combinations <- c(
  ecoregionL3_and_SCD1SEI2020 = TRUE,
  ecoregionL3_and_SEI20vs01 = TRUE
)


#--- Paths (part 1) ------
dir_prj <- ".."

dir_dataraw <- file.path(dir_prj, "data-raw")
stopifnot(dir.exists(dir_dataraw))

dir_data <- file.path(dir_prj, "data")
stopifnot(dir.exists(dir_data))

dir_res <- file.path(dir_prj, "results")
stopifnot(dir.exists(dir_res))


#--- Paths (part 2) ------
dir_dataraw_tu <- file.path(dir_dataraw, "spatialunits")
dir.create(dir_dataraw_tu, recursive = TRUE, showWarnings = FALSE)

dir_data_tu <- file.path(dir_data, "SpatialUnits_Tabulations")
dir.create(dir_data_tu, recursive = TRUE, showWarnings = FALSE)




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
#--- Download and import external data ------
fnames_spatialunits <- list()

#--- * Import sagebrush biome polygon ------
if (isTRUE(list_spatialunits[["sagebiome"]])) {
  fnames_spatialunits[["sagebiome"]] <- newRR3::import_sagebiome_polygon(
    dir_dataraw = dir_dataraw_tu
  )
} else {
  stop("'sagebiome' required as tabulation unit.")
}


#--- * Import EPA ecoregions L3 ------
if (isTRUE(list_spatialunits[["ecoregionL3"]])) {
  fnames_spatialunits[["ecoregionL3"]] <- newRR3::import_EPAecoregionL3_polygon(
    dir_dataraw = dir_dataraw_tu
  )
} else {
  stop("'ecoregionL3' required as tabulation unit.")
}


#--- * Import US state boundaries ------
if (isTRUE(list_spatialunits[["usstate"]])) {
  fnames_spatialunits[["usstate"]] <- newRR3::import_NEadmin1_polygon(
    dir_dataraw = dir_dataraw_tu
  )
} else {
  stop("'usstate' required as tabulation unit.")
}


#--- * Import USA Federal Lands ESRI ------
if (isTRUE(list_spatialunits[["USAFederalLands_ESRI"]])) {
  fnames_spatialunits[["USAFederalLands_ESRI"]] <- newRR3::import_ESRIfederallands_polgyon(
    dir_dataraw = dir_dataraw_tu
  )
}


#--- * Import USA Federal Lands SMA ------
if (isTRUE(list_spatialunits[["USAFederalLands_SMA"]])) {
  fnames_spatialunits[["USAFederalLands_SMA"]] <- newRR3::import_SMAfederallands_polgyon(
    dir_dataraw = dir_dataraw_tu
  )
}


#--- * Import/load Sagebrush Conservation Design SEI data -------
if (
  isTRUE(list_spatialunits[["SCD1SEI2020"]]) ||
    isTRUE(list_spatialunits[["SCD1SEI2020vs2001"]])
) {
  fnames_spatialunits[["SCD1SEI2020"]] <- newRR3::import_scd1sei_raster(
    dir_dataraw = dir_dataraw_tu,
    years = 2020
  )

  tmp_sei <- c("NSA", "CSA", "GOA", "ORA")


  tmp <- terra::levels(terra::rast(fnames_spatialunits[["SCD1SEI2020"]]))[[1]]
  cats_sei2020 <- data.frame(
    tmp,
    sei = tmp_sei,
    stringsAsFactors = FALSE
  )

  stopifnot(
    identical(
      cats_sei2020,
      data.frame(
        Value = 0:3,
        Definition = c(
          "Non-sagebrush areas (not included in analyses)",
          "Core habitat areas",
          "Growth opportunity areas",
          "Other rangeland areas"
        ),
        sei = tmp_sei,
        stringsAsFactors = FALSE
      )
    )
  )

  # Sort by increasing SEI
  cats_sei2020 <- cats_sei2020[c(1, 4, 3, 2), , drop = FALSE]
}

if (list_spatialunits[["SCD1SEI2020vs2001"]]) {
  # 30-m raster for 1998-2001 Sagebrush Ecological Integrity (SEI)
  # of the Sagebrush Conservation Design Phase I
  fnames_spatialunits[["SCD1SEI2001"]] <- newRR3::import_scd1sei_raster(
    dir_dataraw = dir_dataraw_tu,
    years = 2001
  )


  cats_sei2001 <- cats_sei2020

  cats_sei2020vs2001 <- {
    res <- expand.grid(
      from = cats_sei2001[["sei"]],
      to = cats_sei2020[["sei"]],
      stringsAsFactors = FALSE,
      KEEP.OUT.ATTRS = FALSE
    )

    res[, "bi_scale"] <- paste(
      match(res[, "from"], cats_sei2020[["sei"]]),
      match(res[, "to"], cats_sei2001[["sei"]]),
      sep = "-"
    )
    res[, "sign"] <- vapply(
      strsplit(res[, "bi_scale"], split = "-", fixed = TRUE),
      function(x) {
        x <- as.integer(x)
        if (x[[2L]] > x[[1L]]) 1L else if (x[[2L]] < x[[1L]]) -1L else 0L
      },
      FUN.VALUE = NA_integer_
    )

    res[, "levels"] <- newRR3::convolve_levels(
      to = res[, "to"],
      from = res[, "from"]
    )

    res
  }
}



#------ . ------
#--- LOAD SPATIAL TABULATION POLYGONS ------

#--- * Intersected tabulation polygons ------
tu_dtag_poly <- paste0(
  c(
    if (list_spatialunits[["sagebiome"]]) "ecotype",
    if (list_spatialunits[["ecoregionL3"]]) "ecoregionL3",
    if (list_spatialunits[["usstate"]]) "usstate",
    if (list_spatialunits[["USAFederalLands_ESRI"]]) "USAFederalLands_ESRI",
    if (list_spatialunits[["USAFederalLands_SMA"]]) "federallandsma"
  ),
  collapse = "-"
)


fname_tu <- file.path(
  dir_data_tu,
  paste0("TabulationUnits__", tu_dtag_poly, ".gpkg")
)


if (file.exists(fname_tu)) {
  tabulation_units_poly <- sf::st_read(dsn = fname_tu)

} else {
  #--- ** Load polygon components of tabulation units ------
  list_tu_poly <- list()

  if (isTRUE(list_spatialunits[["sagebiome"]])) {
    list_tu_poly[["sagebiome"]] <- sf::st_read(fnames_spatialunits[["sagebiome"]]) |>
      sf::st_transform(crs = 6350)
  }

  if (isTRUE(list_spatialunits[["ecoregionL3"]])) {
    # Subset to sagebrush biome and simplify
    NA_L3NAME <- NULL

    list_tu_poly[["ecoregionL3"]] <- suppressWarnings(
      # suppress:
      #   attribute variables are assumed to be spatially constant
      #   throughout all geometries
      readRDS(fnames_spatialunits[["ecoregionL3"]]) |>
        sf::st_transform(crs = 6350) |>
        sf::st_intersection(list_tu_poly[["sagebiome"]]) |>
        sf::st_make_valid() |>
        dplyr::group_by(NA_L3NAME) |>
        dplyr::summarize()
    )
  }

  if (isTRUE(list_spatialunits[["usstate"]])) {
    # Subset to sagebrush biome
    list_tu_poly[["usstate"]] <- suppressWarnings(
      # suppress:
      #   attribute variables are assumed to be spatially constant
      #   throughout all geometries
      readRDS(fnames_spatialunits[["usstate"]]) |>
        sf::st_transform(crs = 6350) |>
        sf::st_intersection(list_tu_poly[["sagebiome"]]) |>
        sf::st_make_valid()
    )
  }


  #--- USA Federal Lands (ESRI)
  if (list_spatialunits[["USAFederalLands_ESRI"]]) {

    stop("Switch to 'USAFederalLands_SMA'")

    fname_USAFederalLands_sb <- file.path(
      dir_data_tu,
      "20220816_USAFederalLands_ESRI.gpkg"
    )

    if (file.exists(fname_USAFederalLands_sb)) {
      list_tu_poly[["USAFederalLands_ESRI"]] <- sf::st_read(fname_USAFederalLands_sb)

    } else {

      # Subset to sagebrush biome and simplify
      tmp_fl <- sf::st_read(fnames_spatialunits[["USAFederalLands_ESRI"]]) |>
        sf::st_transform(crs = 6350) |>
        sf::st_make_valid() |>
        sf::st_intersection(y = list_tu_poly[["sagebiome"]]) |>
        sf::st_make_valid() |>
        sf::st_combine() |>
        sf::st_union() |>
        sf::st_make_valid() |>
        sf::st_as_sf() # convert sfc to sf object

      sf::st_geometry(tmp_fl) <- "geometry" # rename geometry column
      tmp_fl[, "Status"] <- "Federal Land"

      # Add non-federal land within sagebrush biome
      non_fl_aea_in_sb <-
        sf::st_difference(
          list_tu_poly[["sagebiome"]],
          tmp_fl
        ) |>
        sf::st_make_valid()

      non_fl_aea_in_sb[, "Status"] <- "Non-Federal Land"

      tmp_vars <- c("Status", "geometry")
      list_tu_poly[["USAFederalLands_ESRI"]] <- rbind(
        tmp_fl[, tmp_vars],
        non_fl_aea_in_sb[, tmp_vars]
      )
      sf::st_write(
        list_tu_poly[["USAFederalLands_ESRI"]],
        dsn = fname_USAFederalLands_sb,
        driver = "GPKG"
      )
    }
  }


  #--- USA Federal Lands (SMA)
  if (list_spatialunits[["USAFederalLands_SMA"]]) {

    fname_USAFederalLands_sb <- file.path(
      dir_res,
      "20220817_USAFederalLands_SMA.gpkg"
    )

    if (file.exists(fname_USAFederalLands_sb)) {
      list_tu_poly[["USAFederalLands_SMA"]] <- sf::st_read(fname_USAFederalLands_sb)

    } else {

      poly_federallands <- sf::st_read(
        fnames_spatialunits[["USAFederalLands_SMA"]],
        layer = "SurfaceManagementAgency"
      )

      # Subset to sagebrush biome and simplify
      tmp_fl <-
        sf::st_cast(poly_federallands[, 0], "MULTIPOLYGON") |> # convert MULTISURFACE" to "MULTIPOLYGON"
        sf::st_make_valid() |>
        sf::st_transform(crs = 6350) |>
        sf::st_make_valid() |>
        sf::st_intersection(y = list_tu_poly[["sagebiome"]]) |>
        sf::st_make_valid() |>
        sf::st_combine() |>
        sf::st_union() |>
        sf::st_make_valid() |>
        sf::st_as_sf() # convert sfc to sf object


      sf::st_geometry(tmp_fl) <- "geometry" # rename geometry column
      tmp_fl[, "Status"] <- "Federal Land"

      # Add non-federal land within sagebrush biome
      non_fl_aea_in_sb <-
        sf::st_difference(list_tu_poly[["sagebiome"]], tmp_fl) |>
        sf::st_make_valid()

      non_fl_aea_in_sb[, "Status"] <- "Non-Federal Land"

      tmp_vars <- c("Status", "geometry")
      list_tu_poly[["USAFederalLands_SMA"]] <- rbind(
        tmp_fl[, tmp_vars],
        non_fl_aea_in_sb[, tmp_vars]
      )

      sf::st_write(
        list_tu_poly[["USAFederalLands_SMA"]],
        dsn = fname_USAFederalLands_sb,
        driver = "GPKG"
      )
    }
  }


  #--- WAFWA's Sagebrush Conservation Design
  if (list_spatialunits[["SCD1SEI2020"]]) {
    # data are raster and not polygon -> process later
  }


  #--- ** Intersect tabulation unit polygons ------
  NA_L3NAME <- name <- Status <- km2 <- NULL

  list_tabulations <- list(
    cbind(list_tu_poly[["sagebiome"]][, 0], ecotype = "Sagebrush"),
    dplyr::rename(
      list_tu_poly[["ecoregionL3"]][, "NA_L3NAME"],
      ecoregionL3 = NA_L3NAME
    ),
    dplyr::rename(
      list_tu_poly[["usstate"]][, "name"],
      usstate = name
    )
  )

  if (list_spatialunits[["USAFederalLands_ESRI"]]) {
    list_tabulations[[length(list_tabulations) + 1L]] <- dplyr::rename(
      list_tu_poly[["USAFederalLands_ESRI"]][, "Status"],
      federallandesri = Status
    )
  }

  if (list_spatialunits[["USAFederalLands_SMA"]]) {
    list_tabulations[[length(list_tabulations) + 1L]] <- dplyr::rename(
      list_tu_poly[["USAFederalLands_SMA"]][, "Status"],
      federallandsma = Status
    )
  }



  tmp <- list_tabulations[[1L]]

  for (k1 in seq_along(list_tabulations)[-1]) {
    tmp <- sf::st_intersection(
      tmp,
      if (sf::st_crs(tmp) == sf::st_crs(list_tabulations[[k1]])) {
        list_tabulations[[k1]]
      } else {
        sf::st_transform(list_tabulations[[k1]], crs = sf::st_crs(tmp))
      },
      model = "closed" # closed: DE-9IM compliant behaviour of polygons
    ) |>
      sf::st_make_valid()
  }

  tabulation_units_poly <- tmp

  tabulation_units_poly[, "area_km2"] <- as.numeric(units::set_units(
    sf::st_area(tabulation_units_poly),
    km2
  ))

  dtag_new <- paste(
    colnames(tabulation_units_poly)[seq_along(list_tabulations)],
    collapse = "-"
  )

  stopifnot(tu_dtag_poly == dtag_new)

  dir.create(dirname(fname_tu), recursive = TRUE, showWarnings = FALSE)
  sf::st_write(tabulation_units_poly, dsn = fname_tu, driver = "GPKG")
}


#--- ** List/count intersections ------
tu_cols_poly <- setdiff(
  colnames(tabulation_units_poly),
  c("geom", "geometry", "area_km2")
)
stopifnot(
  tu_cols_poly == strsplit(tu_dtag_poly, split = "-", fixed = TRUE)[[1]]
)


#--- ** Add SCD1SEI2020 or SCD1SEI2020vs2001 to intersection table ------
tmp <- as.data.frame(tabulation_units_poly[, tu_cols_poly, drop = TRUE])

tabulation_units_df_all <- if (list_spatialunits[["SCD1SEI2020vs2001"]]) {
  ids1 <- rep(seq_len(nrow(tmp)), each = nrow(cats_sei2020vs2001))
  ids2 <- rep(seq_len(nrow(cats_sei2020vs2001)), times = nrow(tmp))

  tmp2 <- tmp[ids1, , drop = FALSE]

  if (list_spatialunits[["SCD1SEI2020"]]) {
    tmp2 <- data.frame(
      tmp2,
      SCD1SEI2020 = cats_sei2020vs2001[ids2, "to", drop = TRUE],
      stringsAsFactors = FALSE
    )
  }

  data.frame(
    tmp2,
    SEI20vs01 = cats_sei2020vs2001[ids2, "levels", drop = TRUE],
    stringsAsFactors = FALSE
  )

} else if (
  list_spatialunits[["SCD1SEI2020"]] &&
    !list_spatialunits[["SCD1SEI2020vs2001"]]
) {
  ids1 <- rep(seq_len(nrow(tmp)), each = nrow(cats_sei2020))
  ids2 <- rep(seq_len(nrow(cats_sei2020)), times = nrow(tmp))

  data.frame(
    tmp[ids1, , drop = FALSE],
    SCD1SEI2020 = cats_sei2020[ids2, "sei", drop = TRUE],
    stringsAsFactors = FALSE
  )

} else {
  tmp
}

tu_cols_all <- colnames(tabulation_units_df_all)
tu_dtag_all <- paste0(tu_cols_all, collapse = "-")




#------ . ------
#------ LOOP OVER SPATIAL MASKS ------
for (km in seq_along(mask_sets)) {
  #--- Import external data (part 3) ------

  fnames_gsu_template <- newRR3::import_gsu_template(
    dir_dataraw = dir_dataraw,
    mask_set = mask_sets[[km]],
    version_inputs = meta[["version_inputs"]]
  )


  #--- Load templates of GeoTIFF ------
  gsu_sb_terra <- terra::rast(
    grep(".tif$", fnames_gsu_template, value = TRUE)
  )


  #------ L1: SUID-LOOKUP TABLES ------
  #------ Create suid-lookup tables by cross-tabulation units ------
  list_ctus_gsu <- list()

  tag_dir_tab_x_mask <- paste0("TabulationUnits_", mask_sets[[km]])

  dir_out_ctu <- file.path(
    dir_data_tu,
    tag_dir_tab_x_mask,
    paste0(tag_dir_tab_x_mask, "__", tu_dtag_all)
  )
  dir.create(dir_out_ctu, recursive = TRUE, showWarnings = FALSE)


  #--- * Tabulate gsu: loop over tabulation units ------
  fname_ctu <- file.path(
    dirname(dir_out_ctu),
    paste0(basename(dir_out_ctu), ".rds")
  )

  if (file.exists(fname_ctu)) {
    list_ctus_gsu[[tu_dtag_all]] <- readRDS(fname_ctu)

  } else {
    #--- List all tabulation units combinations
    tag_ctus_all <- unname(apply(
      tabulation_units_df_all,
      MARGIN = 1,
      FUN = newRR3::create_tu_tag
    ))


    #--- Files for each tabulation unit combination
    fname_ctus_all <- file.path(
      dir_out_ctu,
      paste0("Tabulation__", tag_ctus_all, ".rds")
    )

    if (all(file.exists(fname_ctus_all))) {
      list_ctus_gsu[[tu_dtag_all]] <- lapply(fname_ctus_all, readRDS)

    } else {
      message(
        Sys.time(),
        " creating tabulation unit intersections",
        " across ", shQuote(mask_sets[[km]]), " extent."
      )

      if (list_spatialunits[["SCD1SEI2020"]]) {
        sei2020_terra <- terra::rast(fnames_spatialunits[["SCD1SEI2020"]])
      }

      if (list_spatialunits[["SCD1SEI2020vs2001"]]) {
        sei2001_terra <- terra::rast(fnames_spatialunits[["SCD1SEI2001"]])
      }

      # Index to polygons
      ktag_ctus_poly <- match(
        unname(apply(
          tabulation_units_df_all[, tu_cols_poly, drop = FALSE],
          MARGIN = 1,
          FUN = newRR3::create_tu_tag
        )),
        unname(apply(
          tabulation_units_poly[, tu_cols_poly, drop = TRUE],
          MARGIN = 1,
          FUN = newRR3::create_tu_tag
        ))
      )


      #--- Loop over all tabulation unit combinations
      list_ctus_gsu[[tu_dtag_all]] <- list()

      pb <- utils::txtProgressBar(max = length(tag_ctus_all), style = 3)

      for (k1 in seq_along(tag_ctus_all)) {
        # kp=ktag_ctus_poly[k1] is index of tabulation_units_poly
        # k1 is index of tag_ctus_all, fname_ctus_all, tabulation_units_df_all
        # -- k1 and kp are the same if !list_spatialunits[["SCD1SEI2020"]] and !list_spatialunits[["SCD1SEI2020vs2001"]]

        if (file.exists(fname_ctus_all[k1])) {
          list_ctus_gsu[[tu_dtag_all]][[k1]] <- readRDS(fname_ctus_all[k1])

        } else {
          #--- * List suid values within polygon ------
          polyk1 <- tabulation_units_poly[ktag_ctus_poly[k1], ]

          if (list_spatialunits[["SCD1SEI2020"]] || list_spatialunits[["SCD1SEI2020vs2001"]]) {

            #--- ** Intersect SU-grid with SCD1SEI2020 ------
            if (list_spatialunits[["SCD1SEI2020vs2001"]]) {
              ksei20vs01 <- which(
                cats_sei2020vs2001[, "levels", drop = TRUE] == tabulation_units_df_all[k1, "SEI20vs01"]
              )
              ksei2020 <- which(
                cats_sei2020[, "sei", drop = TRUE] == cats_sei2020vs2001[ksei20vs01, "to", drop = TRUE]
              )
            } else {
              ksei2020 <- which(
                cats_sei2020[, "sei", drop = TRUE] == tabulation_units_df_all[k1, "SCD1SEI2020"]
              )
            }

            # Mask out all SCD1SEI2020 areas but current (ksei2020)
            tmp_grid <- terra::crop(gsu_sb_terra, polyk1)
            tmp_grid <- terra::mask(
              tmp_grid,
              mask = terra::resample(
                terra::crop(sei2020_terra, polyk1),
                tmp_grid
              ),
              maskvalue = cats_sei2020[-ksei2020, "Value", drop = TRUE]
            )

            if (list_spatialunits[["SCD1SEI2020vs2001"]]) {
              #--- ** Intersect SU-grid with SEI2001 ------
              ksei2001 <- which(
                cats_sei2001[, "sei", drop = TRUE] == cats_sei2020vs2001[ksei20vs01, "from", drop = TRUE]
              )

              # Mask out all SEI2001 areas but current (ksei2001)
              tmp_grid <- terra::mask(
                tmp_grid,
                mask = terra::resample(
                  terra::crop(sei2001_terra, polyk1),
                  tmp_grid
                ),
                maskvalue = cats_sei2001[-ksei2001, "Value", drop = TRUE]
              )
            }

          } else {
            tmp_grid <- gsu_sb_terra
          }

          tmp <- terra::extract(
            tmp_grid,
            y = polyk1,
            fun = NULL,
            method = "simple",
            exact = FALSE,
            ID = FALSE
          )

          tmps <- tmp[, names(gsu_sb_terra), drop = TRUE]

          # convert NaN to NA
          tmps[is.nan(tmps)] <- NA_real_

          tmp2 <- table(tmps, useNA = "always")

          list_ctus_gsu[[tu_dtag_all]][[k1]] <- list(
            dft = data.frame(
              suid = as.integer(names(tmp2)),
              count = as.vector(tmp2),
              stringsAsFactors = FALSE
            ),
            ctus = tag_ctus_all[k1],
            ctus_orig = unlist(tabulation_units_df_all[k1, , drop = TRUE])
          )

          saveRDS(
            list_ctus_gsu[[tu_dtag_all]][[k1]],
            file = fname_ctus_all[k1]
          )
        }

        utils::setTxtProgressBar(pb, k1)
      }

      close(pb)
    }

    saveRDS(list_ctus_gsu[[tu_dtag_all]], file = fname_ctu)
  }


  #------ Create suid-lookup tables by tabulation units ------
  # Add up suid-lookup tables across suitable cross-tabulation units

  list_tus <- stats::setNames(as.list(tu_cols_all), tu_cols_all)

  if (list_spatialunits_combinations[["ecoregionL3_and_SCD1SEI2020"]]) {
    list_tus[["ecoregionL3-SCD1SEI2020"]] <- c("ecoregionL3", "SCD1SEI2020")
  }

  if (list_spatialunits_combinations[["ecoregionL3_and_SEI20vs01"]]) {
    list_tus[["ecoregionL3-SCD1SEI20vs01"]] <- c("ecoregionL3", "SCD1SEI20vs01")
  }


  #--- * Loop over elements of each tabulation type ------
  for (k1 in seq_along(list_tus)) {
    dir_out_tue <- file.path(
      dir_data_tu,
      tag_dir_tab_x_mask,
      paste0(tag_dir_tab_x_mask, "__", names(list_tus)[k1])
    )
    dir.create(dir_out_tue, recursive = TRUE, showWarnings = FALSE)

    fname_ctue <- file.path(
      dirname(dir_out_tue),
      paste0(basename(dir_out_tue), ".rds")
    )

    if (file.exists(fname_ctue)) {
      list_ctus_gsu[[names(list_tus)[k1]]] <- readRDS(fname_ctue)

    } else {

      # Unique units of element
      list_elems_orig <- unique(
        tabulation_units_df_all[, list_tus[[k1]], drop = FALSE]
      )
      list_elems_tag <- unname(apply(
        list_elems_orig,
        MARGIN = 1,
        newRR3::create_tu_tag
      ))


      fname_tabulations <- file.path(
        dir_out_tue,
        paste0("Tabulation__", list_elems_tag, ".rds")
      )

      if (all(file.exists(fname_tabulations))) {
        list_ctus_gsu[[names(list_tus)[k1]]] <- lapply(
          fname_tabulations,
          readRDS
        )

      } else {
        list_ctus_gsu[[names(list_tus)[k1]]] <- list()

        for (k2 in seq_along(list_elems_tag)) {
          if (file.exists(fname_tabulations[k2])) {
            list_ctus_gsu[[names(list_tus)[k1]]][[k2]] <- readRDS(
              fname_tabulations[k2]
            )

          } else {
            #--- * Load suid-lookup tables from cross-tabulation units for element ------
            tmp_pattern <- if (
              isTRUE(grepl("_", list_elems_tag[k2], fixed = TRUE))
            ) {
              # Combinations (that may occur apart in the text string), e.g.
              # ecoregion-SEI (among ecotype_ecoregion_state_SEI)
              gsub("_", "[[:alnum:]_]+", list_elems_tag[k2])
            } else {
              list_elems_tag[k2]
            }

            ftmps <- list.files(
              path = dir_out_ctu,
              pattern = tmp_pattern,
              full.names = TRUE
            )

            tmp <- lapply(ftmps, readRDS)

            #--- * Merge suid-lookup tables for element ------
            dft <- tmp[[1]][["dft"]]

            # deal with "NAs produced by integer overflow"
            # -> capture warnings as errors: if so, convert int to numeric
            prev_warn <- getOption("warn")
            options(warn = 2)

            for (k3 in seq_along(tmp)[-1]) {
              # Sum up counts of suids that are already in table
              # Note: "NA matches NA and no other value"
              ids <- match(
                dft[, "suid"],
                tmp[[k3]][["dft"]][, "suid"],
                nomatch = 0L
              )
              if (any(ids > 0L)) {
                tmpc <- try(
                  dft[ids > 0L, "count"] + tmp[[k3]][["dft"]][ids, "count"],
                  silent = TRUE
                )

                if (inherits(tmpc, "try-error")) {
                  if (
                    isTRUE(grepl("NAs produced by integer overflow", tmpc))
                  ) {
                    # Convert integers to numeric
                    dft[["count"]] <- as.numeric(dft[["count"]])
                    dft[ids > 0L, "count"] <-
                      dft[ids > 0L, "count"] + tmp[[k3]][["dft"]][ids, "count"]

                  } else {
                    stop(tmpc)
                  }

                } else {
                  dft[ids > 0L, "count"] <- tmpc
                }
              }

              # Add suids (and their counts) if not already in table
              ids_add <- !(tmp[[k3]][["dft"]][, "suid"] %in% dft[, "suid"])
              if (any(ids_add)) {
                dft <- rbind(dft, tmp[[k3]][["dft"]][ids_add, , drop = FALSE])
              }
            }

            options(warn = prev_warn)

            list_ctus_gsu[[names(list_tus)[k1]]][[k2]] <- list(
              dft = dft,
              ctus = list_elems_tag[k2],
              ctus_orig = stats::setNames(
                unlist(list_elems_orig[k2, , drop = TRUE]),
                nm = list_tus[[k1]]
              )
            )

            saveRDS(
              list_ctus_gsu[[names(list_tus)[k1]]][[k2]],
              file = fname_tabulations[k2]
            )
          }
        }
      }

      saveRDS(list_ctus_gsu[[names(list_tus)[k1]]], file = fname_ctue)
    }
  }
}




#--- . ------
#------ SUMMARIZE SPATIAL UNITS ------
fname_aggecoregions <- file.path(
  dir_dataraw_tu,
  "EPAecoregionL3",
  "EcoregionsL3_Simplified.csv"
)

if (!file.exists(fname_aggecoregions)) {
  #--- Define aggregated ecoregions ------
  agged_ecoregions <- list(
    c("Arizona/New Mexico Plateau and Mountains", "AZ/NM Plat/Mts", "Arizona/New Mexico Mountains"),
    c("Arizona/New Mexico Plateau and Mountains", "AZ/NM Plat/Mts", "Arizona/New Mexico Plateau"),
    c("Central Basin and Range", "CBR", "Central Basin and Range"),
    c("Colorado Plateaus", "Colorado Plat", "Colorado Plateaus"),
    c("Columbia Plateau", "Columbia Plat", "Columbia Plateau"),
    c("Northern Basin and Range", "NBR", "Northern Basin and Range"),
    c("Semiarid Prairies", "Semiarid Prairies", "High Plains"),
    c("Semiarid Prairies", "Semiarid Prairies", "Northwestern Glaciated Plains"),
    c("Semiarid Prairies", "Semiarid Prairies", "Northwestern Great Plains"),
    c("Snake River Plain", "SRP", "Snake River Plain"),
    c("Warm Deserts", "Warm Deserts", "Mojave Basin and Range"),
    c("Warm Deserts", "Warm Deserts", "Sonoran Desert"),
    c("Cascades and Blue Mountains", "Blue Mts+Cascades", "Blue Mountains"),
    c("Cascades and Blue Mountains", "Blue Mts+Cascades", "Cascades"),
    c("Cascades and Blue Mountains", "Blue Mts+Cascades", "Eastern Cascades Slopes and Foothills"),
    c("Western Cordillera", "W Cordillera", "Canadian Rockies"),
    c("Western Cordillera", "W Cordillera", "Columbia Mountains/Northern Rockies"),
    c("Western Cordillera", "W Cordillera", "Idaho Batholith"),
    c("Western Cordillera", "W Cordillera", "Klamath Mountains"),
    c("Western Cordillera", "W Cordillera", "Middle Rockies"),
    c("Western Cordillera", "W Cordillera", "North Cascades"),
    c("Western Cordillera", "W Cordillera", "Sierra Nevada"),
    c("Western Cordillera", "W Cordillera", "Southern Rockies"),
    c("Western Cordillera", "W Cordillera", "Wasatch and Uinta Mountains"),
    c("Wyoming Basin", "Wyoming Basin", "Wyoming Basin")
  ) |>
    do.call(rbind, args = _)

  colnames(agged_ecoregions) <- c("NA_AGGNAME", "NA_AGGBREV", "NA_L3NAME")

  write.csv(
    agged_ecoregions,
    file = fname_aggecoregions,
    row.names = FALSE
  )

  if (FALSE) {
    tmp_epa <- list_tu_poly[["ecoregionL3"]]
    tmp_epa <- merge(
      tmp_epa,
      agged_ecoregions,
      by = "NA_L3NAME",
      all.x = TRUE
    )

    plot(tmp_epa[, "NA_AGGBREV"])
  }
}

print(summary(warnings()))

#------. ------
#------. ------
