#----- . ------
#------ PROPORTION OF AREA ------
#
# Tables: spatial summaries
#
#    * Output: `"displayitems/Tables-SpatialUnitTabulations/"`
#

#----- . ------



#----- SET UP ------
stopifnot(requireNamespace("newRR3"))
options(warn = 2L) # convert warnings to errors

#--- Settings ------
req_meta_version <- "3.2.0"

vtag_input <- c("rr", "mirrp", "preds", "novelty")

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


dir_outitems <- file.path(dir_prj, "displayitems")
dir.create(dir_outitems, recursive = TRUE, showWarnings = FALSE)

tag_outitems_ttu <- "Tables-SpatialUnitTabulations"



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

  #--- Paths (part 2) ------
  dir_data_rat <- file.path(
    dir_res,
    paste0("newRR3_RATs-", mask_sets[[km]])
  )
  stopifnot(file.exists(dir_data_rat))




  #--- Table functions ------

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


  prepare_table_from_files <- function(
    tags,
    patterns,
    simslices,
    extent,
    variables,
    path1,
    path2,
    mask
  ) {

    res <- lapply(
      seq_along(tags),
      function(kl) {

        ftmp <- newRR3::locate_tabulated_files(
          filenames = find_tabulation_files(
            pattern = patterns[[kl]],
            path1 = path1,
            path2 = path2,
            mask = mask
          ),
          simslices = simslices,
          extent = extent,
          variables = variables
        )

        xtmp <- newRR3::prepare_df_for_stackedbars(
          filenames = ftmp,
          simslices = simslices,
          extent = extent,
          variables = variables
        )

        cbind(
          xtmp,
          acrmod_level = tags[[kl]]
        )
      }
    )

    do.call(rbind, res)
  }

  # Categories of x as columns
  format_manuscript_table_cats <- function(
    x,
    var,
    cats,
    tags123,
    timevar = "Response",
    idvar = c("Var", "SimSlice", "SpatialUnit"),
    lvls_robustness = NULL
  ) {
    req_vars <- c(idvar, timevar, var, "acrmod_level")

    stopifnot(
      req_vars %in% colnames(x)
    )

    # Convert to data.frame
    # reshape does weird things to column names if a tibble
    x <- as.data.frame(x)[, req_vars, drop = FALSE]

    # Widen by RR categories and acrmod-levels
    # -> RSL-L (low-med-high) | RSL-ML (low-med-high) | ...
    timevar2 <- paste0(timevar, "2")
    x[[timevar2]] <- paste0(
      gsub(".", "+", as.character(x[[timevar]]), fixed = TRUE),
      "-",
      x[["acrmod_level"]]
    )

    xtmp <- suppressWarnings(
      # suppress: some constant variables (values) are really varying
      reshape(
        x,
        direction = "wide",
        idvar = idvar,
        timevar = timevar2,
        v.names = var,
        drop = c(timevar, "acrmod_level")
      )
    )

    if (length(tags123) == 3L) {
      res <- xtmp[, idvar, drop = FALSE]

      for (kl in seq_along(cats)) {
        tmpr <- cats[[kl]]

        tmpvs <- paste0(var, ".", tmpr, "-", tags123)

        if (all(tmpvs %in% colnames(xtmp))) {
          res[[tmpr]] <- ifelse(
            apply(
              cbind(xtmp[[tmpvs[[1L]]]], xtmp[[tmpvs[[3L]]]]),
              MARGIN = 1L,
              anyNA
            ),
            xtmp[[tmpvs[[2L]]]],
            paste0(
              xtmp[[tmpvs[[2L]]]],
              " (",  xtmp[[tmpvs[[1L]]]], "-", xtmp[[tmpvs[[3L]]]], ")"
            )
          )
        }
      }
    } else {
      res <- xtmp
    }

    ids <- do.call(order, unname(res[idvar]))
    res <- res[ids, , drop = FALSE]

    if (!is.null(lvls_robustness) && "robustness" %in% idvar) {
      stopifnot(identical(c("overall", "robust", "nonrobust"), lvls_robustness))
      idvarwor <- setdiff(idvar, "robustness")

      tmpr <- lapply(
        lvls_robustness,
        function(r) res[res[["robustness"]] %in% r, , drop = TRUE]
      )

      # Check that "idvar" columns are identically ordered
      stopifnot(
        vapply(
          tmpr,
          function(x) {
            isTRUE(
              all.equal(
                x[, idvarwor],
                tmpr[[1L]][, idvarwor],
                check.attributes = FALSE
              )
            )
          },
          FUN.VALUE = NA
        )
      )

      tmpr_rnr <- tmpr[[2L]]
      tmpr_rnr[["robustness"]] <- "robust | other"

      tmpc <- setdiff(colnames(tmpr_rnr), idvar)
      for (kl in seq_along(tmpc)) {
        tmpr_rnr[[tmpc[[kl]]]] <- paste(
          tmpr[[2L]][[tmpc[[kl]]]],
          tmpr[[3L]][[tmpc[[kl]]]],
          sep = " | "
        )
      }

      res <- rbind(tmpr[[1L]], tmpr_rnr)
      ids <- do.call(order, unname(res[idvar]))
      res <- res[ids, , drop = FALSE]
    }

    res
  }

  # pixel count (30 x 30 m2) -> 1e3 km2
  pixelcount_to_1e3km2 <- function(x) {
    x * 30^2 / (1e3)^2 / 1e3
  }



  reorder_by_var <- function(x, col = "Var", target = x[[col]]) {
    ids <- lapply(target, function(ttmp) which(x[[col]] == ttmp))
    x[unlist(ids), , drop = FALSE]
  }



  #---- All vtags ------

  #----- . ------
  #--- RR ------
  vtagk <- "rr"
  if (vtagk == "rr") {


    #--- * Table RR-values ------
    fname_table_rr <- file.path(
      dir_outitems,
      tag_outitems_ttu,
      "Appendix",
      paste0(
        "TableSX-1_RR-values_Overall_by_TimeRCPs_1000km2.csv"
      )
    )

    if (!file.exists(fname_table_rr)) {

      list_extents <- "ecotype"
      tag_vars <- c(RSL = "Resilience_response", RST = "Resistance_response")
      tag_ambient <- meta[["simexps"]][["tag_scen1"]]


      #--- Prepare ambient files
      xsut2la <- prepare_table_from_files(
        tags = "med", # even though "med" doesn't exist for ambient
        patterns = paste0(
          meta[["varsets"]][["tags"]][[vtagk]],
          "__(.+)__value-sim_"
        ),
        simslices = meta[["simexps"]][["tag_scen1"]],
        extent = list_extents[[1L]],
        variables = tag_vars,
        path1 = dir_data_rat,
        path2 = dir_res_tu,
        mask = data_mask_sets[[km]]
      )

      #--- Load projected files
      xsut2lf <- prepare_table_from_files(
        tags = meta[["acrmod"]][["signal"]][["tag"]],
        patterns = paste0(
          meta[["varsets"]][["tags"]][[vtagk]],
          "__(.+)__value-acrmod-",
          meta[["acrmod"]][["signal"]][["tag"]]
        ),
        simslices = unlist(meta[["simexps"]][["tag_acrmod"]]),
        extent = list_extents[[1L]],
        variables = tag_vars,
        path1 = dir_data_rat,
        path2 = dir_res_tu,
        mask = data_mask_sets[[km]]
      )

      xsut2 <- rbind(xsut2la, xsut2lf)


      # Clean up stuff
      xsut2[["SimSlice"]] <- gsub("\n", "_", xsut2[["SimSlice"]], fixed = TRUE)

      # Calculate tabulated response
      xsut2[["values_1e3km2"]] <- round(
        pixelcount_to_1e3km2(xsut2[["values"]])
      )

      # Format
      xtable <- format_manuscript_table_cats(
        x = xsut2,
        var = "values_1e3km2",
        cats = meta[["varsets"]][["rr"]][["levels"]],
        tags123 = meta[["acrmod"]][["signal"]][["tag"]]
      )

      dir.create(dirname(fname_table_rr), recursive = TRUE, showWarnings = FALSE)
      write.csv(xtable, file = fname_table_rr, row.names = FALSE)
    }



    #--- * Table RR-net (robust) delta ------

    fname_table_rr <- file.path(
      dir_outitems,
      tag_outitems_ttu,
      "Appendix",
      paste0(
        "TableSX-2_RR-NetDeltaByHistoricalCategory_Overall_by_TimeRCPs_1000km2.csv"
      )
    )

    if (!file.exists(fname_table_rr)) {

      list_extents <- "ecotype"
      lvls_robustness <- c(
        "overall",
        rev(meta[["acrmod"]][["robust"]][["levels"]])
      )
      list_extents <- "ecotype"
      tag_vars <- c(RSL = "Resilience_response", RST = "Resistance_response")


      #--- Load deltas
      xsut2d <- prepare_table_from_files(
        tags = meta[["acrmod"]][["signal"]][["tag"]],
        patterns = paste0(
          meta[["varsets"]][["tags"]][[vtagk]],
          "__(.+)__delta-acrmod-",
          meta[["acrmod"]][["signal"]][["tag"]]
        ),
        simslices = unlist(meta[["simexps"]][["tag_acrmod"]])[-1L],
        extent = list_extents[[1L]],
        variables = tag_vars,
        path1 = dir_data_rat,
        path2 = dir_res_tu,
        mask = data_mask_sets[[km]]
      )

      xsut2d[["robustness"]] <- lvls_robustness[[1L]]


      #--- Load robust deltas (only applicable to median)
      xsut2rd <- prepare_table_from_files(
        tags = meta[["acrmod"]][["signal"]][["tag"]][[2L]],
        patterns = paste0(
          meta[["varsets"]][["tags"]][[vtagk]],
          "__(.+)__robust090delta-acrmod-",
          meta[["acrmod"]][["signal"]][["tag"]][[2L]]
        ),
        simslices = unlist(meta[["simexps"]][["tag_acrmod"]])[-1L],
        extent = list_extents[[1L]],
        variables = tag_vars,
        path1 = dir_data_rat,
        path2 = dir_res_tu,
        mask = data_mask_sets[[km]]
      )

      tmp <- strsplit(
        split = "_",
        x = gsub("H.MH", "H+MH", as.character(xsut2rd[["Response"]]), fixed = TRUE),
        fixed = TRUE
      )

      xsut2rd[["robustness"]] <- vapply(
        tmp,
        `[`, j = 1L,
        FUN.VALUE = NA_character_
      )

      xsut2rd[["Response"]] <- factor(
        vapply(
          tmp,
          function(x) paste(x[-1L], collapse = "_"),
          FUN.VALUE = NA_character_
        ),
        levels = meta[["delta"]][["rr"]][["full"]][["levels"]]
      )


      #--- Combine "all" deltas and robust/nonrobust deltas
      xsut2 <- rbind(xsut2d, xsut2rd)

      xsut2[["robustness"]] <- factor(
        xsut2[["robustness"]],
        levels = lvls_robustness
      )


      #--- ** Identify increases/decreases/stable per future RR category ------

      # per category: decrease if from category -- increase if to category
      tmp <- strsplit(
        as.character(xsut2[["Response"]]),
        split = "_from_",
        fixed = TRUE
      )
      stopifnot(lengths(tmp) == 2L)

      xsut2[["increased"]] <- vapply(
        tmp,
        `[`,
        j = 1L,
        FUN.VALUE = NA_character_
      )

      xsut2[["increased"]] <- gsub(
        pattern = "H.MH",
        replacement = "H+MH",
        x = xsut2[["increased"]],
        fixed = TRUE
      )

      xsut2[["decreased"]] <- vapply(
        tmp,
        `[`,
        j = 2L,
        FUN.VALUE = NA_character_
      )

      xsut2[["decreased"]] <- gsub(
        pattern = "H.MH",
        replacement = "H+MH",
        x = xsut2[["decreased"]],
        fixed = TRUE
      )


      #--- ** Summarize net change ------
      req_var <- c("SpatialUnit", "Var", "SimSlice", "robustness", "acrmod_level")
      tmp <- by(
        xsut2,
        INDICES = as.list(
          xsut2[req_var]
        ),
        FUN = function(x) {
          res <- x[1L, req_var, drop = FALSE]
          rownames(res) <- NULL

          res <- data.frame(
            res,
            ResponseR = meta[["varsets"]][["rr"]][["levels"]],
            net = 0,
            stringsAsFactors = FALSE
          )

          for (k in seq_along(meta[["varsets"]][["rr"]][["levels"]])) {
            rrlvl <- meta[["varsets"]][["rr"]][["levels"]][[k]]
            is_incr <- x[["increased"]] %in% rrlvl
            is_decr <- x[["decreased"]] %in% rrlvl

            res[["net"]][[k]] <-
              sum(x[["values"]][is_incr]) - sum(x[["values"]][is_decr])
          }

          res
        },
        simplify = FALSE
      )

      xsut3 <- do.call(rbind, tmp)


      # Clean up stuff
      xsut3[["SimSlice"]] <- gsub("\n", "_", xsut3[["SimSlice"]], fixed = TRUE)

      # Calculate tabulated response
      xsut3[["net_1e3km2"]] <- round(
        pixelcount_to_1e3km2(xsut3[["net"]])
      )

      # Format
      xtable <- format_manuscript_table_cats(
        x = xsut3,
        var = "net_1e3km2",
        cats = meta[["varsets"]][["rr"]][["levels"]],
        tags123 = meta[["acrmod"]][["signal"]][["tag"]],
        timevar = "ResponseR",
        idvar = c("Var", "SimSlice", "SpatialUnit", "robustness"),
        lvls_robustness = lvls_robustness
      )

      dir.create(dirname(fname_table_rr), recursive = TRUE, showWarnings = FALSE)
      write.csv(xtable, file = fname_table_rr, row.names = FALSE)
    }
    #------ * -----


    #--- * Table RR simpledeltas ------
    fname_tables_textvalues <- file.path(
      dir_outitems,
      tag_outitems_ttu,
      "Appendix",
      paste0(
        "TableST-1_RR-deltas_Overall_by_TimeRCPs_",
        c("1000km2", "percent"),
        ".csv"
      )
    )

    if (!all(file.exists(fname_tables_textvalues))) {
      list_extents <- "ecotype"
      tag_vars <- c(RSL = "Resilience_response", RST = "Resistance_response")


      #--- Load simpledeltas files
      xsut2 <- prepare_table_from_files(
        tags = meta[["acrmod"]][["signal"]][["tag"]],
        patterns = paste0(
          "__simpledelta-acrmod-",
          meta[["acrmod"]][["signal"]][["tag"]],
          "__acrgeo-", list_extents[[1L]], "__"
        ),
        simslices = unlist(meta[["simexps"]][["tag_acrmod"]])[-1L],
        extent = list_extents[[1L]],
        variables = tag_vars,
        path1 = dir_data_rat,
        path2 = dir_res_tu,
        mask = data_mask_sets[[km]]
      )

      # Clean up stuff
      xsut2[["SimSlice"]] <- gsub("\n", "_", xsut2[["SimSlice"]], fixed = TRUE)


      # Calculate tabulated response
      xsut2[["values_1e3km2"]] <- round(
        pixelcount_to_1e3km2(xsut2[["values"]])
      )

      # Format
      xtable <- format_manuscript_table_cats(
        x = as.data.frame(xsut2),
        var = "values_1e3km2",
        cats = meta[["delta"]][["basic"]][["full"]][["levels"]],
        tags123 = meta[["acrmod"]][["signal"]][["tag"]]
      )

      dir.create(dirname(fname_tables_textvalues[[1L]]), recursive = TRUE, showWarnings = FALSE)
      write.csv(xtable, file = fname_tables_textvalues[[1L]], row.names = FALSE)


      #--- ** Calculate tabulated response ------
      xsut3 <-
        dplyr::group_by(xsut2, Var, SimSlice, SpatialUnit, acrmod_level) |>
        dplyr::mutate(
          freq = round(100 * values / sum(values))
        )

      # Format
      xtable <- format_manuscript_table_cats(
        x = as.data.frame(xsut3),
        var = "freq",
        cats = meta[["delta"]][["basic"]][["full"]][["levels"]],
        tags123 = meta[["acrmod"]][["signal"]][["tag"]]
      )

      dir.create(dirname(fname_tables_textvalues[[2L]]), recursive = TRUE, showWarnings = FALSE)
      write.csv(xtable, file = fname_tables_textvalues[[2L]], row.names = FALSE)

      #--- ** in-text values ------
      # 24% (21-30%) of the area decreased to a lower category for resilience
      # and 34% (28-41%) for resistance
    }
    #------ * -----


    #--- * Table RR (robust) delta by historical reference ------
    fname_tables_textvalues <- file.path(
      dir_outitems,
      tag_outitems_ttu,
      "Appendix",
      paste0(
        "TableST-2_RR-deltasByHistoricalCategory_Overall_by_TimeRCPs_",
        c("1000km2", "percent"),
        ".csv"
      )
    )

    if (!all(file.exists(fname_tables_textvalues))) {

      list_extents <- "ecotype"
      lvls_robustness <- c(
        "overall",
        rev(meta[["acrmod"]][["robust"]][["levels"]])
      )
      tag_vars <- c(RSL = "Resilience_response", RST = "Resistance_response")


      #--- Load deltas
      xsut2d <- prepare_table_from_files(
        tags = meta[["acrmod"]][["signal"]][["tag"]],
        patterns = paste0(
          meta[["varsets"]][["tags"]][[vtagk]],
          "__(.+)__delta-acrmod-",
          meta[["acrmod"]][["signal"]][["tag"]]
        ),
        simslices = unlist(meta[["simexps"]][["tag_acrmod"]])[-1L],
        extent = list_extents[[1L]],
        variables = tag_vars,
        path1 = dir_data_rat,
        path2 = dir_res_tu,
        mask = data_mask_sets[[km]]
      )

      xsut2d[["robustness"]] <- lvls_robustness[[1L]]


      #--- Load robust deltas (only applicable to median)
      xsut2rd <- prepare_table_from_files(
        tags = meta[["acrmod"]][["signal"]][["tag"]][[2L]],
        patterns = paste0(
          meta[["varsets"]][["tags"]][[vtagk]],
          "__(.+)__robust090delta-acrmod-",
          meta[["acrmod"]][["signal"]][["tag"]][[2L]]
        ),
        simslices = unlist(meta[["simexps"]][["tag_acrmod"]])[-1L],
        extent = list_extents[[1L]],
        variables = tag_vars,
        path1 = dir_data_rat,
        path2 = dir_res_tu,
        mask = data_mask_sets[[km]]
      )

      tmp <- strsplit(
        split = "_",
        x = gsub("H.MH", "H+MH", as.character(xsut2rd[["Response"]]), fixed = TRUE),
        fixed = TRUE
      )

      xsut2rd[["robustness"]] <- vapply(
        tmp,
        `[`, j = 1L,
        FUN.VALUE = NA_character_
      )

      xsut2rd[["Response"]] <- factor(
        vapply(
          tmp,
          function(x) paste(x[-1L], collapse = "_"),
          FUN.VALUE = NA_character_
        ),
        levels = meta[["delta"]][["rr"]][["full"]][["levels"]]
      )


      #--- Combine "all" deltas and robust/nonrobust deltas
      xsut2 <- rbind(xsut2d, xsut2rd)

      xsut2[["robustness"]] <- factor(
        xsut2[["robustness"]],
        levels = lvls_robustness
      )

      xsut2[["Response"]] <- factor(
        gsub("H.MH", "H+MH", as.character(xsut2[["Response"]]), fixed = TRUE),
        levels = meta[["delta"]][["rr"]][["full"]][["levels"]]
      )

      # Clean up stuff
      xsut2[["SimSlice"]] <- gsub("\n", "_", xsut2[["SimSlice"]], fixed = TRUE)


      #--- ** Sort robust deltas by reference category (historical RR) ------
      tmp1 <- newRR3::sort_deltas_by_reference(
        x = xsut2[["Response"]],
        ref_lvls = meta[["varsets"]][["rr"]][["levels"]],
        x_conv = meta[["delta"]][["rr"]][["full"]],
        simple_conv = meta[["delta"]][["basic"]][["full"]],
        is_robust = FALSE
      )

      tmp2 <- expand.grid(
        meta[["delta"]][["basic"]][["full"]][["levels"]],
        meta[["varsets"]][["rr"]][["levels"]]
      )

      xsut2[["ResponseR"]] <- factor(
        paste0(tmp1[["Response"]], "_from_", tmp1[["Reference"]]),
        levels = newRR3::convolve_levels(tmp2[[1L]], tmp2[[2L]])
      )
      xsut2[["RR_hist"]] <- tmp1[["Reference"]]


      #--- ** Calculate total decrease/stable/increase by reference ------
      # e.g., increase_from_L could be derived from M_from_L and ML_from_L
      xsut3 <-
        dplyr::group_by(
          xsut2,
          Var, SimSlice, SpatialUnit, acrmod_level, robustness, ResponseR
        ) |>
        dplyr::summarize(
          total = sum(values)
        )


      # Calculate tabulated response
      xsut3[["total_1e3km2"]] <- round(
        pixelcount_to_1e3km2(xsut3[["total"]])
      )

      # Format
      xtable <- format_manuscript_table_cats(
        x = xsut3,
        var = "total_1e3km2",
        cats = levels(xsut3[["ResponseR"]]),
        tags123 = meta[["acrmod"]][["signal"]][["tag"]],
        timevar = "ResponseR",
        idvar = c("Var", "SimSlice", "SpatialUnit", "robustness")
      )

      dir.create(dirname(fname_tables_textvalues[[1L]]), recursive = TRUE, showWarnings = FALSE)
      write.csv(xtable, file = fname_tables_textvalues[[1L]], row.names = FALSE)


      #--- ** Calculate percentage response ------
      xsut3 <-
        dplyr::group_by(
          xsut3,
          Var, SimSlice, SpatialUnit, acrmod_level, robustness
        ) |>
        dplyr::mutate(
          percent = round(100 * total / sum(total))
        )

      # Format
      xtable <- format_manuscript_table_cats(
        x = xsut3,
        var = "percent",
        cats = levels(xsut3[["ResponseR"]]),
        tags123 = meta[["acrmod"]][["signal"]][["tag"]],
        timevar = "ResponseR",
        idvar = c("Var", "SimSlice", "SpatialUnit", "robustness")
      )

      dir.create(dirname(fname_tables_textvalues[[2L]]), recursive = TRUE, showWarnings = FALSE)
      write.csv(xtable, file = fname_tables_textvalues[[2L]], row.names = FALSE)


      #--- ** in-text values ------

      # Areas in the Low category under historical conditions remained Low
      # under future projected conditions of which 75% [= 362/485]
      # showed a robust signal for resilience and
      # 64% [= 210/326] for resistance

      # 42% (24-66%) [= 84/201 (133/201-50/205)] of the area in the
      # Moderately-Low category under historical conditions
      # decreased (to the Low category) for resilience and
      # 34% (20-51%) [= 119/354 (171/333-73/373)] for resistance

      # The Moderate category was the category with the largest decreases in
      # area under future projected climate with decreases of
      # 37% (33-49%) [= 137/369 (168/342-127/381)] for resilience and
      # 56% (48-71%) [= 185/331 (244/343-143/299)] for resistance

      # Remaining areas in the Moderate category under historical conditions
      # remained in the Moderate category of which
      # 41% [= 95/231] was robust for resilience and
      # 15% [= 21/141] for resistance

      # The High or Moderately-High category was the category with the
      # largest fraction that decreased under future projected climate with
      # decreases of 59% (51-69%) [= 55/93 (50/72-65/128)] for resilience and
      # 61% (51-63%) [= 88/145 (60/95-110/216)] for resistance
    }
    #--- * ------


    #--- * Table RR (cat/cont) robustness ------
    fname_tables_textvalues <- file.path(
      dir_outitems,
      tag_outitems_ttu,
      "Appendix",
      paste0(
        "TableST-3_RR-robustness_Overall_by_TimeRCPs_",
        c("1000km2", "percent"),
        ".csv"
      )
    )

    if (!all(file.exists(fname_tables_textvalues))) {
      list_extents <- "ecotype"
      tag_vars <- list(
        cat = c(RSL = "Resilience_response", RST = "Resistance_response"),
        cont = c(RSL = "Resilience_index", RST = "Resistance_index")
      )

      #--- Load areas of robustness
      xsut2 <- prepare_table_from_files(
        tags = meta[["acrmod"]][["signal"]][["tag"]][[2L]],
        patterns = paste0(
          meta[["varsets"]][["tags"]][[vtagk]],
          "__(.+)__delta-acrmod-robust090__"
        ),
        simslices = unlist(meta[["simexps"]][["tag_acrmod"]])[-1L],
        extent = list_extents[[1L]],
        variables = unlist(tag_vars),
        path1 = dir_data_rat,
        path2 = dir_res_tu,
        mask = data_mask_sets[[km]]
      )

      # Clean up stuff
      xsut2[["SimSlice"]] <- gsub("\n", "_", xsut2[["SimSlice"]], fixed = TRUE)


      #--- ** Calculate tabulated response ------
      xsut2[["values_1e3km2"]] <- round(
        pixelcount_to_1e3km2(xsut2[["values"]])
      )

      xsut3 <- dplyr::group_by(
        xsut2,
        Var, SimSlice, SpatialUnit, acrmod_level
      ) |>
        dplyr::mutate(
          freq = values / sum(values)
        )


      # Format
      xtable <- format_manuscript_table_cats(
        x = xsut3,
        var = "values_1e3km2",
        cats = meta$acrmod$robust[["levels"]],
        timevar = "Response",
        tags123 = meta[["acrmod"]][["signal"]][["tag"]][[2L]]
      )

      dir.create(dirname(fname_tables_textvalues[[1L]]), recursive = TRUE, showWarnings = FALSE)
      write.csv(xtable, file = fname_tables_textvalues[[1L]], row.names = FALSE)


      # Format
      xtable <- format_manuscript_table_cats(
        x = xsut3,
        var = "freq",
        cats = meta$acrmod$robust[["levels"]],
        timevar = "Response",
        tags123 = meta[["acrmod"]][["signal"]][["tag"]][[2L]]
      )

      dir.create(dirname(fname_tables_textvalues[[2L]]), recursive = TRUE, showWarnings = FALSE)
      write.csv(xtable, file = fname_tables_textvalues[[2L]], row.names = FALSE)


      #--- ** in-text values ------
      # Robust signals occurred across the study area for resilience (
      # 48% (= 554 / (610 + 554)) for categories and
      # 43% (= 503 / (660 + 503)) for the continuous index)
      # and for resistance (
      # 35% (= 411 / (753 + 411)) for categories and
      # 53% (= 618 / (545 + 618)) for the continuous index)


      if (FALSE) {
        #--- ** devel & troubleshoot ------
        xfname_agree <- "../results/newRR3_RATs-combined/RCP45_2064-2099-clim_AcrMod_Deltas_RR/RR__RCP45_2064-2099-clim__delta-acrmod-agree.csv"
        xrat_agree <- utils::read.csv(xfname_agree)
        tmpv <- paste0("count_", data_mask_sets[[km]])
        ids <- !is.na(xrat_agree[[tmpv]]) & xrat_agree[[tmpv]] > 0
        xrat_agree <- xrat_agree[ids, , drop = FALSE]


        tag_varsagree_used <- list(
          RSL_cat = "Resilience_response_delta_acrmodagree",
          RST_cat = "Resistance_response_delta_acrmodagree",
          RSL_cont = "Resilience_index_delta_acrmodagree",
          RST_cont = "Resistance_index_delta_acrmodagree"
        )

        get_robust_vs_nonrobust <- function(var) {
          tmp <- c(
            nonrobust = sum(
              (
                newRR3::robustness(
                  x = xrat_agree[, var, drop = TRUE],
                  frq_robust = 0.9
                ) %in% 0) *
                xrat_agree[["count_rangelands"]]
            ),
            robust = sum(
              (newRR3::robustness(
                x = xrat_agree[, var, drop = TRUE],
                frq_robust = 0.9
              ) %in% 1) *
                xrat_agree[["count_rangelands"]]
            )
          )

          c(
            area = round(pixelcount_to_1e3km2(tmp)),
            pct = round(tmp / sum(tmp), 2)
          )
        }

        get_robust_vs_nonrobust("Resilience_response_delta_acrmodagree")
        # area.nonrobust    area.robust  pct.nonrobust     pct.robust
        #         610.00         554.00           0.52           0.48

        get_robust_vs_nonrobust("Resistance_response_delta_acrmodagree")
        # area.nonrobust    area.robust  pct.nonrobust     pct.robust
        #         753.00         411.00           0.65           0.35

        get_robust_vs_nonrobust("Resilience_index_delta_acrmodagree")
        # area.nonrobust    area.robust  pct.nonrobust     pct.robust
        #         660.00         503.00           0.57           0.43

        get_robust_vs_nonrobust("Resistance_index_delta_acrmodagree")
        # area.nonrobust    area.robust  pct.nonrobust     pct.robust
        #         545.00         618.00           0.47           0.53

      }
    }
    #--- * ------



    #--- * Table SEI x RR (robust) deltas ------
    fname_tables_textvalues <- file.path(
      dir_outitems,
      tag_outitems_ttu,
      "Appendix",
      paste0(
        "TableST-4_RRxSEI-deltas_by_TimeRCPs_",
        c("1000km2", "percent"),
        ".csv"
      )
    )

    if (!all(file.exists(fname_tables_textvalues))) {
      list_extents <- "SCD1SEI2020"
      lvls_robustness <- c(
        "overall",
        rev(meta[["acrmod"]][["robust"]][["levels"]])
      )
      tag_vars <- c(RSL = "Resilience_response", RST = "Resistance_response")


      #--- Load deltas
      xsut2d <- prepare_table_from_files(
        tags = meta[["acrmod"]][["signal"]][["tag"]],
        patterns = paste0(
          meta[["varsets"]][["tags"]][[vtagk]],
          "__(.+)__simpledelta-acrmod-",
          meta[["acrmod"]][["signal"]][["tag"]]
        ),
        simslices = unlist(meta[["simexps"]][["tag_acrmod"]])[-1L],
        extent = list_extents[[1L]],
        variables = tag_vars,
        path1 = dir_data_rat,
        path2 = dir_res_tu,
        mask = data_mask_sets[[km]]
      )

      xsut2d[["robustness"]] <- lvls_robustness[[1L]]


      #--- Load robust deltas (only applicable to median)
      xsut2rd <- prepare_table_from_files(
        tags = meta[["acrmod"]][["signal"]][["tag"]][[2L]],
        patterns = paste0(
          meta[["varsets"]][["tags"]][[vtagk]],
          "__(.+)__robust090simpledelta-acrmod-",
          meta[["acrmod"]][["signal"]][["tag"]][[2L]]
        ),
        simslices = unlist(meta[["simexps"]][["tag_acrmod"]])[-1L],
        extent = list_extents[[1L]],
        variables = tag_vars,
        path1 = dir_data_rat,
        path2 = dir_res_tu,
        mask = data_mask_sets[[km]]
      )

      tmp <- strsplit(
        split = "_",
        x = as.character(xsut2rd[["Response"]]),
        fixed = TRUE
      )

      xsut2rd[["robustness"]] <- vapply(
        tmp,
        `[`, j = 1L,
        FUN.VALUE = NA_character_
      )

      xsut2rd[["Response"]] <- vapply(
        tmp,
        `[`, j = 2L,
        FUN.VALUE = NA_character_
      )


      #--- Combine "all" deltas and robust/nonrobust deltas
      xsut2 <- rbind(xsut2d, xsut2rd)

      xsut2[["robustness"]] <- factor(
        xsut2[["robustness"]],
        levels = lvls_robustness
      )


      # Clean up stuff
      xsut2[["SimSlice"]] <- gsub("\n", "_", xsut2[["SimSlice"]], fixed = TRUE)


      #--- ** Calculate tabulated response ------
      xsut2[["values_1e3km2"]] <- round(
        pixelcount_to_1e3km2(xsut2[["values"]])
      )

      xsut3 <- dplyr::group_by(
        xsut2,
        Var, SimSlice, SpatialUnit, acrmod_level
      ) |>
        dplyr::mutate(
          freq = values / sum(values)
        )


      # Format
      xtable <- format_manuscript_table_cats(
        x = xsut3,
        var = "values_1e3km2",
        cats = meta[["delta"]][["basic"]][["full"]][["levels"]],
        timevar = "Response",
        tags123 = meta[["acrmod"]][["signal"]][["tag"]],
        idvar = c("Var", "SimSlice", "SpatialUnit", "robustness"),
        lvls_robustness = lvls_robustness
      )

      dir.create(dirname(fname_tables_textvalues[[1L]]), recursive = TRUE, showWarnings = FALSE)
      write.csv(xtable, file = fname_tables_textvalues[[1L]], row.names = FALSE)


      #--- ** in-text values ------
      # Core Sagebrush Areas experienced the largest proportion proportional
      # declines in
      # resilience, 40% (32-49%) (= 54 / (54 + 80 + 1); = 67 / (67 + 68 + 1); = 43 / (43 + 90 + 3)), and
      # resistance, 52% (44-60%) (= 71 / (71 + 64 + 1); = 81 / (81 + 53 + 1); = 60 / (60 + 73 + 2)).

      # R&R declines in
      # resilience, 15% (14%-20%), and in (= 74 / (74 + 395 + 10); = 94 / (94 + 383 + 2); = 68 / (68 + 381 + 30))
      # resistance, 23% (20%-27%), (= 110 / (110 + 362 + 7); = 129 / (129 + 349 + 1); = 95 / (95 + 359 + 24))
      # are projected over only small proportions  of Other Rangelands.
    }
    #----- * ------
  }


  #----- . ------
  #--- MIRRP ------
  vtagk <- "mirrp"
  if (vtagk == "mirrp") {

    #--- * Table MIRRP extents ------
    fname_tables_textvalues <- file.path(
      dir_outitems,
      tag_outitems_ttu,
      "Appendix",
      paste0(
        "TableSM-1-MIRR_Overall_by_TimeRCPs_",
        c("1000km2", "percent"),
        ".csv"
      )
    )

    if (!all(file.exists(fname_tables_textvalues))) {
      list_extents <- "ecotype"
      tag_vars <- c(`MIRRP-RSL` = "Resilience_mirrp", `MIRRP-RST` = "Resistance_mirrp")


      #--- Load values files
      xsut2 <- prepare_table_from_files(
        tags = meta[["acrmod"]][["signal"]][["tag"]],
        patterns = paste0(
          meta[["varsets"]][["tags"]][[vtagk]],
          "__(.+)__value-acrmod-",
          meta[["acrmod"]][["signal"]][["tag"]],
          "__acrgeo-", list_extents[[1L]], "__"
        ),
        simslices = unlist(meta[["simexps"]][["tag_acrmod"]])[-1L],
        extent = list_extents[[1L]],
        variables = tag_vars,
        path1 = dir_data_rat,
        path2 = dir_res_tu,
        mask = data_mask_sets[[km]]
      )

      # Use short variable names
      xsut2[["Response2"]] <- xsut2[["Response"]]
      xsut2[["Response"]] <- factor(
        meta[["varsets"]][["preds"]][["varname"]][xsut2[["Response"]]],
        levels = meta[["varsets"]][["preds"]][["varname"]],
        labels = meta[["varsets"]][["preds"]][["short10"]]
      )


      # Clean up stuff
      xsut2[["SimSlice"]] <- gsub("\n", "_", xsut2[["SimSlice"]], fixed = TRUE)


      # Calculate tabulated response
      xsut2[["values_1e3km2"]] <- round(
        pixelcount_to_1e3km2(xsut2[["values"]])
      )

      # Format
      xtable <- format_manuscript_table_cats(
        x = as.data.frame(xsut2),
        var = "values_1e3km2",
        cats = meta[["varsets"]][["preds"]][["short10"]],
        tags123 = meta[["acrmod"]][["signal"]][["tag"]]
      )

      dir.create(dirname(fname_tables_textvalues[[1L]]), recursive = TRUE, showWarnings = FALSE)
      write.csv(xtable, file = fname_tables_textvalues[[1L]], row.names = FALSE)


      #--- ** Calculate tabulated response ------
      xsut3 <-
        dplyr::group_by(xsut2, Var, SimSlice, SpatialUnit, acrmod_level) |>
        dplyr::mutate(
          freq = round(100 * values / sum(values))
        )

      # Format
      xtable <- format_manuscript_table_cats(
        x = as.data.frame(xsut3),
        var = "freq",
        cats = meta[["varsets"]][["preds"]][["short10"]],
        tags123 = meta[["acrmod"]][["signal"]][["tag"]]
      )

      dir.create(dirname(fname_tables_textvalues[[2L]]), recursive = TRUE, showWarnings = FALSE)
      write.csv(xtable, file = fname_tables_textvalues[[2L]], row.names = FALSE)

      #--- ** in-text values ------
      # Annual temperature is the most impactful variable for explaining
      # resilience changes across 52% (59%-38%) of the area and
      # resistance changes across 53% (60%-41%) of the areas

      # Increasing temperature of the coldest month was the most impactful
      # variable over larger areas of
      # resistance (12 (17-8)) than
      # resilience (24 (22-25))
    }
    #----- * ------
  }


  #----- . ------
  #--- CONTINUOUS: PREDICTORS, RR-cont, RF-certainty ------
  for (vtagk in c("preds", "rr", "rfcertainty")) {

    #--- * Table values, deltas, and proportion of robust extent ------
    fname_tables_textvalues <- file.path(
      dir_outitems,
      tag_outitems_ttu,
      "Appendix",
      paste0(
        "TableSC_1-",
        vtagk,
        "-ValuesDeltasRobustness_Overall_by_TimeRCPs.csv"
      )
    )

    if (!all(file.exists(fname_tables_textvalues))) {
      list_extents <- "ecotype"
      tag_vars <- switch(EXPR = vtagk,
        preds = meta[["varsets"]][["preds"]][["varname"]],
        rr = c(RSL = "Resilience_index", RST = "Resistance_index"),
        rfcertainty = c(
          `RF-certainty (RSL)` = "Resilience_excessp",
          `RF-certainty (RST)` = "Resistance_excessp"
        )
      )

      #--- ** Prepare values ------
      #--- Prepare ambient files
      xsut2la <- prepare_table_from_files(
        tags = "med", # even though "med" doesn't exist for ambient
        patterns = paste0(
          meta[["varsets"]][["tags"]][[vtagk]],
          "__(.+)__value-sim_"
        ),
        simslices = meta[["simexps"]][["tag_scen1"]],
        extent = list_extents[[1L]],
        variables = tag_vars,
        path1 = dir_data_rat,
        path2 = dir_res_tu,
        mask = data_mask_sets[[km]]
      )

      #--- Load projected files
      xsut2lf <- prepare_table_from_files(
        tags = meta[["acrmod"]][["signal"]][["tag"]],
        patterns = paste0(
          meta[["varsets"]][["tags"]][[vtagk]],
          "__(.+)__value-acrmod-",
          meta[["acrmod"]][["signal"]][["tag"]]
        ),
        simslices = unlist(meta[["simexps"]][["tag_acrmod"]]),
        extent = list_extents[[1L]],
        variables = tag_vars,
        path1 = dir_data_rat,
        path2 = dir_res_tu,
        mask = data_mask_sets[[km]]
      )

      xsut2 <- rbind(xsut2la, xsut2lf)


      # Clean up stuff
      xsut2[["SimSlice"]] <- gsub("\n", "_", xsut2[["SimSlice"]], fixed = TRUE)

      xsut2[["values_signif"]] <- signif(xsut2[["values"]], digits = 3)

      # Format
      xtable_values <- format_manuscript_table_cats(
        x = xsut2,
        cats = gsub(".", "+", levels(xsut2[["Response"]]), fixed = TRUE),
        var = "values_signif",
        timevar = "Response",
        tags123 = meta[["acrmod"]][["signal"]][["tag"]]
      )


      #--- ** Prepare deltas ------
      #--- Load deltas
      xsut2 <- prepare_table_from_files(
        tags = meta[["acrmod"]][["signal"]][["tag"]],
        patterns = paste0(
          meta[["varsets"]][["tags"]][[vtagk]],
          "__(.+)__delta-acrmod-",
          meta[["acrmod"]][["signal"]][["tag"]]
        ),
        simslices = unlist(meta[["simexps"]][["tag_acrmod"]])[-1L],
        extent = list_extents[[1L]],
        variables = tag_vars,
        path1 = dir_data_rat,
        path2 = dir_res_tu,
        mask = data_mask_sets[[km]]
      )

      # Clean up stuff
      xsut2[["SimSlice"]] <- gsub("\n", "_", xsut2[["SimSlice"]], fixed = TRUE)

      xsut2[["values_signif"]] <- signif(xsut2[["values"]], digits = 3)

      # Format
      xtable_deltas <- format_manuscript_table_cats(
        x = xsut2,
        cats = gsub(".", "+", levels(xsut2[["Response"]]), fixed = TRUE),
        var = "values_signif",
        timevar = "Response",
        tags123 = meta[["acrmod"]][["signal"]][["tag"]]
      )

      #--- ** in-text values ------
      # Annual temperature is projected to increase by
      # 3.25 (2-4.06) under RCP 4.5 for 2064-2099 throughout the study area

      # The second most impactful variable was temperature of the coldest
      # month, which is projected to increase by
      # 3.06 (2.31-3.94) throughout the study area

      # climatic water deficit which is projected to
      # increase by 59.1 (33.1-82.9) throughout the study area

      # June-September precipitation, which is projected to increase by
      # 3.44 (-7.45-16.6) averaged across the study area



      #--- ** Prepare robustness ------
      #--- Load areas of robustness
      xsut2 <- prepare_table_from_files(
        tags = meta[["acrmod"]][["signal"]][["tag"]][[2L]],
        patterns = paste0(
          meta[["varsets"]][["tags"]][[vtagk]],
          "__(.+)__delta-acrmod-robust090__"
        ),
        simslices = unlist(meta[["simexps"]][["tag_acrmod"]])[-1L],
        extent = list_extents[[1L]],
        variables = unlist(tag_vars),
        path1 = dir_data_rat,
        path2 = dir_res_tu,
        mask = data_mask_sets[[km]]
      )

      # Clean up stuff
      xsut2[["SimSlice"]] <- gsub("\n", "_", xsut2[["SimSlice"]], fixed = TRUE)


      #--- ** Calculate tabulated response ------
      xsut2[["values_1e3km2"]] <- round(
        pixelcount_to_1e3km2(xsut2[["values"]])
      )

      xsut3 <- dplyr::group_by(
        xsut2,
        Var, SimSlice, SpatialUnit, acrmod_level
      ) |>
        dplyr::mutate(
          freq = values / sum(values)
        )


      # Format
      if (FALSE) {
        xtable_robustextent <- format_manuscript_table_cats(
          x = xsut3,
          var = "values_1e3km2",
          cats = meta$acrmod$robust[["levels"]],
          timevar = "Response",
          tags123 = meta[["acrmod"]][["signal"]][["tag"]][[2L]]
        )
      }

      # Format
      xtable_robustextent <- format_manuscript_table_cats(
        x = xsut3,
        var = "freq",
        cats = meta$acrmod$robust[["levels"]],
        timevar = "Response",
        tags123 = meta[["acrmod"]][["signal"]][["tag"]][[2L]]
      )


      #--- ** Combine values, deltas, robust extent ------
      tmp_vars <- c("Var", "SimSlice", "SpatialUnit")

      xtable_overall <- merge(
        x = xtable_values[, c(tmp_vars, "X50+"), drop = FALSE],
        y = xtable_deltas[, c(tmp_vars, "X50+"), drop = FALSE],
        by = tmp_vars,
        suffixes = c("_value", "_delta"),
        all = TRUE
      ) |>
        merge(
          y = xtable_robustextent[, c(tmp_vars, "freq.robust-med"), drop = FALSE],
          by = tmp_vars,
          all = TRUE
        )

      xtable_overall[["freq.robust-med"]] <- round(
        100 * xtable_overall[["freq.robust-med"]],
        digits = 0
      )

      xtable_overall <- reorder_by_var(
        xtable_overall,
        col = "Var",
        target = meta[["varsets"]][[vtagk]][["varname"]]
      )


      dir.create(dirname(fname_tables_textvalues[[1L]]), recursive = TRUE, showWarnings = FALSE)
      write.csv(xtable_overall, file = fname_tables_textvalues[[1L]], row.names = FALSE)
    }
    #----- * ------
  }



  #----- . ------
  #--- NOVELTY ------
  vtagk <- "novelty"
  if (vtagk == "novelty") {

    #--- * Table novelty values and extents ------
    fname_table_novelty <- file.path(
      dir_outitems,
      tag_outitems_ttu,
      "Appendix",
      paste0(
        "TableSN-1_novelty-ValuesExtents_Overall_by_TimeRCPs_1000km2.csv"
      )
    )

    if (!file.exists(fname_table_novelty)) {
      list_extents <- "ecotype"
      tag_ambient <- meta[["simexps"]][["tag_scen1"]]


      #--- ** Prepare values ------
      tag_vars <- c(
        `Novelty (NT1)` = "NT1",
        `Novelty (DI-ratio)` = "AOArdi071"
      )

      #--- Prepare ambient files
      xsut2la <- prepare_table_from_files(
        tags = "med", # even though "med" doesn't exist for ambient
        patterns = paste0(
          meta[["varsets"]][["tags"]][[vtagk]],
          "__(.+)__value-sim_"
        ),
        simslices = meta[["simexps"]][["tag_scen1"]],
        extent = list_extents[[1L]],
        variables = tag_vars,
        path1 = dir_data_rat,
        path2 = dir_res_tu,
        mask = data_mask_sets[[km]]
      )

      #--- Load projected files
      xsut2lf <- prepare_table_from_files(
        tags = meta[["acrmod"]][["signal"]][["tag"]],
        patterns = paste0(
          meta[["varsets"]][["tags"]][[vtagk]],
          "__(.+)__value-acrmod-",
          meta[["acrmod"]][["signal"]][["tag"]]
        ),
        simslices = unlist(meta[["simexps"]][["tag_acrmod"]]),
        extent = list_extents[[1L]],
        variables = tag_vars,
        path1 = dir_data_rat,
        path2 = dir_res_tu,
        mask = data_mask_sets[[km]]
      )

      xsut2 <- rbind(xsut2la, xsut2lf)


      # Clean up stuff
      xsut2[["SimSlice"]] <- gsub("\n", "_", xsut2[["SimSlice"]], fixed = TRUE)

      xsut2[["values_signif"]] <- signif(xsut2[["values"]], digits = 3)

      # Format
      xtable_values <- format_manuscript_table_cats(
        x = xsut2,
        cats = gsub(".", "+", levels(xsut2[["Response"]]), fixed = TRUE),
        var = "values_signif",
        timevar = "Response",
        tags123 = meta[["acrmod"]][["signal"]][["tag"]]
      )



      #--- ** Prepare extents ------
      tag_vars <- c(
        `Novelty (NT1)` = "NT1_isnovel",
        `Novelty (DI-ratio)` = "AOArdi071_isnovel"
      )

      #--- Prepare ambient files
      xsut2la <- prepare_table_from_files(
        tags = "med", # even though "med" doesn't exist for ambient
        patterns = paste0(
          meta[["varsets"]][["tags"]][[vtagk]],
          "__(.+)__value-sim_"
        ),
        simslices = meta[["simexps"]][["tag_scen1"]],
        extent = list_extents[[1L]],
        variables = tag_vars,
        path1 = dir_data_rat,
        path2 = dir_res_tu,
        mask = data_mask_sets[[km]]
      )

      #--- Load projected files
      xsut2lf <- prepare_table_from_files(
        tags = meta[["acrmod"]][["signal"]][["tag"]],
        patterns = paste0(
          meta[["varsets"]][["tags"]][[vtagk]],
          "__(.+)__value-acrmod-",
          meta[["acrmod"]][["signal"]][["tag"]]
        ),
        simslices = unlist(meta[["simexps"]][["tag_acrmod"]]),
        extent = list_extents[[1L]],
        variables = tag_vars,
        path1 = dir_data_rat,
        path2 = dir_res_tu,
        mask = data_mask_sets[[km]]
      )

      xsut2 <- rbind(xsut2la, xsut2lf)


      # Clean up stuff
      xsut2[["SimSlice"]] <- gsub("\n", "_", xsut2[["SimSlice"]], fixed = TRUE)

      # Calculate tabulated response
      xsut2[["values_1e3km2"]] <- round(
        pixelcount_to_1e3km2(xsut2[["values"]])
      )

      xsut2 <- dplyr::group_by(
        xsut2,
        Var, SimSlice, SpatialUnit, acrmod_level
      ) |>
        dplyr::mutate(
          freq = values / sum(values)
        )

      xsut2[["freqR"]] <- round(100 * xsut2[["freq"]], digits = 0)

      # Format
      xtable_extent <- format_manuscript_table_cats(
        x = xsut2,
        var = "freqR",
        cats = meta[["varsets"]][[vtagk]][["levels"]],
        tags123 = meta[["acrmod"]][["signal"]][["tag"]]
      )


      #--- ** Combine values, novel extent ------
      tmp_vars <- c("Var", "SimSlice", "SpatialUnit")

      xtable_extent[["Var"]] <- gsub("_isnovel", "", xtable_extent[["Var"]])

      xtable_overall <- merge(
        x = xtable_values[, c(tmp_vars, "X50+"), drop = FALSE],
        y = xtable_extent,
        by = tmp_vars,
        suffixes = c("_value", "_extent"),
        all = TRUE
      )


      dir.create(dirname(fname_table_novelty), recursive = TRUE, showWarnings = FALSE)
      write.csv(xtable_overall, file = fname_table_novelty, row.names = FALSE)
    }
    #----- * ------


  }
  #------ . -----
}

print(summary(warnings()))

#------. ------
#------. ------
